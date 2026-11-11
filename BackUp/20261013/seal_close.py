#!/usr/bin/env python3
"""
seal_close.py - closed (watertight) meshes of hauled-out seals from survey meshes, for volume and mass.

The survey mesh shows only the upper surface of a seal (nadir photogrammetry). This module rasterises the mesh
around each seal (1 cm DSM, highest surface per cell), fits a robust ground plane to a ring around the outline
(seal3d functions), takes cross-sections perpendicular to the principal axis of the outline every 1 cm and closes
each section below the observed top with one of four bottom models:

  flat    column between the DSM and the ground plane (upper bound; = V_hf of seal3d)
  ell     ellipse: width = outline chord, height = DSM maximum of the section, equator at H/2 (= V_ell of seal3d)
  mirror  the observed top above H/2 mirrored about the plane z = H/2 and clipped by the ground
  super   top = the observed DSM inside the outline; underside = half-ellipse from the equator z_eq = k*H down to a
          flat contact patch of half-width c*a on the ground (a = half chord of the outline; c = C_CONTACT = 0.65 by
          default; c = 0 is a line contact, c -> 1 a box).  k (and a shape exponent n) come from a superellipse
          |v/a|^n + |(z-z_eq)/(H-z_eq)|^n = 1 fitted to the observed top of the section: the equator is where the
          observed top meets the outline.  The volume for c = 0 and c = 0.85 is reported as well (the underside is not
          observed: this is the bracket).  V_super <= V_flat always.

The sections are swept into a closed triangle mesh (one ring of M vertices per section, strips between sections,
fans at the two ends).  Volume comes from the sections (Cavalieri: sum of section areas x 1 cm - exact for slices
perpendicular to one axis, whatever the bend of the body) and, as a check, from the signed volume of the mesh.

Volumes, masses and meshes are reported as reconstructed: no size fit of any kind (version 1.2 removed the optional size fit
to the observers' lines and the 'scale' flag of version 1.1; observers' lines are used only to check the outlines).
--s-vert divides heights and volumes by a known vertical scale factor s of the flight (f / f_ref); the default 1 means
no correction, as in the paper.  Mass = rho * V for rho = 1000 and 1020 kg/m3.

Usage:
  python seal_close.py <labelInput> [--variant super|mirror|ell|flat|all] [--s-vert 1.0]
                       [--crops DIR] [--out DIR] [--reg DX DY] [--no-ply] [--res 0.01]
  Expects <labelInput>/Predict/MeshCrop_wide/*.ply (or MeshCrop), Predict/SpP_LRG_level_2_level#2_<date>.kml and
  Predict/Measurements_3D_<date>.csv (optional: registration).
  Writes <out>/MeshClosed/<crop>.ply (chosen variant; --variant all writes <crop>_<variant>.ply),
         <out>/<date>_sealClosed.csv and .kml.
Written 2026-09 for the PV_PL pipeline (NPWC); needs seal3d.py (same folder), numpy, scipy; trimesh optional (mesh check).
"""
import os, re, sys, glob, math, json, csv, argparse, time
import numpy as np
from matplotlib.path import Path
from scipy.ndimage import map_coordinates
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import seal3d as S

__version__ = '1.2'
VARIANTS = ('flat', 'ell', 'mirror', 'super')
RHO = (1000.0, 1020.0)
N_RING = 64           # vertices per section ring in the swept mesh
MESH_STEP = 2         # every n-th 1-cm section goes into the mesh (volume in the CSV uses all sections)
K_EQ_MIRROR = 0.5     # equator height / H for the mirror variant
N_GRID = np.arange(1.25, 4.01, 0.25)   # superellipse exponent of the observed top (shape descriptor, places the equator)
K_GRID = np.arange(0.30, 0.801, 0.05)  # equator height / H
N_BOTTOM = 2.0        # exponent of the underside (half-ellipse from the equator to the contact patch)
C_CONTACT = 0.65      # contact half-width / outline half-width of the super variant
C_RANGE = (0.0, 0.85)  # sensitivity: line contact and wide contact
H_MIN = 0.03          # outlines whose DSM never rises above this (m) get status 'no_height' (nothing to close)


# ---------------------------------------------------------------------------------------------------- geometry
def pca_frame(ring):
    c = ring[:-1].mean(0)
    ev, vec = np.linalg.eigh(np.cov((ring[:-1] - c).T))
    ax = vec[:, 1]
    ax = ax if ax[0] >= 0 else -ax
    pv = np.array([-ax[1], ax[0]])
    return c, ax, pv


def chords(ring_uv, u):
    """intersections of the closed polyline ring_uv (n,2) with the line u = const -> sorted v values (even count)"""
    a, b = ring_uv[:-1], ring_uv[1:]
    du = b[:, 0] - a[:, 0]
    m = ((a[:, 0] <= u) & (b[:, 0] > u)) | ((b[:, 0] <= u) & (a[:, 0] > u))
    t = (u - a[m, 0]) / np.where(du[m] == 0, 1e-12, du[m])
    v = a[m, 1] + t * (b[m, 1] - a[m, 1])
    return np.sort(v)


def superellipse_fit(eta, zeta):
    """fit exponent n and equator fraction k of the top profile zeta = z/H at eta = v/a (|eta|<=1):
    z/H = k + (1-k) (1 - |eta|^n)^(1/n).  Grid search; returns n, k, rms."""
    best = (2.0, 0.5, np.inf)
    ae = np.abs(eta)
    for n in N_GRID:
        f = np.clip(1 - ae ** n, 0, None) ** (1.0 / n)
        for k in K_GRID:
            r = zeta - (k + (1 - k) * f)
            e = float(np.mean(r * r))
            if e < best[2]:
                best = (float(n), float(k), e)
    return best[0], best[1], math.sqrt(best[2])


def section_polygon(v, ztop, variant, res=0.01, fit=None):
    """closed polygon (M,2) in the (v,z) plane of one section and its area.
    v: chord samples (n,), ztop: observed height above ground (n,), both from edge to edge."""
    n = len(v)
    H = float(ztop.max()) if n else 0.0
    if n < 3 or H <= 0:
        return None, 0.0, {}
    v0, v1 = float(v[0]), float(v[-1])
    a = 0.5 * (v1 - v0)
    vc = 0.5 * (v0 + v1)
    info = {'H': H, 'W': v1 - v0}
    if variant == 'flat':
        top = np.column_stack([v, np.clip(ztop, 0, None)])
        bot = np.column_stack([v[::-1], np.zeros(n)])
    elif variant == 'ell':
        eta = np.clip((v - vc) / max(a, 1e-6), -1, 1)
        f = np.sqrt(np.clip(1 - eta ** 2, 0, None))
        top = np.column_stack([v, 0.5 * H + 0.5 * H * f])
        bot = np.column_stack([v[::-1], (0.5 * H - 0.5 * H * f)[::-1]])
    elif variant == 'mirror':
        zeq = K_EQ_MIRROR * H
        zt = np.clip(ztop, 0, None)
        zb = np.minimum(zt, np.clip(2 * zeq - zt, 0, None))
        keep = zt >= zeq
        if keep.sum() < 2:
            return None, 0.0, info
        i0, i1 = np.where(keep)[0][[0, -1]]
        top = np.column_stack([v[i0:i1 + 1], zt[i0:i1 + 1]])
        bot = np.column_stack([v[i0:i1 + 1][::-1], zb[i0:i1 + 1][::-1]])
        info['W_eq'] = float(v[i1] - v[i0])
    elif variant == 'super':
        eta = np.clip((v - vc) / max(a, 1e-6), -1, 1)
        zt = np.clip(ztop, 0, None)
        zeta = zt / H
        if fit is None:
            use = zeta > 0.15                     # the lowest part of the observed top is the photogrammetric skirt
            nn, kk, rms = superellipse_fit(eta[use], zeta[use]) if use.sum() >= 5 else (2.0, 0.5, np.nan)
        else:
            nn, kk, rms = fit
        zeq = kk * H
        fb = np.clip(1 - np.abs(eta) ** N_BOTTOM, 0, None) ** (1.0 / N_BOTTOM)
        def bottom(cc):                           # underside: half-ellipse from the equator reaching the ground at |eta| = cc
            d = zeq / max((1 - cc ** N_BOTTOM) ** (1.0 / N_BOTTOM), 1e-6)
            return np.clip(zeq - d * fb, 0, None)
        botc = bottom(C_CONTACT)
        top = np.column_stack([v, np.maximum(zt, botc)]); bot = np.column_stack([v[::-1], botc[::-1]])
        info.update(n=nn, k=kk, rms=rms, edge=0.5 * (zeta[0] + zeta[-1]), asym=abs(zeta[0] - zeta[-1]))
        for cc, key in zip(C_RANGE, ('area_c0', 'area_c85')):
            bb = bottom(cc)
            info[key] = float(np.trapezoid(np.clip(zt - bb, 0, None), v))
    else:
        raise ValueError(variant)
    poly = np.vstack([top, bot])
    x, y = poly[:, 0], poly[:, 1]
    area = 0.5 * abs(np.dot(x, np.roll(y, -1)) - np.dot(y, np.roll(x, -1)))
    return poly, float(area), info


def resample_ring(poly, m):
    """resample a closed polygon to m points by arc length, starting at poly[0]"""
    p = np.vstack([poly, poly[:1]])
    d = np.r_[0, np.cumsum(np.hypot(*np.diff(p, axis=0).T))]
    if d[-1] <= 0:
        return np.repeat(poly[:1], m, axis=0)
    t = np.linspace(0, d[-1], m, endpoint=False)
    return np.column_stack([np.interp(t, d, p[:, 0]), np.interp(t, d, p[:, 1])])


def sweep_mesh(rings3d, apex0, apex1):
    """rings3d: list of (M,3) arrays (same M, consistent start/orientation); apex0/apex1: (3,) end points.
    Returns vertices (N,3), faces (F,3) of a closed mesh."""
    M = rings3d[0].shape[0]
    K = len(rings3d)
    V = np.vstack([apex0[None, :], np.vstack(rings3d), apex1[None, :]])
    F = []
    for k in range(K - 1):
        b0, b1 = 1 + k * M, 1 + (k + 1) * M
        for i in range(M):
            j = (i + 1) % M
            F.append((b0 + i, b1 + i, b1 + j))
            F.append((b0 + i, b1 + j, b0 + j))
    b0 = 1
    for i in range(M):
        j = (i + 1) % M
        F.append((0, b0 + i, b0 + j))
    b1 = 1 + (K - 1) * M
    last = len(V) - 1
    for i in range(M):
        j = (i + 1) % M
        F.append((last, b1 + j, b1 + i))
    return V, np.array(F, dtype=np.int64)


def signed_volume(V, F):
    T = V[F]
    return float(np.einsum('ij,ij->i', T[:, 0], np.cross(T[:, 1], T[:, 2])).sum() / 6.0)


def mesh_checks(V, F):
    """watertight / manifold checks without trimesh: every edge in exactly two faces with opposite orientation"""
    e = np.vstack([F[:, [0, 1]], F[:, [1, 2]], F[:, [2, 0]]])
    N = len(V)
    key = e[:, 0] * N + e[:, 1]
    rkey = e[:, 1] * N + e[:, 0]
    ks, cnt = np.unique(key, return_counts=True)
    dup = int((cnt > 1).sum())                      # an edge used twice in the same direction
    opp = np.isin(key, rkey)                        # each directed edge must have its opposite
    boundary = int((~opp).sum())
    und = np.unique(np.minimum(e[:, 0], e[:, 1]) * N + np.maximum(e[:, 0], e[:, 1]))
    euler = N - len(und) + len(F)
    out = {'watertight': bool(boundary == 0 and dup == 0), 'boundary_edges': boundary, 'euler': int(euler)}
    try:
        import trimesh
        m = trimesh.Trimesh(V, F, process=False)
        out['trimesh_watertight'] = bool(m.is_watertight)
        out['trimesh_volume'] = float(m.volume)
    except Exception:
        pass
    return out


# ---------------------------------------------------------------------------------------------------- core
def close_from_field(H, GX, GY, ring, pl, res=0.01, variants=VARIANTS, build_mesh=True, scale=(1.0, 1.0, 1.0)):
    """H: height above the ground plane on the grid (GX, GY); ring: outline (closed, local XY); pl: ground plane.
    scale = (sx, sy, sz): optional scale factors along the axis, across it and vertically (applied to the mesh and volumes;
    run_flight uses (1, 1, 1 / s_vert), i.e. no size fit).
    Returns dict: per variant volume (m3, scaled), mesh (V, F) if build_mesh, diagnostics."""
    c, ax, pv = pca_frame(ring)
    ruv = np.column_stack([(ring[:, 0] - c[0]) * ax[0] + (ring[:, 1] - c[1]) * ax[1],
                           (ring[:, 0] - c[0]) * pv[0] + (ring[:, 1] - c[1]) * pv[1]])
    u0, u1 = ruv[:, 0].min(), ruv[:, 0].max()
    us = np.arange(u0 + 0.5 * res, u1, res)
    x0, y0 = GX[0, 0] - 0.5 * res, GY[0, 0] - 0.5 * res
    Hc = np.clip(np.nan_to_num(H, nan=0.0), 0, None)
    sx, sy, sz = scale
    out = {v: {'area': [], 'polys': [], 'u': []} for v in variants}
    n_multi = 0
    fits = []
    W_prof, H_prof, mids, u_used = [], [], [], []
    for u in us:
        vs = chords(ruv, u)
        if len(vs) < 2:
            continue
        if len(vs) > 2:
            n_multi += 1
        va, vb = vs[0], vs[-1]
        if vb - va < 2 * res:
            continue
        nv = max(3, int(round((vb - va) / res)) + 1)
        v = np.linspace(va, vb, nv)
        px = c[0] + u * ax[0] + v * pv[0]
        py = c[1] + u * ax[1] + v * pv[1]
        z = map_coordinates(Hc, [(py - y0) / res - 0.5, (px - x0) / res - 0.5], order=1, mode='nearest')
        z = np.clip(z, 0, None)
        if z.max() <= 0:
            continue
        W_prof.append(vb - va); H_prof.append(z.max()); mids.append(0.5 * (va + vb)); u_used.append(u)
        fit = None
        for var in variants:
            poly, area, info = section_polygon(v, z, var, res, fit=None)
            if poly is None:
                continue
            if var == 'super':
                fits.append((info['n'], info['k'], info['rms'], info['edge'], info['asym']))
                out[var].setdefault('area_c0', []).append(info['area_c0']); out[var].setdefault('area_c85', []).append(info['area_c85'])
            out[var]['area'].append(area)
            out[var]['polys'].append(poly)
            out[var]['u'].append(u)
    # curved midline length (chord midpoints smoothed over 5 cm), comparable with the observers' length lines
    L_mid = float(u1 - u0)
    if len(mids) > 5:
        mm = np.convolve(np.pad(np.array(mids), 2, mode='edge'), np.ones(5) / 5, mode='valid')
        L_mid = float(np.sum(np.hypot(np.diff(np.array(u_used)), np.diff(mm))) + res)
    res_out = {'n_sections': len(us), 'n_multi_chord': n_multi, 'L_axis': float(u1 - u0), 'L_mid': L_mid, 'H_max': float(max(H_prof)) if H_prof else 0.0,
               'W_max': float(max(W_prof)) if W_prof else 0.0, 'scale': scale}
    if fits:
        fa = np.array(fits)
        res_out['super_n_med'] = float(np.median(fa[:, 0])); res_out['super_k_med'] = float(np.median(fa[:, 1])); res_out['super_rms_med'] = float(np.nanmedian(fa[:, 2]))
        res_out['edge_frac_med'] = float(np.median(fa[:, 3])); res_out['asym_med'] = float(np.median(fa[:, 4]))
    for var in variants:
        A = np.array(out[var]['area'])
        vol = float(A.sum() * res) * sx * sy * sz
        rec = {'V': vol, 'n_sec': int(len(A))}
        if var == 'super':
            rec['V_c0'] = float(np.sum(out[var].get('area_c0', [])) * res) * sx * sy * sz; rec['V_c85'] = float(np.sum(out[var].get('area_c85', [])) * res) * sx * sy * sz
        if build_mesh and len(A) >= 2:
            rings = []
            sel = list(range(0, len(out[var]['u']), MESH_STEP))
            if sel[-1] != len(out[var]['u']) - 1:
                sel.append(len(out[var]['u']) - 1)
            for u, poly in ((out[var]['u'][k], out[var]['polys'][k]) for k in sel):
                r2 = resample_ring(poly, N_RING)
                vv, zz = r2[:, 0] * sy, r2[:, 1] * sz
                uu = u * sx
                X = c[0] + uu * ax[0] + vv * pv[0]
                Y = c[1] + uu * ax[1] + vv * pv[1]
                Z = S.plane_eval(pl, X, Y) + zz
                rings.append(np.column_stack([X, Y, Z]))
            def apex(u, poly):                        # end point of the body, on the ground plane
                vc = 0.5 * (poly[:, 0].min() + poly[:, 0].max()) * sy
                X = c[0] + u * sx * ax[0] + vc * pv[0]; Y = c[1] + u * sx * ax[1] + vc * pv[1]
                return np.array([X, Y, S.plane_eval(pl, X, Y)])
            a0 = apex(out[var]['u'][0] - 0.5 * res, out[var]['polys'][0]); a1 = apex(out[var]['u'][-1] + 0.5 * res, out[var]['polys'][-1])
            V, F = sweep_mesh(rings, a0, a1)
            sv = signed_volume(V, F)
            if sv < 0:
                F = F[:, [0, 2, 1]]; sv = -sv
            rec['mesh'] = (V, F); rec['V_mesh'] = sv
        res_out[var] = rec
    return res_out


def height_field(ring, others, mesh, res=0.01, margin=0.6, ring_d=(0.20, 0.45), other_buf=0.06):
    """DSM, robust ground plane and height field around one outline (seal3d conventions).  Returns dict or None."""
    ring = S.simplify_ring(ring, 0.003)
    if len(ring) < 5 or S.poly_area(ring) < 0.01:
        return None
    xmin, ymin = ring.min(0) - margin; xmax, ymax = ring.max(0) + margin
    nx = int(math.ceil((xmax - xmin) / res)); ny = int(math.ceil((ymax - ymin) / res))
    q = mesh.query(xmin, ymin, xmax, ymax)
    if q is None or q[0] is None:
        return None
    Z, _ = S.rasterize(q[0], q[1], None, xmin, ymin, nx, ny, res)
    gx = xmin + (np.arange(nx) + 0.5) * res; gy = ymin + (np.arange(ny) + 0.5) * res
    GX, GY = np.meshgrid(gx, gy); P = np.column_stack([GX.ravel(), GY.ravel()])
    seal = Path(ring).contains_points(P).reshape(ny, nx)
    perim = float(np.sum(np.hypot(*np.diff(ring, axis=0).T)))
    cand = np.where(~seal.ravel())[0]
    d_own = np.full(P.shape[0], np.inf); pos = np.zeros(P.shape[0])
    d_own[cand], pos[cand] = S.dist_to_segments(P[cand, 0], P[cand, 1], ring, return_pos=True)
    d_own = d_own.reshape(ny, nx); pos = pos.reshape(ny, nx)
    excl = np.zeros(P.shape[0], bool)
    for o in others:
        o = S.simplify_ring(o, 0.003)
        if o[:, 0].max() < xmin or o[:, 0].min() > xmax or o[:, 1].max() < ymin or o[:, 1].min() > ymax:
            continue
        ins = Path(o).contains_points(P)
        c2 = np.where(~ins & (P[:, 0] > o[:, 0].min() - other_buf) & (P[:, 0] < o[:, 0].max() + other_buf) &
                      (P[:, 1] > o[:, 1].min() - other_buf) & (P[:, 1] < o[:, 1].max() + other_buf))[0]
        if len(c2):
            ins[c2[S.dist_to_segments(P[c2, 0], P[c2, 1], o) < other_buf]] = True
        excl |= ins
    excl = excl.reshape(ny, nx)
    valid = np.isfinite(Z)
    gmask = (~seal) & (d_own >= ring_d[0]) & (d_own <= ring_d[1]) & (~excl) & valid
    if gmask.sum() < 150:
        return {'status': 'no_ground', 'ring': ring}
    pl, gi = S.fit_ground_sectors(GX, GY, Z, gmask, pos, perim)
    if pl is None:
        return {'status': 'no_ground', 'ring': ring}
    H = Z.astype(np.float64) - S.plane_eval(pl, GX, GY)
    Hs = np.where(valid, H, np.nan)
    for _ in range(6):                                   # fill mesh holes by neighbour means
        miss = ~np.isfinite(Hs)
        if not miss.any():
            break
        Hp = np.pad(Hs, 1, constant_values=np.nan)
        stack = np.stack([Hp[dy:dy + ny, dx:dx + nx] for dy in range(3) for dx in range(3)])
        fin = np.isfinite(stack); cnt = fin.sum(0)
        mm = np.where(cnt > 0, np.where(fin, stack, 0).sum(0) / np.maximum(cnt, 1), np.nan)
        Hs[miss] = mm[miss]
    Hs = np.nan_to_num(Hs, nan=0.0)
    return {'status': 'ok', 'H': Hs, 'GX': GX, 'GY': GY, 'seal': seal, 'plane': pl, 'ring': ring, 'cover': float((seal & valid).sum() / max(1, seal.sum())),
            'ground_mad': gi['cells_mad'], 'ground_sectors': gi['n_sectors_inlier'], 'slope_deg': gi['slope_deg'], 'frac_below': float(np.mean(H[seal & valid] < -0.02)) if (seal & valid).any() else np.nan}


# ---------------------------------------------------------------------------------------------------- observers
def read_lines(path, shift=(0.0, 0.0)):
    out = []
    if not path or not os.path.exists(path):
        return out
    for pm in S.read_kml(path):
        for ln in pm['lines']:
            if len(ln) < 2:
                continue
            xy = S.lonlat_to_local(ln) + np.asarray(shift)
            out.append({'xy': xy, 'L': float(np.sum(np.hypot(*np.diff(xy, axis=0).T)))})
    return out


def match_lines(recs, lines):
    """one-to-one: a line belongs to an outline if >= 50 % of its resampled points are inside; largest fraction first"""
    if not lines:
        return {}
    cents = np.array([r['ring'].mean(0) for r in recs])
    cand = []
    for li, ln in enumerate(lines):
        xy = ln['xy']; d = np.r_[0, np.cumsum(np.hypot(*np.diff(xy, axis=0).T))]
        t = np.linspace(0, d[-1], 25)
        q = np.column_stack([np.interp(t, d, xy[:, 0]), np.interp(t, d, xy[:, 1])])
        mid = q[12]
        near = np.where(np.hypot(cents[:, 0] - mid[0], cents[:, 1] - mid[1]) < 2.5)[0]
        for j in near:
            fr = Path(recs[j]['ring']).contains_points(q).mean()
            if fr >= 0.5:
                cand.append((fr, li, int(j)))
    cand.sort(reverse=True)
    ul, uj, out = set(), set(), {}
    for fr, li, j in cand:
        if li in ul or j in uj:
            continue
        ul.add(li); uj.add(j); out[j] = (lines[li]['L'], fr)
    return out


# ---------------------------------------------------------------------------------------------------- flight
def write_ply(path, V, F):
    with open(path, 'wb') as f:
        f.write(('ply\nformat binary_little_endian 1.0\ncomment seal_close %s closed seal mesh\nelement vertex %d\nproperty float x\nproperty float y\n'
                 'property float z\nelement face %d\nproperty list uchar int vertex_indices\nend_header\n' % (__version__, len(V), len(F))).encode())
        f.write(np.ascontiguousarray(V, dtype='<f4').tobytes())
        fa = np.zeros(len(F), dtype=[('n', 'u1'), ('i', '<i4', 3)])
        fa['n'] = 3; fa['i'] = F
        f.write(fa.tobytes())


def row_flags(r):
    """quality flags of one CSV row (dict): '' when clean, else ';'-joined names"""
    f = []
    def g(k):
        try:
            v = float(r.get(k, np.nan)); return v
        except (TypeError, ValueError):
            return np.nan
    if g('H_max_m') < 0.10:
        f.append('low')                                          # body lower than 10 cm: not reconstructed, not a seal or a misplaced outline (seal3d 'flat')
    if g('edge_asym') > 0.5:
        f.append('asym')                                         # DSM heights at the two outline edges differ by > 50 % of H: outline off the body, animal on its side or on a slope
    if g('n_sections') > 0 and g('n_multi_chord') / g('n_sections') > 0.10:
        f.append('bent')                                         # concave outline: strongly bent or merged animals
    if g('ground_sectors') < 6 or g('ground_mad_m') > 0.06:
        f.append('ground')                                       # ground seen on too few sides or rough (seal3d 'ground_partial' / 'ground_rough')
    if g('mesh_cover') < 0.95:
        f.append('holes')                                        # mesh missing under the outline (seal3d 'mesh_gap')
    if g('frac_below_ground') > 0.03:
        f.append('below')                                        # outline partly below the ground plane (seal3d 'below_ground')
    vm, vf = g('V_mesh_m3'), g('V_m3')
    if np.isfinite(vm) and np.isfinite(vf) and vf > 0 and abs(vm / vf - 1) > 0.03:
        f.append('mesh_dev')                                     # swept mesh volume differs from the section volume by > 3 % (twisted sweep)
    return ';'.join(f)


def flight_paths(labelInput, crops=None):
    bs = os.path.basename(os.path.normpath(labelInput)); date1 = bs[:15]
    pred = os.path.join(labelInput, 'Predict')
    if crops is None:
        for cnd in (os.path.join(pred, 'MeshCrop_wide'), os.path.join(pred, 'MeshCrop'), os.path.join(labelInput, 'MeshCrop')):
            if glob.glob(os.path.join(cnd, '*.ply')):
                crops = cnd; break
    kml = glob.glob(os.path.join(pred, 'SpP_LRG_level_2*.kml')) or glob.glob(os.path.join(labelInput, 'SpP_LRG_level_2*.kml'))
    s3d = os.path.join(pred, 'Measurements_3D_%s.csv' % date1)
    return {'bs': bs, 'date1': date1, 'pred': pred, 'crops': crops, 'kml': kml[0] if kml else None, 'seal3d_csv': s3d if os.path.exists(s3d) else None}


def registration_from_seal3d(path):
    with open(path, newline='') as f:
        for row in csv.DictReader(f):
            try:
                return float(row.get('reg_dx_m') or 0), float(row.get('reg_dy_m') or 0)
            except ValueError:
                return 0.0, 0.0
    return 0.0, 0.0


def run_flight(labelInput, variant='super', s_vert=1.0, crops=None, out_dir=None, reg=None, write_meshes=True, res=0.01, verbose=True):
    fp = flight_paths(labelInput, crops)
    bs, date1 = fp['bs'], fp['date1']
    out_dir = out_dir or fp['pred']
    os.makedirs(out_dir, exist_ok=True)
    if fp['kml'] is None or fp['crops'] is None:
        print('skip (no level-2 kml or crops):', labelInput); return None
    recs = S.load_outlines(fp['kml'])
    if reg is None:
        reg = registration_from_seal3d(fp['seal3d_csv']) if fp['seal3d_csv'] else (0.0, 0.0)
    sh = np.asarray(reg, float)
    for r in recs:
        r['ring'] = r['ring'] + sh
    cents = np.array([r['ring'].mean(0) for r in recs])
    plys = sorted(glob.glob(os.path.join(fp['crops'], '*.ply')))
    parts = []
    for p in plys:
        try:
            parts.append(S.MeshPart(p))
        except Exception:
            pass
    pc = np.array([[0.5 * (p.bbox[0] + p.bbox[2]), 0.5 * (p.bbox[1] + p.bbox[3])] for p in parts])
    vs = list(VARIANTS) if variant == 'all' else [variant]        # meshes written for these; volumes always for all
    t0 = time.time()
    rows, meshes = [], {}
    # pass 1: raw measurements
    for i, r in enumerate(recs):
        ring = r['ring']
        row = {'seal_id': '%s_%03d' % (date1, i + 1), 'img': r['data'].get('img', ''), 'blob': r['data'].get('blob', ''), 'status': 'ok', 'crop': ''}
        if len(parts):
            d = np.hypot(pc[:, 0] - cents[i][0], pc[:, 1] - cents[i][1])
            j = int(np.argmin(d))
            near = [k for k in np.argsort(d)[:4] if d[k] < 1.5]
            mesh = S.MeshSet([parts[k] for k in (near or [j])])
            row['crop'] = os.path.basename(parts[j].path)
        else:
            row['status'] = 'no_mesh'; rows.append(row); continue
        others = [recs[k]['ring'] for k in range(len(recs)) if k != i and np.hypot(*(cents[k] - cents[i])) < 4.0]
        try:
            hf = height_field(ring, others, mesh, res=res)
        except Exception as ex:
            hf = None; row['status'] = 'error:%s' % type(ex).__name__
        if hf is None or hf.get('status') != 'ok':
            row['status'] = (hf or {}).get('status', row['status'] if row['status'] != 'ok' else 'no_mesh'); rows.append(row); continue
        try:
            cl = close_from_field(hf['H'], hf['GX'], hf['GY'], hf['ring'], hf['plane'], res=res, build_mesh=False)
        except Exception as ex:
            row['status'] = 'error:%s' % type(ex).__name__; rows.append(row); continue
        row.update({'L_axis_m': cl['L_axis'], 'L_mid_m': cl['L_mid'], 'W_max_m': cl['W_max'], 'H_max_m': cl['H_max'], 'n_sections': cl['n_sections'], 'n_multi_chord': cl['n_multi_chord'],
                    'super_n': cl.get('super_n_med', np.nan), 'super_k': cl.get('super_k_med', np.nan), 'super_rms_m': cl.get('super_rms_med', np.nan),
                    'edge_frac': cl.get('edge_frac_med', np.nan), 'edge_asym': cl.get('asym_med', np.nan),
                    'mesh_cover': hf['cover'], 'ground_mad_m': hf['ground_mad'], 'ground_sectors': hf['ground_sectors'], 'slope_deg': hf['slope_deg'], 'frac_below_ground': hf['frac_below']})
        if cl['H_max'] < H_MIN or cl['super']['n_sec'] < 3:
            row['status'] = 'no_height'; rows.append(row); continue
        for var in VARIANTS:
            row['V_%s_raw' % var] = cl[var]['V']
        row['V_super_c0_raw'] = cl['super']['V_c0']; row['V_super_c85_raw'] = cl['super']['V_c85']
        row['_hf'] = hf
        rows.append(row)
    # pass 2: volumes as reconstructed (optionally divided by a known vertical scale s), mass, closed meshes
    sz = 1.0 / float(s_vert) if s_vert else 1.0
    chosen = variant if variant in VARIANTS else 'super'
    mesh_dir = os.path.join(out_dir, 'MeshClosed')
    if write_meshes:
        os.makedirs(mesh_dir, exist_ok=True)
    n_ok, n_wt = 0, 0
    for r in rows:
        hf = r.pop('_hf', None)
        r['s_vert'] = float(s_vert)
        if r['status'] != 'ok':
            continue
        n_ok += 1
        r['V_m3'] = r['V_%s_raw' % chosen] * sz; r['variant'] = chosen
        for rho in RHO:
            r['M_%d_kg' % int(rho)] = rho * r['V_m3']
        if write_meshes and hf is not None:
            try:
                cl = close_from_field(hf['H'], hf['GX'], hf['GY'], hf['ring'], hf['plane'], res=res, variants=vs, build_mesh=True, scale=(1.0, 1.0, sz))
                for var in vs:
                    if 'mesh' not in cl[var]:
                        continue
                    V, F = cl[var]['mesh']
                    chk = mesh_checks(V, F)
                    if var == chosen:
                        r['V_mesh_m3'] = cl[var]['V_mesh']; r['watertight'] = chk['watertight']; r['n_faces'] = int(len(F)); n_wt += int(chk['watertight'])
                    name = os.path.splitext(r['crop'])[0] + ('' if variant != 'all' else '_' + var) + '.ply'
                    write_ply(os.path.join(mesh_dir, name), V, F)
                    r['ply'] = name if var == chosen else r.get('ply', '')
            except Exception as ex:
                r['watertight'] = False; r['status'] = 'mesh_error:%s' % type(ex).__name__
    # write CSV
    for r in rows:
        r['flags'] = row_flags(r) if r['status'] == 'ok' else ''
    cols = ['seal_id', 'img', 'blob', 'crop', 'status', 'flags', 'variant', 'L_axis_m', 'L_mid_m', 'W_max_m', 'H_max_m', 's_vert',
            'V_m3', 'V_mesh_m3', 'M_1000_kg', 'M_1020_kg'] + ['V_%s_raw' % v for v in list(VARIANTS) + ['super_c0', 'super_c85']] + \
           ['super_n', 'super_k', 'super_rms_m', 'edge_frac', 'edge_asym', 'n_sections', 'n_multi_chord', 'mesh_cover', 'ground_mad_m', 'ground_sectors', 'slope_deg', 'frac_below_ground',
            'watertight', 'n_faces', 'ply']
    csv_path = os.path.join(out_dir, '%s_sealClosed.csv' % date1)
    with open(csv_path, 'w', newline='') as f:
        w = csv.writer(f); w.writerow(cols)
        for r in rows:
            w.writerow([('%.6g' % r[c]) if isinstance(r.get(c), float) else r.get(c, '') for c in cols])
    # KML with volume labels
    try:
        kml_path = os.path.join(out_dir, '%s_sealClosed.kml' % date1)
        with open(kml_path, 'w') as f:
            f.write('<?xml version="1.0" encoding="UTF-8"?>\n<kml xmlns="http://www.opengis.net/kml/2.2"><Document>\n')
            for r, rec in zip(rows, recs):
                if r['status'] != 'ok':
                    continue
                cx, cy = rec['ring'].mean(0) - sh
                lon, lat = S.utm_to_lonlat(np.array([cx + S.SHIFT[0]]), np.array([cy + S.SHIFT[1]]))
                f.write('<Placemark><name>V %.1f L, M %.0f kg</name><Point><coordinates>%.8f,%.8f,0</coordinates></Point></Placemark>\n' % (
                    1000 * r['V_m3'], r['M_1000_kg'], lon[0], lat[0]))
            f.write('</Document></kml>\n')
    except Exception:
        pass
    if verbose:
        print('%s: %d outlines, %d measured, %d watertight meshes, s_vert %.3f, %.0f s -> %s' % (
            bs, len(recs), n_ok, n_wt, s_vert, time.time() - t0, csv_path))
    return rows


def main(argv=None):
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument('labelInput')
    ap.add_argument('--variant', default='super', choices=list(VARIANTS) + ['all'])
    ap.add_argument('--s-vert', type=float, default=1.0)
    ap.add_argument('--crops', default=None)
    ap.add_argument('--out', default=None)
    ap.add_argument('--reg', nargs=2, type=float, default=None, metavar=('DX', 'DY'))
    ap.add_argument('--no-ply', action='store_true')
    ap.add_argument('--res', type=float, default=0.01)
    a = ap.parse_args(argv)
    run_flight(a.labelInput, variant=a.variant, s_vert=a.s_vert, crops=a.crops, out_dir=a.out, reg=tuple(a.reg) if a.reg else None,
               write_meshes=not a.no_ply, res=a.res)


if __name__ == '__main__':
    main()
