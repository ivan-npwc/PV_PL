# -*- coding: utf-8 -*-
"""
seal3d.py  -  3D morphometry of hauled-out seals from UAV photogrammetry
===========================================================================
PV_PL / LRG pipeline, step after LRG_PredsToPolygons_level_2.r and
Agisoft_export_model.r (replaces LRG_3d_CROP.r + LRG_3d_Measurements.r).
Pure numpy + matplotlib (Path, optional plots): runs in any Python 3
(Windows r-reticulate env, Linux, WSL) without GDAL, scipy, Open3D or Rvcg.

Inputs per flight folder (labelInput):
  <bs>_Model/<bs>_Model_N.ply   mesh tiles from <bs>_Model_no_water.psx exported
                                by Agisoft_export_model.r (EPSG:32610,
                                shift 435000 / 5451000 / -100)
  Predict/SpP_LRG_level_2_level#2_<date>.kml   seal outlines (WGS84)

Per flight
  0. Registration check: the XY shift of all outlines that maximises the
     height contrast (DSM inside outline minus DSM in a ring around it),
     summed over up to 40 seals (FFT correlation, +-1 m). Applied if >= 2 cm.
Per seal
  1. Rasterise the mesh around the seal to a max-Z surface (DSM, 1 cm cells)
     = the surface a nadir camera sees.
  2. Ground: ring 20-45 cm outside the outline (neighbouring outlines + 6 cm
     excluded), split into 12 sectors along the outline; each sector gives
     one point (mean XY, median Z); Tukey-IRLS plane through the sector points.
     The 20 cm skip avoids the photogrammetric "skirt" around the body (mesh
     still 1-4 cm above ground 10-20 cm from the outline).
  3. Height map H = DSM - ground plane inside the outline (clipped at 0).
  4. Centre line: cubic v = f(u) in the PCA frame of the outline, clipped to
     the outline; stations every 1 cm along it.
  5. At each station (normal to the centre line): width (outline), dorsal
     height (max H), cross-section area (integral of H), dorsal arc.
  6. Outputs: L_2D (centre line length - comparable with observer lengths),
     L_axis (straight PCA extent), L_3D_dorsal (over the back), W/H maxima and
     at 10..90 % of length, V_hf (height-field volume above the ground plane,
     includes the unseen undercut), V_ell (sum of elliptical sections
     pi/4*W*H*ds), elliptical girth, dorsal surface area, indicative mass
     M_est = 1020 kg/m3 * V_ell, and quality flags (see add_flags()).
  7. v1.2: slope-aligned metrics from the ground plane: L_plane (centre line
     on the substrate plane), L_3D_abs (dorsal ridge in absolute 3D), W_plane,
     H_perp (height normal to the substrate), V_ell_plane, slope along/across.

Usage
  python seal3d.py <labelInput> [--res 0.01] [--plots] [--time-budget 0]
  python seal3d.py --root <PV_UI> --plots          (all flights found)
  python seal3d.py --root <PV_UI> --list
  From R: Modules/LRG/LRG_3d_Morphometry.r
Written 2026-09 for the PV_PL pipeline (NPWC); version in __version__.
"""
import os, re, sys, glob, math, json, time, argparse, csv
import xml.etree.ElementTree as ET
import numpy as np
from matplotlib.path import Path

__version__ = '1.2'
SHIFT = np.array([435000.0, 5451000.0, -100.0])  # Metashape export shift (E, N, Z)
UTM_ZONE = 10
RHO_KG_M3 = 1020.0  # indicative whole-body density used for M_est (see notes)


# ==========================================================================
# I/O
# ==========================================================================
_PLY_T = {'float': '<f4', 'float32': '<f4', 'double': '<f8', 'float64': '<f8',
          'uchar': 'u1', 'uint8': 'u1', 'char': 'i1', 'int8': 'i1',
          'short': '<i2', 'int16': '<i2', 'ushort': '<u2', 'uint16': '<u2',
          'int': '<i4', 'int32': '<i4', 'uint': '<u4', 'uint32': '<u4'}


def read_ply(path, want_faces=True):
    """Binary little-endian PLY (Metashape / VCG). Returns dict with
    'xyz' (n,3 float64), 'rgb' (n,3 uint8 or None), 'faces' (m,3 int32 or None)."""
    with open(path, 'rb') as f:
        header = []
        while True:
            line = f.readline()
            if not line:
                raise ValueError('bad PLY header: ' + path)
            header.append(line.decode('ascii', 'ignore').strip())
            if header[-1] == 'end_header':
                break
        offset = f.tell()
    fmt = [h for h in header if h.startswith('format')][0]
    if 'binary_little_endian' not in fmt:
        raise ValueError('only binary_little_endian PLY supported: ' + fmt)
    elems, cur = [], None
    for h in header:
        p = h.split()
        if not p:
            continue
        if p[0] == 'element':
            cur = {'name': p[1], 'n': int(p[2]), 'props': []}
            elems.append(cur)
        elif p[0] == 'property':
            if p[1] == 'list':
                cur['props'].append(('list', _PLY_T[p[2]], _PLY_T[p[3]], p[4]))
            else:
                cur['props'].append((p[2], _PLY_T[p[1]]))
    buf = np.memmap(path, dtype='u1', mode='r', offset=offset)
    pos = 0
    out = {'xyz': None, 'rgb': None, 'faces': None}
    for e in elems:
        if e['name'] == 'vertex':
            dt = np.dtype([(pp[0], pp[1]) for pp in e['props']])
            arr = np.frombuffer(buf, dtype=dt, count=e['n'], offset=pos)
            pos += dt.itemsize * e['n']
            out['xyz'] = np.stack([arr['x'], arr['y'], arr['z']], 1).astype(np.float64)
            if 'red' in dt.names:
                out['rgb'] = np.stack([arr['red'], arr['green'], arr['blue']], 1).astype(np.uint8)
        elif e['name'] == 'face' and want_faces:
            pr = e['props']
            if len(pr) == 1 and pr[0][0] == 'list':
                dt = np.dtype([('n', pr[0][1]), ('v', pr[0][2], (3,))])
                arr = np.frombuffer(buf, dtype=dt, count=e['n'], offset=pos)
                if e['n'] and not np.all(arr['n'][:: max(1, e['n'] // 1000)] == 3):
                    raise ValueError('non-triangle faces in ' + path)
                out['faces'] = np.ascontiguousarray(arr['v']).astype(np.int32)
                pos += dt.itemsize * e['n']
            else:
                raise ValueError('unsupported face layout in ' + path)
        else:
            break
    del buf
    return out


def _strip(tag):
    return tag.split('}', 1)[-1]


def _coords(txt):
    vals = []
    for tok in txt.replace('\n', ' ').split():
        p = tok.split(',')
        if len(p) >= 2:
            vals.append((float(p[0]), float(p[1])))
    return np.array(vals, dtype=np.float64)


def read_kml(path):
    """Placemarks: {'name','data','polys':[ring(n,2)],'lines':[(n,2)]}, (lon, lat)."""
    tree = ET.parse(path)
    out = []
    for pm in tree.iter():
        if _strip(pm.tag) != 'Placemark':
            continue
        rec = {'name': None, 'data': {}, 'polys': [], 'lines': []}
        for el in pm.iter():
            t = _strip(el.tag)
            if t == 'name' and rec['name'] is None:
                rec['name'] = (el.text or '').strip()
            elif t in ('SimpleData', 'Data'):
                key = el.attrib.get('name')
                val = el.text
                if t == 'Data':
                    for ch in el:
                        if _strip(ch.tag) == 'value':
                            val = ch.text
                rec['data'][key] = (val or '').strip()
            elif t == 'Polygon':
                for ob in el.iter():
                    if _strip(ob.tag) == 'outerBoundaryIs':
                        for c in ob.iter():
                            if _strip(c.tag) == 'coordinates':
                                rec['polys'].append(_coords(c.text or ''))
            elif t == 'LineString':
                for c in el:
                    if _strip(c.tag) == 'coordinates':
                        rec['lines'].append(_coords(c.text or ''))
        out.append(rec)
    return out


# ==========================================================================
# Geodesy: WGS84 lon/lat <-> UTM (Krueger series, sub-mm inside the zone)
# ==========================================================================
def lonlat_to_utm(lon, lat, zone=UTM_ZONE, north=True):
    lon = np.asarray(lon, dtype=np.float64)
    lat = np.asarray(lat, dtype=np.float64)
    a = 6378137.0
    f = 1 / 298.257223563
    k0 = 0.9996
    n = f / (2 - f)
    A = a / (1 + n) * (1 + n ** 2 / 4 + n ** 4 / 64 + n ** 6 / 256)
    al = [0,
          n / 2 - 2 * n ** 2 / 3 + 5 * n ** 3 / 16 + 41 * n ** 4 / 180 - 127 * n ** 5 / 288 + 7891 * n ** 6 / 37800,
          13 * n ** 2 / 48 - 3 * n ** 3 / 5 + 557 * n ** 4 / 1440 + 281 * n ** 5 / 630 - 1983433 * n ** 6 / 1935360,
          61 * n ** 3 / 240 - 103 * n ** 4 / 140 + 15061 * n ** 5 / 26880 + 167603 * n ** 6 / 181440,
          49561 * n ** 4 / 161280 - 179 * n ** 5 / 168 + 6601661 * n ** 6 / 7257600,
          34729 * n ** 5 / 80640 - 3418889 * n ** 6 / 1995840,
          212378941 * n ** 6 / 319334400]
    e = math.sqrt(f * (2 - f))
    lon0 = math.radians((zone - 1) * 6 - 180 + 3)
    phi = np.radians(lat)
    lam = np.radians(lon) - lon0
    t = np.sinh(np.arctanh(np.sin(phi)) - e * np.arctanh(e * np.sin(phi)))
    xi_p = np.arctan2(t, np.cos(lam))
    eta_p = np.arctanh(np.sin(lam) / np.sqrt(1 + t ** 2))
    xi = xi_p.copy()
    eta = eta_p.copy()
    for j in range(1, 7):
        xi = xi + al[j] * np.sin(2 * j * xi_p) * np.cosh(2 * j * eta_p)
        eta = eta + al[j] * np.cos(2 * j * xi_p) * np.sinh(2 * j * eta_p)
    E = 500000.0 + k0 * A * eta
    N = (0.0 if north else 10000000.0) + k0 * A * xi
    return E, N


def utm_to_lonlat(E, N, zone=UTM_ZONE, north=True, iters=6):
    """Inverse by Newton iteration on the forward series (mm level)."""
    E = np.asarray(E, dtype=np.float64)
    N = np.asarray(N, dtype=np.float64)
    lon0 = (zone - 1) * 6 - 180 + 3
    lat = N / 111132.0
    lon = lon0 + (E - 500000.0) / (111320.0 * np.cos(np.radians(lat)))
    for _ in range(iters):
        e1, n1 = lonlat_to_utm(lon, lat, zone, north)
        d = 1e-6
        e2, n2 = lonlat_to_utm(lon + d, lat, zone, north)
        e3, n3 = lonlat_to_utm(lon, lat + d, zone, north)
        j11, j12 = (e2 - e1) / d, (e3 - e1) / d
        j21, j22 = (n2 - n1) / d, (n3 - n1) / d
        det = j11 * j22 - j12 * j21
        de, dn = E - e1, N - n1
        lon = lon + (j22 * de - j12 * dn) / det
        lat = lat + (-j21 * de + j11 * dn) / det
    return lon, lat


def lonlat_to_local(lonlat):
    E, N = lonlat_to_utm(lonlat[:, 0], lonlat[:, 1])
    return np.column_stack([E - SHIFT[0], N - SHIFT[1]])


# ==========================================================================
# 2D geometry helpers
# ==========================================================================
def rdp(pts, eps):
    """Ramer-Douglas-Peucker simplification of an open polyline."""
    pts = np.asarray(pts)
    if len(pts) < 3:
        return pts
    keep = np.zeros(len(pts), bool)
    keep[0] = keep[-1] = True
    stack = [(0, len(pts) - 1)]
    while stack:
        i, j = stack.pop()
        if j <= i + 1:
            continue
        a, b = pts[i], pts[j]
        ab = b - a
        L = np.hypot(*ab)
        seg = pts[i + 1:j]
        if L == 0:
            d = np.hypot(*(seg - a).T)
        else:
            d = np.abs(ab[0] * (seg[:, 1] - a[1]) - ab[1] * (seg[:, 0] - a[0])) / L
        k = int(np.argmax(d))
        if d[k] > eps:
            m = i + 1 + k
            keep[m] = True
            stack.append((i, m))
            stack.append((m, j))
    return pts[keep]


def simplify_ring(ring, eps=0.003):
    """Closed ring -> simplified closed ring (first point repeated at the end)."""
    ring = np.asarray(ring, dtype=np.float64)
    if len(ring) > 1 and np.allclose(ring[0], ring[-1]):
        ring = ring[:-1]
    if len(ring) < 8:
        return np.vstack([ring, ring[:1]])
    d = np.hypot(*(ring - ring[0]).T)
    i1 = int(np.argmax(d))
    a = rdp(ring[:i1 + 1], eps)
    b = rdp(np.vstack([ring[i1:], ring[:1]]), eps)
    return np.vstack([a, b[1:]])


def poly_area(ring):
    x, y = ring[:, 0], ring[:, 1]
    return 0.5 * abs(np.dot(x, np.roll(y, -1)) - np.dot(y, np.roll(x, -1)))


def dist_to_segments(px, py, ring, chunk=20000, return_pos=False):
    """Distance from points to a closed polyline; optionally the arc position (m)
    of the nearest point along the ring."""
    a = ring[:-1]
    b = ring[1:]
    ab = b - a
    L2 = (ab ** 2).sum(1)
    L2[L2 == 0] = 1e-12
    s0 = np.r_[0, np.cumsum(np.sqrt(L2))][:-1]
    out = np.empty(px.shape[0])
    pos = np.empty(px.shape[0]) if return_pos else None
    for s in range(0, px.shape[0], chunk):
        x = px[s:s + chunk, None]
        y = py[s:s + chunk, None]
        t = ((x - a[None, :, 0]) * ab[None, :, 0] + (y - a[None, :, 1]) * ab[None, :, 1]) / L2[None]
        t = np.clip(t, 0, 1)
        dx = x - (a[None, :, 0] + t * ab[None, :, 0])
        dy = y - (a[None, :, 1] + t * ab[None, :, 1])
        d2 = dx * dx + dy * dy
        k = d2.argmin(1)
        r = np.arange(len(k))
        out[s:s + chunk] = np.sqrt(d2[r, k])
        if return_pos:
            pos[s:s + chunk] = s0[k] + t[r, k] * np.sqrt(L2[k])
    return (out, pos) if return_pos else out


# ==========================================================================
# Mesh rasterisation (max-Z surface = what a nadir camera sees)
# ==========================================================================
_BARY = {}


def _bary(n):
    if n not in _BARY:
        ij = np.array([(i, j) for i in range(n + 1) for j in range(n + 1 - i)], dtype=np.float64) / n
        _BARY[n] = np.column_stack([ij[:, 0], ij[:, 1], 1 - ij[:, 0] - ij[:, 1]])
    return _BARY[n]


def rasterize(xyz, faces, rgb, x0, y0, nx, ny, res, max_sub=40):
    """Sample each triangle at <= res/2 spacing and keep the highest sample per
    cell. Returns Z (ny,nx) float32 (NaN = no surface) and RGB (ny,nx,3)."""
    Z = np.full(ny * nx, -np.inf)
    col = np.zeros((ny * nx, 3), np.uint8)
    if faces is None or len(faces) == 0:
        return np.full((ny, nx), np.nan, np.float32), col.reshape(ny, nx, 3)
    T = xyz[faces]
    e01 = np.hypot(T[:, 0, 0] - T[:, 1, 0], T[:, 0, 1] - T[:, 1, 1])
    e12 = np.hypot(T[:, 1, 0] - T[:, 2, 0], T[:, 1, 1] - T[:, 2, 1])
    e20 = np.hypot(T[:, 2, 0] - T[:, 0, 0], T[:, 2, 1] - T[:, 0, 1])
    emax = np.maximum(np.maximum(e01, e12), e20)
    nsub = np.clip(np.ceil(emax / (0.5 * res)).astype(int), 1, max_sub)
    C = rgb[faces].astype(np.float32) if rgb is not None else None
    cells_all, z_all, c_all = [], [], []
    for n in np.unique(nsub):
        idx = np.where(nsub == n)[0]
        B = _bary(int(n))
        step = max(1, 400000 // len(B))
        for s in range(0, len(idx), step):
            ii = idx[s:s + step]
            Pp = np.einsum('kj,mjd->mkd', B, T[ii])
            ix = np.floor((Pp[..., 0] - x0) / res).astype(np.int64)
            iy = np.floor((Pp[..., 1] - y0) / res).astype(np.int64)
            ok = (ix >= 0) & (ix < nx) & (iy >= 0) & (iy < ny)
            if not ok.any():
                continue
            cells_all.append((iy * nx + ix)[ok])
            z_all.append(Pp[..., 2][ok])
            if C is not None:
                c_all.append(np.einsum('kj,mjd->mkd', B, C[ii])[ok])
    if not cells_all:
        return np.full((ny, nx), np.nan, np.float32), col.reshape(ny, nx, 3)
    cells = np.concatenate(cells_all)
    zz = np.concatenate(z_all)
    order = np.lexsort((zz, cells))
    cells = cells[order]
    zz = zz[order]
    last = np.r_[cells[1:] != cells[:-1], True]
    Z[cells[last]] = zz[last]
    if C is not None:
        cc = np.concatenate(c_all)[order]
        col[cells[last]] = np.clip(cc[last], 0, 255).astype(np.uint8)
    Z = Z.reshape(ny, nx)
    Z[~np.isfinite(Z)] = np.nan
    return Z.astype(np.float32), col.reshape(ny, nx, 3)


# ==========================================================================
# One mesh file with a 1 m tile index (memory-lean: float32 XYZ)
# ==========================================================================
class MeshPart:
    def __init__(self, path, tile=1.0):
        m = read_ply(path)
        self.path = path
        self.tile = tile
        xyz = m['xyz']
        self.origin = np.floor(xyz[:, :2].mean(0))
        xyz[:, 0] -= self.origin[0]
        xyz[:, 1] -= self.origin[1]
        self.xyz = xyz.astype(np.float32)
        self.rgb = m['rgb']
        F = m['faces'] if m['faces'] is not None else np.zeros((0, 3), np.int32)
        del m, xyz
        self.bbox = (float(self.xyz[:, 0].min() + self.origin[0]), float(self.xyz[:, 1].min() + self.origin[1]),
                     float(self.xyz[:, 0].max() + self.origin[0]), float(self.xyz[:, 1].max() + self.origin[1]))
        c = self.xyz[F[:, 0], :2]
        key = (np.floor(c[:, 0] / tile).astype(np.int64) * 100003 + np.floor(c[:, 1] / tile).astype(np.int64))
        order = np.argsort(key, kind='stable')
        self.key = key[order]
        self.F = F[order]
        del key, order, c
        self.nv, self.nf = len(self.xyz), len(self.F)
        if self.nf:  # median edge length (mesh resolution diagnostic)
            s = self.F[:: max(1, self.nf // 20000)]
            e = np.hypot(*(self.xyz[s[:, 0], :2] - self.xyz[s[:, 1], :2]).T)
            self.edge_med = float(np.median(e))
        else:
            self.edge_med = np.nan

    def query(self, xmin, ymin, xmax, ymax, pad=0.05):
        ox, oy = self.origin
        x0, y0, x1, y1 = xmin - ox - pad, ymin - oy - pad, xmax - ox + pad, ymax - oy + pad
        txs = np.arange(math.floor(x0 / self.tile) - 1, math.floor(x1 / self.tile) + 2)
        tys = np.arange(math.floor(y0 / self.tile) - 1, math.floor(y1 / self.tile) + 2)
        keys = (txs[:, None] * 100003 + tys[None, :]).ravel()
        lo = np.searchsorted(self.key, keys, 'left')
        hi = np.searchsorted(self.key, keys, 'right')
        sel = [np.arange(a, b) for a, b in zip(lo, hi) if b > a]
        if not sel:
            return None
        F = self.F[np.concatenate(sel)]
        T = self.xyz[F]
        ok = ((T[..., 0].max(1) >= x0) & (T[..., 0].min(1) <= x1) &
              (T[..., 1].max(1) >= y0) & (T[..., 1].min(1) <= y1))
        F = F[ok]
        if len(F) == 0:
            return None
        uv, inv = np.unique(F.ravel(), return_inverse=True)
        xyz = self.xyz[uv].astype(np.float64)
        xyz[:, 0] += ox
        xyz[:, 1] += oy
        rgb = self.rgb[uv] if self.rgb is not None else np.zeros((len(uv), 3), np.uint8)
        return xyz, inv.reshape(-1, 3).astype(np.int32), rgb


class MeshSet:
    """Several MeshPart objects queried together."""
    def __init__(self, parts):
        self.parts = parts

    def query(self, xmin, ymin, xmax, ymax):
        xyz_l, F_l, rgb_l, off = [], [], [], 0
        for p in self.parts:
            b = p.bbox
            if b[0] > xmax or b[2] < xmin or b[1] > ymax or b[3] < ymin:
                continue
            q = p.query(xmin, ymin, xmax, ymax)
            if q is None:
                continue
            xyz_l.append(q[0])
            F_l.append(q[1] + off)
            rgb_l.append(q[2])
            off += len(q[0])
        if not xyz_l:
            return None, None, None
        return np.vstack(xyz_l), np.vstack(F_l), np.vstack(rgb_l)


def ply_bbox(path):
    """XY bbox of a PLY (vertex block only)."""
    m = read_ply(path, want_faces=False)
    if m['xyz'] is None or len(m['xyz']) == 0:
        return None, 0
    x, y = m['xyz'][:, 0], m['xyz'][:, 1]
    return (float(x.min()), float(y.min()), float(x.max()), float(y.max())), int(len(x))


# ==========================================================================
# Ground and helpers
# ==========================================================================
def robust_plane(x, y, z, iters=15):
    x0, y0 = x.mean(), y.mean()
    A = np.column_stack([np.ones_like(x), x - x0, y - y0])
    w = np.ones_like(z)
    coef = np.zeros(3)
    for _ in range(iters):
        sw = np.sqrt(w)
        coef, *_ = np.linalg.lstsq(A * sw[:, None], z * sw, rcond=None)
        r = z - A @ coef
        s = 1.4826 * np.median(np.abs(r - np.median(r))) + 1e-4
        u = r / (4.685 * s)
        w = np.where(np.abs(u) < 1, (1 - u ** 2) ** 2, 0.0)
        if w.sum() < 10:
            break
    return {'x0': x0, 'y0': y0, 'c': coef}


def plane_eval(pl, x, y):
    return pl['c'][0] + pl['c'][1] * (x - pl['x0']) + pl['c'][2] * (y - pl['y0'])


def fit_ground_sectors(GX, GY, Z, gmask, pos, perim, nbins=12, min_cells=25):
    """Sector-balanced robust plane. The ground ring is split into `nbins`
    sectors along the outline; each gives one point (mean XY, median Z); a
    Tukey-IRLS plane (weights ~ sqrt(n cells)) is fitted through them, so a
    rock or an unsegmented animal in one sector is an outlier point instead
    of tilting the whole plane."""
    b = np.floor(pos[gmask] / perim * nbins).astype(int) % nbins
    xs, ys, zs = GX[gmask], GY[gmask], Z[gmask].astype(np.float64)
    pts = []
    for k in range(nbins):
        m = b == k
        if m.sum() >= min_cells:
            pts.append((xs[m].mean(), ys[m].mean(), np.median(zs[m]), m.sum(), k))
    pts = np.array(pts) if pts else np.zeros((0, 5))
    info = {'n_sectors': int(len(pts))}
    if len(pts) < 4:
        return None, info
    x, y, z, n = pts[:, 0], pts[:, 1], pts[:, 2], pts[:, 3]
    x0, y0 = x.mean(), y.mean()
    A = np.column_stack([np.ones_like(x), x - x0, y - y0])
    w0 = np.sqrt(n)
    w = w0.copy()
    for _ in range(20):
        sw = np.sqrt(w)
        coef, *_ = np.linalg.lstsq(A * sw[:, None], z * sw, rcond=None)
        r = z - A @ coef
        s = max(1.4826 * np.median(np.abs(r - np.median(r))), 0.005)
        u = r / (4.685 * s)
        w = w0 * np.where(np.abs(u) < 1, (1 - u ** 2) ** 2, 0.0)
        if (w > 0).sum() < 3:
            break
    pl = {'x0': x0, 'y0': y0, 'c': coef}
    rc = zs - plane_eval(pl, xs, ys)
    inl = w > 0
    ks = np.sort(pts[inl, 4].astype(int))
    gaps = np.diff(np.r_[ks, ks[0] + nbins]) if len(ks) else np.array([nbins])
    info.update({'n_sectors_inlier': int(inl.sum()), 'max_sector_gap': int(gaps.max()),
                 'sector_resid_sd': float(np.sqrt(np.mean(r[inl] ** 2))) if inl.any() else np.nan,
                 'cells_mad': float(1.4826 * np.median(np.abs(rc - np.median(rc)))),
                 'slope_deg': float(np.degrees(np.arctan(np.hypot(coef[1], coef[2])))),
                 'pts': pts, 'w': w})
    return pl, info


def ellipse_perimeter(a, b):
    """Ramanujan II; a, b = semi-axes."""
    h = ((a - b) / (a + b + 1e-12)) ** 2
    return np.pi * (a + b) * (1 + 3 * h / (10 + np.sqrt(4 - 3 * h)))


def _smooth(a, k=5):
    if len(a) < k:
        return a
    return np.convolve(np.pad(a, k // 2, mode='edge'), np.ones(k) / k, mode='valid')


# ==========================================================================
# Morphometry of one seal
# ==========================================================================
def measure_seal(ring, others, mesh, res=0.01, margin=0.7, ring_d=(0.20, 0.45),
                 other_buf=0.06, keep_rasters=False):
    """ring: (n,2) local XY outline (closed); others: nearby outlines; mesh:
    object with .query(xmin,ymin,xmax,ymax). Returns dict of metrics."""
    out = {'status': 'ok'}
    ring = simplify_ring(ring, 0.003)
    if len(ring) < 5 or poly_area(ring) < 0.01:
        return {'status': 'tiny_polygon'}
    xmin, ymin = ring.min(0) - margin
    xmax, ymax = ring.max(0) + margin
    nx = int(math.ceil((xmax - xmin) / res))
    ny = int(math.ceil((ymax - ymin) / res))
    xyz, F, rgb = mesh.query(xmin, ymin, xmax, ymax)
    if xyz is None:
        return {'status': 'no_mesh'}
    Z, RGB = rasterize(xyz, F, rgb, xmin, ymin, nx, ny, res)
    gx = xmin + (np.arange(nx) + 0.5) * res
    gy = ymin + (np.arange(ny) + 0.5) * res
    GX, GY = np.meshgrid(gx, gy)
    P = np.column_stack([GX.ravel(), GY.ravel()])
    seal = Path(ring).contains_points(P).reshape(ny, nx)
    perim = float(np.sum(np.hypot(*np.diff(ring, axis=0).T)))
    # distance to own outline + arc position of the nearest outline point
    bb = ((P[:, 0] > ring[:, 0].min() - ring_d[1] - res) & (P[:, 0] < ring[:, 0].max() + ring_d[1] + res) &
          (P[:, 1] > ring[:, 1].min() - ring_d[1] - res) & (P[:, 1] < ring[:, 1].max() + ring_d[1] + res))
    cand = np.where(~seal.ravel() & bb)[0]
    d_own = np.full(P.shape[0], np.inf)
    pos = np.zeros(P.shape[0])
    d_own[cand], pos[cand] = dist_to_segments(P[cand, 0], P[cand, 1], ring, return_pos=True)
    d_own = d_own.reshape(ny, nx)
    pos = pos.reshape(ny, nx)
    # neighbouring seals (inside or < other_buf) are excluded from the ground ring
    excl = np.zeros(P.shape[0], bool)
    n_touch, min_gap = 0, np.inf
    for o in others:
        o = simplify_ring(o, 0.003)
        if (o[:, 0].max() < xmin or o[:, 0].min() > xmax or o[:, 1].max() < ymin or o[:, 1].min() > ymax):
            continue
        ins = Path(o).contains_points(P)
        c2 = np.where(~ins & (P[:, 0] > o[:, 0].min() - other_buf) & (P[:, 0] < o[:, 0].max() + other_buf) &
                      (P[:, 1] > o[:, 1].min() - other_buf) & (P[:, 1] < o[:, 1].max() + other_buf))[0]
        ex = ins
        if len(c2):
            ex[c2[dist_to_segments(P[c2, 0], P[c2, 1], o) < other_buf]] = True
        excl |= ex
        g = float(dist_to_segments(o[:, 0], o[:, 1], ring).min())
        if Path(ring).contains_points(o[:-1]).any():
            g = 0.0
        min_gap = min(min_gap, g)
        if g < 0.05:
            n_touch += 1
    excl = excl.reshape(ny, nx)
    ring_mask = (~seal) & (d_own >= ring_d[0]) & (d_own <= ring_d[1]) & (~excl)
    valid = np.isfinite(Z)
    gmask = ring_mask & valid
    out['ring_valid_frac'] = float(gmask.sum() / max(1, ring_mask.sum()))
    out['n_touching'] = n_touch
    out['min_gap_m'] = min_gap if np.isfinite(min_gap) else np.nan
    out['mesh_cover_frac'] = float((seal & valid).sum() / max(1, seal.sum()))
    if gmask.sum() < 150:
        out['status'] = 'no_ground'
        return out
    pl, gi = fit_ground_sectors(GX, GY, Z, gmask, pos, perim)
    out['ground_sectors'] = gi.get('n_sectors', 0)
    if pl is None:
        out['status'] = 'no_ground'
        return out
    out['ground_sectors_inlier'] = gi['n_sectors_inlier']
    out['ground_max_sector_gap'] = gi['max_sector_gap']
    out['ground_sector_sd_m'] = gi['sector_resid_sd']
    out['ground_cells_mad_m'] = gi['cells_mad']
    out['ground_slope_deg'] = gi['slope_deg']
    pl_alt = robust_plane(GX[gmask], GY[gmask], Z[gmask].astype(np.float64))
    # ---------------- height field --------------------------------------
    H = Z.astype(np.float64) - plane_eval(pl, GX, GY)
    Hs = np.where(seal & valid, H, np.nan)
    for _ in range(4):  # fill small mesh holes inside the outline
        miss = seal & ~np.isfinite(Hs)
        if not miss.any():
            break
        Hp = np.pad(Hs, 1, constant_values=np.nan)
        stack = np.stack([Hp[dy:dy + ny, dx:dx + nx] for dy in range(3) for dx in range(3)])
        fin = np.isfinite(stack)
        cnt = fin.sum(0)
        mm = np.where(cnt > 0, np.where(fin, stack, 0).sum(0) / np.maximum(cnt, 1), np.nan)
        Hs[miss] = mm[miss]
    out['frac_below_ground'] = float(np.mean(np.nan_to_num(Hs[seal], nan=0.0) < -0.02))
    Hc = np.clip(np.nan_to_num(Hs, nan=0.0), 0, None)
    Hc[~seal] = 0.0
    Ha = np.clip(Z.astype(np.float64) - plane_eval(pl_alt, GX, GY), 0, None)
    out['V_hf_altplane_m3'] = float(np.sum(Ha[seal & valid]) * res * res)
    # ---------------- 2D shape: PCA frame + cubic centre line ------------
    sx, sy = GX[seal], GY[seal]
    if len(sx) < 50:
        out['status'] = 'tiny_polygon'
        return out
    cxy = np.array([sx.mean(), sy.mean()])
    evals, evecs = np.linalg.eigh(np.cov(np.vstack([sx - cxy[0], sy - cxy[1]])))
    ax = evecs[:, 1]
    ax = ax if ax[0] >= 0 else -ax
    pv = np.array([-ax[1], ax[0]])
    u = (sx - cxy[0]) * ax[0] + (sy - cxy[1]) * ax[1]
    v = (sx - cxy[0]) * pv[0] + (sy - cxy[1]) * pv[1]
    out['area_m2'] = float(seal.sum() * res * res)
    out['perimeter_m'] = perim
    out['L_axis_m'] = float(u.max() - u.min() + res)
    out['W_axis_m'] = float(v.max() - v.min() + res)
    ub = np.floor((u - u.min()) / 0.02).astype(int)
    nb = ub.max() + 1
    vmin = np.full(nb, np.inf)
    vmax = np.full(nb, -np.inf)
    np.minimum.at(vmin, ub, v)
    np.maximum.at(vmax, ub, v)
    cnt = np.bincount(ub, minlength=nb)
    uc = u.min() + (np.arange(nb) + 0.5) * 0.02
    okb = cnt >= 3
    mid = 0.5 * (vmin + vmax)
    wts = np.where(okb, np.clip(vmax - vmin, 0.02, None), 0)
    Lx = uc[okb].max() - uc[okb].min()
    core = okb & (uc > uc[okb].min() + 0.05 * Lx) & (uc < uc[okb].max() - 0.05 * Lx)
    deg = 3 if core.sum() > 12 else 1
    if core.sum() > deg + 2:
        cf = np.polyfit(uc[core], mid[core], deg, w=wts[core])
        out['centerline_rms_m'] = float(np.sqrt(np.mean((mid[core] - np.polyval(cf, uc[core])) ** 2)))
    else:
        cf = np.array([0.0, 0.0])
        out['centerline_rms_m'] = np.nan
    uu = np.linspace(u.min() - 0.05, u.max() + 0.05, 2000)
    vv = np.polyval(cf, uu)
    cxs = cxy[0] + uu * ax[0] + vv * pv[0]
    cys = cxy[1] + uu * ax[1] + vv * pv[1]
    ins_c = Path(ring).contains_points(np.column_stack([cxs, cys]))
    if ins_c.sum() < 10:
        out['status'] = 'bad_centerline'
        return out
    i0, i1 = np.where(ins_c)[0][[0, -1]]
    cxs, cys = cxs[i0:i1 + 1], cys[i0:i1 + 1]
    s_cum = np.r_[0, np.cumsum(np.hypot(np.diff(cxs), np.diff(cys)))]
    L2 = float(s_cum[-1])
    out['L_2D_m'] = L2
    # ---------------- stations along the centre line --------------------
    ds = res
    st = np.arange(0, L2 + 1e-9, ds)
    px = np.interp(st, s_cum, cxs)
    py = np.interp(st, s_cum, cys)
    tx, ty = np.gradient(px), np.gradient(py)
    tn = np.hypot(tx, ty) + 1e-12
    tx, ty = tx / tn, ty / tn
    nxv, nyv = -ty, tx
    w_off = np.arange(-0.7, 0.7 + 1e-9, res)
    QX = px[:, None] + w_off[None, :] * nxv[:, None]
    QY = py[:, None] + w_off[None, :] * nyv[:, None]
    ixq = np.floor((QX - xmin) / res).astype(int)
    iyq = np.floor((QY - ymin) / res).astype(int)
    okq = (ixq >= 0) & (ixq < nx) & (iyq >= 0) & (iyq < ny)
    ixq = np.clip(ixq, 0, nx - 1)
    iyq = np.clip(iyq, 0, ny - 1)
    inq = seal[iyq, ixq] & okq
    hq = Hc[iyq, ixq] * inq
    c0 = len(w_off) // 2
    n_st = len(st)
    width, hmax, area, arc = (np.zeros(n_st) for _ in range(4))
    for k in range(n_st):
        row = inq[k]
        if row[c0]:
            c = c0
        else:
            idx = np.where(row)[0]
            if len(idx) == 0:
                continue
            c = idx[np.argmin(np.abs(idx - c0))]
        l = c
        while l > 0 and row[l - 1]:
            l -= 1
        r = c
        while r + 1 < len(row) and row[r + 1]:
            r += 1
        hh = hq[k, l:r + 1]
        width[k] = (r - l + 1) * res
        hmax[k] = hh.max()
        area[k] = hh.sum() * res
        hp = np.r_[0.0, hh, 0.0]
        arc[k] = np.sum(np.sqrt(res ** 2 + np.diff(hp) ** 2)) - res
    ws, hs, As = _smooth(width), _smooth(hmax), _smooth(area)
    kW, kH, kA = int(np.argmax(ws)), int(np.argmax(hs)), int(np.argmax(As))
    out['W_max_m'] = float(ws[kW])
    out['W_max_pos'] = float(st[kW] / L2)
    out['H_max_m'] = float(hs[kH])
    out['H_max_pos'] = float(st[kH] / L2)
    core = (st >= 0.15 * L2) & (st <= 0.85 * L2)
    out['H_core_max_m'] = float(hs[core].max()) if core.any() else float(hs[kH])
    out['H_p99_m'] = float(np.percentile(Hc[seal], 99))
    out['H_mean_m'] = float(Hc[seal].mean())
    ntip = max(2, int(round(0.07 * n_st)))
    out['H_tipA_m'] = float(hmax[:ntip].mean())
    out['H_tipB_m'] = float(hmax[-ntip:].mean())
    out['A_max_m2'] = float(As[kA])
    out['A_max_pos'] = float(st[kA] / L2)
    out['L_3D_dorsal_m'] = float(np.sum(np.sqrt(np.diff(st) ** 2 + np.diff(_smooth(hmax, 3)) ** 2)))
    # ---------------- slope-aligned (ground-plane) metrics -----------------
    ga, gb = float(pl['c'][1]), float(pl['c'][2])          # dz/dx, dz/dy of the ground plane
    cos_t = 1.0 / math.sqrt(1.0 + ga * ga + gb * gb)
    out['ground_dzdx'] = ga
    out['ground_dzdy'] = gb
    sl_along = np.abs(ga * tx + gb * ty)                    # tan(slope) along the body axis
    sl_across = np.abs(ga * nxv + gb * nyv)                 # tan(slope) across the body
    out['slope_along_deg'] = float(np.degrees(np.arctan(np.median(sl_along))))
    out['slope_across_deg'] = float(np.degrees(np.arctan(np.median(sl_across))))
    dxs, dys = np.diff(px), np.diff(py)
    dzg = ga * dxs + gb * dys
    out['L_plane_m'] = float(np.sum(np.sqrt(dxs ** 2 + dys ** 2 + dzg ** 2)))   # centre line on the substrate plane
    zr = plane_eval(pl, px, py) + _smooth(hmax, 3)
    out['L_3D_abs_m'] = float(np.sum(np.sqrt(dxs ** 2 + dys ** 2 + np.diff(zr) ** 2)))  # dorsal ridge, absolute 3D
    fw = np.sqrt(1.0 + (ga * nxv + gb * nyv) ** 2)
    fs = np.sqrt(1.0 + (ga * tx + gb * ty) ** 2)
    w_pl = _smooth(width * fw)
    h_pp = _smooth(hmax * cos_t)
    kWp = int(np.argmax(w_pl))
    out['W_plane_max_m'] = float(w_pl[kWp])
    out['H_perp_max_m'] = float(h_pp.max())
    out['H_perp_core_max_m'] = float(h_pp[core].max()) if core.any() else float(h_pp.max())
    out['V_ell_plane_m3'] = float(np.sum(np.pi / 4 * width * fw * hmax * cos_t * ds * fs))
    out['G_ell_plane_max_m'] = float(ellipse_perimeter(w_pl / 2, h_pp / 2).max())
    out['area_plane_m2'] = float(seal.sum() * res * res / cos_t)
    out['V_hf_m3'] = float(Hc.sum() * res * res)
    out['V_ell_m3'] = float(np.sum(np.pi / 4 * width * hmax) * ds)
    girth = ellipse_perimeter(ws / 2, hs / 2)
    kG = int(np.argmax(girth))
    out['G_ell_max_m'] = float(girth[kG])
    out['G_ell_pos'] = float(st[kG] / L2)
    out['G_dorsal_arc_at_Wmax_m'] = float(_smooth(arc)[kW])
    for frac in (0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9):
        k = min(n_st - 1, int(round(frac * (n_st - 1))))
        out['W%02d_m' % int(frac * 100 + .5)] = float(ws[k])
        out['H%02d_m' % int(frac * 100 + .5)] = float(hs[k])
    gyH, gxH = np.gradient(np.where(seal, Hc, 0.0), res)
    out['S_dorsal_m2'] = float(np.sum(np.sqrt(1 + gxH ** 2 + gyH ** 2)[seal]) * res * res)
    out['vol_centroid_pos'] = float(np.sum(st * area) / max(1e-9, np.sum(area)) / L2)
    out['centroid_x'] = float(cxy[0])
    out['centroid_y'] = float(cxy[1])
    out['heading_deg'] = float((np.degrees(np.arctan2(ax[0], ax[1]))) % 180)  # axis azimuth
    out['_profile'] = {'s': st[::2], 'w': width[::2], 'h': hmax[::2], 'a': area[::2]}
    if keep_rasters:
        out['_r'] = dict(Z=Z, RGB=RGB, H=Hs, seal=seal, ring=ring_mask, excl=excl, x0=xmin, y0=ymin, res=res,
                         cx=cxs, cy=cys, st=st, width=width, hmax=hmax, area=area, plane=pl, outline=ring, ginfo=gi)
    return out


# ==========================================================================
# Registration check: outline vs mesh XY offset (per flight)
# ==========================================================================
def _xcorr_valid(A, K):
    """Correlation C[s] = sum_x A[x+s] K[x] for all shifts s where K (smaller,
    centred) is fully inside A. A and K are 2D; returns array of shape A-K+1."""
    sa, sk = np.array(A.shape), np.array(K.shape)
    n = sa + sk - 1
    fa = np.fft.rfft2(A, s=tuple(n))
    fk = np.fft.rfft2(K[::-1, ::-1], s=tuple(n))
    c = np.fft.irfft2(fa * fk, s=tuple(n))
    return c[sk[0] - 1:sa[0], sk[1] - 1:sa[1]]


def registration_maps(recs, mesh_query, idxs, search=1.0, res=0.03, ring_d=(0.20, 0.45)):
    """Sum over seals of the 'height contrast' map: for every XY shift s of the
    outline, mean DSM inside the shifted outline minus mean DSM in its ground
    ring. Returns (sum_map, n_seals)."""
    total = None
    used = 0
    for i in idxs:
        ring = simplify_ring(recs[i]['ring'], 0.005)
        pad = search + ring_d[1] + 0.1
        x0, y0 = ring.min(0) - pad
        x1, y1 = ring.max(0) + pad
        nx, ny = int(math.ceil((x1 - x0) / res)), int(math.ceil((y1 - y0) / res))
        q = mesh_query(x0, y0, x1, y1)
        if q[0] is None:
            continue
        Z, _ = rasterize(q[0], q[1], None, x0, y0, nx, ny, res)
        V = np.isfinite(Z)
        if V.mean() < 0.5:
            continue
        Zf = np.where(V, Z - np.nanmedian(Z), 0.0)
        Vf = V.astype(float)
        # kernel = outline and ring masks on a window of (poly bbox + ring)
        kp = ring_d[1] + 0.05
        kx0, ky0 = ring.min(0) - kp
        kx1, ky1 = ring.max(0) + kp
        knx, kny = int(math.ceil((kx1 - kx0) / res)), int(math.ceil((ky1 - ky0) / res))
        gx = kx0 + (np.arange(knx) + 0.5) * res
        gy = ky0 + (np.arange(kny) + 0.5) * res
        GX, GY = np.meshgrid(gx, gy)
        P = np.column_stack([GX.ravel(), GY.ravel()])
        inside = Path(ring).contains_points(P)
        d = np.full(P.shape[0], np.inf)
        d[~inside] = dist_to_segments(P[~inside, 0], P[~inside, 1], ring)
        Kin = inside.reshape(kny, knx).astype(float)
        Kr = ((d >= ring_d[0]) & (d <= ring_d[1])).reshape(kny, knx).astype(float)
        # kernel origin offset inside the big patch (zero shift position)
        ox = int(round((kx0 - x0) / res))
        oy = int(round((ky0 - y0) / res))
        s_in = _xcorr_valid(Zf, Kin)
        n_in = _xcorr_valid(Vf, Kin)
        s_r = _xcorr_valid(Zf, Kr)
        n_r = _xcorr_valid(Vf, Kr)
        ok = (n_in > 0.8 * Kin.sum()) & (n_r > 0.5 * Kr.sum())
        sc = np.where(ok, s_in / np.maximum(n_in, 1) - s_r / np.maximum(n_r, 1), np.nan)
        # crop to +-search around zero shift
        ns = int(round(search / res))
        if oy - ns < 0 or ox - ns < 0 or oy + ns >= sc.shape[0] or ox + ns >= sc.shape[1]:
            continue
        sc = sc[oy - ns:oy + ns + 1, ox - ns:ox + ns + 1]
        if not np.isfinite(sc[ns, ns]):
            continue
        sc = np.nan_to_num(sc, nan=np.nanmin(sc) if np.isfinite(sc).any() else 0.0)
        total = sc if total is None else total + sc
        used += 1
    return total, used


def registration_result(total, used, res=0.03):
    """Peak of the summed contrast map -> shift to ADD to the outlines."""
    if total is None or used < 5:
        return {'reg_used': used, 'reg_dx_m': 0.0, 'reg_dy_m': 0.0, 'reg_gain': np.nan, 'reg_ok': False}
    ns = total.shape[0] // 2
    k = np.unravel_index(np.argmax(total), total.shape)
    # parabolic sub-cell refinement
    def sub(a, b, c):
        den = a - 2 * b + c
        return 0.0 if den == 0 else 0.5 * (a - c) / den
    ky, kx = k
    fy = sub(total[ky - 1, kx], total[ky, kx], total[ky + 1, kx]) if 0 < ky < total.shape[0] - 1 else 0
    fx = sub(total[ky, kx - 1], total[ky, kx], total[ky, kx + 1]) if 0 < kx < total.shape[1] - 1 else 0
    dx = (kx + fx - ns) * res
    dy = (ky + fy - ns) * res
    s0 = total[ns, ns] / used
    s1 = total[ky, kx] / used
    at_edge = bool(kx in (0, total.shape[1] - 1) or ky in (0, total.shape[0] - 1))
    return {'reg_used': used, 'reg_dx_m': float(dx), 'reg_dy_m': float(dy), 'reg_score0_m': float(s0),
            'reg_score_m': float(s1), 'reg_gain': float(s1 / s0) if s0 > 0 else np.nan,
            'reg_ok': not at_edge, 'reg_at_edge': at_edge}


def estimate_registration(recs, mesh_query, idxs, search=1.0, res=0.03):
    total, used = registration_maps(recs, mesh_query, idxs, search, res)
    return registration_result(total, used, res)


# ==========================================================================
# Quality flags and derived values
# ==========================================================================
def add_flags(m):
    if m.get('status') != 'ok':
        m['quality'] = 'fail'
        m['flags'] = m.get('status', 'fail')
        return m
    f = []
    if m['ground_sectors_inlier'] < 6 or m['ground_max_sector_gap'] >= 4:
        f.append('ground_partial')          # ground seen on too few sides
    if m['ground_sector_sd_m'] > 0.05 or m['ground_cells_mad_m'] > 0.06:
        f.append('ground_rough')            # rocks / relief around the seal
    if m['H_tipA_m'] > 0.15 and m['H_tipB_m'] > 0.15:
        f.append('ends_raised')             # info: banana posture, or lying on a ridge/log
    if m['frac_below_ground'] > 0.03:
        f.append('below_ground')
    if m['n_touching'] > 0:
        f.append('touching')
    if m['mesh_cover_frac'] < 0.95:
        f.append('mesh_gap')
    if m['H_max_m'] < 0.10:
        f.append('flat')                    # not reconstructed in 3D (moved?) or not a seal
    if not (0.6 <= m['L_2D_m'] <= 2.2) or m['H_max_m'] > 0.60 or m['W_max_m'] < 0.15:
        f.append('size_outlier')
    if m['L_2D_m'] / max(m['W_max_m'], 1e-6) < 2.0:
        f.append('shape_compact')           # partial / curled / merged outline
    if (m.get('centerline_rms_m') or 0) > 0.03:
        f.append('bent')
    major = {'ground_partial', 'ground_rough', 'below_ground', 'mesh_gap', 'size_outlier', 'flat'}
    nmaj = len(set(f) & major)
    m['quality'] = 'good' if nmaj == 0 and not (set(f) - {'ends_raised'}) else \
        ('fair' if nmaj == 0 else ('poor' if nmaj == 1 else 'bad'))
    m['flags'] = ';'.join(f)
    m['V_ratio_ell_hf'] = m['V_ell_m3'] / max(m['V_hf_m3'], 1e-9)
    m['M_est_kg'] = RHO_KG_M3 * m['V_ell_m3']
    return m


# ==========================================================================
# Flight runner
# ==========================================================================
OUT_COLS = ['seal_id', 'img', 'blob', 'E_utm', 'N_utm', 'lon', 'lat', 'quality', 'flags',
            'L_2D_m', 'L_axis_m', 'L_3D_dorsal_m', 'W_max_m', 'W_max_pos', 'W_axis_m',
            'H_max_m', 'H_max_pos', 'H_core_max_m', 'H_p99_m', 'H_mean_m', 'H_tipA_m', 'H_tipB_m',
            'W10_m', 'W20_m', 'W30_m', 'W40_m', 'W50_m', 'W60_m', 'W70_m', 'W80_m', 'W90_m',
            'H10_m', 'H20_m', 'H30_m', 'H40_m', 'H50_m', 'H60_m', 'H70_m', 'H80_m', 'H90_m',
            'area_m2', 'perimeter_m', 'A_max_m2', 'A_max_pos', 'V_hf_m3', 'V_ell_m3', 'V_ratio_ell_hf',
            'V_hf_altplane_m3', 'G_ell_max_m', 'G_ell_pos', 'G_dorsal_arc_at_Wmax_m', 'S_dorsal_m2',
            'M_est_kg', 'vol_centroid_pos', 'heading_deg',
            'ground_sectors', 'ground_sectors_inlier', 'ground_max_sector_gap', 'ground_sector_sd_m',
            'ground_cells_mad_m', 'ground_slope_deg', 'ground_dzdx', 'ground_dzdy', 'slope_along_deg', 'slope_across_deg',
            'L_plane_m', 'L_3D_abs_m', 'W_plane_max_m', 'H_perp_max_m', 'H_perp_core_max_m', 'V_ell_plane_m3',
            'G_ell_plane_max_m', 'area_plane_m2', 'ring_valid_frac', 'frac_below_ground',
            'mesh_cover_frac', 'n_touching', 'min_gap_m', 'centerline_rms_m', 'mesh_part', 'mesh_edge_med_m',
            'reg_dx_m', 'reg_dy_m', 'reg_gain', 'status']


def flight_inputs(labelInput):
    bs = os.path.basename(os.path.normpath(labelInput))
    date1 = bs[:15]
    plys = sorted(glob.glob(os.path.join(labelInput, bs + '_Model', '*.ply')))
    kml = os.path.join(labelInput, 'Predict', 'SpP_LRG_level_2_level#2_%s.kml' % date1)
    return bs, date1, plys, kml


def load_outlines(kml):
    pm = read_kml(kml)
    recs = []
    for k, p in enumerate(pm):
        if not p['polys']:
            continue
        rng = max(p['polys'], key=len)
        if len(rng) < 4:
            continue
        loc = lonlat_to_local(rng)
        recs.append({'k': k, 'ring': loc, 'data': p['data'],
                     'bbox': (loc[:, 0].min(), loc[:, 1].min(), loc[:, 0].max(), loc[:, 1].max())})
    return recs


def run_flight(labelInput, out_dir=None, res=0.01, time_budget=0, cache_dir=None, plots=False,
               verbose=True, include_all_parts=False):
    """Process one flight. With time_budget>0 the run stops after ~time_budget s
    and can be resumed (results cached per seal in cache_dir). Returns
    'done', 'partial' or 'skip'."""
    t0 = time.time()
    bs, date1, plys, kml = flight_inputs(labelInput)
    out_dir = out_dir or os.path.join(labelInput, 'Predict')
    cache_dir = cache_dir or out_dir
    os.makedirs(cache_dir, exist_ok=True)
    os.makedirs(out_dir, exist_ok=True)
    cache = os.path.join(cache_dir, '_seal3d_cache_%s.jsonl' % bs)
    if not plys or not os.path.exists(kml):
        if verbose:
            print('skip (no model or no level-2 kml):', labelInput)
        return 'skip'
    recs = load_outlines(kml)
    done = {}
    if os.path.exists(cache):
        for line in open(cache):
            try:
                d = json.loads(line)
                done[d['seal_idx']] = d
            except Exception:
                pass
    # assign seals to mesh parts by bbox overlap (bbox from the vertex block)
    part_info = []
    bb_cache = os.path.join(cache_dir, '_seal3d_bbox_%s.json' % bs)
    bbj = json.load(open(bb_cache)) if os.path.exists(bb_cache) else {}
    for p in plys:
        key = os.path.basename(p) + '|%d' % os.path.getsize(p)
        if key not in bbj:
            bbj[key] = ply_bbox(p)
            json.dump(bbj, open(bb_cache, 'w'))
        if bbj[key][0] is not None:
            part_info.append((p, bbj[key][0]))
    cents = np.array([r['ring'].mean(0) for r in recs]) if recs else np.zeros((0, 2))

    def overlap(b1, b2):
        w = min(b1[2], b2[2]) - max(b1[0], b2[0])
        h = min(b1[3], b2[3]) - max(b1[1], b2[1])
        return max(0, w) * max(0, h)
    assign = {}
    for i, r in enumerate(recs):
        ov = [overlap(r['bbox'], b) for _, b in part_info]
        if ov and max(ov) > 0:
            assign[i] = [j for j, o in enumerate(ov) if o > 0] if include_all_parts else [int(np.argmax(ov))]
    # ---- registration check (outline vs mesh XY offset), cached per flight
    reg_file = os.path.join(cache_dir, '_seal3d_reg_%s.json' % bs)
    if os.path.exists(reg_file):
        reg = json.load(open(reg_file))
    else:
        order = sorted([i for i in range(len(recs)) if i in assign], key=lambda i: -poly_area(recs[i]['ring']))
        cand = order[:40]
        by_part = {}
        for i in cand:
            by_part.setdefault(tuple(assign[i]), []).append(i)
        total, used = None, 0
        for pset, idxs in by_part.items():
            mesh_r = MeshSet([MeshPart(part_info[j][0]) for j in pset])
            t_, u_ = registration_maps(recs, mesh_r.query, idxs, search=1.0, res=0.03)
            del mesh_r
            if t_ is not None:
                total = t_ if total is None else total + t_
                used += u_
        reg = registration_result(total, used, res=0.03)
        mag = math.hypot(reg['reg_dx_m'], reg['reg_dy_m'])
        reg['reg_applied'] = bool(reg.get('reg_ok') and mag >= 0.02)
        json.dump(reg, open(reg_file, 'w'))
        if verbose:
            print('  registration: dx=%.3f dy=%.3f m, gain=%.2f, n=%d, applied=%s' % (
                reg['reg_dx_m'], reg['reg_dy_m'], reg.get('reg_gain', np.nan), reg['reg_used'],
                reg['reg_applied']), flush=True)
    if reg.get('reg_applied'):
        sh = np.array([reg['reg_dx_m'], reg['reg_dy_m']])
        for r in recs:
            r['ring_orig'] = r['ring']
            r['ring'] = r['ring'] + sh
        cents = np.array([r['ring'].mean(0) for r in recs])
    todo = [i for i in range(len(recs)) if i not in done]
    status = 'done'
    groups = {}
    for i in todo:
        groups.setdefault(tuple(assign.get(i, [])), []).append(i)
    with open(cache, 'a') as fh:
        for pset, idxs in sorted(groups.items(), key=lambda kv: kv[0]):
            if time_budget and time.time() - t0 > time_budget:
                status = 'partial'
                break
            if not pset:
                for i in idxs:
                    d = {'seal_idx': i, 'status': 'no_mesh'}
                    fh.write(json.dumps(d) + '\n')
                    done[i] = d
                continue
            tl = time.time()
            parts = [MeshPart(part_info[j][0]) for j in pset]
            mesh = MeshSet(parts)
            if verbose:
                print('  mesh %s loaded in %.1fs (%s faces)' % (
                    ','.join(os.path.basename(part_info[j][0]) for j in pset), time.time() - tl,
                    '+'.join(str(p.nf) for p in parts)), flush=True)
            for i in idxs:
                if time_budget and time.time() - t0 > time_budget:
                    status = 'partial'
                    break
                near = [recs[j]['ring'] for j in range(len(recs))
                        if j != i and np.hypot(*(cents[j] - cents[i])) < 4.0]
                try:
                    m = measure_seal(recs[i]['ring'], near, mesh, res=res)
                except Exception as ex:  # keep going; record the failure
                    m = {'status': 'error:%s:%s' % (type(ex).__name__, str(ex)[:60])}
                prof = m.pop('_profile', None)
                m['seal_idx'] = i
                m['mesh_part'] = '+'.join(os.path.basename(part_info[j][0]).replace(bs + '_', '') for j in pset)
                m['mesh_edge_med_m'] = float(np.nanmean([p.edge_med for p in parts]))
                if prof is not None:
                    m['_profile'] = {k: [round(float(x), 4) for x in v] for k, v in prof.items()}
                fh.write(json.dumps(m) + '\n')
                fh.flush()
                done[i] = m
            del mesh, parts
            if status == 'partial':
                break
    if status == 'partial' or len(done) < len(recs):
        if verbose:
            print('partial: %d/%d seals in %.0fs' % (len(done), len(recs), time.time() - t0))
        return 'partial'
    write_outputs(labelInput, recs, done, out_dir, bs, date1, plots=plots, reg=reg)
    if verbose:
        print('done: %d seals in %.0fs -> %s' % (len(recs), time.time() - t0, out_dir))
    return 'done'


def write_outputs(labelInput, recs, done, out_dir, bs, date1, plots=False, reg=None):
    rows, prows = [], []
    reg = reg or {}
    for i, r in enumerate(recs):
        m = dict(done.get(i, {'status': 'missing'}))
        prof = m.pop('_profile', None)
        m = add_flags(m)
        m['reg_dx_m'] = reg.get('reg_dx_m', 0.0) if reg.get('reg_applied') else 0.0
        m['reg_dy_m'] = reg.get('reg_dy_m', 0.0) if reg.get('reg_applied') else 0.0
        m['reg_gain'] = reg.get('reg_gain', np.nan)
        cx, cy = r.get('ring_orig', r['ring']).mean(0)
        if 'centroid_x' in m:
            cx, cy = m['centroid_x'] - m['reg_dx_m'], m['centroid_y'] - m['reg_dy_m']
        E, N = cx + SHIFT[0], cy + SHIFT[1]
        lon, lat = utm_to_lonlat(np.array([E]), np.array([N]))
        m.update({'seal_id': '%s_%03d' % (date1, i + 1), 'img': r['data'].get('img', ''),
                  'blob': r['data'].get('blob', ''), 'E_utm': round(E, 3), 'N_utm': round(N, 3),
                  'lon': round(float(lon[0]), 8), 'lat': round(float(lat[0]), 8)})
        rows.append(m)
        if prof:
            L = m.get('L_2D_m', np.nan)
            for s, w, h, a in zip(prof['s'], prof['w'], prof['h'], prof['a']):
                prows.append((m['seal_id'], s, round(s / L, 4) if L else '', w, h, a))
    base = os.path.join(out_dir, 'Measurements_3D_%s' % date1)
    with open(base + '.csv', 'w', newline='') as f:
        w = csv.writer(f)
        w.writerow(OUT_COLS)
        for m in rows:
            w.writerow([_fmt(m.get(c, ''), c) for c in OUT_COLS])
    with open(base + '_profiles.csv', 'w', newline='') as f:
        w = csv.writer(f)
        w.writerow(['seal_id', 's_m', 's_rel', 'width_m', 'height_m', 'section_area_m2'])
        w.writerows(prows)
    write_kml(base + '.kml', recs, rows, 'Measurements_3D_%s' % date1)
    if plots:
        try:
            plot_flight(labelInput, recs, rows, base + '_overview.png')
        except Exception as ex:
            print('plot failed:', ex)
    return rows


def _fmt(v, key=''):
    if isinstance(v, (float, np.floating)):
        v = float(v)
        if not np.isfinite(v):
            return ''
        if key in ('lon', 'lat'):
            return '%.8f' % v
        if key in ('E_utm', 'N_utm'):
            return '%.3f' % v
        return ('%.4f' % v).rstrip('0').rstrip('.') if abs(v) < 1e5 else '%.3f' % v
    return v


def write_kml(path, recs, rows, name):
    col = {'good': 'ff00ff00', 'fair': 'ff00ffff', 'poor': 'ff0080ff', 'bad': 'ff0000ff', 'fail': 'ff808080'}
    keys = ['quality', 'flags', 'L_2D_m', 'L_3D_dorsal_m', 'W_max_m', 'H_max_m', 'V_hf_m3', 'V_ell_m3',
            'G_ell_max_m', 'M_est_kg', 'ground_sector_sd_m']
    L = ['<?xml version="1.0" encoding="UTF-8"?>', '<kml xmlns="http://www.opengis.net/kml/2.2">',
         '<Document><name>%s</name>' % name]
    for q, c in col.items():
        L.append('<Style id="%s"><LineStyle><color>%s</color><width>2</width></LineStyle>'
                 '<PolyStyle><fill>0</fill></PolyStyle></Style>' % (q, c))
    for r, m in zip(recs, rows):
        ring = r.get('ring_orig', r['ring'])
        E, N = ring[:, 0] + SHIFT[0], ring[:, 1] + SHIFT[1]
        lon, lat = utm_to_lonlat(E, N)
        coords = ' '.join('%.8f,%.8f' % (a, b) for a, b in zip(lon, lat))
        if m.get('status') == 'ok':
            nm = 'L%.0f W%.0f H%.0f V%.0fL' % (m['L_2D_m'] * 100, m['W_max_m'] * 100, m['H_max_m'] * 100,
                                               m['V_ell_m3'] * 1000)
        else:
            nm = 'no3D:%s' % m.get('status')
        ed = ''.join('<Data name="%s"><value>%s</value></Data>' % (k, _fmt(m.get(k, ''))) for k in keys)
        L.append('<Placemark><name>%s</name><styleUrl>#%s</styleUrl><ExtendedData>'
                 '<Data name="seal_id"><value>%s</value></Data>%s</ExtendedData>'
                 '<Polygon><outerBoundaryIs><LinearRing><coordinates>%s</coordinates></LinearRing>'
                 '</outerBoundaryIs></Polygon></Placemark>' % (nm, m.get('quality', 'fail'), m['seal_id'], ed, coords))
    L.append('</Document></kml>')
    open(path, 'w', encoding='utf-8').write('\n'.join(L))


def plot_flight(labelInput, recs, rows, fn):
    import matplotlib
    matplotlib.use('Agg')
    import matplotlib.pyplot as plt
    cols = {'good': 'lime', 'fair': 'yellow', 'poor': 'orange', 'bad': 'red', 'fail': 'gray'}
    xs = np.concatenate([r['ring'][:, 0] for r in recs])
    ys = np.concatenate([r['ring'][:, 1] for r in recs])
    asp = max(0.3, min(3, (np.ptp(ys) + 2) / (np.ptp(xs) + 2)))
    fig, ax = plt.subplots(figsize=(12, 12 * asp))
    for r, m in zip(recs, rows):
        ax.plot(r['ring'][:, 0], r['ring'][:, 1], '-', color=cols.get(m.get('quality'), 'gray'), lw=1)
        if m.get('status') == 'ok':
            ax.text(r['ring'][:, 0].mean(), r['ring'][:, 1].mean(), '%.0f' % (m['V_ell_m3'] * 1000),
                    fontsize=6, ha='center', va='center', color='w')
    ax.set_aspect('equal')
    ax.set_facecolor('0.15')
    ax.set_title('%s  3D: numbers = V_ell (L); colour = quality' % os.path.basename(labelInput))
    plt.tight_layout()
    plt.savefig(fn, dpi=110)
    plt.close(fig)


def find_flights(root, include_thermal=False):
    out = []
    dirs = set()
    # fixed depths (site/flight/labelInput or site/labelInput): a recursive
    # glob through the image folders is far too slow on large archives
    for pat in (('*', '*', '*', '*_Model'), ('*', '*', '*_Model')):
        for p in glob.glob(os.path.join(root, *pat)):
            if os.path.basename(p) == os.path.basename(os.path.dirname(p)) + '_Model':
                dirs.add(os.path.dirname(p))
    for d in sorted(dirs):
        bs, date1, plys, kml = flight_inputs(d)
        if not plys or not os.path.exists(kml):
            continue
        if not include_thermal and 'THERMAL' in bs.upper():
            continue
        if re.search(r'_err(\b|_|$)', d):
            continue
        out.append(d)
    return out


def main(argv=None):
    ap = argparse.ArgumentParser(description='3D morphometry of seals (seal3d %s)' % __version__)
    ap.add_argument('labelInput', nargs='*')
    ap.add_argument('--root', help='PV_UI root: process all flights found')
    ap.add_argument('--list', action='store_true')
    ap.add_argument('--res', type=float, default=0.01)
    ap.add_argument('--time-budget', type=float, default=0)
    ap.add_argument('--out-dir', default=None, help='default: <labelInput>/Predict')
    ap.add_argument('--cache-dir', default=None)
    ap.add_argument('--plots', action='store_true')
    ap.add_argument('--thermal', action='store_true')
    a = ap.parse_args(argv)
    flights = list(a.labelInput)
    if a.root:
        flights += find_flights(a.root, a.thermal)
    if a.list:
        for f in flights:
            print(f)
        return
    t0 = time.time()
    for f in flights:
        left = a.time_budget - (time.time() - t0) if a.time_budget else 0
        if a.time_budget and left < 10:
            print('time budget exhausted')
            break
        print('==', f, flush=True)
        run_flight(f, out_dir=a.out_dir, res=a.res, time_budget=left, cache_dir=a.cache_dir, plots=a.plots)


if __name__ == '__main__':
    main()
