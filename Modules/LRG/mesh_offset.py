#!/usr/bin/env python3
"""mesh_offset.py - horizontal offset between two exports of the same scene (e.g. the original final
project and a re-processed one): the shift T such that mesh B = mesh A moved by T. Outlines drawn on the
orthomosaic of project A must be moved by T to match mesh B (seal3d.run_flight(..., reg_init=T)).

Method: the model tiles of both folders (same names up to the folder prefix) are rasterised as DSMs
(max z per cell), reduced to relief (median removed) and cross-correlated by FFT over +-search metres;
the per-tile shifts are combined by their correlation-weighted median. Vertical offsets are ignored
(seal heights are measured relative to the local ground plane).

Usage:  python mesh_offset.py <dir_A> <dir_B> [search_m]
        from R: mo <- reticulate::import_from_path("mesh_offset", path = mod_dir); mo$tile_offset(dirA, dirB)
"""
import os, re, sys, glob
import numpy as np


def read_ply_xyz(path):
    with open(path, 'rb') as f:
        header = b''
        while True:
            line = f.readline()
            header += line
            if line.strip() == b'end_header' or not line:
                break
        h = header.decode('ascii', 'ignore')
        nv = int(re.search(r'element vertex (\d+)', h).group(1))
        vblock = h.split('element vertex')[1].split('element face')[0]
        props = re.findall(r'property (\w+) (\w+)', vblock)
        tmap = {'float': '<f4', 'float32': '<f4', 'double': '<f8', 'float64': '<f8', 'uchar': 'u1', 'uint8': 'u1',
                'int': '<i4', 'int32': '<i4', 'uint': '<u4', 'uint32': '<u4', 'short': '<i2', 'ushort': '<u2', 'char': 'i1'}
        dt = np.dtype([(n, tmap[t]) for t, n in props])
        v = np.fromfile(f, dtype=dt, count=nv)
    return np.column_stack([v['x'], v['y'], v['z']]).astype(float)


def dsm(xyz, x0, y0, nx, ny, res):
    ix = ((xyz[:, 0] - x0) / res).astype(int)
    iy = ((xyz[:, 1] - y0) / res).astype(int)
    ok = (ix >= 0) & (ix < nx) & (iy >= 0) & (iy < ny)
    Z = np.full((ny, nx), -np.inf)
    np.maximum.at(Z, (iy[ok], ix[ok]), xyz[ok, 2])
    Z[~np.isfinite(Z)] = np.nan
    return Z


def xcorr_shift(Za, Zb, res, search):
    """Shift T (dx, dy) in metres such that Zb(x) ~ Za(x - T), i.e. B is A moved by T."""
    fa = np.nan_to_num(Za - np.nanmedian(Za)); fb = np.nan_to_num(Zb - np.nanmedian(Zb))
    fa -= fa.mean(); fb -= fb.mean()
    F = np.fft.rfft2(fa); G = np.fft.rfft2(fb)
    cc = np.fft.fftshift(np.fft.irfft2(np.conj(F) * G, s=fa.shape))   # peak at the displacement of B relative to A
    cy, cx = np.array(cc.shape) // 2
    ns = int(round(search / res))
    ns = min(ns, cy - 1, cx - 1)
    win = cc[cy - ns:cy + ns + 1, cx - ns:cx + ns + 1]
    ky, kx = np.unravel_index(np.argmax(win), win.shape)
    norm = np.sqrt((fa ** 2).sum() * (fb ** 2).sum())
    corr = float(win.max() / norm) if norm > 0 else 0.0
    at_edge = ky in (0, win.shape[0] - 1) or kx in (0, win.shape[1] - 1)
    return (kx - ns) * res, (ky - ns) * res, corr, at_edge


def tile_offset(dir_a, dir_b, res=0.05, search=8.0, min_corr=0.25, verbose=True):
    """Median shift (dx, dy) over the tile pairs of dir_a (reference) and dir_b (re-processed)."""
    tiles_a = sorted(glob.glob(os.path.join(dir_a, '*.ply')))
    tiles_b = sorted(glob.glob(os.path.join(dir_b, '*.ply')))
    pairs = []
    for pa in tiles_a:
        k = re.search(r'_(\d+)\.ply$', os.path.basename(pa))
        if not k:
            continue
        pb = [p for p in tiles_b if p.endswith('_%s.ply' % k.group(1))]
        if pb:
            pairs.append((pa, pb[0]))
    res_list = []
    for pa, pb in pairs:
        A = read_ply_xyz(pa); B = read_ply_xyz(pb)
        x0 = max(A[:, 0].min(), B[:, 0].min()) - search; x1 = min(A[:, 0].max(), B[:, 0].max()) + search
        y0 = max(A[:, 1].min(), B[:, 1].min()) - search; y1 = min(A[:, 1].max(), B[:, 1].max()) + search
        nx, ny = int((x1 - x0) / res), int((y1 - y0) / res)
        if nx < 20 or ny < 20:
            continue
        Za = dsm(A, x0, y0, nx, ny, res); Zb = dsm(B, x0, y0, nx, ny, res)
        dx, dy, corr, at_edge = xcorr_shift(Za, Zb, res, search)
        if verbose:
            print('  %s: dx=%+.2f dy=%+.2f corr=%.2f%s' % (os.path.basename(pa), dx, dy, corr, ' (AT EDGE)' if at_edge else ''))
        if corr >= min_corr and not at_edge:
            res_list.append((dx, dy, corr))
    if not res_list:
        return None
    w = np.array([r[2] for r in res_list])
    k = int(np.argsort(w)[len(w) // 2]) if len(w) < 3 else None
    dx = float(np.median([r[0] for r in res_list])); dy = float(np.median([r[1] for r in res_list]))
    if verbose:
        print('  offset B relative to A: dx=%+.2f dy=%+.2f m (%d tiles, corr %.2f-%.2f)' % (dx, dy, len(res_list), w.min(), w.max()))
    return [dx, dy]


def tile_offsets(dir_a, dir_b, res=0.05, search=8.0, min_corr=0.25, verbose=True):
    """Per-tile offsets {tile file name of dir_b: [dx, dy]} (B relative to A); tiles without a reliable
    correlation get the median of the others (or [0, 0])."""
    tiles_a = sorted(glob.glob(os.path.join(dir_a, '*.ply')))
    tiles_b = sorted(glob.glob(os.path.join(dir_b, '*.ply')))
    out, good = {}, []
    for pb in tiles_b:
        k = re.search(r'_(\d+)\.ply$', os.path.basename(pb))
        pa = [p for p in tiles_a if k and p.endswith('_%s.ply' % k.group(1))]
        if not pa:
            continue
        A = read_ply_xyz(pa[0]); B = read_ply_xyz(pb)
        x0 = max(A[:, 0].min(), B[:, 0].min()) - search; x1 = min(A[:, 0].max(), B[:, 0].max()) + search
        y0 = max(A[:, 1].min(), B[:, 1].min()) - search; y1 = min(A[:, 1].max(), B[:, 1].max()) + search
        nx, ny = int((x1 - x0) / res), int((y1 - y0) / res)
        if nx < 20 or ny < 20:
            continue
        Za = dsm(A, x0, y0, nx, ny, res); Zb = dsm(B, x0, y0, nx, ny, res)
        dx, dy, corr, at_edge = xcorr_shift(Za, Zb, res, search)
        ok = corr >= min_corr and not at_edge
        if verbose:
            print('  %s: dx=%+.2f dy=%+.2f corr=%.2f%s' % (os.path.basename(pb), dx, dy, corr, '' if ok else ' (unreliable)'))
        out[os.path.basename(pb)] = [float(dx), float(dy)] if ok else None
        if ok:
            good.append((dx, dy))
    med = [float(np.median([g[0] for g in good])), float(np.median([g[1] for g in good]))] if good else [0.0, 0.0]
    for k, v in out.items():
        if v is None:
            out[k] = med
    return out


if __name__ == '__main__':
    a, b = sys.argv[1], sys.argv[2]
    s = float(sys.argv[3]) if len(sys.argv) > 3 else 8.0
    print(tile_offset(a, b, search=s))
