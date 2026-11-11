#!/usr/bin/env python3
"""LRG_3d_CROP_wide.py - wide mesh crops of seals for the 3D measurements (LRG_3d_Measurements_v2.r).

Cuts one PLY per seal outline from the exported model tiles (<labelInput>/<bs>_Model/<bs>_Model_N.ply,
Agisoft_export_model.r) with a margin of PAD metres around the bounding box of the outline
(Predict/SpP_LRG_level_2_level#2_<date>.kml, LRG_PredsToPolygons_level_2.r). The margin gives the
measurement script enough undisturbed ground around the seal to fit the ground plane (the MeshCrop crops of
LRG_3d_CROP.r end 15-30 cm from the seal, inside the photogrammetric "skirt", which made the heights noisy:
CV of height between two flights of the same animals 14 % vs 11 % with a 55 cm margin).

Output: <labelInput>/Predict/MeshCrop_wide/<date>#E<utm>N<utm>_id<NNN>.ply  (binary PLY, no colour),
        id = number of the outline in the KML (seal_id of seal3d = <date>_<NNN>).
No height window is applied (the measurement script keeps the highest surface over every cell), so ghost
surfaces below the seal do not matter. Nothing is deleted; an existing MeshCrop_wide is overwritten.

Usage:   python LRG_3d_CROP_wide.py <labelInput> [PAD]       (PAD default 0.55 m)
Needs seal3d.py in the same folder (read_ply, read_kml, MeshPart, load_outlines, SHIFT).
Runs on the exported tiles part by part, ~5-60 s per flight, memory = the largest tile.
"""
import os, sys, glob, time
import numpy as np
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import seal3d as S


def crop_flight(labelInput, pad=0.55, out_name='MeshCrop_wide'):
    labelInput = os.path.normpath(labelInput)
    bs, date1, plys, kml = S.flight_inputs(labelInput)
    if not plys:
        print('no model tiles in', os.path.join(labelInput, bs + '_Model')); return 0
    if not os.path.exists(kml):
        print('no outlines', kml); return 0
    recs = S.load_outlines(kml)
    out = os.path.join(labelInput, 'Predict', out_name)
    os.makedirs(out, exist_ok=True)
    acc = {i: [] for i in range(len(recs))}
    for p in plys:
        try:
            part = S.MeshPart(p)
        except Exception as e:
            print('skip tile', os.path.basename(p), e); continue
        for i, r in enumerate(recs):
            x0, y0, x1, y1 = r['bbox']
            if part.bbox[0] > x1 + pad or part.bbox[2] < x0 - pad or part.bbox[1] > y1 + pad or part.bbox[3] < y0 - pad:
                continue
            q = part.query(x0 - pad, y0 - pad, x1 + pad, y1 + pad)
            if q is not None:
                acc[i].append(q)
        del part
    n = 0
    for i, qs in acc.items():
        if not qs:
            continue
        xyz = np.vstack([q[0] for q in qs])
        off = np.cumsum([0] + [len(q[0]) for q in qs[:-1]])
        F = np.vstack([q[1] + o for q, o in zip(qs, off)]).astype(np.int32)
        c = recs[i]['ring'][:-1].mean(0) + S.SHIFT[:2]
        name = '%s#E%.6fN%.6f_id%03d.ply' % (date1, c[0], c[1], i + 1)
        with open(os.path.join(out, name), 'wb') as fh:
            fh.write(('ply\nformat binary_little_endian 1.0\nelement vertex %d\nproperty float x\nproperty float y\n'
                      'property float z\nelement face %d\nproperty list uchar int vertex_indices\nend_header\n'
                      % (len(xyz), len(F))).encode())
            fh.write(xyz.astype('<f4').tobytes())
            rec = np.empty(len(F), dtype=[('n', 'u1'), ('v', '<i4', 3)]); rec['n'] = 3; rec['v'] = F
            fh.write(rec.tobytes())
        n += 1
    return n


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print(__doc__); sys.exit(1)
    t0 = time.time()
    n = crop_flight(sys.argv[1], float(sys.argv[2]) if len(sys.argv) > 2 else 0.55)
    print('%s: %d crops written to Predict/MeshCrop_wide (%.0f s)' % (os.path.basename(os.path.normpath(sys.argv[1])), n, time.time() - t0))
