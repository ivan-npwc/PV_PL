#!/usr/bin/env python3
"""register_outlines.py - write the seal outlines of a flight folder shifted onto its mesh.

For a re-processed project (e.g. <flight>_fixf built with a fixed focal length) the outlines drawn on the orthomosaic of
the original project sit metres away from the new mesh. seal3d.run_flight(..., reg_init=mesh_offset.tile_offsets(...))
finds the shift of every mesh tile and stores it in Predict/_seal3d_reg_<bs>.json ('per_tile': init + refinement).
This module applies those shifts to the level-2 outline KML itself, so that the other steps of the pipeline
(LRG_3d_CROP_wide.py, LRG_3d_Measurements_v2.r = HF2, seal_close.py = CB) can run on the folder unchanged.

register_kml(labelInput):
  reads  Predict/SpP_LRG_level_2_level#2_<date>.kml      (original outlines, WGS84)
         Predict/_seal3d_reg_<bs>.json                     (registration of the HF1 run with reg_init)
         Predict/Measurements_3D_<date>.csv                (mesh tile of every seal: column mesh_part)
  writes Predict/SpP_LRG_level_2_level#2_<date>.kml      (shifted outlines; the original is renamed *.unregistered.bak)
         Predict/_outline_registration_<bs>.json           (shift applied to every outline; its presence = already done)
  Every outline is moved by the total shift of its tile (dx, dy of 'per_tile'); outlines without a tile (outside the
  mesh) get the median shift. Nothing else in the KML is changed (names, extended data, order).
Usage: python register_outlines.py <labelInput>
"""
import os, re, sys, json, csv
import numpy as np
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import seal3d as S

_PM = re.compile(r'<(?:\w+:)?Placemark\b.*?</(?:\w+:)?Placemark>', re.S)
_CO = re.compile(r'(<(?:\w+:)?coordinates>)(.*?)(</(?:\w+:)?coordinates>)', re.S)


def _shift_coords(text, dxy):
    out = []
    for tok in text.split():
        p = tok.split(',')
        if len(p) < 2:
            out.append(tok); continue
        lon, lat = float(p[0]), float(p[1])
        loc = S.lonlat_to_local(np.array([[lon, lat]]))[0] + dxy
        lon2, lat2 = S.utm_to_lonlat(np.array([loc[0] + S.SHIFT[0]]), np.array([loc[1] + S.SHIFT[1]]))
        rest = (',' + ','.join(p[2:])) if len(p) > 2 else ''
        out.append('%.9f,%.9f%s' % (float(lon2[0]), float(lat2[0]), rest))
    return ' '.join(out)


def tile_shifts(labelInput):
    """{seal index i (0-based, as seal3d numbers the outlines): [dx, dy]} and the median shift"""
    bs, date1, plys, kml = S.flight_inputs(labelInput)
    pred = os.path.join(labelInput, 'Predict')
    reg_file = os.path.join(pred, '_seal3d_reg_%s.json' % bs)
    csv_file = os.path.join(pred, 'Measurements_3D_%s.csv' % date1)
    if not os.path.exists(reg_file):
        raise FileNotFoundError('no registration file (run seal3d with reg_init first): ' + reg_file)
    reg = json.load(open(reg_file))
    per_tile = reg.get('per_tile') or {}
    glob_sh = [reg.get('reg_dx_m', 0.0), reg.get('reg_dy_m', 0.0)] if reg.get('reg_applied') else [0.0, 0.0]
    part_of = {}
    if os.path.exists(csv_file):
        with open(csv_file, newline='') as f:
            for row in csv.DictReader(f):
                part_of[row['seal_id']] = (row.get('mesh_part') or '').split('+')[0]
    shifts, used = {}, []
    recs = S.load_outlines(kml)
    for i in range(len(recs)):
        sid = '%s_%03d' % (date1, i + 1)
        part = part_of.get(sid, '')
        v = per_tile.get(bs + '_' + part) if part else None
        if v is not None:
            shifts[i] = [float(v['dx']), float(v['dy'])]
            used.append(shifts[i])
    if used:
        med = [float(np.median([u[0] for u in used])), float(np.median([u[1] for u in used]))]
    elif per_tile:
        med = [float(np.median([v['dx'] for v in per_tile.values()])), float(np.median([v['dy'] for v in per_tile.values()]))]
    else:
        med = [float(glob_sh[0]), float(glob_sh[1])]
    for i in range(len(recs)):
        shifts.setdefault(i, med)
    return recs, shifts, med, reg_file


def register_kml(labelInput, verbose=True):
    labelInput = os.path.normpath(labelInput)
    bs, date1, plys, kml = S.flight_inputs(labelInput)
    pred = os.path.join(labelInput, 'Predict')
    done_file = os.path.join(pred, '_outline_registration_%s.json' % bs)
    if os.path.exists(done_file):
        info = json.load(open(done_file))
        if verbose:
            print('outlines already registered (%s): median shift %.2f, %.2f m' % (os.path.basename(kml), *info['median']))
        return info
    recs, shifts, med, reg_file = tile_shifts(labelInput)
    text = open(kml, encoding='utf8').read()
    k_to_i = {r['k']: i for i, r in enumerate(recs)}
    pms = list(_PM.finditer(text))
    out, pos, n_sh = [], 0, 0
    for k, m in enumerate(pms):
        dxy = np.array(shifts.get(k_to_i.get(k, -1), med), float)
        block = _CO.sub(lambda c: c.group(1) + _shift_coords(c.group(2), dxy) + c.group(3), m.group(0))
        out.append(text[pos:m.start()]); out.append(block); pos = m.end(); n_sh += 1
    out.append(text[pos:])
    new = ''.join(out)
    bak = kml + '.unregistered.bak'
    if not os.path.exists(bak):
        os.rename(kml, bak)
    with open(kml, 'w', encoding='utf8') as f:
        f.write(new)
    info = {'kml': os.path.basename(kml), 'backup': os.path.basename(bak), 'registration': os.path.basename(reg_file),
            'placemarks': n_sh, 'outlines': len(recs), 'median': med,
            'shifts': {'%s_%03d' % (date1, i + 1): [round(v[0], 4), round(v[1], 4)] for i, v in shifts.items()}}
    json.dump(info, open(done_file, 'w'), indent=1)
    if verbose:
        mags = [np.hypot(*v) for v in shifts.values()]
        print('registered %d outlines of %s: median shift (%.2f, %.2f) m, |shift| %.2f-%.2f m' % (
            len(recs), bs, med[0], med[1], min(mags), max(mags)))
    return info


if __name__ == '__main__':
    if len(sys.argv) < 2:
        print(__doc__); sys.exit(1)
    register_kml(sys.argv[1])
