# 3D morphometry of hauled-out seals (PV_PL, 2026)

The modules in `Modules/LRG/` measure seals in the photogrammetric meshes of UAV orthophoto surveys of haulouts, under the
seal outlines drawn by the detector. These are the versions used in

> Usatov IA, Burkanov VN, Altukhov AV, Bowker A, Nordstrom C, Tucker S, Trukhanova IS. Success factors of three-dimensional
> reconstruction of harbor seals (*Phoca vitulina*) from UAV orthophoto surveys of haulouts: an end-to-end automated
> pipeline for population morphometry. PLOS ONE (submitted).

The code and data of the paper (the R scripts that reproduce every number, table and figure, and the datasets) are archived
on Zenodo (DOI to be added).

## Three implementations of the measurements

| Implementation | Files | What it does |
|---|---|---|
| HF1, height field 1 | `seal3d.py` (version 1.2); from R: `LRG_3d_Morphometry.r` | Rasterizes the mesh around each seal to the highest surface (DSM, 1 cm cells), fits a ground plane to a ring 20-45 cm outside the outline and measures length, width, height, girth, area and volumes inside the outline, with quality flags |
| HF2, height field 2 | `LRG_3d_Measurements_v2.r` with `FUNCTIONS_SEAL_3D_MEASUREMENTS_v2.r` | Independent R implementation of the height field; all parameters set a priori, with the values of HF1 where both do the same step |
| CB, closed body | `seal_close.py` (version 1.2); from R: `LRG_3d_Close.r` | Closes the observed top of each seal with one of four models of the unseen underside (`flat`, `ell`, `mirror`, `super`) into a watertight mesh; volume and mass |

`LRG_3d_CROP_wide.py` cuts one mesh crop per outline with 55 cm of ground around the seal (`Predict/MeshCrop_wide/`), the
input of HF2 and CB.

## Order of the steps (one flight folder, `labelInput`)

1. `Agisoft_export_model.r`: mesh tiles `<bs>_Model/<bs>_Model_N.ply` (EPSG:32610, export shift 435000 / 5451000 / -100).
2. `LRG_PredsToPolygons_level_2.r`: seal outlines `Predict/SpP_LRG_level_2_level#2_<date>.kml`.
3. HF1: `python seal3d.py <labelInput> --plots` works on the mesh tiles directly and writes
   `Predict/Measurements_3D_<date>.csv` (one row per outline), `_profiles.csv`, `.kml` and `_overview.png`.
   All flights of an archive: `python seal3d.py --root <PV_UI> --plots`.
4. `python LRG_3d_CROP_wide.py <labelInput> [PAD]` (PAD = 0.55 m): `Predict/MeshCrop_wide/*.ply`.
5. HF2: `Rscript LRG_3d_Measurements_v2.r <labelInput> [OUTDIR] [s_vert]`: `Predict/<date>_sealVolume_v2.csv` and `.kml`.
6. CB: `python seal_close.py <labelInput> [--variant super|mirror|ell|flat|all] [--s-vert 1.0]`:
   `Predict/MeshClosed/<crop>.ply`, `Predict/<date>_sealClosed.csv` and `.kml`.
   Uses the outline-mesh registration of HF1 (`Measurements_3D_<date>.csv`) when it exists.

The R wrappers `LRG_3d_Morphometry.r` and `LRG_3d_Close.r` run steps 3 and 6 for a given `labelInput` through
reticulate; set the path of the Python interpreter at their top.

## Method in brief

- **Registration.** One XY shift of all outlines of a flight (within +-1 m) that maximizes the height contrast between the
  inside of the outlines and a ring around them; applied when it is 2 cm or more.
- **Ground.** A ring 20-45 cm outside the outline. HF1 excludes neighboring outlines and 6 cm around them, splits the ring
  into 12 sectors along the outline and fits a robust (Tukey) plane through one point per sector; HF2 fits a Tukey-weighted
  plane to all ring cells, each of 12 angular sectors with the same total weight. The first 20 cm are skipped because the
  mesh descends from the body to the ground over 10-20 cm (photogrammetric skirt).
- **HF1 and HF2.** Height H = DSM - ground plane inside the outline. Centerline in the frame of the principal axes of the
  outline, stations every 1 cm: width, dorsal height, cross-section area. Length along the centerline (L_2D, comparable
  with lengths measured on the orthomosaic), maximum width and height, height-field volume V_hf, volume of elliptical
  sections V_ell, elliptical girth; seal3d 1.2 also gives slope-aligned measurements (length on the substrate plane,
  height normal to it). Indicative mass = 1020 kg/m3 x V.
- **CB.** Cross-sections every 1 cm perpendicular to the principal axis; the `super` model keeps the observed top and closes
  it with a half-ellipse from the equator down to a flat contact patch of half-width c x a on the ground (c = 0.65; the
  volumes for c = 0 and c = 0.85 bracket it). Volume from the sections (Cavalieri) and, as a check, from the closed mesh.
  Mass = 1000 and 1020 kg/m3 x V.
- **Quality flags** (HF1): `ground_partial`, `ground_rough`, `below_ground`, `mesh_gap`, `flat`, `size_outlier`; informative:
  `touching`, `shape_compact`, `bent`, `ends_raised`. Quality classes good, fair, poor, bad, fail.

## Vertical scale of the model

Without ground control, camera self-calibration in Metashape can change the vertical scale of the model: all heights and
volumes are multiplied by s = f / f_ref, while lengths and widths are not. f is the focal length refined by Metashape
(calibration table of the processing report, row F, pixels); f_ref is the calibrated value: Mini 2 2545.74, Mavic 2 Pro
4062.99, Mini 3 Pro 2795.69, Mavic 3 Thermal 2773.50 (12 MP) or 5547.00 (48 MP) pixels. A quick check from the first page
of the report: s = Flying altitude / (Ground resolution x f_ref). `s_vert` (HF2) and `--s-vert` (CB) divide heights and
volumes by a known s; the default 1 means no correction, as in the paper. Prevention: keep f fixed at the calibrated value,
or put vertical control (objects of known height) in every survey.

## Requirements

- Python 3 with numpy and matplotlib (`seal3d.py`, `LRG_3d_CROP_wide.py`); scipy for `seal_close.py`; trimesh optional
  (independent check of the closed meshes).
- R with reticulate (wrappers) and Rvcg, geometry and sf (HF2).

## Changes for the paper (October 2026)

- HF2: all parameters are set a priori, as in HF1: ground ring 20-45 cm (was 10-45 cm), the outline is used as it is (was
  grown by 5 mm); the observers' lines are used only to check the results.
- CB, `seal_close.py` 1.2: no size fit to the observers' lines (the `--scale-mode` option of version 1.1 is removed);
  volumes and masses are reported as reconstructed.
- The previous versions are in `Modules/LRG/old/` with the date suffix `_20260923`.
