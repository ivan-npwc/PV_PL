# Agisoft_mesh_smoothing_test.r  -  how the mesh settings (depth-map quality, depth filtering, face limit, mesh smoothing,
# decimation) change the 3D measurements of the same seals. One source() does everything:
#   source("C:/Users/usato/SSL_UI/GIT/PV_PL/Modules/LRG/Agisoft_mesh_smoothing_test.r")
#
# For every flight in `flights`:
#   1. copies the final project of the paper (<flight>_Model_no_water.psx + .files, plain file copy; the original is never
#      opened or written) to <flight>_smooth/<flight>_smooth_Model_no_water.psx, relinks its cameras to the photos of the
#      original folder (the project stores relative photo paths), drops the meshes of the copy and, for every
#      variant in `variants`, builds a mesh from the SAME alignment (outlines, registration and vertical scale are identical):
#        high        depth maps High (downscale 2), Mild filtering, 50M faces  <- settings of the final projects (reference);
#                    the High/Mild depth maps stored in the final project are reused when present
#        high_s2 / high_s5 / high_s15   the High mesh smoothed in Metashape (Tools > Mesh > Smooth, strength 2 / 5 / 15)
#        high_dec10  the High mesh decimated to 10 % of its faces
#        high_f5M    High depth maps, mesh built with a 5M face limit instead of 50M
#        high_re     High / Mild depth maps rebuilt from scratch (repeatability of the depth maps + mesh)
#        med         depth maps Medium (4), Mild
#        low         depth maps Low (8), Mild
#        high_mod    depth maps High, Moderate filtering
#        high_agg    depth maps High, Aggressive filtering
#        uh          depth maps Ultra High (downscale 1), Mild (mesh capped at 50M faces like the others)
#   2. exports the mesh tiles of every variant with the clipping polygons of the pipeline (as Agisoft_export_model.r)
#      into a variant flight folder  <flight>_sm-<variant>/<flight>_sm-<variant>_Model/<...>_Model_<polygon>.ply
#      and checks that the `high` tiles sit where the original tiles are (mesh_offset.py; expected 0.00 m)
#   3. re-measures the same outlines with HF1 (seal3d.py) in every variant folder -> Predict/Measurements_3D_<date>.csv
#   4. compares every variant (and the original measurements of the flight, row "orig") with `high`, seal by seal:
#      height (max, core), width, V_ell, H/W, quality flags, mesh edge length
#      -> PV_UI/mesh_smoothing_test_compare.csv  (one row per flight x variant) and mesh_smoothing_test_per_seal.csv
# Resumable: variants whose tiles / measurements exist are skipped unless overwrite = TRUE. Log: PV_UI/mesh_smoothing_test_log.txt.
# Cost per flight (M3T, ~150 photos, GPU): mesh build 4-5 min per variant, depth maps 3 min at High (Ultra High ~4x), HF1 a few
# minutes per variant; i.e. roughly 1.5-2 h per flight. include_ultra = FALSE skips "uh". The <flight>_smooth copies (2-3 GB each)
# can be deleted when the test is over. Note: seal3d.py --root and other batch scripts that look for *_Model folders will also
# find the <flight>_sm-* folders; delete or move them when the test is over.

##### SETTINGS ###################################################################################
PV_UI     = "C:/Users/usato/SSL_UI/PV_UI"
PV_PL_dir = "C:/Users/usato/SSL_UI/GIT/PV_PL"
py_pth_win = "C:\\Users\\usato\\AppData\\Local\\r-miniconda\\envs\\r-reticulate\\python.exe"
py_pth_unx = "/home/ivan/anaconda3/bin/python"

flights = c("20240918_125737_M3T_20M",      # M3T, rock plateau H0063 (many large seals, factory f)
            "20240918_140528_M3T_20M",      # M3T, rock plateau H0028 (the seal of Fig. 2)
            "20240917_091131_MINI3PRO_20M", # Mn3, slopes H0055
            "20230518_104633_MINI2_20m")    # Mn2, H0052A (coarsest GSD)
variants = c("high", "high_s2", "high_s5", "high_s15", "high_dec10", "high_f5M", "high_re", "med", "low", "high_mod", "high_agg", "uh")
include_ultra = TRUE       # FALSE: skip "uh" (Ultra High depth maps, the slowest step)
overwrite  = FALSE         # TRUE: rebuild tiles and re-measure even where results exist
do_compare = TRUE
##################################################################################################

library(reticulate); library(sf)
sys_info <- Sys.info()
py_pth = if (sys_info["sysname"] == "Windows") py_pth_win else py_pth_unx
use_python(py_pth, required = TRUE)
mod_dir  = file.path(PV_PL_dir, "Modules", "LRG")
seal3d      <- import_from_path("seal3d", path = mod_dir)
mesh_offset <- import_from_path("mesh_offset", path = mod_dir)
log_file = file.path(PV_UI, "mesh_smoothing_test_log.txt")
logit <- function(...) {msg = paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "  ", ...); message(msg); cat(msg, "\n", file = log_file, append = TRUE)}
if (!include_ultra) variants = setdiff(variants, "uh")

find_flight <- function(name) {
  for (site in list.dirs(PV_UI, recursive = FALSE)) {
    p = file.path(site, name); if (dir.exists(p) && dir.exists(file.path(p, "Predict"))) return(normalizePath(p, winslash = "/"))
    for (d in list.dirs(site, recursive = FALSE)) { p = file.path(d, name); if (dir.exists(p)) return(normalizePath(p, winslash = "/")) }
  }
  NA
}

# clipping polygons of the pipeline (Polygons/Predict, EPSG:32610, buffered 4 m) as a Python literal [[name, [[x, y], ...]], ...]
polygons_py <- function(labelInput) {
  PolPred = st_read(list.files(file.path(labelInput, "Polygons", "Predict"), full.names = TRUE)[1], quiet = TRUE)
  PolPred = st_buffer(st_transform(PolPred, "EPSG:32610"), dist = 4)
  out = c()
  for (pn in unique(PolPred$Name)) {
    co = st_coordinates(PolPred[PolPred$Name == pn, ]$geometry[[1]])[, 1:2, drop = FALSE]
    out = c(out, paste0("[\"", pn, "\", [", paste(apply(co, 1, function(x) paste0("[", x[1], ",", x[2], "]")), collapse = ","), "]]"))
  }
  paste0("[", paste(out, collapse = ","), "]")
}

variant_dir  <- function(lab, v) paste0(lab, "_sm-", v)
tiles_dir    <- function(lab, v) file.path(variant_dir(lab, v), paste0(basename(lab), "_sm-", v, "_Model"))
prepare_variant_folder <- function(lab, v) {
  vd = variant_dir(lab, v); dir.create(file.path(vd, "Predict"), recursive = TRUE, showWarnings = FALSE)
  file.copy(list.files(file.path(lab, "Predict"), pattern = "^SpP_LRG_level_2.*\\.kml$", full.names = TRUE), file.path(vd, "Predict"), overwrite = TRUE)
  dir.create(tiles_dir(lab, v), showWarnings = FALSE)
}

# ---------------------------------------------------------------- 1-2. mesh variants from the same alignment, exported as tiles
build_variants <- function(lab, todo) {
  bs = basename(lab); sm = paste0(lab, "_smooth"); dir.create(sm, showWarnings = FALSE)
  orig = normalizePath(file.path(lab, paste0(bs, "_Model_no_water.psx")), winslash = "/", mustWork = TRUE)
  copy = normalizePath(file.path(sm, paste0(bs, "_smooth_Model_no_water.psx")), winslash = "/", mustWork = FALSE)
  out_dirs = sapply(todo, function(v) normalizePath(tiles_dir(lab, v), winslash = "/", mustWork = FALSE))
  for (v in todo) prepare_variant_folder(lab, v)
  py_run_string(paste0('
import Metashape, os, json, time, shutil
print("Metashape", Metashape.app.version, flush=True)
orig = r"', orig, '"
copy = r"', copy, '"
bs = "', bs, '"
todo = ', paste0("[", paste0("\"", todo, "\"", collapse = ","), "]"), '
out_dirs = ', paste0("{", paste0("\"", todo, "\": r\"", out_dirs, "\"", collapse = ","), "}"), '
polys = ', polygons_py(lab), '
log_path = r"', normalizePath(file.path(sm, paste0(bs, "_smooth_variants.json")), winslash = "/", mustWork = FALSE), '"
info = json.load(open(log_path)) if os.path.exists(log_path) else {}

files_src = orig[:-4] + ".files"; files_dst = copy[:-4] + ".files"
if not (os.path.exists(copy) and os.path.isdir(files_dst)):   # plain file copy of the final project; the original is never opened
    t = time.time()
    if os.path.isdir(files_dst):
        shutil.rmtree(files_dst)
    shutil.copytree(files_src, files_dst)
    shutil.copy2(orig, copy)                         # the .psx is copied last: its presence means the copy is complete
    print("project copied in", round(time.time() - t), "s", flush=True)
doc = Metashape.Document()
try:
    doc.open(copy, ignore_lock=True)
except TypeError:
    doc.open(copy)
chunk = doc.chunk
# the project stores the photo paths relative to the .psx: in the copy they point into <flight>_smooth/, which has no
# photos. Relink every camera to the photo of the original flight folder (needed for depth maps and vertex colours).
orig_dir = os.path.dirname(orig); copy_dir = os.path.dirname(copy); copy_name = os.path.basename(copy_dir); orig_name = os.path.basename(orig_dir)
n_fix = n_missing = 0; pth = ""
for cam in chunk.cameras:
    if cam.photo is None:
        continue
    pth = cam.photo.path
    if os.path.exists(pth):
        continue
    cand = [pth.replace(copy_name, orig_name, 1), os.path.join(orig_dir, os.path.relpath(pth, copy_dir)) if pth.startswith(copy_dir) else None,
            os.path.join(orig_dir, "Aerial_Images_For_Model", os.path.basename(pth))]
    cand = [c for c in cand if c and os.path.exists(c)]
    if cand:
        try:
            cam.photo.path = cand[0]; n_fix += 1
        except Exception as e:
            print("photo path not assignable:", e, flush=True); n_missing += 1
    else:
        n_missing += 1
print("photo paths relinked to the original folder:", n_fix, "; still missing:", n_missing, flush=True)
if n_missing:
    raise RuntimeError("%d photos of the copied project cannot be found (last missing: %s)" % (n_missing, pth))
try:                                                # tie points and cameras are kept; the meshes of the copy are rebuilt per variant
    if chunk.models:
        chunk.remove(list(chunk.models))
except Exception as e:
    print("could not remove existing models:", e, flush=True)

DM = {"uh": (1, "Mild"), "high": (2, "Mild"), "high_re": (2, "Mild"), "high_f5M": (2, "Mild"), "med": (4, "Mild"), "low": (8, "Mild"),
      "high_mod": (2, "Moderate"), "high_agg": (2, "Aggressive")}
FILT = {"Mild": Metashape.MildFiltering, "Moderate": Metashape.ModerateFiltering, "Aggressive": Metashape.AggressiveFiltering}
FILT_CODE = {"0": "Mild", "1": "Moderate", "2": "Aggressive", "3": "None"}     # BuildDepthMaps/filter_mode in the asset meta
FACES = {"high_f5M": 5000000}
SMOOTH = {"high_s2": 2, "high_s5": 5, "high_s15": 15}
DECIM = {"high_dec10": 0.10}
current_dm = None                                    # (downscale, filter) of the depth maps now in the chunk
dm_source = None                                     # "project" (reused from the final project) or "rebuilt"
base = None                                          # the High mesh, kept for the smoothing / decimation variants

def dm_meta(dm):
    """(downscale, filter) of a depth maps asset from its meta, or None"""
    try:
        try:
            meta = dict(dm.meta)
        except Exception:
            meta = {k: dm.meta[k] for k in dm.meta.keys()}
        ds = [v for k, v in meta.items() if k.endswith("/downscale")]
        fm = [v for k, v in meta.items() if k.endswith("/filter_mode")]
        if ds and fm:
            f = str(fm[0]).strip()
            f = FILT_CODE.get(f, f.replace("FilterMode.", "").replace("Filtering", "").strip().capitalize())
            return (int(float(ds[0])), f)
    except Exception as e:
        print("depth maps meta not readable:", e, flush=True)
    return None

def ensure_depth_maps(key, force=False):
    global current_dm, dm_source
    want = DM[key]
    if current_dm is None and not force and chunk.depth_maps is not None:
        have = dm_meta(chunk.depth_maps)
        print("depth maps in the project:", have, flush=True)
        if have == want:
            current_dm = want; dm_source = "project"  # e.g. the High / Mild depth maps stored in the final project
    if force or current_dm != want:
        try:
            if chunk.depth_maps_sets:
                chunk.remove(list(chunk.depth_maps_sets))
        except Exception as e:
            print("old depth maps not removed:", e, flush=True)
        t = time.time()
        chunk.buildDepthMaps(downscale=want[0], filter_mode=FILT[want[1]], max_neighbors=16)
        try:
            chunk.depth_maps = chunk.depth_maps_sets[-1]
        except Exception:
            pass
        current_dm = want; dm_source = "rebuilt"
        print("depth maps", want, "built in", round(time.time() - t), "s", flush=True)

def nfaces(m):
    """number of faces of a model across API versions (2.3 has no Model.face_count)"""
    for f in (lambda: int(m.face_count), lambda: len(m.faces), lambda: int(m.statistics().faces)):
        try:
            return f()
        except Exception:
            pass
    return None

def nverts(m):
    for f in (lambda: int(m.vertex_count), lambda: len(m.vertices), lambda: int(m.statistics().vertices)):
        try:
            return f()
        except Exception:
            pass
    return None

def build_mesh(face_count=50000000):
    t = time.time()
    keys0 = set(mm.key for mm in chunk.models)
    chunk.buildModel(source_data=Metashape.DepthMapsData, surface_type=Metashape.Arbitrary, interpolation=Metashape.EnabledInterpolation,
                     face_count=Metashape.CustomFaceCount, face_count_custom=face_count, vertex_colors=True, volumetric_masks=False)
    new = [mm for mm in chunk.models if mm.key not in keys0]
    m = new[-1] if new else chunk.model
    set_active(m)
    print("mesh built in", round(time.time() - t), "s:", nfaces(m), "faces", flush=True)
    return m

def set_active(m):
    try:
        chunk.model = m
    except Exception as e:
        print("chunk.model not assignable:", e, flush=True)

def high_mesh():
    """the High mesh (built once per run), set as the active model"""
    global base
    if base is None or base.key not in [mm.key for mm in chunk.models]:
        ensure_depth_maps("high")
        base = build_mesh()
    set_active(base)
    return base

def derived(kind, param):
    """the High mesh smoothed (kind "smooth", param = strength) or decimated (kind "decimate", param = fraction of the faces).
    Metashape 2.x: the operation with replace_asset=False creates a new model asset and leaves the High mesh untouched.
    Older API (no model / replace_asset arguments): the operation is done in place on a copy of the High mesh."""
    global base
    b = high_mesh()
    keys0 = set(mm.key for mm in chunk.models)
    t = time.time()
    try:
        if kind == "smooth":
            chunk.smoothModel(strength=param, model=b, replace_asset=False)
        else:
            chunk.decimateModel(face_count=int(nfaces(b) * param), model=b, replace_asset=False)
        new = [mm for mm in chunk.models if mm.key not in keys0]
        if new:
            res = new[-1]
        else:
            print("WARNING: no new model asset; the High mesh was modified in place and is rebuilt for the next variant", flush=True)
            res = b; base = None
    except TypeError:
        try:
            m = b.copy()
        except Exception as e:
            print("Model.copy() not available (", e, "): the High mesh is rebuilt for this variant", flush=True)
            ensure_depth_maps("high"); m = build_mesh()
        set_active(m)
        if kind == "smooth":
            chunk.smoothModel(strength=param)
        else:
            chunk.decimateModel(face_count=int(nfaces(m) * param))
        res = m
    set_active(res)
    print(kind, param, "done in", round(time.time() - t), "s:", nfaces(res), "faces", flush=True)
    return res

def cleanup_models():
    """remove every model except the High mesh (the tiles are what the measurements use; keeps the copy small)"""
    extra = [mm for mm in chunk.models if base is None or mm.key != base.key]
    if extra:
        try:
            chunk.remove(extra)
        except Exception as e:
            print("models not removed:", e, flush=True)
    if base is not None:
        set_active(base)

def export_tiles(out_dir, tag, model=None):
    """tiles clipped by the pipeline polygons, EPSG:32610 with the shift of Agisoft_export_model.r; the chunk crs and the
    shapes of the copy are left as they are (the clipping polygon is added in the shapes crs and removed afterwards)"""
    crs_utm = Metashape.CoordinateSystem("EPSG::32610")
    if chunk.shapes is None:
        chunk.shapes = Metashape.Shapes(); chunk.shapes.crs = crs_utm
    scrs = chunk.shapes.crs
    try:
        same = (scrs is None) or (scrs.authority == crs_utm.authority)
    except Exception:
        same = str(scrs) == str(crs_utm)
    if scrs is None:
        chunk.shapes.crs = crs_utm
    os.makedirs(out_dir, exist_ok=True)
    kw = dict(format=Metashape.ModelFormatPLY, binary=True, precision=9, texture_format=Metashape.ImageFormatPNG, save_texture=True,
              save_uv=True, strip_extensions=False, clip_to_boundary=True, crs=crs_utm, shift=Metashape.Vector((435000, 5451000, -100)))
    for name, coords in polys:
        if same:
            pts = [Metashape.Vector((x, y)) for x, y in coords]
        else:
            pts = [Metashape.CoordinateSystem.transform(Metashape.Vector((x, y, 0)), crs_utm, scrs) for x, y in coords]
            pts = [Metashape.Vector((p.x, p.y)) for p in pts]
        shape = chunk.shapes.addShape()
        shape.geometry = Metashape.Geometry.Polygon(pts)
        shape.label = "Cropping Polygon"
        shape.boundary_type = Metashape.Shape.BoundaryType.OuterBoundary
        path = os.path.join(out_dir, "%s_Model_%s.ply" % (tag, name))
        if model is not None:
            try:
                chunk.exportModel(path, model=model, **kw)
            except TypeError:
                set_active(model); chunk.exportModel(path, **kw)
        else:
            chunk.exportModel(path, **kw)
        try:
            chunk.shapes.remove([shape])
        except Exception as e:
            print("clipping polygon not removed:", e, flush=True)

def record(v, m, t0, extra=None):
    d = {"faces": nfaces(m) if m else None, "vertices": nverts(m) if m else None, "seconds": round(time.time() - t0), "depth_maps": dm_source}
    if extra:
        d.update(extra)
    info[v] = d
    json.dump(info, open(log_path, "w"), indent=1)
    print("variant", v, d, flush=True)

for v in todo:
    t0 = time.time()
    tag = bs + "_sm-" + v
    if v == "high":
        m = high_mesh()
        record(v, m, t0, {"depth_downscale": 2, "filter": "Mild", "face_limit": 50000000})
        export_tiles(out_dirs[v], tag, m)            # the High mesh stays in the project (base of the next variants)
        doc.save()
        continue
    if v in DM:
        ensure_depth_maps(v, force=(v == "high_re"))
        m = build_mesh(FACES.get(v, 50000000))
        record(v, m, t0, {"depth_downscale": DM[v][0], "filter": DM[v][1], "face_limit": FACES.get(v, 50000000)})
    elif v in SMOOTH:
        m = derived("smooth", SMOOTH[v])
        record(v, m, t0, {"depth_downscale": 2, "filter": "Mild", "face_limit": 50000000, "smooth_strength": SMOOTH[v]})
    elif v in DECIM:
        n = nfaces(high_mesh())
        m = derived("decimate", DECIM[v])
        record(v, m, t0, {"depth_downscale": 2, "filter": "Mild", "face_limit": 50000000, "faces_before": n, "decimated_to": int(n * DECIM[v])})
    else:
        print("unknown variant", v, flush=True)
        continue
    export_tiles(out_dirs[v], tag, m)
    cleanup_models()
    doc.save()
if current_dm is not None and current_dm != DM["high"]:
    try:
        chunk.remove(list(chunk.depth_maps_sets))   # do not leave Ultra High depth maps in the copy
        doc.save()
    except Exception:
        pass
del doc
'))
  invisible(TRUE)
}

# ---------------------------------------------------------------- 4. comparison with the "high" variant, seal by seal
num <- function(x) suppressWarnings(as.numeric(x))
read_m3d <- function(folder) {
  bs = basename(folder); f = file.path(folder, "Predict", paste0("Measurements_3D_", substr(bs, 1, 15), ".csv"))
  if (!file.exists(f)) return(NULL)
  d = read.csv(f, stringsAsFactors = FALSE); d$HW_core = num(d$H_core_max_m) / num(d$W_max_m); d
}
compare_variants <- function(labs) {
  out = NULL; per_seal = NULL
  for (lab in labs) {
    ref = read_m3d(variant_dir(lab, "high")); if (is.null(ref)) {logit("no reference (high) measurements for ", basename(lab)); next}
    ref_ok = ref[ref$status == "ok" & ref$quality %in% c("good", "fair"), ]
    vinfo = tryCatch(jsonlite::fromJSON(file.path(paste0(lab, "_smooth"), paste0(basename(lab), "_smooth_variants.json"))), error = function(e) list())
    for (v in c("orig", variants)) {
      d = if (v == "orig") read_m3d(lab) else read_m3d(variant_dir(lab, v)); if (is.null(d)) next
      m = merge(ref_ok[, c("seal_id", "L_2D_m", "H_max_m", "H_core_max_m", "W_max_m", "V_ell_m3", "HW_core")],
                d[d$status == "ok", c("seal_id", "quality", "H_max_m", "H_core_max_m", "W_max_m", "V_ell_m3", "HW_core", "mesh_edge_med_m", "ground_cells_mad_m")],
                by = "seal_id", suffixes = c("_ref", ""))
      big = m[num(m$L_2D_m) > 0.93, ]; small = m[num(m$L_2D_m) <= 0.64, ]
      rat = function(x, y) round(median(num(x) / num(y), na.rm = TRUE), 3)
      dcm = function(x, y) round(100 * median(num(x) - num(y), na.rm = TRUE), 2)
      out = rbind(out, data.frame(flight = basename(lab), variant = v, faces = if (!is.null(vinfo[[v]]$faces)) vinfo[[v]]$faces else NA,
                                  n_ref_ok = nrow(ref_ok), n_matched = nrow(m), n_ok_variant = sum(d$status == "ok" & d$quality %in% c("good", "fair")),
                                  still_good_fair_pct = round(100 * mean(m$quality %in% c("good", "fair")), 1),
                                  H_ratio = rat(m$H_max_m, m$H_max_m_ref), H_core_ratio = rat(m$H_core_max_m, m$H_core_max_m_ref),
                                  dH_cm = dcm(m$H_max_m, m$H_max_m_ref), dH_core_cm = dcm(m$H_core_max_m, m$H_core_max_m_ref),
                                  W_ratio = rat(m$W_max_m, m$W_max_m_ref), V_ratio = rat(m$V_ell_m3, m$V_ell_m3_ref), HW_ratio = rat(m$HW_core, m$HW_core_ref),
                                  H_ratio_large = if (nrow(big) >= 5) rat(big$H_max_m, big$H_max_m_ref) else NA,
                                  H_ratio_small = if (nrow(small) >= 5) rat(small$H_max_m, small$H_max_m_ref) else NA,
                                  n_large = nrow(big), n_small = nrow(small),
                                  mesh_edge_cm = round(100 * median(num(m$mesh_edge_med_m), na.rm = TRUE), 2),
                                  ground_mad_cm = round(100 * median(num(m$ground_cells_mad_m), na.rm = TRUE), 2), row.names = NULL))
      per_seal = rbind(per_seal, data.frame(flight = basename(lab), variant = v, seal_id = m$seal_id, L_2D_m = m$L_2D_m, quality = m$quality,
                                            H_ratio = num(m$H_max_m) / num(m$H_max_m_ref), H_core_ratio = num(m$H_core_max_m) / num(m$H_core_max_m_ref),
                                            W_ratio = num(m$W_max_m) / num(m$W_max_m_ref), V_ratio = num(m$V_ell_m3) / num(m$V_ell_m3_ref)))
    }
  }
  list(summary = out, per_seal = per_seal)
}

# ---------------------------------------------------------------- main loop
logit("==== mesh smoothing test: ", length(flights), " flights x ", length(variants), " variants (", paste(variants, collapse = ", "), ")")
done = c()
for (fl in flights) {
  lab = find_flight(fl)
  if (is.na(lab)) {logit("NOT FOUND  ", fl); next}
  t0 = Sys.time(); logit("---- ", lab)
  ok = tryCatch({
    todo = variants[sapply(variants, function(v) overwrite || length(list.files(tiles_dir(lab, v), pattern = "\\.ply$")) == 0)]
    if (length(todo)) {logit("building: ", paste(todo, collapse = ", ")); build_variants(lab, todo)} else logit("all tiles exist")
    if ("high" %in% todo) {                      # the rebuilt tiles must sit where the original tiles are
      off = tryCatch(mesh_offset$tile_offset(file.path(lab, paste0(basename(lab), "_Model")), tiles_dir(lab, "high"), search = 2, verbose = FALSE),
                     error = function(e) NULL)
      if (is.null(off)) logit("offset check high vs original tiles: no result") else
        logit("offset high vs original tiles: dx=", round(off[[1]], 2), " dy=", round(off[[2]], 2), " m",
              if (max(abs(unlist(off))) > 0.05) "   <-- UNEXPECTED, check the export" else "   (ok)")
    }
    for (v in variants) {
      vd = variant_dir(lab, v); m3d = file.path(vd, "Predict", paste0("Measurements_3D_", substr(fl, 1, 15), ".csv"))
      if (length(list.files(tiles_dir(lab, v), pattern = "\\.ply$")) == 0) {logit("no tiles for ", v); next}
      if (file.exists(m3d) && !overwrite && !(v %in% todo)) next
      prepare_variant_folder(lab, v)
      if (overwrite || v %in% todo) unlink(list.files(file.path(vd, "Predict"), pattern = "^_seal3d_(cache|reg)_", full.names = TRUE))
      st3d = seal3d$run_flight(normalizePath(vd), res = 0.01, plots = FALSE)      # same alignment as the original: no initial shift needed
      logit("HF1 ", v, ": ", st3d)
    }
    TRUE
  }, error = function(e) {logit("ERROR  ", fl, ": ", conditionMessage(e)); FALSE})
  logit(if (ok) "done  " else "failed  ", fl, "  ", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), " min")
  if (ok) done = c(done, lab)
}
if (do_compare && length(done)) {
  cmp = compare_variants(done)
  if (!is.null(cmp$summary)) {
    print(cmp$summary, row.names = FALSE)
    write.csv(cmp$summary, file.path(PV_UI, "mesh_smoothing_test_compare.csv"), row.names = FALSE)
    write.csv(cmp$per_seal, file.path(PV_UI, "mesh_smoothing_test_per_seal.csv"), row.names = FALSE)
    logit("comparison written to ", file.path(PV_UI, "mesh_smoothing_test_compare.csv"))
  }
}
