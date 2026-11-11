# source("/mnt/c/Users/usato/SSL_UI/GIT/PV_PL/Modules/LRG/LRG_PredsToPolygons_level_2.r")

#labelInput= "/mnt/c/Users/usato/SSL_UI/PV_UI/2023_H0052A_OPP/20230518_104633/20230518_104633_MINI2_20m" 

library(sp)
library(EBImage)
library(sf)
library(dplyr)
library(smoothr)

Species = "LRG_level_2"

if(exists("SppBLB_fin")){remove(SppBLB_fin)}

PRJ = CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs")
date1 = substr(basename(labelInput),1,15)
predsDir = file.path(labelInput,"Predict","Preds")
SpP_pth  = file.path(labelInput,"Predict",paste0("SpP_",Species,"_",date1))
kmlPathSave = file.path(labelInput,"Predict",paste0("SpP_",Species,"_","level#2_",date1,".kml"))
unlink(kmlPathSave)

listPreds = list.files(predsDir, full.names=T, pattern=paste0("Preds_",Species))

# Финальный список обработанных полигонов (по одному на животное)
all_smoothed = list()

#########################################################################
for (f in 1:length(listPreds)) {
  PredsRDS   = readRDS(listPreds[f])
  listImageBl = PredsRDS$listImageBl
  preds       = PredsRDS$preds
  dim(preds)  = c(PredsRDS$DimPreds)

  for (q in 1:length(listImageBl)) {

    name       = basename(listImageBl[q])
    origImgPTH = listImageBl[q]
    origImg    = readImage(origImgPTH)
    dim_orig   = dim(origImg)

    bs   = tools::file_path_sans_ext(name)
    splt = strsplit(bs,"_")

    west  = as.numeric(splt[[1]][3])
    east  = as.numeric(splt[[1]][4])
    south = as.numeric(splt[[1]][1])
    north = as.numeric(splt[[1]][2])

    RefSort = data.frame(west=west, east=east, south=south, north=north)

    msk  = preds[q, , , ]
    msk2 = as.Image(msk)
    msk3 = resize(msk2, 256, 256)
    msk4 = thresh(msk3, 18, 18, 0.009)
    msk5 = fillHull(msk4)

    msk6  = dilate(msk5, makeBrush(9, shape="disc"))
    msk7  = erode(msk6,  makeBrush(7, shape="disc"))
    msk8  = fillHull(msk7)

    msk9  = resize(msk8, dim_orig[1], dim_orig[2])
    msk10 = msk9 > 0.5
    msk11 = as.Image(msk10)
    msk12 = fillHull(msk11)

    # --- Watershed: разделяем слипшиеся объекты через перешеек ---
    # distmap даёт карту расстояний до ближайшего фона (0) для каждого пикселя маски.
    # Локальные максимумы distmap — центры объектов; watershed режет по «долинам» между ними.
    dist_map  = distmap(msk12)                   # расстояние до фона
    msk13     = watershed(dist_map, tolerance=2, ext=1)
    # tolerance — минимальная «глубина» долины между двумя максимумами,
    #   при которой объекты считаются разными. Увеличь (3–5) если разрезает слишком агрессивно.
    # ext — радиус поиска локальных максимумов (1–2 обычно достаточно).
    # После watershed пиксели фона = 0, каждый объект = уникальный int.
    # Применяем маску чтобы убрать возможные артефакты за пределами msk12
    msk13     = msk13 * (msk12 > 0)
    msk13     = as.Image(msk13)

    if (max(msk13) == 0) next

    oc = ocontour(msk13)

    # ----------------------------------------------------------------
    # Морфологический анализ всех blob сразу через computeFeatures.
    #
    # msk13   — labeled image (watershed), каждый blob = уникальный int
    # msk_ref — предсказание модели в float [0..1] (до бинаризации) —
    #           используется как «интенсивность» для basic/moment.
    #           b.mean = средняя уверенность модели внутри blob (confidence score)
    # ----------------------------------------------------------------
    msk_ref = resize(msk2, dim_orig[1], dim_orig[2])   # float [0..1]

    feat_shape  = tryCatch(computeFeatures.shape(msk13),           error = function(e) NULL)
    feat_moment = tryCatch(computeFeatures.moment(msk13),          error = function(e) NULL)
    feat_basic  = tryCatch(computeFeatures.basic(msk13, msk_ref),  error = function(e) NULL)

    NSdifStep = (RefSort$north - RefSort$south) / dim_orig[2]
    WEdifStep = (RefSort$east  - RefSort$west)  / dim_orig[1]

    # ----------------------------------------------------------------
    # Обрабатываем каждый blob (животное) индивидуально
    # ----------------------------------------------------------------
    for (o in 1:length(oc)) {

      pts = data.frame(
        x = as.numeric(oc[[o]][,1]),
        y = as.numeric(oc[[o]][,2])
      )

      pts$lat = RefSort$west  + (pts$x * WEdifStep)
      pts$lon = RefSort$south + ((dim_orig[2] - pts$y) * NSdifStep)

      if (nrow(pts) < 4) next

      coords = cbind(pts$lat, pts$lon)
      coords = rbind(coords, coords[1,])

      poly_sf = tryCatch({
        pg = st_polygon(list(coords))
        st_sfc(pg, crs = 4326)
      }, error = function(e) NULL)

      if (is.null(poly_sf)) next
      poly_sf = st_make_valid(poly_sf)
      if (st_is_empty(poly_sf)) next

      poly_buf   = st_buffer(poly_sf, dist = 0.000001)
      poly_buf   = st_make_valid(poly_buf)
      poly_union = st_union(poly_buf)
      poly_union = st_collection_extract(poly_union, "POLYGON")
      poly_union = st_cast(poly_union, "POLYGON")
      poly_sf2   = st_as_sf(poly_union)

      areas    = as.numeric(st_area(poly_sf2))
      poly_sf2 = poly_sf2[areas > 0.05, ]
      if (nrow(poly_sf2) == 0) next

      poly_smooth = tryCatch(
        smooth(poly_sf2, method = "ksmooth", smoothness = 13),
        error = function(e) poly_sf2
      )
      poly_smooth = st_make_valid(poly_smooth)
      poly_smooth = poly_smooth[!st_is_empty(poly_smooth), ]
      if (nrow(poly_smooth) == 0) next

      poly_smooth = st_simplify(poly_smooth, preserveTopology = TRUE, dTolerance = 0.0005)
      poly_smooth = st_make_valid(poly_smooth)

      # --- Базовые метаданные ---
      poly_smooth$img  = name
      poly_smooth$blob = as.integer(o)
      poly_smooth$id   = paste0(name, "#", o)

      # --- shape: геометрия blob в пикселях ---
      # Индекс строки в feat_* соответствует label в watershed (1..N)
      if (!is.null(feat_shape) && o <= nrow(feat_shape)) {
        fs = feat_shape[o, ]
        poly_smooth$s_area        = round(fs["s.area"],        2)  # площадь, пикс²
        poly_smooth$s_perimeter   = round(fs["s.perimeter"],   2)  # периметр, пикс
        poly_smooth$s_radius_mean = round(fs["s.radius.mean"], 2)  # ср. радиус
        poly_smooth$s_radius_sd   = round(fs["s.radius.sd"],   2)  # разброс радиуса
        poly_smooth$s_radius_min  = round(fs["s.radius.min"],  2)
        poly_smooth$s_radius_max  = round(fs["s.radius.max"],  2)
        # Округлость: 4π·A / P² → 1.0 = идеальный круг, → 0 = вытянутый/изрезанный
        circ = (4 * pi * fs["s.area"]) / (fs["s.perimeter"]^2)
        poly_smooth$s_circularity = round(circ, 4)
      } else {
        poly_smooth$s_area = poly_smooth$s_perimeter =
        poly_smooth$s_radius_mean = poly_smooth$s_radius_sd =
        poly_smooth$s_radius_min  = poly_smooth$s_radius_max =
        poly_smooth$s_circularity = NA_real_
      }

      # --- moment: центр масс и ориентация ---
      if (!is.null(feat_moment) && o <= nrow(feat_moment)) {
        fm = feat_moment[o, ]
        poly_smooth$m_cx           = round(fm["m.cx"],          3)  # центр масс X, пикс
        poly_smooth$m_cy           = round(fm["m.cy"],          3)  # центр масс Y, пикс
        poly_smooth$m_majoraxis    = round(fm["m.majoraxis"],   2)  # большая полуось, пикс
        poly_smooth$m_minoraxis    = round(fm["m.minoraxis"],   2)  # малая полуось, пикс
        poly_smooth$m_eccentricity = round(fm["m.eccentricity"],4)  # 0 = круг, 1 = линия
        poly_smooth$m_theta        = round(fm["m.theta"],       4)  # угол главной оси, рад
      } else {
        poly_smooth$m_cx = poly_smooth$m_cy = poly_smooth$m_majoraxis =
        poly_smooth$m_minoraxis = poly_smooth$m_eccentricity =
        poly_smooth$m_theta = NA_real_
      }

      # --- basic: уверенность модели внутри blob ---
      if (!is.null(feat_basic) && o <= nrow(feat_basic)) {
        fb = feat_basic[o, ]
        poly_smooth$b_mean = round(fb["b.mean"], 4)  # ср. уверенность [0..1]
        poly_smooth$b_sd   = round(fb["b.sd"],   4)  # разброс уверенности
        poly_smooth$b_mad  = round(fb["b.mad"],  4)  # медианное абс. отклонение
        poly_smooth$b_q001 = round(fb["b.q001"], 4)  # нижний перцентиль (0.1%)
        poly_smooth$b_q999 = round(fb["b.q999"], 4)  # верхний перцентиль (99.9%)
      } else {
        poly_smooth$b_mean = poly_smooth$b_sd  = poly_smooth$b_mad  =
        poly_smooth$b_q001 = poly_smooth$b_q999 = NA_real_
      }

      all_smoothed[[length(all_smoothed)+1]] = poly_smooth
    }

    print(paste0("IMGS ", q, "/", length(listImageBl), "  PREDS  ", f, "/", length(listPreds)))
  }
}

#############################################################################################################################
# Объединяем все индивидуально обработанные полигоны в один sf-объект
if (length(all_smoothed) == 0) stop("Полигоны не найдены!")

smoothed_final = do.call(rbind, all_smoothed)
smoothed_final = st_make_valid(smoothed_final)
smoothed_final = smoothed_final[!st_is_empty(smoothed_final), ]

write_sf(smoothed_final, kmlPathSave)
print(paste0("Сохранено полигонов: ", nrow(smoothed_final), " -> ", kmlPathSave))
