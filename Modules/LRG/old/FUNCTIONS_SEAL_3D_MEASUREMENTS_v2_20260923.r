# FUNCTIONS_SEAL_3D_MEASUREMENTS_v2.r
# Improved 3D measurements of seals from MeshCrop meshes (Predict/MeshCrop/*.ply, LRG_3d_CROP.r).
# Version 2 (2026-09), built on Ivan Usatov's LRG_3D_Measurenents.r / FUNCTIONS_SEAL_3D_MEASUREMENTS.r and
# calibrated against the observers' manual lengths and widths (Observer_count/Length.kml, Width.kml).
#
# What changed against the original functions and why (details in README_LRG_R_v2.md):
#  * No random re-sampling + ball pivoting (mesh_reconstract, slice filters, seal_pca): the Metashape mesh is
#    used as it is (small pieces removed), so results do not depend on the random state and nothing is eroded.
#  * World Z is the vertical; the body axis comes from the outline in plan view (no upside-down check).
#  * Body footprint = the seal outline of LRG_PredsToPolygons_level_2.r (SpP_LRG_level_2_level#2_<date>.kml).
#    MeshCrop is cut with the larger SpP_LRG_<date>.kml polygons, so every crop has 15-30 cm of ground around
#    the seal: the ground plane is fitted to that ring (robust, Tukey biweight). Without an outline the body is
#    found from the heights alone (mode "mesh", less accurate: see the README).
#  * The surface is rasterised on a 1 cm grid as a DSM (highest triangle over each cell, so a second surface below,
#    e.g. water or a ghost layer of the model, is ignored); holes are
#    filled from their neighbours. No Laplacian smoothing (300 iterations shrank the body by 8 % in length,
#    6 % in width and 32 % in height), no watertight loop, no scale factors 1.22 / 1.33.
#  * Measurements: plan length along the curved midline (as the observers measure) and maximum width across it;
#    height above ground; spine = 3D length of the dorsal ridge (as get_spine, on the undistorted surface);
#    girth of the cross-sections (dorsal arc + ground chord) and the envelope "chest" of the original code;
#    volume between the surface and the ground; indicative mass = rho * V (rho = 1020 kg/m3), not 3000 * V.
#  * The hard filters (file < 90 kB, "flat" < 0.21 m) are replaced by quality flags.

v2_defaults <- list(
  shift = c(435000, 5451000),  # local mesh coordinates = UTM 10N - shift (as in LRG_3d_CROP.r)
  crs = 32610,
  cell = 0.01,           # raster cell, m
  ring_gap = 0.10,       # ground ring starts this far outside the outline, m (the mesh rises 5.6, 3.1, 1.2 and
                         #   0.2 cm above the ground 2, 4, 6 and 8 cm from the outline: photogrammetric skirt)
  ring_min = 100,        # fewer ring cells than this: the gap is reduced step by step (2 cm) down to 4 cm
  ring_max = 0.45,       # ground ring ends this far from the outline, m (MeshCrop crops end at 15-30 cm anyway)
  ring_c = 4.685,        # Tukey biweight constant (in MAD units) for the ground plane
  in_delta = 0.0,        # inside the outline, cells lower than this above ground are not body, m (> 0 was worse)
  out_grow = 0.005,      # grow the outline by 5 mm: removes the width bias against the observers (-1.1 -> +0.2 cm)
  delta = 0.08,          # mode "mesh": body = cells higher than delta above the ground plane, m (best of 3-8 cm)
  seed_frac = 0.5,       # mode "mesh": body must be connected to cells higher than seed_frac * max height
  min_comp_frac = 0.05,  # mesh pieces smaller than this share of the faces are removed
  k_med = 5,             # running-median window along the body (stations)
  k_mean = 5,            # running-mean window after the median
  core = c(0.15, 0.85),  # core part of the body (share of length) for H_core and girth
  rho = 1020,            # kg/m3 for the indicative mass
  s_vert = 1             # vertical scale factor of the model (heights are divided by s_vert)
)

## ---------------------------------------------------------------- small helpers
run_med <- function(x, k) {
  if (length(x) < 3 || k < 3) return(x)
  k <- min(k, length(x) - (1 - length(x) %% 2)); if (k %% 2 == 0) k <- k - 1
  if (k < 3) return(x)
  as.numeric(stats::runmed(x, k, endrule = "median"))
}
run_mean <- function(x, k) {
  if (length(x) < 3 || k < 2) return(x)
  h <- k %/% 2; n <- length(x)
  vapply(seq_len(n), function(i) mean(x[max(1, i - h):min(n, i + h)]), 0)
}
smooth_prof <- function(x, p) run_mean(run_med(x, p$k_med), p$k_mean)

# point in polygon (ray casting), vectorised over points; poly = n x 2 matrix (open or closed ring).
# Only the points inside the bounding box of the polygon are tested (the rest are FALSE).
pip <- function(x, y, poly) {
  inside <- rep(FALSE, length(x))
  cand <- which(x >= min(poly[, 1]) & x <= max(poly[, 1]) & y >= min(poly[, 2]) & y <= max(poly[, 2]))
  if (!length(cand)) return(inside)
  xc <- x[cand]; yc <- y[cand]; res <- rep(FALSE, length(cand))
  n <- nrow(poly); j <- n
  for (i in seq_len(n)) {
    xi <- poly[i, 1]; yi <- poly[i, 2]; xj <- poly[j, 1]; yj <- poly[j, 2]
    hit <- ((yi > yc) != (yj > yc)) & (xc < (xj - xi) * (yc - yi) / (yj - yi + 1e-300) + xi)
    res <- xor(res, hit); j <- i
  }
  inside[cand] <- res
  inside
}

# binary erosion / dilation with a (2r+1) x (2r+1) square, separable (rows, then columns)
shift_op <- function(M, r, fun) {
  nr <- nrow(M); nc <- ncol(M)
  one_dir <- function(M, byrow) {
    out <- M
    for (d in c(-r:-1, 1:r)) {
      S <- matrix(fun == "and", nr, nc)
      if (byrow) { ii <- max(1, 1 + d):min(nr, nr + d); if (length(ii) && ii[1] <= ii[length(ii)]) S[ii, ] <- M[ii - d, , drop = FALSE] }
      else { jj <- max(1, 1 + d):min(nc, nc + d); if (length(jj) && jj[1] <= jj[length(jj)]) S[, jj] <- M[, jj - d, drop = FALSE] }
      out <- if (fun == "and") out & S else out | S
    }
    out
  }
  one_dir(one_dir(M, TRUE), FALSE)
}
dilate <- function(M, r) if (r <= 0) M else shift_op(M, r, "or")
erode <- function(M, r) if (r <= 0) M else shift_op(M, r, "and")

# 4-connected components of a logical matrix: label matrix
label_components <- function(M) {
  lab <- matrix(0L, nrow(M), ncol(M)); cur <- 0L; nr <- nrow(M)
  for (s in which(M)) {
    if (lab[s] != 0L) next
    cur <- cur + 1L; queue <- s; lab[s] <- cur; head <- 1L
    while (head <= length(queue)) {
      q <- queue[head]; head <- head + 1L
      i <- (q - 1L) %% nr + 1L; j <- (q - 1L) %/% nr + 1L
      nb <- c(if (i > 1) q - 1L, if (i < nr) q + 1L, if (j > 1) q - nr, if (j < ncol(M)) q + nr)
      nb <- nb[M[nb] & lab[nb] == 0L]
      if (length(nb)) { lab[nb] <- cur; queue <- c(queue, nb) }
    }
  }
  lab
}
largest_component <- function(M) {
  lab <- label_components(M); if (max(lab) == 0) return(M)
  tb <- tabulate(lab[lab > 0]); lab == which.max(tb)
}

# fill NA cells that are not connected to the raster border through NA cells (holes), from their neighbours
fill_holes <- function(Z) {
  na <- is.na(Z); nr <- nrow(Z); nc <- ncol(Z)
  if (!any(na)) return(list(Z = Z, filled = matrix(FALSE, nr, nc)))
  lab <- label_components(na)
  edge_labs <- unique(c(lab[1, ], lab[nr, ], lab[, 1], lab[, nc])); edge_labs <- edge_labs[edge_labs > 0]
  hole <- na & !(lab %in% edge_labs)
  for (it in 1:300) {
    todo <- which(hole & is.na(Z)); if (!length(todo)) break
    i <- (todo - 1L) %% nr + 1L; j <- (todo - 1L) %/% nr + 1L
    Z[todo] <- vapply(seq_along(todo), function(t) {
      v <- Z[max(1, i[t] - 1):min(nr, i[t] + 1), max(1, j[t] - 1):min(nc, j[t] + 1)]
      if (all(is.na(v))) NA_real_ else mean(v, na.rm = TRUE)
    }, 0)
  }
  list(Z = Z, filled = hole)
}

## ---------------------------------------------------------------- inputs
read_crop <- function(path, p = v2_defaults) {
  m <- Rvcg::vcgImport(path, silent = TRUE)
  nf <- ncol(m$it)
  if (is.null(nf) || nf < 50) return(NULL)
  m <- Rvcg::vcgIsolated(m, facenum = max(20, round(p$min_comp_frac * nf)), silent = TRUE)
  if (is.null(m$it) || ncol(m$it) < 50) return(NULL)
  m
}

# seal outlines (SpP_LRG_level_2_level#2_<date>.kml) as a list of rings in local mesh coordinates
read_outlines <- function(kml, p = v2_defaults) {
  x <- sf::st_read(kml, quiet = TRUE)
  x <- sf::st_transform(sf::st_zm(x), p$crs)
  x <- x[sf::st_geometry_type(x) %in% c("POLYGON", "MULTIPOLYGON"), ]
  rings <- lapply(sf::st_geometry(x), function(g) {
    cc <- sf::st_coordinates(g)
    keep <- cc[, "L1"] == 1
    if ("L2" %in% colnames(cc)) keep <- keep & cc[, "L2"] == 1
    sweep(cc[keep, 1:2, drop = FALSE], 2, p$shift)
  })
  cen <- t(vapply(rings, function(r) colMeans(r[-nrow(r), , drop = FALSE]), c(0, 0)))
  list(rings = rings, cen = cen)
}

# outline that belongs to a MeshCrop file (centroid in the file name, "...#E<utm>N<utm>.ply")
match_outline <- function(file, ol, p = v2_defaults, tol = 0.35) {
  nm <- sub("\\.ply$", "", strsplit(basename(file), "#")[[1]][2])      # "E<utm>N<utm>" (+ optional "_id###")
  e <- as.numeric(sub("^E([0-9.]+)N.*$", "\\1", nm)); n <- as.numeric(sub("^E[0-9.]+N([0-9.]+)(_.*)?$", "\\1", nm))
  c0 <- c(e, n) - p$shift
  d <- sqrt((ol$cen[, 1] - c0[1])^2 + (ol$cen[, 2] - c0[2])^2)
  k <- which.min(d)
  if (!length(k) || d[k] > tol) return(NULL)
  near <- which(d < 3 & seq_along(d) != k)
  list(ring = ol$rings[[k]], others = ol$rings[near], dist = d[k], E = e, N = n)
}

## ---------------------------------------------------------------- ground
# robust plane z = b0 + b1 (x - cx) + b2 (y - cy): L1 start, then Tukey biweight IRLS
fit_plane_robust <- function(x, y, z, c = 4.685, iter = 30) {
  cx <- mean(x); cy <- mean(y); X <- cbind(1, x - cx, y - cy)
  b <- as.numeric(qr.solve(X, z))
  for (k in seq_len(iter)) {
    r <- as.numeric(z - X %*% b); sw <- sqrt(1 / pmax(abs(r), 1e-4))
    bn <- as.numeric(qr.solve(X * sw, z * sw)); if (max(abs(bn - b)) < 1e-7) { b <- bn; break }; b <- bn
  }
  for (k in seq_len(iter)) {
    r <- as.numeric(z - X %*% b); s <- max(stats::mad(r), 0.002)
    u <- r / (c * s); w <- ifelse(abs(u) < 1, (1 - u^2)^2, 0)
    if (sum(w > 0) < 10) break
    sw <- sqrt(w)
    bn <- as.numeric(qr.solve(X * sw, z * sw)); if (max(abs(bn - b)) < 1e-7) { b <- bn; break }; b <- bn
  }
  r <- as.numeric(z - X %*% b)
  list(b = b, cx = cx, cy = cy, resid = r, mad = stats::mad(r))
}
plane_z <- function(g, x, y) g$b[1] + g$b[2] * (x - g$cx) + g$b[3] * (y - g$cy)

## ---------------------------------------------------------------- raster of the surface
# DSM of a mesh on a regular grid: for every cell centre the HIGHEST triangle that covers it (a second surface
# below - water, a ghost layer of the model - is ignored). Fully vectorised over (triangle, cell) pairs.
dsm_maxz <- function(U, W, Z, Fi, ug, vg) {
  cell <- ug[2] - ug[1]; nu <- length(ug); nv <- length(vg)
  tu <- matrix(U[Fi], ncol = 3); tv <- matrix(W[Fi], ncol = 3); tz <- matrix(Z[Fi], ncol = 3)
  i0 <- pmax(1L, ceiling((pmin(tu[, 1], tu[, 2], tu[, 3]) - ug[1]) / cell - 1e-9) + 1L)
  i1 <- pmin(nu, floor((pmax(tu[, 1], tu[, 2], tu[, 3]) - ug[1]) / cell + 1e-9) + 1L)
  j0 <- pmax(1L, ceiling((pmin(tv[, 1], tv[, 2], tv[, 3]) - vg[1]) / cell - 1e-9) + 1L)
  j1 <- pmin(nv, floor((pmax(tv[, 1], tv[, 2], tv[, 3]) - vg[1]) / cell + 1e-9) + 1L)
  ni <- pmax(0L, i1 - i0 + 1L); nj <- pmax(0L, j1 - j0 + 1L); cnt <- ni * nj
  keep <- which(cnt > 0); if (!length(keep)) return(rep(NA_real_, nu * nv))
  f <- rep(keep, cnt[keep]); k <- sequence(cnt[keep]) - 1L
  ii <- i0[f] + k %% ni[f]; jj <- j0[f] + k %/% ni[f]
  px <- ug[ii]; py <- vg[jj]
  x1 <- tu[f, 1]; y1 <- tv[f, 1]; x2 <- tu[f, 2]; y2 <- tv[f, 2]; x3 <- tu[f, 3]; y3 <- tv[f, 3]
  det <- (y2 - y3) * (x1 - x3) + (x3 - x2) * (y1 - y3)
  ok <- abs(det) > 1e-14
  l1 <- ((y2 - y3) * (px - x3) + (x3 - x2) * (py - y3)) / det
  l2 <- ((y3 - y1) * (px - x3) + (x1 - x3) * (py - y3)) / det
  l3 <- 1 - l1 - l2
  tol <- -1e-7
  ins <- ok & l1 >= tol & l2 >= tol & l3 >= tol
  if (!any(ins)) return(rep(NA_real_, nu * nv))
  z <- (l1 * tz[f, 1] + l2 * tz[f, 2] + l3 * tz[f, 3])[ins]
  cid <- (ii + (jj - 1L) * nu)[ins]
  o <- order(cid, -z)
  cid <- cid[o]; z <- z[o]
  first <- !duplicated(cid)
  out <- rep(NA_real_, nu * nv); out[cid[first]] <- z[first]
  out
}

# 1) grid in the body frame (rows = u along the body, columns = v across) with the mesh heights Z (no ground yet)
seal_grid <- function(m, ol = NULL, p = v2_defaults) {
  V <- t(m$vb[1:3, ]); Fi <- t(m$it)
  if (!is.null(ol)) {                                   # body frame from the outline
    ring <- ol$ring
    seg <- sqrt(rowSums(diff(ring)^2)); tt0 <- c(0, cumsum(seg))
    tt <- seq(0, max(tt0), by = p$cell)
    P <- cbind(stats::approx(tt0, ring[, 1], tt)$y, stats::approx(tt0, ring[, 2], tt)$y)
    c0 <- colMeans(P)
    ev <- eigen(stats::cov(P), symmetric = TRUE)$vectors
  } else {                                              # body frame from the high part of the mesh
    border <- Rvcg::vcgBorder(m)$bordervb == 1
    g0 <- fit_plane_robust(V[border, 1], V[border, 2], V[border, 3])
    hv <- V[, 3] - plane_z(g0, V[, 1], V[, 2])
    hi <- hv > stats::quantile(hv, 0.7)
    c0 <- colMeans(V[hi, 1:2, drop = FALSE])
    ev <- eigen(stats::cov(V[hi, 1:2, drop = FALSE]), symmetric = TRUE)$vectors
  }
  e1 <- ev[, 1]; if (e1[1] < 0) e1 <- -e1
  e2 <- c(-e1[2], e1[1])
  U <- (V[, 1] - c0[1]) * e1[1] + (V[, 2] - c0[2]) * e1[2]
  W <- (V[, 1] - c0[1]) * e2[1] + (V[, 2] - c0[2]) * e2[2]
  cell <- p$cell
  ug <- seq(floor(min(U) / cell) * cell - cell, ceiling(max(U) / cell) * cell + cell, by = cell)
  vg <- seq(floor(min(W) / cell) * cell - cell, ceiling(max(W) / cell) * cell + cell, by = cell)
  G <- expand.grid(u = ug, v = vg)
  nu <- length(ug)
  Xg <- c0[1] + G$u * e1[1] + G$v * e2[1]; Yg <- c0[2] + G$u * e1[2] + G$v * e2[2]
  zq <- dsm_maxz(U, W, V[, 3], Fi, ug, vg)
  list(Z = matrix(zq, nrow = nu), Xm = matrix(Xg, nrow = nu), Ym = matrix(Yg, nrow = nu), u = ug, v = vg, c0 = c0, e1 = e1, e2 = e2,
       n_vert = nrow(V), n_face = nrow(Fi), border_xyz = if (is.null(ol)) V[Rvcg::vcgBorder(m)$bordervb == 1, , drop = FALSE] else NULL)
}

# ground plane from ring cells; every angular sector around the seal gets the same total weight (a neighbour or
# a rock on one side cannot tilt the plane), Tukey biweight against outliers
fit_ground_sectors <- function(x, y, z, cx, cy, nsec = 12, c = 4.685, iter = 30) {
  sec <- floor(((atan2(y - cy, x - cx) + pi) / (2 * pi)) * nsec) %% nsec
  ns <- tabulate(sec + 1, nsec); wsec <- ifelse(ns[sec + 1] > 0, 1 / ns[sec + 1], 0)
  xm <- mean(x); ym <- mean(y); X <- cbind(1, x - xm, y - ym)
  b <- as.numeric(qr.solve(X * sqrt(wsec), z * sqrt(wsec)))
  for (k in seq_len(iter)) {
    r <- as.numeric(z - X %*% b); s <- max(stats::mad(r), 0.002)
    u <- r / (c * s); w <- ifelse(abs(u) < 1, (1 - u^2)^2, 0) * wsec
    if (sum(w > 0) < 10) break
    sw <- sqrt(w)
    bn <- as.numeric(qr.solve(X * sw, z * sw)); if (max(abs(bn - b)) < 1e-7) { b <- bn; break }; b <- bn
  }
  r <- as.numeric(z - X %*% b)
  list(b = b, cx = xm, cy = ym, resid = r, mad = stats::mad(r), n_sectors = sum(ns >= 5))
}

# 2) outline masks (shifted by the flight registration), ground ring, ground plane, heights above ground
finish_raster <- function(G, ol = NULL, p = v2_defaults, shift = c(0, 0)) {
  cell <- p$cell; nu <- nrow(G$Z); nv <- ncol(G$Z)
  Z <- G$Z; Xm <- G$Xm; Ym <- G$Ym; mesh_cov <- !is.na(Z)
  inside <- NULL
  if (!is.null(ol)) {
    ring <- sweep(ol$ring, 2, shift, "+")
    inside <- matrix(pip(Xm, Ym, ring), nrow = nu)
    other <- matrix(FALSE, nu, nv)
    for (o in ol$others) other <- other | matrix(pip(Xm, Ym, sweep(o, 2, shift, "+")), nrow = nu)
    gap_used <- NA
    for (gap_m in seq(p$ring_gap, 0.04, by = -0.02)) {
      gap <- max(1, round(gap_m / cell))
      ring_cells <- mesh_cov & !dilate(inside, gap) & !dilate(other, gap) & dilate(inside, round(p$ring_max / cell))
      if (sum(ring_cells) >= p$ring_min) { gap_used <- gap_m; break }
    }
    if (is.na(gap_used)) { ring_cells <- mesh_cov & !dilate(inside, 4) & !other; gap_used <- 0.04 }
    if (sum(ring_cells) < 10) return(NULL)
    cxy <- colMeans(ring[-nrow(ring), , drop = FALSE])
    g <- fit_ground_sectors(Xm[ring_cells], Ym[ring_cells], Z[ring_cells], cxy[1], cxy[2], c = p$ring_c)
    if (p$out_grow != 0) {                              # exact buffer of the outline polygon (sf)
      pg <- sf::st_buffer(sf::st_polygon(list(rbind(ring, ring[1, ]))), p$out_grow)
      rg <- sf::st_coordinates(pg); rg <- rg[rg[, "L1"] == 1 & rg[, "L2"] == 1, 1:2, drop = FALSE]
      inside <- matrix(pip(Xm, Ym, rg), nrow = nu)
    }
    n_ring <- sum(ring_cells)
  } else {
    B <- G$border_xyz
    g <- fit_plane_robust(B[, 1], B[, 2], B[, 3], c = p$ring_c); g$n_sectors <- NA
    n_ring <- nrow(B); gap_used <- NA
  }
  H <- Z - plane_z(g, Xm, Ym)
  fh <- fill_holes(H)
  list(H = fh$Z / p$s_vert, filled = fh$filled, mesh_cov = mesh_cov, inside = inside, u = G$u, v = G$v, g = g,
       c0 = G$c0, e1 = G$e1, e2 = G$e2, n_vert = G$n_vert, n_face = G$n_face, n_ring = n_ring, gap_used = gap_used,
       ground_mad = g$mad, ground_sectors = g$n_sectors, slope_deg = atan(sqrt(g$b[2]^2 + g$b[3]^2)) * 180 / pi)
}

seal_raster <- function(m, ol = NULL, p = v2_defaults, shift = c(0, 0)) finish_raster(seal_grid(m, ol, p), ol, p, shift)

# registration of the outlines (orthophoto) to the mesh, one XY shift per flight, as in seal3d: for every trial shift
# the mean mesh height inside the shifted outline minus the mean height 4-12 cm around it, summed over the seals.
# grids = list of seal_grid() results, ols = matching list of match_outline() results
register_flight <- function(grids, ols, p = v2_defaults, search = 0.10, min_shift = 0.02) {
  sh <- seq(-search, search, by = p$cell); ns <- length(sh)
  total <- matrix(0, ns, ns); used <- 0
  shift_mat <- function(M, di, dj) {                   # M shifted by (di, dj) cells, FALSE outside
    nr <- nrow(M); nc <- ncol(M); S <- matrix(FALSE, nr, nc)
    ii <- max(1, 1 + di):min(nr, nr + di); jj <- max(1, 1 + dj):min(nc, nc + dj)
    if (length(ii) && length(jj) && min(ii) <= max(ii) && min(jj) <= max(jj)) S[ii, jj] <- M[ii - di, jj - dj]
    S
  }
  for (k in seq_along(grids)) {
    G <- grids[[k]]; ol <- ols[[k]]
    if (is.null(G) || is.null(ol)) next
    Z <- G$Z; ok <- !is.na(Z); if (mean(ok) < 0.5) next
    Zc <- Z - stats::median(Z, na.rm = TRUE)
    inside <- matrix(pip(G$Xm, G$Ym, ol$ring), nrow = nrow(Z))
    ringm <- dilate(inside, 12) & !dilate(inside, 4)
    sc <- matrix(NA_real_, ns, ns)
    for (a in seq_len(ns)) for (b in seq_len(ns)) {
      du <- sh[a] * G$e1[1] + sh[b] * G$e1[2]; dv <- sh[a] * G$e2[1] + sh[b] * G$e2[2]
      di <- round(du / p$cell); dj <- round(dv / p$cell)
      I <- shift_mat(inside, di, dj) & ok; R <- shift_mat(ringm, di, dj) & ok
      if (sum(I) < 0.8 * sum(inside) || sum(R) < 0.3 * sum(ringm)) next
      sc[a, b] <- mean(Zc[I]) - mean(Zc[R])
    }
    c0 <- (ns + 1) / 2
    if (!is.finite(sc[c0, c0])) next
    sc[!is.finite(sc)] <- min(sc, na.rm = TRUE)
    total <- total + sc; used <- used + 1
  }
  if (used < 5) return(list(dx = 0, dy = 0, used = used, gain = NA, applied = FALSE))
  k <- which(total == max(total), arr.ind = TRUE)[1, ]
  c0 <- (ns + 1) / 2
  dx <- sh[k[1]]; dy <- sh[k[2]]
  at_edge <- any(k %in% c(1, ns))
  gain <- total[k[1], k[2]] / total[c0, c0]
  list(dx = dx, dy = dy, used = used, gain = gain, at_edge = at_edge,
       applied = !at_edge && sqrt(dx^2 + dy^2) >= min_shift)
}

## ---------------------------------------------------------------- measurements from the raster
body_mask <- function(r, p = v2_defaults) {
  H <- r$H
  if (!is.null(r$inside)) {
    body <- r$inside & !is.na(H)
    if (p$in_delta > 0) body <- body & H > p$in_delta
  } else {
    body <- !is.na(H) & H > p$delta
    lab <- label_components(body)
    if (max(lab) == 0) return(body)
    top <- !is.na(H) & H > p$seed_frac * max(H[body], na.rm = TRUE)
    keep <- unique(lab[top & lab > 0])
    body <- matrix(lab %in% keep, nrow(H))
  }
  largest_component(body)
}

measure_raster <- function(r, p = v2_defaults) {
  H <- r$H; cell <- p$cell
  body <- body_mask(r, p)
  if (sum(body) < 30) return(list(status = "no body"))
  rows <- which(rowSums(body) > 0)
  ii <- min(rows):max(rows); n <- length(ii)
  width <- mid <- hmax <- vr <- gir <- rep(NA_real_, n)
  for (k in seq_len(n)) {
    b <- body[ii[k], ]
    if (!any(b)) next
    rl <- rle(b); ends <- cumsum(rl$lengths); starts <- ends - rl$lengths + 1
    runs <- which(rl$values); best <- runs[which.max(rl$lengths[runs])]
    j <- starts[best]:ends[best]
    h <- pmax(H[ii[k], j], 0)
    width[k] <- length(j) * cell
    mid[k] <- mean(r$v[j])
    hmax[k] <- max(h)
    vr[k] <- r$v[j[which.max(h)]]
    gir[k] <- sum(sqrt(cell^2 + diff(h)^2)) + h[1] + h[length(h)] + width[k]   # dorsal arc + drops + ground chord
  }
  fillna <- function(x) { ok <- !is.na(x); if (sum(ok) < 2) return(x); stats::approx(which(ok), x[ok], seq_along(x), rule = 2)$y }
  width <- fillna(width); mid <- fillna(mid); hmax <- fillna(hmax); vr <- fillna(vr); gir <- fillna(gir)
  s_mid <- smooth_prof(mid, p)
  dmid <- c(diff(s_mid), 0)
  cos_t <- 1 / sqrt(1 + (dmid / cell)^2)
  L_plan <- sum(sqrt(cell^2 + diff(s_mid)^2)) + cell               # curved midline, first to last station
  # midline clipped to the body: stations whose smoothed midline point lies inside the body mask (as the
  # cubic centre line of seal3d, which ends where it leaves the outline); tips off the axis are not counted
  jm <- pmin(pmax(round((s_mid - r$v[1]) / cell) + 1, 1), ncol(body))
  on <- body[cbind(ii, jm)]
  L_clip <- if (any(on)) { kk <- range(which(on)); sum(sqrt(cell^2 + diff(s_mid[kk[1]:kk[2]])^2)) + cell } else L_plan
  w_perp <- smooth_prof(width * cos_t, p)
  kW <- which.max(w_perp)
  h_s <- smooth_prof(hmax, p)
  kH <- which.max(h_s)
  frac <- (seq_len(n) - 0.5) / n
  core <- frac >= p$core[1] & frac <= p$core[2]; if (!any(core)) core <- rep(TRUE, n)
  g_s <- smooth_prof(gir, p)
  kG <- which(core)[which.max(g_s[core])]
  # spine: dorsal ridge in world 3D (includes the ground slope along the body), as get_spine
  uu <- r$u[ii]; vv <- smooth_prof(vr, p)
  xw <- r$c0[1] + uu * r$e1[1] + vv * r$e2[1]
  yw <- r$c0[2] + uu * r$e1[2] + vv * r$e2[2]
  zw <- (plane_z(r$g, xw, yw) - plane_z(r$g, r$c0[1], r$c0[2])) / p$s_vert + h_s
  spine <- sum(sqrt(diff(xw)^2 + diff(yw)^2 + diff(zw)^2))
  # envelope "chest" of the original code, on the undistorted surface
  env <- apply(ifelse(body, pmax(H, 0), NA), 2, function(x) if (all(is.na(x))) NA else max(x, na.rm = TRUE))
  env <- env[!is.na(env)]
  chest_env <- sum(sqrt(cell^2 + diff(env)^2)) + env[1] + env[length(env)] + length(env) * cell
  V <- sum(ifelse(body, pmax(H, 0), 0)) * cell^2                    # volume between the surface and the ground
  V_ell <- sum(pi / 4 * w_perp * h_s) * cell                         # elliptic sections W x H (as seal3d V_ell)
  area <- sum(body) * cell^2
  cover <- 1 - sum(body & r$filled) / sum(body)
  list(status = "ok", L_plan = L_plan, L_clip = L_clip, W_max = w_perp[kW], W_pos = frac[kW], H_max = h_s[kH], H_pos = frac[kH],
       H_core = max(h_s[core]), spine = spine, G_max = g_s[kG], G_pos = frac[kG], chest_env = chest_env,
       V = V, V_ell = V_ell, M_est = p$rho * V, area = area, cover = cover, n_st = n)
}

# one MeshCrop file; ol = result of match_outline() or NULL (mode "mesh")
measure_seal_v2 <- function(path, ol = NULL, p = v2_defaults, shift = c(0, 0), grid = NULL) {
  if (is.null(grid)) {
    m <- read_crop(path, p)
    if (is.null(m)) return(list(status = "too few faces"))
    grid <- seal_grid(m, ol, p)
  }
  r <- finish_raster(grid, ol, p, shift)
  if (is.null(r)) return(list(status = "no ground around the outline"))
  out <- measure_raster(r, p)
  c(out, list(mode = if (is.null(ol)) "mesh" else "outline", n_vert = r$n_vert, n_face = r$n_face, n_ring = r$n_ring, gap_used = r$gap_used,
              ground_mad = r$ground_mad, ground_sectors = r$ground_sectors, slope_deg = r$slope_deg))
}

# quality flag (nothing is discarded; anything but "good" means "use with care"):
#   holes        - more than 20 % of the body raster had to be filled (mesh holes)
#   flat         - highest point < 8 cm above the ground: no body relief (water, shadow, wrong outline)
#   sparse       - fewer than 300 triangles in the crop
#   rough_ground - MAD of the ground ring around the plane > 7 cm: above this the heights disagree with seal3d by
#                  > 14 % (2-6 % below it); rocks, kelp or a neighbour in the ring
#   short / blob - outline shorter than 40 cm or wider than 0.8 of its length (not a seal, or two seals)
quality_v2 <- function(x) {
  if (!identical(x$status, "ok")) return("fail")
  bad <- c(if (x$cover < 0.8) "holes", if (x$H_max < 0.08) "flat", if (x$n_face < 300) "sparse",
           if (x$ground_mad > 0.07) "rough_ground", if (x$L_plan < 0.4) "short", if (x$W_max > 0.8 * x$L_plan) "blob")
  if (length(bad) == 0) "good" else paste(bad, collapse = "+")
}
# recompute the flag for a CSV written by LRG_3d_Measurements_v2.r (e.g. after a change of the thresholds)
requality_v2 <- function(csv) {
  d <- read.csv(csv, colClasses = c(Nord = "character", East = "character"))
  d$quality <- vapply(seq_len(nrow(d)), function(i) quality_v2(as.list(d[i, ])), "")
  write.csv(d, csv, row.names = FALSE); invisible(d)
}
