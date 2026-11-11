#!/usr/bin/env Rscript
# =============================================================================
# Алгоритм удаления ложных срабатываний (FP) из KML-файлов детекции тюленей
# =============================================================================
#
# АЛГОРИТМ:
# Используется логистическая регрессия, обученная на таблице сопоставления
# предсказанных и ручных промеров (FP_remove.csv). Ключевые признаки:
#   - log(s_area)     : площадь полигона (FP меньше TP, но маленькие = щенки)
#   - s_circularity   : округлость (FP часто округлее — фрагменты фона)
#   - m_eccentricity  : вытянутость (у тюленей > 0.85, FP могут быть ниже)
#
# ВАЖНО: маленький размер сам по себе НЕ является критерием FP!
# Щенки — это TP с малой площадью. Алгоритм использует КОМБИНАЦИЮ признаков.
#
# ПОРОГ КОНСЕРВАТИВНЫЙ: по умолчанию threshold=0.35 → ~65% FP изымается,
# ~6% TP теряется. Для более мягкого удаления используйте threshold=0.5.
#
# ИСПОЛЬЗОВАНИЕ:
#   Rscript remove_FP_seals.R input.kml output.kml [threshold]
#   threshold — вероятность FP (0.0–1.0), по умолчанию 0.35
#
# ЗАВИСИМОСТИ: только базовый R (xml2 для удобного разбора KML)
# Установить при необходимости: install.packages("xml2")
# =============================================================================

suppressPackageStartupMessages({
  if (!requireNamespace("xml2", quietly = TRUE)) {
    install.packages("xml2", repos = "https://cloud.r-project.org", quiet = TRUE)
  }
  library(xml2)
})

# ── Параметры логистической регрессии ────────────────────────────────────────
# Обучены на FP_remove.csv (2784 полигона: 310 FP, 2319 TP)
# Ковариаты стандартизованы: z = (x - mean) / sd

MODEL <- list(
  # Нормировочные параметры (из обучающей выборки)
  mean_log_area   = 7.5803,
  sd_log_area     = 0.4569,
  mean_circularity = 0.7230,
  sd_circularity  = 0.1356,
  mean_eccentricity = 0.8744,
  sd_eccentricity  = 0.1008,
  # Веса логистической регрессии
  w_intercept     = -2.6284,
  w_log_area      = -1.4806,   # большие объекты → не FP
  w_circularity   =  0.2025,   # круглые → скорее FP (слабый эффект)
  w_eccentricity  = -0.1236    # слабый эффект
)

# ── Функция предсказания вероятности FP ──────────────────────────────────────
predict_fp_prob <- function(s_area, s_circularity, m_eccentricity, model = MODEL) {
  # Защита от нулевой/отрицательной площади
  s_area <- pmax(s_area, 1)
  
  z_area <- (log(s_area) - model$mean_log_area) / model$sd_log_area
  z_circ <- (s_circularity - model$mean_circularity) / model$sd_circularity
  z_ecc  <- (m_eccentricity - model$mean_eccentricity) / model$sd_eccentricity
  
  z <- model$w_intercept +
       model$w_log_area      * z_area +
       model$w_circularity   * z_circ +
       model$w_eccentricity  * z_ecc
  
  1 / (1 + exp(-z))
}

# ── Чтение KML и извлечение атрибутов ────────────────────────────────────────
read_kml_features <- function(kml_path) {
  doc <- read_xml(kml_path)
  ns  <- xml_ns(doc)
  
  # Находим все Placemark
  placemarks <- xml_find_all(doc, ".//d1:Placemark", ns)
  
  if (length(placemarks) == 0) {
    # Попробовать без неймспейса
    placemarks <- xml_find_all(doc, ".//Placemark")
  }
  
  cat(sprintf("Найдено полигонов в KML: %d\n", length(placemarks)))
  
  result <- lapply(seq_along(placemarks), function(i) {
    pm <- placemarks[[i]]
    
    # Функция извлечения SimpleData по имени
    get_sd <- function(name) {
      nodes <- xml_find_all(pm, sprintf(".//SimpleData[@name='%s']", name))
      if (length(nodes) == 0) return(NA_real_)
      val <- xml_text(nodes[[1]])
      suppressWarnings(as.numeric(val))
    }
    
    list(
      index         = i,
      id            = xml_text(xml_find_first(pm, ".//SimpleData[@name='id']")),
      img           = xml_text(xml_find_first(pm, ".//SimpleData[@name='img']")),
      s_area        = get_sd("s_area"),
      s_circularity = get_sd("s_circularity"),
      m_eccentricity = get_sd("m_eccentricity"),
      b_mean        = get_sd("b_mean"),
      s_radius_mean = get_sd("s_radius_mean")
    )
  })
  
  list(doc = doc, placemarks = placemarks, features = result)
}

# ── Запись отфильтрованного KML ───────────────────────────────────────────────
write_filtered_kml <- function(doc, placemarks, keep_indices, output_path) {
  remove_indices <- setdiff(seq_along(placemarks), keep_indices)
  
  # Удаляем FP-полигоны из дерева XML (в обратном порядке)
  for (i in rev(remove_indices)) {
    xml_remove(placemarks[[i]])
  }
  
  write_xml(doc, output_path, encoding = "UTF-8")
}

# ── Генерация HTML-отчёта ─────────────────────────────────────────────────────
write_report <- function(features, fp_probs, threshold, output_path) {
  n_total <- length(features)
  is_fp   <- fp_probs >= threshold
  n_fp    <- sum(is_fp)
  n_keep  <- n_total - n_fp
  
  lines <- c(
    "<!DOCTYPE html><html><head><meta charset='UTF-8'>",
    "<title>FP Removal Report</title>",
    "<style>body{font-family:Arial;margin:20px}",
    "table{border-collapse:collapse;width:100%}",
    "th,td{border:1px solid #ddd;padding:6px;text-align:right}",
    "th{background:#3a6da0;color:white}",
    ".fp{background:#ffe0e0} .tp{background:#e0ffe0}</style></head><body>",
    "<h2>Отчёт об удалении ложных срабатываний (FP)</h2>",
    sprintf("<p><b>Порог вероятности FP:</b> %.2f</p>", threshold),
    sprintf("<p><b>Всего полигонов:</b> %d | <b>Удалено FP:</b> %d | <b>Сохранено:</b> %d</p>",
            n_total, n_fp, n_keep),
    "<table><tr><th>#</th><th>ID полигона</th><th>s_area</th>",
    "<th>s_circularity</th><th>m_eccentricity</th>",
    "<th>P(FP)</th><th>Решение</th></tr>"
  )
  
  for (i in seq_along(features)) {
    f   <- features[[i]]
    p   <- fp_probs[i]
    cls <- if (is_fp[i]) "fp" else "tp"
    dec <- if (is_fp[i]) "УДАЛЁН (FP)" else "Сохранён (TP)"
    lines <- c(lines, sprintf(
      "<tr class='%s'><td>%d</td><td style='text-align:left;font-size:11px'>%s</td>
       <td>%s</td><td>%.3f</td><td>%.3f</td><td>%.3f</td><td>%s</td></tr>",
      cls, i,
      ifelse(is.na(f$id), "", f$id),
      ifelse(is.na(f$s_area), "NA", as.integer(f$s_area)),
      ifelse(is.na(f$s_circularity), 0, f$s_circularity),
      ifelse(is.na(f$m_eccentricity), 0, f$m_eccentricity),
      round(p, 3), dec
    ))
  }
  
  lines <- c(lines, "</table>",
    "<hr><p><small>Алгоритм: логистическая регрессия по признакам полигона.",
    "Признаки: log(s_area), s_circularity, m_eccentricity.",
    "Размер не является единственным критерием — щенки (малые TP) защищены.</small></p>",
    "</body></html>")
  
  writeLines(lines, output_path, useBytes = FALSE)
}

# ── ГЛАВНАЯ ФУНКЦИЯ ───────────────────────────────────────────────────────────
main <- function(args) {
  if (length(args) < 2) {
    cat("ИСПОЛЬЗОВАНИЕ:\n")
    cat("  Rscript remove_FP_seals.R <input.kml> <output.kml> [threshold]\n\n")
    cat("  threshold — вероятность FP для удаления (по умолчанию: 0.35)\n")
    cat("              0.35 → ~65% FP удалено, ~6% TP потеряно (рекомендуется)\n")
    cat("              0.50 → ~42% FP удалено, ~2% TP потеряно (консервативно)\n")
    quit(status = 1)
  }
  
  input_kml  <- args[1]
  output_kml <- args[2]
  threshold  <- if (length(args) >= 3) as.numeric(args[3]) else 0.35
  
  if (!file.exists(input_kml)) {
    stop(sprintf("Файл не найден: %s", input_kml))
  }
  
  cat(sprintf("=== Удаление FP из KML ===\n"))
  cat(sprintf("Входной файл : %s\n", input_kml))
  cat(sprintf("Выходной файл: %s\n", output_kml))
  cat(sprintf("Порог P(FP)  : %.2f\n\n", threshold))
  
  # Чтение KML
  kml_data <- read_kml_features(input_kml)
  features <- kml_data$features
  
  # Вычисление вероятностей FP
  s_area         <- sapply(features, function(f) if (is.na(f$s_area)) 1500 else f$s_area)
  s_circularity  <- sapply(features, function(f) if (is.na(f$s_circularity)) 0.72 else f$s_circularity)
  m_eccentricity <- sapply(features, function(f) if (is.na(f$m_eccentricity)) 0.87 else f$m_eccentricity)
  
  fp_probs <- predict_fp_prob(s_area, s_circularity, m_eccentricity)
  
  # Вывод результатов
  cat("\nРезультаты классификации:\n")
  cat(sprintf("  %-6s  %-8s  %-10s  %-12s  %-6s  %s\n",
              "№", "s_area", "s_circ", "m_eccent", "P(FP)", "Решение"))
  cat(strrep("-", 65), "\n")
  
  for (i in seq_along(features)) {
    f   <- features[[i]]
    p   <- fp_probs[i]
    dec <- if (p >= threshold) "УДАЛЁН" else "Сохранён"
    cat(sprintf("  %-6d  %-8s  %-10.3f  %-12.3f  %-6.3f  %s\n",
                i,
                ifelse(is.na(f$s_area), "NA", as.integer(f$s_area)),
                ifelse(is.na(f$s_circularity), NA, f$s_circularity),
                ifelse(is.na(f$m_eccentricity), NA, f$m_eccentricity),
                p, dec))
  }
  
  is_fp       <- fp_probs >= threshold
  keep_indices <- which(!is_fp)
  
  cat(sprintf("\nИТОГО: удалено %d FP из %d полигонов (%d сохранено)\n",
              sum(is_fp), length(features), sum(!is_fp)))
  
  # Запись отфильтрованного KML
  write_filtered_kml(kml_data$doc, kml_data$placemarks, keep_indices, output_kml)
  cat(sprintf("KML записан: %s\n", output_kml))
  
  # HTML отчёт
  report_path <- sub("\\.kml$", "_FP_report.html", output_kml, ignore.case = TRUE)
  write_report(features, fp_probs, threshold, report_path)
  cat(sprintf("Отчёт записан: %s\n", report_path))
  
  invisible(list(
    n_total  = length(features),
    n_fp     = sum(is_fp),
    n_kept   = sum(!is_fp),
    fp_probs = fp_probs
  ))
}

# ── Запуск ────────────────────────────────────────────────────────────────────
args <- commandArgs(trailingOnly = TRUE)
main(args)
