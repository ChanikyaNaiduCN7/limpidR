#' Classify relative microplastic hotspots
#'
#' This is a relative classification, not geostatistical interpolation.
#'
#' @param data Data frame.
#' @param value_col Numeric indicator column.
#' @param method Quantile or robust-z classification.
#' @return Input data with Hotspot_Score and Hotspot_Class.
#' @export
classify_hotspots <- function(data, value_col = "MP_Mean",
                              method = c("quantile", "robust_z")) {
  method <- match.arg(method)
  .require_cols(data, value_col, "hotspot data")
  x <- data[[value_col]]
  if (!is.numeric(x)) .limpid_stop("Hotspot value column must be numeric.")
  out <- data
  if (method == "quantile") {
    q <- unique(stats::quantile(x, probs = c(0, .5, .75, .9, 1), na.rm = TRUE, names = FALSE, type = 7))
    if (length(q) < 3L) .limpid_stop("Too few distinct values for quantile hotspot classification.")
    if (length(q) == 5L) {
      out$Hotspot_Class <- cut(x, breaks = q, include.lowest = TRUE,
                               labels = c("low", "moderate", "high", "very_high"))
    } else {
      out$Hotspot_Class <- cut(x, breaks = q, include.lowest = TRUE,
                               labels = paste0("q", seq_len(length(q)-1L)))
    }
    out$Hotspot_Score <- x
  } else {
    med <- stats::median(x, na.rm = TRUE); mad <- stats::mad(x, center = med, constant = 1.4826, na.rm = TRUE)
    z <- if (is.finite(mad) && mad > 0) (x - med) / mad else rep(0, length(x))
    out$Hotspot_Score <- z
    out$Hotspot_Class <- cut(z, breaks = c(-Inf, 0, 1, 2, Inf),
                             labels = c("low", "moderate", "high", "very_high"))
  }
  out
}

.prepare_lake_points <- function(data, value_col = "MP_Mean") {
  if (.is_limpid_db(data)) {
    d <- build_model_data(data, include_environment = FALSE)
  } else d <- data
  .require_cols(d, c("Lake_ID", "Lake_Name", "Latitude", "Longitude", value_col), "lake-map data")
  key <- interaction(d$Lake_ID, drop = TRUE)
  g <- split(seq_len(nrow(d)), key)
  rows <- lapply(g, function(i) data.frame(
    Lake_ID = d$Lake_ID[i[1]], Lake_Name = d$Lake_Name[i[1]],
    Latitude = d$Latitude[i[1]], Longitude = d$Longitude[i[1]],
    value = .safe_mean(d[[value_col]][i]), n = sum(!is.na(d[[value_col]][i])), stringsAsFactors = FALSE
  ))
  do.call(rbind, rows)
}

#' Plot lake locations
#'
#' @param data limpid_db or data.frame with coordinates.
#' @param value_col Optional numeric column controlling point size.
#' @return ggplot object.
#' @export
plot_lake_map <- function(data, value_col = "MP_Mean") {
  d <- .prepare_lake_points(data, value_col)
  d <- d[is.finite(d$Latitude) & is.finite(d$Longitude), , drop = FALSE]
  if (!nrow(d)) .limpid_stop("No finite lake coordinates are available.")
  d$.x <- d$Longitude; d$.y <- d$Latitude; d$.value <- d$value
  ggplot2::ggplot(d, ggplot2::aes(x = .x, y = .y)) +
    ggplot2::geom_point(ggplot2::aes(size = .value), alpha = 0.75) +
    ggplot2::coord_equal() +
    ggplot2::labs(x = "Longitude", y = "Latitude", size = value_col,
                  title = "Freshwater microplastic monitoring locations") +
    ggplot2::theme_minimal()
}

#' Map relative lake hotspots
#'
#' @param data limpid_db or data.frame.
#' @param value_col Numeric indicator.
#' @param method Relative hotspot classification method.
#' @return ggplot object.
#' @export
map_mp_hotspots <- function(data, value_col = "MP_Mean",
                            method = c("quantile", "robust_z")) {
  method <- match.arg(method)
  d <- .prepare_lake_points(data, value_col)
  names(d)[names(d) == "value"] <- value_col
  d <- classify_hotspots(d, value_col, method)
  d <- d[is.finite(d$Latitude) & is.finite(d$Longitude), , drop = FALSE]
  d$.x <- d$Longitude; d$.y <- d$Latitude; d$.value <- d[[value_col]]; d$.class <- d$Hotspot_Class
  ggplot2::ggplot(d, ggplot2::aes(x = .x, y = .y)) +
    ggplot2::geom_point(ggplot2::aes(size = .value, shape = .class), alpha = 0.8) +
    ggplot2::coord_equal() +
    ggplot2::labs(x = "Longitude", y = "Latitude", size = value_col, shape = "Relative class",
                  title = "Relative microplastic hotspot map",
                  subtitle = "Point classification only; no spatial interpolation is implied") +
    ggplot2::theme_minimal()
}
