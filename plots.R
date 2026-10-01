#' Plot seasonal abundance
#'
#' @param data limpid_db or event-level data.frame.
#' @param lake Optional lake name or Lake_ID filter.
#' @param value_col Abundance column.
#' @return ggplot object.
#' @export
plot_seasonality <- function(data, lake = NULL, value_col = "MP_Mean") {
  d <- .resolve_response_data(data)
  .require_cols(d, c("Season_Global", value_col), "seasonality data")
  if (!is.null(lake)) {
    keep <- rep(FALSE, nrow(d))
    if ("Lake_Name" %in% names(d)) keep <- keep | d$Lake_Name %in% lake
    if ("Lake_ID" %in% names(d)) keep <- keep | d$Lake_ID %in% lake
    d <- d[keep, , drop = FALSE]
  }
  if (!nrow(d)) .limpid_stop("No rows remain after filtering.")
  d$.season <- factor(d$Season_Global, levels = c("Pre-monsoon", "Monsoon", "Post-monsoon", "Winter"))
  d$.value <- d[[value_col]]
  ggplot2::ggplot(d, ggplot2::aes(x = .season, y = .value)) +
    ggplot2::geom_boxplot(outlier.shape = NA) +
    ggplot2::geom_point(position = ggplot2::position_jitter(width = 0.08, height = 0, seed = 1), alpha = 0.65) +
    ggplot2::labs(x = "Season", y = value_col, title = "Seasonal microplastic abundance") +
    ggplot2::theme_minimal()
}

.comp_long <- function(d, cols, id_col) {
  .require_cols(d, c(id_col, cols), "composition plot data")
  pieces <- lapply(cols, function(z) data.frame(id = d[[id_col]], component = sub("_pct$", "", z), percent = d[[z]], stringsAsFactors = FALSE))
  do.call(rbind, pieces)
}

#' Plot morphology profile
#' @param data limpid_db or morphology data.frame.
#' @param id_col Bar identity column.
#' @return ggplot object.
#' @export
plot_morphology_profile <- function(data, id_col = "Lake_Name") {
  d <- .prepare_composition(data, "morphology", id_col)
  if (!id_col %in% names(d)) .limpid_stop("'", id_col, "' is not available.")
  # Aggregate duplicate identities before plotting.
  s <- .summarise_composition(d, .comp_cols("morphology"), id_col)
  z <- .comp_long(s, .comp_cols("morphology"), id_col)
  ggplot2::ggplot(z, ggplot2::aes(x = id, y = percent, fill = component)) +
    ggplot2::geom_col() + ggplot2::coord_flip() +
    ggplot2::labs(x = NULL, y = "Mean composition (%)", fill = "Morphology", title = "Microplastic morphology profile") +
    ggplot2::theme_minimal()
}

#' Plot polymer profile
#' @param data limpid_db or polymer data.frame.
#' @param id_col Bar identity column.
#' @return ggplot object.
#' @export
plot_polymer_profile <- function(data, id_col = "Lake_Name") {
  d <- .prepare_composition(data, "polymer", id_col)
  if (!id_col %in% names(d)) .limpid_stop("'", id_col, "' is not available.")
  s <- .summarise_composition(d, .comp_cols("polymer"), id_col)
  z <- .comp_long(s, .comp_cols("polymer"), id_col)
  ggplot2::ggplot(z, ggplot2::aes(x = id, y = percent, fill = component)) +
    ggplot2::geom_col() + ggplot2::coord_flip() +
    ggplot2::labs(x = NULL, y = "Mean composition (%)", fill = "Polymer", title = "Microplastic polymer profile") +
    ggplot2::theme_minimal()
}

#' Plot a depth profile
#'
#' Refuses to infer depth from lake-event summaries. Supply genuinely depth-resolved rows.
#'
#' @param data Depth-resolved data.frame.
#' @param depth_col Depth column in metres.
#' @param value_col Abundance/indicator column.
#' @param group_col Optional grouping column.
#' @return ggplot object.
#' @export
plot_depth_profile <- function(data, depth_col = "Depth_m", value_col = "MP_Mean", group_col = NULL) {
  if (.is_limpid_db(data)) .limpid_stop("The bundled synthetic event dataset is not a depth-resolved raw-sample table. Supply a depth-resolved data.frame.")
  .require_cols(data, c(depth_col, value_col), "depth-profile data")
  if (!is.numeric(data[[depth_col]]) || !is.numeric(data[[value_col]])) .limpid_stop("Depth and value columns must be numeric.")
  d <- data; d$.depth <- d[[depth_col]]; d$.value <- d[[value_col]]
  if (is.null(group_col)) d$.group <- "all" else {
    .require_cols(d, group_col, "depth-profile data"); d$.group <- d[[group_col]]
  }
  ggplot2::ggplot(d, ggplot2::aes(x = .value, y = .depth, group = .group)) +
    ggplot2::geom_path(ggplot2::aes(linetype = .group)) +
    ggplot2::geom_point() +
    ggplot2::scale_y_reverse() +
    ggplot2::labs(x = value_col, y = paste0(depth_col, " (increasing downward)"), linetype = group_col %||% "Group",
                  title = "Depth-resolved microplastic profile") +
    ggplot2::theme_minimal()
}
