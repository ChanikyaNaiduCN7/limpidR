#' Calculate transparent microplastic risk components
#'
#' The function does not embed a universal polymer-hazard ranking or arbitrary
#' high/medium/low thresholds. Researchers must supply the reference abundance,
#' optional polymer hazard scores, and any composite scaling assumptions explicitly.
#'
#' @param data limpid_db or event-level data.frame.
#' @param abundance_reference Positive reference abundance in the same unit as MP_Mean.
#' @param hazard_scores Optional named numeric vector for polymer categories.
#' @param weights Optional named weights for a composite score.
#' @param component_max Required named positive maxima when `weights` is supplied.
#' @param thresholds Optional named ascending numeric cut points for classifying the composite score.
#' @return Data frame of risk components and, only when requested, a composite score/class.
#' @export
calculate_risk <- function(data, abundance_reference, hazard_scores = NULL,
                           weights = NULL, component_max = NULL, thresholds = NULL) {
  if (!is.numeric(abundance_reference) || length(abundance_reference) != 1L ||
      !is.finite(abundance_reference) || abundance_reference <= 0) {
    .limpid_stop("'abundance_reference' must be a positive finite scalar supplied by the researcher.")
  }
  d <- .resolve_response_data(data)
  .require_cols(d, c("Event_ID", "MP_Mean"), "risk data")

  if (.is_limpid_db(data)) {
    if ("Polymer_Composition" %in% names(data)) d <- .left_merge(d, data$Polymer_Composition, by = "Event_ID")
    if ("Size_Composition" %in% names(data)) d <- .left_merge(d, data$Size_Composition, by = "Event_ID")
  }
  out <- d[c(intersect(c("Event_ID", "Lake_ID", "Lake_Name", "Season_Global"), names(d)), "MP_Mean")]
  out$contamination_factor <- d$MP_Mean / abundance_reference

  polymer_cols <- .comp_cols("polymer")
  if (!is.null(hazard_scores)) {
    if (is.null(names(hazard_scores))) .limpid_stop("'hazard_scores' must be a named numeric vector.")
    .require_cols(d, polymer_cols, "polymer risk data")
    keys <- sub("_pct$", "", polymer_cols)
    missing_scores <- setdiff(keys, names(hazard_scores))
    if (length(missing_scores)) .limpid_stop("Missing hazard score(s): ", paste(missing_scores, collapse = ", "))
    m <- as.matrix(d[polymer_cols]); suppressWarnings(storage.mode(m) <- "numeric")
    hs <- hazard_scores[keys]
    out$polymer_hazard_component <- as.numeric((m / 100) %*% hs)
  } else {
    out$polymer_hazard_component <- NA_real_
  }

  if ("Fine_LT250_pct" %in% names(d)) out$fine_particle_fraction <- d$Fine_LT250_pct / 100 else out$fine_particle_fraction <- NA_real_

  if (!is.null(weights)) {
    if (is.null(names(weights)) || !is.numeric(weights) || any(weights < 0) || sum(weights) <= 0) {
      .limpid_stop("'weights' must be a named, non-negative numeric vector with positive total weight.")
    }
    components <- c("contamination_factor", "polymer_hazard_component", "fine_particle_fraction")
    chosen <- intersect(names(weights), components)
    if (!length(chosen)) .limpid_stop("No supported component names were supplied in 'weights'.")
    if (is.null(component_max) || is.null(names(component_max))) {
      .limpid_stop("Supplying 'weights' also requires named 'component_max' values so scaling assumptions are explicit.")
    }
    miss <- setdiff(chosen, names(component_max))
    if (length(miss) || any(component_max[chosen] <= 0)) .limpid_stop("Positive component_max values are required for every weighted component.")
    w <- weights[chosen] / sum(weights[chosen])
    mat <- sapply(chosen, function(z) pmin(out[[z]] / component_max[[z]], 1))
    if (is.vector(mat)) mat <- matrix(mat, ncol = 1, dimnames = list(NULL, chosen))
    score <- rowSums(sweep(mat, 2, w, `*`), na.rm = FALSE)
    out$composite_risk_score <- score
    if (!is.null(thresholds)) {
      if (!is.numeric(thresholds) || is.unsorted(thresholds, strictly = TRUE)) .limpid_stop("'thresholds' must be strictly increasing numeric cut points.")
      labs <- c("class_1", paste0("class_", seq_len(length(thresholds)) + 1L))
      out$risk_class <- cut(score, breaks = c(-Inf, thresholds, Inf), labels = labs, right = TRUE)
    }
  }
  out
}
