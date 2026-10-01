`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) y else x
}

.limpid_stop <- function(...) stop(..., call. = FALSE)

.require_cols <- function(data, cols, context = "data") {
  missing <- setdiff(cols, names(data))
  if (length(missing)) {
    .limpid_stop(context, " is missing required column(s): ", paste(missing, collapse = ", "))
  }
  invisible(TRUE)
}

.is_limpid_db <- function(x) inherits(x, "limpid_db")

.get_table <- function(x, table) {
  if (.is_limpid_db(x)) {
    if (!table %in% names(x)) .limpid_stop("Table '", table, "' is not present in the database.")
    return(x[[table]])
  }
  if (is.data.frame(x)) return(x)
  .limpid_stop("Expected a limpid_db object or data.frame.")
}

.left_merge <- function(x, y, by) {
  if (!nrow(x)) return(x)
  if (!nrow(y)) return(x)
  x$.limpid_order__ <- seq_len(nrow(x))
  out <- merge(x, y, by = by, all.x = TRUE, sort = FALSE, suffixes = c("", ".y"))
  out <- out[order(out$.limpid_order__), , drop = FALSE]
  out$.limpid_order__ <- NULL
  rownames(out) <- NULL
  out
}

.trim_character_columns <- function(data) {
  is_chr <- vapply(data, is.character, logical(1))
  data[is_chr] <- lapply(data[is_chr], function(x) {
    y <- trimws(x)
    y[y == ""] <- NA_character_
    y
  })
  data
}

.safe_mean <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
.safe_sd <- function(x) if (sum(!is.na(x)) < 2L) NA_real_ else stats::sd(x, na.rm = TRUE)
.safe_median <- function(x) if (all(is.na(x))) NA_real_ else stats::median(x, na.rm = TRUE)
.safe_iqr <- function(x) if (all(is.na(x))) NA_real_ else stats::IQR(x, na.rm = TRUE)

.shannon_from_percent <- function(x) {
  x <- as.numeric(x)
  x <- x[is.finite(x) & x > 0]
  if (!length(x)) return(NA_real_)
  p <- x / sum(x)
  -sum(p * log(p))
}

.resolve_response_data <- function(x) {
  if (.is_limpid_db(x)) return(build_model_data(x, include_environment = TRUE))
  if (is.data.frame(x)) return(x)
  .limpid_stop("Expected a limpid_db object or data.frame.")
}

.as_formula <- function(formula, data) {
  if (!is.null(formula)) return(stats::as.formula(formula))
  candidates <- c("Season_Global", "Lake_Type")
  rhs <- candidates[candidates %in% names(data)]
  if (!length(rhs)) return(stats::as.formula("MP_Mean ~ 1"))
  stats::reformulate(rhs, response = "MP_Mean")
}

.comp_cols <- function(type = c("morphology", "size", "polymer")) {
  type <- match.arg(type)
  switch(type,
    morphology = c("Fibres_pct", "Fragments_pct", "Films_pct", "Beads_pct"),
    size = c("Fine_LT250_pct", "Intermediate_250_1000_pct", "Coarse_GT1000_pct"),
    polymer = c("PE_pct", "PP_pct", "PET_PES_pct", "PA_Nylon_pct", "PS_EPS_pct", "PVC_pct", "OtherPolymer_pct")
  )
}

.prepare_composition <- function(x, type, group_by = NULL) {
  table <- switch(type,
    morphology = "Morphology_Composition",
    size = "Size_Composition",
    polymer = "Polymer_Composition"
  )
  d <- .get_table(x, table)
  if (.is_limpid_db(x) && !is.null(group_by)) {
    ev <- x[["Sampling_Events"]]
    lk <- x[["Lake_Master"]]
    keep_ev <- intersect(c("Event_ID", "Season_Global", "Event_Label", "Sampling_Date"), names(ev))
    d <- .left_merge(d, ev[keep_ev], by = "Event_ID")
    if ("Lake_ID" %in% names(d) && "Lake_ID" %in% names(lk)) {
      keep_lk <- setdiff(intersect(c("Lake_ID", "Lake_Type", "Urbanisation_Class", "State_UT", "Region_Group"), names(lk)), "Lake_ID")
      if (length(keep_lk)) d <- .left_merge(d, lk[c("Lake_ID", keep_lk)], by = "Lake_ID")
    }
  }
  d
}
