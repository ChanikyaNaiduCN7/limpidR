#' Validate one microplastic table
#'
#' @param data A data.frame.
#' @param type Expected schema type.
#' @param tolerance Percentage-closure tolerance.
#' @return Data frame of validation checks.
#' @export
validate_mp_data <- function(data,
                             type = c("abundance", "morphology", "size", "polymer", "events", "lake"),
                             tolerance = 0.2) {
  type <- match.arg(type)
  if (!is.data.frame(data)) .limpid_stop("'data' must be a data.frame.")
  checks <- list()
  add <- function(check, status, detail) {
    checks[[length(checks) + 1L]] <<- data.frame(check = check, status = status, detail = detail, stringsAsFactors = FALSE)
  }

  required <- switch(type,
    abundance = c("Event_ID", "MP_Mean", "Unit"),
    morphology = c("Event_ID", .comp_cols("morphology")),
    size = c("Event_ID", .comp_cols("size")),
    polymer = c("Event_ID", .comp_cols("polymer")),
    events = c("Event_ID", "Lake_ID", "Season_Global", "Matrix"),
    lake = c("Lake_ID", "Lake_Name")
  )
  miss <- setdiff(required, names(data))
  add("required_columns", if (length(miss)) "FAIL" else "PASS",
      if (length(miss)) paste("Missing:", paste(miss, collapse = ", ")) else "All required columns are present.")
  if (length(miss)) return(do.call(rbind, checks))

  id <- if ("Event_ID" %in% names(data)) "Event_ID" else "Lake_ID"
  dup <- duplicated(data[[id]]) & !is.na(data[[id]])
  add("unique_primary_identifier", if (any(dup)) "FAIL" else "PASS",
      paste(sum(dup), "duplicate identifier row(s)."))

  if (type == "abundance") {
    bad <- !is.na(data$MP_Mean) & (!is.finite(data$MP_Mean) | data$MP_Mean < 0)
    add("non_negative_abundance", if (any(bad)) "FAIL" else "PASS", paste(sum(bad), "invalid abundance value(s)."))
  }

  if (type %in% c("morphology", "size", "polymer")) {
    cols <- .comp_cols(type)
    m <- as.matrix(data[cols])
    suppressWarnings(storage.mode(m) <- "numeric")
    bad_range <- sum(!is.na(m) & (m < 0 | m > 100))
    sums <- rowSums(m, na.rm = FALSE)
    complete <- stats::complete.cases(m)
    bad_sum <- sum(complete & abs(sums - 100) > tolerance)
    add("percentage_bounds", if (bad_range) "FAIL" else "PASS", paste(bad_range, "component value(s) outside 0-100%."))
    add("percentage_closure", if (bad_sum) "FAIL" else "PASS",
        paste(bad_sum, "complete profile(s) outside 100 +/-", tolerance, "%."))
  }

  do.call(rbind, checks)
}

#' Check a LIMPID-India database
#'
#' Performs key, foreign-key, abundance, composition, coordinate, unit and method-link checks.
#'
#' @param data A `limpid_db` object.
#' @param strict Stop when a failing check is found.
#' @param tolerance Percentage closure tolerance.
#' @return A `limpid_validation` data frame.
#' @export
check_database <- function(data, strict = FALSE, tolerance = 0.2) {
  if (!.is_limpid_db(data)) .limpid_stop("check_database() requires a limpid_db object.")
  required_tables <- c("Lake_Master", "Sampling_Events", "MP_Abundance",
                       "Morphology_Composition", "Size_Composition", "Polymer_Composition")
  rows <- list()
  add <- function(check, status, detail) {
    rows[[length(rows) + 1L]] <<- data.frame(check = check, status = status, detail = detail, stringsAsFactors = FALSE)
  }

  missing_tables <- setdiff(required_tables, names(data))
  add("required_tables", if (length(missing_tables)) "FAIL" else "PASS",
      if (length(missing_tables)) paste("Missing:", paste(missing_tables, collapse = ", ")) else "All core tables are present.")
  if (length(missing_tables)) {
    out <- do.call(rbind, rows); class(out) <- c("limpid_validation", class(out))
    if (strict) .limpid_stop("Database validation failed: missing core tables.")
    return(out)
  }

  lakes <- data$Lake_Master
  events <- data$Sampling_Events
  add("unique_Lake_ID", if (anyDuplicated(lakes$Lake_ID)) "FAIL" else "PASS",
      paste(anyDuplicated(lakes$Lake_ID), "duplicate Lake_ID position (0 means none)."))
  add("unique_Event_ID", if (anyDuplicated(events$Event_ID)) "FAIL" else "PASS",
      paste(anyDuplicated(events$Event_ID), "duplicate Event_ID position (0 means none)."))

  bad_lake_fk <- setdiff(unique(events$Lake_ID), unique(lakes$Lake_ID))
  add("event_to_lake_foreign_key", if (length(bad_lake_fk)) "FAIL" else "PASS",
      if (length(bad_lake_fk)) paste("Unknown Lake_ID:", paste(bad_lake_fk, collapse = ", ")) else "All events link to a known lake.")

  for (nm in c("MP_Abundance", "Morphology_Composition", "Size_Composition", "Polymer_Composition")) {
    tab <- data[[nm]]
    missing_ev <- setdiff(events$Event_ID, tab$Event_ID)
    extra_ev <- setdiff(tab$Event_ID, events$Event_ID)
    status <- if (length(missing_ev) || length(extra_ev) || anyDuplicated(tab$Event_ID)) "FAIL" else "PASS"
    detail <- paste0("missing=", length(missing_ev), "; extra=", length(extra_ev), "; duplicate=", as.integer(anyDuplicated(tab$Event_ID) > 0))
    add(paste0("event_alignment_", nm), status, detail)
  }

  parts <- list(
    validate_mp_data(data$MP_Abundance, "abundance", tolerance),
    validate_mp_data(data$Morphology_Composition, "morphology", tolerance),
    validate_mp_data(data$Size_Composition, "size", tolerance),
    validate_mp_data(data$Polymer_Composition, "polymer", tolerance)
  )
  for (i in seq_along(parts)) {
    p <- parts[[i]]
    for (j in seq_len(nrow(p))) add(paste(names(parts)[i] %||% "table", p$check[j], sep = ":"), p$status[j], p$detail[j])
  }

  if (all(c("Latitude", "Longitude") %in% names(lakes))) {
    bad_lat <- !is.na(lakes$Latitude) & (lakes$Latitude < -90 | lakes$Latitude > 90)
    bad_lon <- !is.na(lakes$Longitude) & (lakes$Longitude < -180 | lakes$Longitude > 180)
    add("coordinate_ranges", if (any(bad_lat | bad_lon)) "FAIL" else "PASS",
        paste(sum(bad_lat | bad_lon), "lake coordinate row(s) outside valid ranges."))
  }

  if ("Unit" %in% names(data$MP_Abundance)) {
    units <- unique(stats::na.omit(data$MP_Abundance$Unit))
    add("abundance_units", if (length(units) == 1L) "PASS" else "WARN",
        paste("Observed unit(s):", paste(units, collapse = ", ")))
  }

  if ("Method_Metadata" %in% names(data) && "Method_ID" %in% names(events)) {
    unknown <- setdiff(unique(stats::na.omit(events$Method_ID)), unique(stats::na.omit(data$Method_Metadata$Method_ID)))
    add("method_foreign_key", if (length(unknown)) "FAIL" else "PASS",
        if (length(unknown)) paste("Unknown Method_ID:", paste(unknown, collapse = ", ")) else "All event Method_ID values resolve.")
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  class(out) <- c("limpid_validation", class(out))
  if (isTRUE(strict) && any(out$status == "FAIL")) .limpid_stop("Database validation failed. Inspect check_database(data) for details.")
  out
}

#' @export
print.limpid_validation <- function(x, ...) {
  print.data.frame(x, row.names = FALSE)
  cat("\nSummary:\n")
  print(table(x$status, useNA = "ifany"))
  invisible(x)
}
