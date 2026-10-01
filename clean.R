.normalize_mp_unit <- function(x) {
  y <- tolower(trimws(as.character(x)))
  y <- gsub("\u03bc", "u", y, fixed = TRUE)
  y <- gsub("\u00b3", "3", y, fixed = TRUE)
  y <- gsub("\\s+", "", y)
  y <- gsub("items", "particles", y, fixed = TRUE)
  y <- gsub("item", "particle", y, fixed = TRUE)
  y
}

.unit_factor_to_per_l <- function(unit) {
  u <- .normalize_mp_unit(unit)
  map <- c(
    "particlesl^-1" = 1, "particles/l" = 1, "particle/l" = 1,
    "particlesperliter" = 1, "particlesperlitre" = 1,
    "particlesml^-1" = 1000, "particles/ml" = 1000,
    "particlesm^-3" = 0.001, "particles/m3" = 0.001,
    "particles100ml^-1" = 10, "particles/100ml" = 10
  )
  unname(map[u])
}

#' Convert freshwater microplastic abundance units
#'
#' @param x Numeric abundance.
#' @param from Source unit, scalar or vector.
#' @param to Target unit. Currently particles per litre or particles per cubic metre.
#' @return Numeric vector.
#' @export
convert_mp_units <- function(x, from, to = "particles L^-1") {
  if (!is.numeric(x)) .limpid_stop("'x' must be numeric.")
  if (length(from) == 1L) from <- rep(from, length(x))
  if (length(from) != length(x)) .limpid_stop("'from' must have length 1 or length(x).")
  f <- .unit_factor_to_per_l(from)
  if (anyNA(f)) {
    bad <- unique(from[is.na(f)])
    .limpid_stop("Unsupported source unit(s): ", paste(bad, collapse = ", "))
  }
  per_l <- x * f
  target <- .normalize_mp_unit(to)[1]
  if (target %in% c("particlesl^-1", "particles/l")) return(per_l)
  if (target %in% c("particlesm^-3", "particles/m3")) return(per_l * 1000)
  .limpid_stop("Unsupported target unit: ", to)
}

#' Clean a microplastic abundance table
#'
#' Standardizes whitespace and abundance units without silently deleting invalid values.
#'
#' @param data A data.frame or limpid_db.
#' @param abundance_col Abundance column.
#' @param unit_col Unit column.
#' @param target_unit Target abundance unit.
#' @param invalid_action What to do with negative/non-finite abundance.
#' @return Cleaned object of the same broad type.
#' @export
clean_mp_data <- function(data, abundance_col = "MP_Mean", unit_col = "Unit",
                          target_unit = "particles L^-1",
                          invalid_action = c("error", "flag", "na")) {
  invalid_action <- match.arg(invalid_action)
  isdb <- .is_limpid_db(data)
  d <- if (isdb) data$MP_Abundance else data
  if (!is.data.frame(d)) .limpid_stop("Expected a data.frame or limpid_db.")
  d <- .trim_character_columns(d)
  .require_cols(d, c(abundance_col, unit_col), "abundance data")

  invalid <- !is.na(d[[abundance_col]]) & (!is.finite(d[[abundance_col]]) | d[[abundance_col]] < 0)
  if (any(invalid)) {
    if (invalid_action == "error") .limpid_stop(sum(invalid), " invalid abundance value(s) found.")
    if (invalid_action == "na") d[[abundance_col]][invalid] <- NA_real_
    if (invalid_action == "flag") d$Clean_Flag <- ifelse(invalid, "invalid_abundance", NA_character_)
  }

  ok <- !is.na(d[[abundance_col]]) & !is.na(d[[unit_col]])
  d[[abundance_col]][ok] <- convert_mp_units(d[[abundance_col]][ok], d[[unit_col]][ok], target_unit)
  d[[unit_col]][ok] <- target_unit

  if (isdb) {
    data$MP_Abundance <- d
    return(data)
  }
  d
}
