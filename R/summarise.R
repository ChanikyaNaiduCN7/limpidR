#' Build an event-level modelling table
#'
#' Joins abundance to event metadata, lake metadata, environmental context and model-readiness flags.
#'
#' @param data A limpid_db.
#' @param include_environment Join Environmental_Context where available.
#' @param primary_eligible_only Keep only rows explicitly marked primary-model eligible.
#' @return Event-level data.frame.
#' @export
build_model_data <- function(data, include_environment = TRUE, primary_eligible_only = FALSE) {
  if (!.is_limpid_db(data)) .limpid_stop("build_model_data() requires a limpid_db object.")
  .require_cols(data$MP_Abundance, c("Event_ID", "MP_Mean"), "MP_Abundance")
  .require_cols(data$Sampling_Events, c("Event_ID", "Lake_ID"), "Sampling_Events")

  d <- .left_merge(data$MP_Abundance, data$Sampling_Events, by = "Event_ID")
  if ("Lake_Master" %in% names(data)) {
    lk <- data$Lake_Master
    keep <- setdiff(names(lk), intersect(names(lk), names(d)))
    keep <- c("Lake_ID", keep)
    keep <- unique(keep[keep %in% names(lk)])
    d <- .left_merge(d, lk[keep], by = "Lake_ID")
  }
  if (isTRUE(include_environment) && "Environmental_Context" %in% names(data)) {
    en <- data$Environmental_Context
    keep <- c("Event_ID", setdiff(names(en), intersect(names(en), names(d))))
    keep <- unique(keep[keep %in% names(en)])
    d <- .left_merge(d, en[keep], by = "Event_ID")
  }
  if ("Model_Readiness" %in% names(data)) {
    mr <- data$Model_Readiness
    keep <- c("Event_ID", setdiff(names(mr), intersect(names(mr), names(d))))
    keep <- unique(keep[keep %in% names(mr)])
    d <- .left_merge(d, mr[keep], by = "Event_ID")
  }
  if (isTRUE(primary_eligible_only)) {
    flag <- intersect(c("Primary_Environmental_Model", "Primary_Model_Eligible"), names(d))
    if (!length(flag)) .limpid_stop("No primary-model eligibility flag exists in the joined data.")
    f <- as.character(d[[flag[1]]])
    d <- d[tolower(f) %in% c("yes", "true", "1"), , drop = FALSE]
  }
  rownames(d) <- NULL
  d
}

#' Summarise microplastic abundance
#'
#' @param data limpid_db or event-level data.frame.
#' @param by Grouping columns.
#' @param value_col Abundance column.
#' @param conf_level Confidence level for t-based mean confidence interval.
#' @return Summary data.frame.
#' @export
summarise_abundance <- function(data, by = c("Lake_Name", "Season_Global"),
                                value_col = "MP_Mean", conf_level = 0.95) {
  d <- .resolve_response_data(data)
  .require_cols(d, c(by, value_col), "abundance data")
  if (!is.numeric(d[[value_col]])) .limpid_stop("'", value_col, "' must be numeric.")
  key <- do.call(interaction, c(d[by], list(drop = TRUE, lex.order = TRUE, sep = "\r")))
  groups <- split(seq_len(nrow(d)), key)
  out <- lapply(groups, function(idx) {
    x <- d[[value_col]][idx]
    n <- sum(!is.na(x))
    mn <- .safe_mean(x); s <- .safe_sd(x)
    se <- if (n > 1L && is.finite(s)) s / sqrt(n) else NA_real_
    crit <- if (n > 1L) stats::qt(1 - (1 - conf_level) / 2, df = n - 1L) else NA_real_
    row <- d[idx[1], by, drop = FALSE]
    cbind(row, data.frame(
      n = n, mean = mn, sd = s, median = .safe_median(x), IQR = .safe_iqr(x),
      se = se, ci_low = if (is.finite(se)) mn - crit * se else NA_real_,
      ci_high = if (is.finite(se)) mn + crit * se else NA_real_, stringsAsFactors = FALSE
    ))
  })
  ans <- do.call(rbind, out)
  rownames(ans) <- NULL
  ans
}

#' Summarise provenance and model-use status
#'
#' @param data A limpid_db.
#' @return Named list of summary tables.
#' @export
provenance_summary <- function(data) {
  if (!.is_limpid_db(data)) .limpid_stop("provenance_summary() requires a limpid_db object.")
  out <- list()
  if ("Enrichment_Provenance" %in% names(data)) {
    p <- data$Enrichment_Provenance
    for (field in intersect(c("Confidence_Class", "Use_For_Modeling", "Source_Type", "Source_Organisation"), names(p))) {
      tab <- as.data.frame(table(p[[field]], useNA = "ifany"), stringsAsFactors = FALSE)
      names(tab) <- c(field, "n")
      out[[field]] <- tab
    }
  }
  if ("Model_Readiness" %in% names(data)) {
    m <- data$Model_Readiness
    for (field in intersect(c("Primary_Environmental_Model", "Sensitivity_Model", "Official_WQ_Match"), names(m))) {
      tab <- as.data.frame(table(m[[field]], useNA = "ifany"), stringsAsFactors = FALSE)
      names(tab) <- c(field, "n")
      out[[paste0("Model_Readiness_", field)]] <- tab
    }
  }
  out
}
