#' Check or enforce compositional closure
#'
#' @param data Data frame containing percentage components.
#' @param cols Component columns.
#' @param tolerance Allowed deviation from 100 percent.
#' @param action Return flags, renormalize complete positive rows, or error.
#' @return Data frame with closure columns or renormalized components.
#' @export
composition_closure <- function(data, cols, tolerance = 0.2,
                                action = c("flag", "renormalize", "error")) {
  action <- match.arg(action)
  .require_cols(data, cols, "composition data")
  m <- as.matrix(data[cols])
  suppressWarnings(storage.mode(m) <- "numeric")
  if (any(!is.na(m) & (m < 0 | m > 100))) .limpid_stop("Composition contains values outside 0-100%.")
  sums <- rowSums(m, na.rm = FALSE)
  complete <- stats::complete.cases(m)
  bad <- complete & abs(sums - 100) > tolerance
  if (action == "error" && any(bad)) .limpid_stop(sum(bad), " row(s) fail percentage closure.")
  if (action == "renormalize") {
    valid <- complete & is.finite(sums) & sums > 0
    m[valid, ] <- m[valid, , drop = FALSE] / sums[valid] * 100
    data[cols] <- m
    sums <- rowSums(m, na.rm = FALSE)
    bad <- complete & abs(sums - 100) > tolerance
  }
  data$Composition_Sum_pct <- sums
  data$Closure_OK <- ifelse(complete, !bad, NA)
  data
}

.summarise_composition <- function(data, cols, group_by = NULL) {
  .require_cols(data, cols, "composition data")
  if (is.null(group_by) || !length(group_by)) {
    group_by <- character(0)
    groups <- list(all = seq_len(nrow(data)))
  } else {
    .require_cols(data, group_by, "composition data")
    key <- do.call(interaction, c(data[group_by], list(drop = TRUE, lex.order = TRUE, sep = "\r")))
    groups <- split(seq_len(nrow(data)), key)
  }
  out <- lapply(groups, function(idx) {
    means <- vapply(cols, function(z) .safe_mean(data[[z]][idx]), numeric(1))
    dom <- if (all(is.na(means))) NA_character_ else cols[which.max(means)]
    base <- if (length(group_by)) data[idx[1], group_by, drop = FALSE] else data.frame()
    cbind(base, as.data.frame(as.list(means), check.names = FALSE),
          dominant_component = sub("_pct$", "", dom),
          shannon = .shannon_from_percent(means), n_profiles = length(idx), stringsAsFactors = FALSE)
  })
  ans <- do.call(rbind, out); rownames(ans) <- NULL; ans
}

#' Analyse morphology composition
#' @param data limpid_db or morphology data.frame.
#' @param group_by Optional grouping columns.
#' @return Summary data.frame.
#' @export
analyse_morphology <- function(data, group_by = c("Lake_Name", "Season_Global")) {
  d <- .prepare_composition(data, "morphology", group_by)
  .summarise_composition(d, .comp_cols("morphology"), group_by)
}

#' Analyse particle-size composition
#' @param data limpid_db or size-composition data.frame.
#' @param group_by Optional grouping columns.
#' @return Summary data.frame.
#' @export
analyse_size_distribution <- function(data, group_by = c("Lake_Name", "Season_Global")) {
  d <- .prepare_composition(data, "size", group_by)
  .summarise_composition(d, .comp_cols("size"), group_by)
}

#' Analyse polymer composition
#' @param data limpid_db or polymer-composition data.frame.
#' @param group_by Optional grouping columns.
#' @return Summary data.frame.
#' @export
analyse_polymers <- function(data, group_by = c("Lake_Name", "Season_Global")) {
  d <- .prepare_composition(data, "polymer", group_by)
  .summarise_composition(d, .comp_cols("polymer"), group_by)
}

#' Centred log-ratio transform for percentage compositions
#'
#' Zero components are replaced by an explicit pseudocount before closure and CLR.
#'
#' @param data Data frame.
#' @param cols Composition columns.
#' @param pseudocount Positive replacement on the proportion scale.
#' @return Data frame containing CLR columns prefixed by `clr_`.
#' @export
clr_transform <- function(data, cols, pseudocount = 1e-06) {
  if (!is.numeric(pseudocount) || length(pseudocount) != 1L || pseudocount <= 0) .limpid_stop("'pseudocount' must be a positive scalar.")
  .require_cols(data, cols, "composition data")
  m <- as.matrix(data[cols]); suppressWarnings(storage.mode(m) <- "numeric")
  if (any(!is.na(m) & m < 0)) .limpid_stop("CLR cannot be applied to negative components.")
  out <- matrix(NA_real_, nrow(m), ncol(m), dimnames = list(NULL, paste0("clr_", cols)))
  for (i in seq_len(nrow(m))) {
    x <- m[i, ]
    if (anyNA(x)) next
    if (sum(x) <= 0) next
    p <- x / sum(x)
    p[p == 0] <- pseudocount
    p <- p / sum(p)
    lx <- log(p)
    out[i, ] <- lx - mean(lx)
  }
  cbind(data, as.data.frame(out, check.names = FALSE))
}

#' Aitchison distance between compositional profiles
#'
#' @param data Data frame.
#' @param cols Composition columns.
#' @param pseudocount Zero replacement used by CLR.
#' @return A `dist` object.
#' @export
aitchison_distance <- function(data, cols, pseudocount = 1e-06) {
  z <- clr_transform(data, cols, pseudocount)
  m <- as.matrix(z[paste0("clr_", cols)])
  complete <- stats::complete.cases(m)
  if (sum(complete) < 2L) .limpid_stop("At least two complete compositional profiles are required.")
  stats::dist(m[complete, , drop = FALSE])
}
