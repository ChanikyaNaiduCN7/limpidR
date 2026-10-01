#' Reproducibility session information
#'
#' @return `sessionInfo()` object with package and database-version attributes.
#' @export
limpid_session_info <- function() {
  s <- utils::sessionInfo()
  attr(s, "limpidR_version") <- "0.1.0"
  attr(s, "LIMPID_India_release") <- "v1.0.0"
  s
}

#' Recommended LIMPID-India citation text
#'
#' @param doi Optional assigned dataset DOI.
#' @param creators Creator string. Must be supplied for a final citation.
#' @return Character citation.
#' @export
limpid_citation <- function(doi = NULL, creators = "Chanikya Naidu") {
  id <- if (is.null(doi) || !nzchar(doi)) "https://doi.org/[DOI]" else paste0("https://doi.org/", sub("^https?://doi.org/", "", doi))
  paste0(creators, " (2026). LIMPID-India v1.0.0: Lake Inventory of Microplastic Pollution in Indian Freshwaters [Data set]. ", id)
}
