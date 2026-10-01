#' LIMPID-India table names
#'
#' @return Character vector of table names bundled with the package.
#' @export
limpid_tables <- function() {
  c(
    "Lake_Master", "Sampling_Events", "MP_Abundance",
    "Morphology_Composition", "Size_Composition", "Polymer_Composition",
    "Method_Metadata", "QAQC", "Source_References", "Station_Master",
    "Environmental_Context", "Enrichment_Provenance", "Catchment_Standard",
    "WaterQuality_External", "WaterQuality_Event_Summary", "Controlled_Vocab",
    "Model_Readiness", "Release_Data_Dictionary"
  )
}

.read_csv_base <- function(path) {
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE,
                  na.strings = c("", "NA"), fileEncoding = "UTF-8")
}

.coerce_sampling_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (is.numeric(x)) return(as.Date(x, origin = "1899-12-30"))
  if (is.character(x)) {
    suppressWarnings({
      num <- as.numeric(x)
      if (sum(!is.na(num)) > sum(nzchar(x)) / 2) return(as.Date(num, origin = "1899-12-30"))
    })
    d <- suppressWarnings(as.Date(x))
    return(d)
  }
  x
}

#' Load LIMPID-India data
#'
#' Loads the bundled synthetic demonstration dataset, a directory of CSV tables, or a compatible
#' Excel workbook. Excel input requires the suggested `readxl` package.
#'
#' @param path NULL for bundled data, a directory containing CSV tables, or an xlsx file.
#' @param tables Tables to load.
#' @param validate Run structural validation after loading.
#' @param quiet Suppress load summary.
#' @return An object of class `limpid_db`.
#' @export
load_limpid <- function(path = NULL, tables = limpid_tables(), validate = TRUE, quiet = FALSE) {
  tables <- unique(as.character(tables))
  unknown <- setdiff(tables, limpid_tables())
  if (length(unknown)) .limpid_stop("Unknown table(s): ", paste(unknown, collapse = ", "))

  out <- setNames(vector("list", length(tables)), tables)
  source_label <- NULL

  if (is.null(path)) {
    root <- system.file("extdata", package = "limpidR")
    if (!nzchar(root) && dir.exists("inst/extdata")) root <- "inst/extdata"
    if (!nzchar(root) || !dir.exists(root)) .limpid_stop("Bundled extdata directory could not be located.")
    for (nm in tables) {
      f <- file.path(root, paste0(nm, ".csv"))
      if (!file.exists(f)) .limpid_stop("Bundled table missing: ", nm)
      out[[nm]] <- .read_csv_base(f)
    }
    source_label <- "bundled synthetic LIMPID-India-format example"
  } else if (dir.exists(path)) {
    for (nm in tables) {
      f <- file.path(path, paste0(nm, ".csv"))
      if (!file.exists(f)) .limpid_stop("CSV table missing: ", f)
      out[[nm]] <- .read_csv_base(f)
    }
    source_label <- normalizePath(path, winslash = "/", mustWork = FALSE)
  } else if (file.exists(path) && grepl("\\.xlsx?$", path, ignore.case = TRUE)) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      .limpid_stop("Reading Excel files requires the suggested package 'readxl'. Install it or supply a CSV directory.")
    }
    available <- readxl::excel_sheets(path)
    missing <- setdiff(tables, available)
    if (length(missing)) .limpid_stop("Workbook is missing sheet(s): ", paste(missing, collapse = ", "))
    for (nm in tables) out[[nm]] <- as.data.frame(readxl::read_excel(path, sheet = nm), stringsAsFactors = FALSE)
    source_label <- normalizePath(path, winslash = "/", mustWork = FALSE)
  } else {
    .limpid_stop("'path' must be NULL, an existing directory, or an existing xlsx/xls file.")
  }

  out <- lapply(out, .trim_character_columns)
  if ("Sampling_Events" %in% names(out) && "Sampling_Date" %in% names(out$Sampling_Events)) {
    out$Sampling_Events$Sampling_Date <- .coerce_sampling_date(out$Sampling_Events$Sampling_Date)
  }
  class(out) <- c("limpid_db", "list")
  attr(out, "version") <- "LIMPID-India v1.0.0"
  attr(out, "source") <- source_label

  if (isTRUE(validate)) {
    chk <- check_database(out, strict = FALSE)
    if (any(chk$status == "FAIL")) .limpid_stop("Loaded database failed structural validation. Run check_database() for details.")
  }
  if (!isTRUE(quiet)) message("Loaded ", length(out), " LIMPID-India table(s) from ", source_label, ".")
  out
}

#' @export
print.limpid_db <- function(x, ...) {
  cat("<limpid_db>\n")
  cat("Version: ", attr(x, "version") %||% "unknown", "\n", sep = "")
  cat("Source:  ", attr(x, "source") %||% "unknown", "\n", sep = "")
  counts <- vapply(x, nrow, integer(1))
  cat("Tables:  ", length(x), "\n", sep = "")
  print(data.frame(table = names(counts), rows = unname(counts), row.names = NULL))
  invisible(x)
}
