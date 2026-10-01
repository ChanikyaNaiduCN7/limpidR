\name{load_limpid}
\alias{load_limpid}
\alias{limpid_tables}
\alias{check_database}
\alias{validate_mp_data}
\alias{clean_mp_data}
\alias{convert_mp_units}
\alias{print.limpid_db}
\alias{print.limpid_validation}
\title{Load, clean and validate freshwater microplastic data}
\description{Functions for reading LIMPID-India tables, unit harmonization and structural validation.}
\usage{
limpid_tables()

load_limpid(path = NULL, tables = limpid_tables(), validate = TRUE, quiet = FALSE)

check_database(data, strict = FALSE, tolerance = 0.2)

validate_mp_data(data, type = c("abundance", "morphology", "size", "polymer",
  "events", "lake"), tolerance = 0.2)

clean_mp_data(data, abundance_col = "MP_Mean", unit_col = "Unit",
  target_unit = "particles L^-1", invalid_action = c("error", "flag", "na"))

convert_mp_units(x, from, to = "particles L^-1")
}
\arguments{
\item{path}{NULL, a compatible CSV directory, or an Excel workbook. Excel input requires \pkg{readxl}.}
\item{tables}{Character vector of table names.}
\item{validate}{Logical; validate after loading.}
\item{quiet}{Logical; suppress load message.}
\item{data}{A data frame or \code{limpid_db}, depending on function.}
\item{strict}{Logical; stop when a failing database check is found.}
\item{tolerance}{Allowed percentage deviation from 100 for composition closure.}
\item{type}{Expected table schema.}
\item{abundance_col}{Name of abundance column.}
\item{unit_col}{Name of unit column.}
\item{target_unit}{Target abundance unit.}
\item{invalid_action}{Action for invalid abundance values.}
\item{x}{Numeric abundance vector.}
\item{from}{Source abundance unit.}
\item{to}{Target abundance unit.}
}
\value{A loaded database, validation table, cleaned object, or converted numeric vector.}
\examples{
db <- load_limpid()
check_database(db)
convert_mp_units(1000, "particles m^-3", "particles L^-1")
}