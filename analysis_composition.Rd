\name{summarise_abundance}
\alias{summarise_abundance}
\alias{analyse_morphology}
\alias{analyse_size_distribution}
\alias{analyse_polymers}
\alias{composition_closure}
\alias{clr_transform}
\alias{aitchison_distance}
\alias{build_model_data}
\alias{provenance_summary}
\title{Descriptive and compositional microplastic analysis}
\description{Summaries, event-level joins, composition closure, CLR transformation and Aitchison distance.}
\usage{
summarise_abundance(data, by = c("Lake_Name", "Season_Global"),
  value_col = "MP_Mean", conf_level = 0.95)

analyse_morphology(data, group_by = c("Lake_Name", "Season_Global"))

analyse_size_distribution(data, group_by = c("Lake_Name", "Season_Global"))

analyse_polymers(data, group_by = c("Lake_Name", "Season_Global"))

composition_closure(data, cols, tolerance = 0.2,
  action = c("flag", "renormalize", "error"))

clr_transform(data, cols, pseudocount = 1e-06)

aitchison_distance(data, cols, pseudocount = 1e-06)

build_model_data(data, include_environment = TRUE, primary_eligible_only = FALSE)

provenance_summary(data)
}
\arguments{
\item{data}{A \code{limpid_db} or data frame, depending on function.}
\item{by}{Grouping columns for abundance summaries.}
\item{value_col}{Numeric abundance or indicator column.}
\item{conf_level}{Confidence level for mean confidence intervals.}
\item{group_by}{Optional composition grouping columns.}
\item{cols}{Composition component columns.}
\item{tolerance}{Allowed deviation from 100 percent.}
\item{action}{Closure action.}
\item{pseudocount}{Positive zero replacement on the proportion scale.}
\item{include_environment}{Join environmental context into model data.}
\item{primary_eligible_only}{Restrict to explicitly primary-model-eligible rows.}
}
\value{A data frame, distance object, or named list, depending on function.}
\examples{
db <- load_limpid()
summarise_abundance(db)
analyse_polymers(db)
}
