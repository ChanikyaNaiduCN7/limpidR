\name{calculate_risk}
\alias{calculate_risk}
\alias{limpid_session_info}
\alias{limpid_citation}
\title{Transparent risk components and reproducibility metadata}
\description{Calculate assumption-explicit risk components and report reproducibility/citation metadata.}
\usage{
calculate_risk(data, abundance_reference, hazard_scores = NULL,
  weights = NULL, component_max = NULL, thresholds = NULL)

limpid_session_info()

limpid_citation(doi = NULL, creators = "Chanikya Naidu")
}
\arguments{
\item{data}{A \code{limpid_db} or event-level data frame.}
\item{abundance_reference}{Positive reference abundance in the same unit as MP_Mean.}
\item{hazard_scores}{Optional named polymer hazard scores supplied by the researcher.}
\item{weights}{Optional named composite-score weights.}
\item{component_max}{Explicit positive scaling maxima required when weights are used.}
\item{thresholds}{Optional ascending numeric composite-score cut points.}
\item{doi}{Assigned LIMPID-India dataset DOI.}
\item{creators}{Creator string for citation output.}
}
\value{A risk-component data frame, sessionInfo object, or citation string.}
\details{No universal polymer hazard scores, composite weights or risk classes are hard-coded. Assumptions must be supplied explicitly.}
