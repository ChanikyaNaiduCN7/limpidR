\name{model_mp_abundance}
\alias{model_mp_abundance}
\alias{predict_mp}
\alias{cross_validate_mp}
\alias{print.limpid_model}
\alias{predict.limpid_model}
\title{Model and validate freshwater microplastic abundance}
\description{Lognormal or Gamma regression with original-scale prediction and grouped cross-validation.}
\usage{
model_mp_abundance(data, formula = NULL,
  method = c("lognormal_lm", "gamma_glm"), na_action = c("omit", "fail"))

predict_mp(model, newdata, interval = c("none", "confidence", "prediction"), level = 0.95)

cross_validate_mp(data, formula = NULL,
  method = c("lognormal_lm", "gamma_glm"), group = "Lake_ID")
}
\arguments{
\item{data}{A \code{limpid_db} or model-ready data frame.}
\item{formula}{Model formula with untransformed abundance response.}
\item{method}{Model family.}
\item{na_action}{Missing-data handling for fitting.}
\item{model}{A \code{limpid_model}.}
\item{newdata}{Data frame for prediction.}
\item{interval}{Prediction interval type.}
\item{level}{Interval confidence level.}
\item{group}{Grouping column for held-out folds.}
}
\value{A \code{limpid_model}, prediction data frame, or cross-validation result list.}
\details{The default lognormal model uses a log1p response and Duan smearing for back-transformation. Grouped cross-validation defaults to Lake_ID to reduce spatial leakage.}
\examples{
db <- load_limpid()
m <- model_mp_abundance(db, MP_Mean ~ Season_Global + Lake_Type)
cross_validate_mp(db, MP_Mean ~ Season_Global + Lake_Type)
}
