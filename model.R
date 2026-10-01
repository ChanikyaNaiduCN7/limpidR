#' Fit a freshwater microplastic abundance model
#'
#' The default lognormal model fits `log1p(response)` with ordinary least squares
#' and uses Duan's smearing estimator for predictions on the original scale.
#'
#' @param data limpid_db or modelling data.frame.
#' @param formula Model formula with an untransformed abundance response.
#' @param method `lognormal_lm` or `gamma_glm`.
#' @param na_action Omit incomplete model rows or fail.
#' @return A `limpid_model` object.
#' @export
model_mp_abundance <- function(data, formula = NULL,
                               method = c("lognormal_lm", "gamma_glm"),
                               na_action = c("omit", "fail")) {
  method <- match.arg(method); na_action <- match.arg(na_action)
  d <- .resolve_response_data(data)
  f <- .as_formula(formula, d)
  response <- all.vars(f[[2]])[1]
  .require_cols(d, unique(all.vars(f)), "model data")
  if (!is.numeric(d[[response]])) .limpid_stop("Model response must be numeric.")

  needed <- unique(all.vars(f))
  cc <- stats::complete.cases(d[needed])
  if (na_action == "fail" && any(!cc)) .limpid_stop(sum(!cc), " model row(s) contain missing values.")
  used <- d[cc, , drop = FALSE]
  if (nrow(used) < 3L) .limpid_stop("At least three complete observations are required.")

  if (method == "lognormal_lm") {
    used$.limpid_response <- log1p(used[[response]])
    rhs <- paste(deparse(f[[3]]), collapse = " ")
    fit_formula <- stats::as.formula(paste(".limpid_response ~", rhs))
    fit <- stats::lm(fit_formula, data = used)
    smear <- mean(exp(stats::residuals(fit)), na.rm = TRUE)
  } else {
    if (any(used[[response]] <= 0)) .limpid_stop("Gamma(log) modelling requires strictly positive response values.")
    fit <- stats::glm(f, data = used, family = stats::Gamma(link = "log"))
    smear <- NA_real_
  }

  out <- list(
    fit = fit, method = method, formula = f, response = response,
    n = nrow(used), omitted = sum(!cc), smearing = smear,
    training_columns = names(used)
  )
  class(out) <- "limpid_model"
  out
}

#' @export
print.limpid_model <- function(x, ...) {
  cat("<limpid_model>\nMethod: ", x$method, "\nFormula: ", paste(deparse(x$formula), collapse = " "),
      "\nN: ", x$n, " (omitted ", x$omitted, ")\n", sep = "")
  invisible(x)
}

#' @export
predict.limpid_model <- function(object, newdata, ...) {
  predict_mp(object, newdata = newdata, ...)
}

#' Predict microplastic abundance
#'
#' @param model A limpid_model.
#' @param newdata New data frame.
#' @param interval `none`, `confidence`, or `prediction` for lognormal LM; Gamma supports confidence intervals.
#' @param level Interval confidence level.
#' @return Data frame with prediction and optional limits.
#' @export
predict_mp <- function(model, newdata, interval = c("none", "confidence", "prediction"), level = 0.95) {
  if (!inherits(model, "limpid_model")) .limpid_stop("'model' must be a limpid_model.")
  if (!is.data.frame(newdata)) .limpid_stop("'newdata' must be a data.frame.")
  interval <- match.arg(interval)

  if (model$method == "lognormal_lm") {
    if (interval == "none") {
      eta <- stats::predict(model$fit, newdata = newdata)
      pred <- pmax(0, exp(eta) * model$smearing - 1)
      return(data.frame(prediction = as.numeric(pred)))
    }
    p <- stats::predict(model$fit, newdata = newdata, interval = interval, level = level)
    p <- exp(p) * model$smearing - 1
    p[p < 0] <- 0
    return(data.frame(prediction = p[, "fit"], lower = p[, "lwr"], upper = p[, "upr"]))
  }

  pr <- stats::predict(model$fit, newdata = newdata, type = "link", se.fit = TRUE)
  eta <- as.numeric(pr$fit); se <- as.numeric(pr$se.fit)
  pred <- exp(eta)
  if (interval == "none") return(data.frame(prediction = pred))
  z <- stats::qnorm(1 - (1 - level) / 2)
  data.frame(prediction = pred, lower = exp(eta - z * se), upper = exp(eta + z * se))
}

#' Grouped cross-validation for abundance models
#'
#' By default performs leave-one-lake-out validation, avoiding random row splits that
#' can leak lake-specific information between training and test sets.
#'
#' @param data limpid_db or modelling data.frame.
#' @param formula Model formula.
#' @param method Model method.
#' @param group Grouping column used to define held-out folds.
#' @return List with overall metrics, fold metrics and row predictions.
#' @export
cross_validate_mp <- function(data, formula = NULL,
                              method = c("lognormal_lm", "gamma_glm"),
                              group = "Lake_ID") {
  method <- match.arg(method)
  d <- .resolve_response_data(data)
  f <- .as_formula(formula, d)
  response <- all.vars(f[[2]])[1]
  .require_cols(d, c(group, unique(all.vars(f))), "cross-validation data")
  groups <- unique(stats::na.omit(d[[group]]))
  if (length(groups) < 2L) .limpid_stop("Grouped cross-validation requires at least two groups.")

  preds <- list(); folds <- list()
  for (g in groups) {
    test_idx <- !is.na(d[[group]]) & d[[group]] == g
    train <- d[!test_idx, , drop = FALSE]
    test <- d[test_idx, , drop = FALSE]
    if (!nrow(test)) next
    ans <- tryCatch({
      mod <- model_mp_abundance(train, formula = f, method = method, na_action = "omit")
      pp <- predict_mp(mod, test)$prediction
      obs <- test[[response]]
      ok <- is.finite(obs) & is.finite(pp)
      if (!any(ok)) stop("No complete test predictions")
      data.frame(group = as.character(g), observed = obs[ok], predicted = pp[ok], stringsAsFactors = FALSE)
    }, error = function(e) {
      data.frame(group = as.character(g), observed = numeric(0), predicted = numeric(0), stringsAsFactors = FALSE)
    })
    preds[[as.character(g)]] <- ans
  }
  p <- do.call(rbind, preds)
  if (is.null(p) || !nrow(p)) .limpid_stop("No cross-validation fold produced valid predictions.")
  p$error <- p$predicted - p$observed
  p$abs_error <- abs(p$error)
  p$sq_error <- p$error^2

  fold_split <- split(seq_len(nrow(p)), p$group)
  fm <- lapply(fold_split, function(i) {
    o <- p$observed[i]; q <- p$predicted[i]
    data.frame(group = p$group[i[1]], n = length(i),
               RMSE = sqrt(mean((q-o)^2)), MAE = mean(abs(q-o)),
               bias = mean(q-o), stringsAsFactors = FALSE)
  })
  fold_metrics <- do.call(rbind, fm); rownames(fold_metrics) <- NULL
  sst <- sum((p$observed - mean(p$observed))^2)
  r2 <- if (sst > 0) 1 - sum((p$predicted-p$observed)^2)/sst else NA_real_
  overall <- data.frame(n = nrow(p), groups = length(unique(p$group)),
                        RMSE = sqrt(mean(p$sq_error)), MAE = mean(p$abs_error),
                        bias = mean(p$error), R2_predictive = r2)
  list(overall = overall, folds = fold_metrics, predictions = p,
       method = method, formula = f, group = group)
}
