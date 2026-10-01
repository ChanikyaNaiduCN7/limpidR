test_that("lognormal model fits and predicts", {
  db <- load_limpid(quiet = TRUE)
  m <- model_mp_abundance(db, MP_Mean ~ Season_Global + Lake_Type)
  expect_s3_class(m, "limpid_model")
  d <- build_model_data(db, include_environment = FALSE)
  p <- predict_mp(m, d[1:5, ])
  expect_equal(nrow(p), 5)
  expect_true(all(p$prediction >= 0))
})

test_that("grouped cross-validation returns metrics", {
  db <- load_limpid(quiet = TRUE)
  cv <- cross_validate_mp(db, MP_Mean ~ Season_Global + Lake_Type, group = "Lake_ID")
  expect_true(all(c("RMSE", "MAE", "R2_predictive") %in% names(cv$overall)))
  expect_true(cv$overall$n > 0)
})
