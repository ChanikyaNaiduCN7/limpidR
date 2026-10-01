test_that("risk requires explicit reference abundance", {
  db <- load_limpid(quiet = TRUE)
  expect_error(calculate_risk(db, 0), "positive")
  r <- calculate_risk(db, abundance_reference = 10)
  expect_true("contamination_factor" %in% names(r))
  expect_false("composite_risk_score" %in% names(r))
})

test_that("composite risk requires explicit scaling", {
  db <- load_limpid(quiet = TRUE)
  expect_error(calculate_risk(db, 10, weights = c(contamination_factor = 1)), "component_max")
})

test_that("hotspot classification does not interpolate", {
  x <- data.frame(MP_Mean = 1:20)
  z <- classify_hotspots(x)
  expect_true(all(c("Hotspot_Score", "Hotspot_Class") %in% names(z)))
})

test_that("depth plot rejects event-level database", {
  db <- load_limpid(quiet = TRUE)
  expect_error(plot_depth_profile(db), "depth-resolved")
})
