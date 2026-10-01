test_that("abundance summaries are reproducible", {
  db <- load_limpid(quiet = TRUE)
  s <- summarise_abundance(db, by = "Season_Global")
  expect_true(all(c("n", "mean", "sd", "median", "IQR") %in% names(s)))
  expect_equal(sum(s$n), nrow(db$MP_Abundance))
})

test_that("composition closure and CLR work", {
  db <- load_limpid(quiet = TRUE)
  p <- db$Polymer_Composition
  cols <- c("PE_pct", "PP_pct", "PET_PES_pct", "PA_Nylon_pct", "PS_EPS_pct", "PVC_pct", "OtherPolymer_pct")
  c1 <- composition_closure(p, cols)
  expect_true(all(c1$Closure_OK, na.rm = TRUE))
  z <- clr_transform(p, cols)
  expect_true(all(paste0("clr_", cols) %in% names(z)))
  d <- aitchison_distance(p[1:4, ], cols)
  expect_s3_class(d, "dist")
})

test_that("morphology and polymer summaries expose dominant component", {
  db <- load_limpid(quiet = TRUE)
  m <- analyse_morphology(db, group_by = "Lake_Name")
  p <- analyse_polymers(db, group_by = "Lake_Name")
  expect_true("dominant_component" %in% names(m))
  expect_true("dominant_component" %in% names(p))
})
