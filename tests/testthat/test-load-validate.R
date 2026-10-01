test_that("bundled synthetic LIMPID-India-format data load and validate", {
  db <- load_limpid(quiet = TRUE)
  expect_s3_class(db, "limpid_db")
  expect_gt(nrow(db$Lake_Master), 1)
  expect_gt(nrow(db$Sampling_Events), 3)
  expect_equal(nrow(db$MP_Abundance), nrow(db$Sampling_Events))

  chk <- check_database(db)
  expect_false(any(chk$status == "FAIL"))
})

test_that("unit conversion is explicit and correct", {
  expect_equal(convert_mp_units(1000, "particles m^-3"), 1)
  expect_equal(convert_mp_units(2, "particles mL^-1"), 2000)
  expect_equal(convert_mp_units(10, "particles/100mL"), 100)
  expect_error(convert_mp_units(1, "mystery unit"), "Unsupported")
})

test_that("validation detects bad composition", {
  x <- data.frame(Event_ID = "E1", Fibres_pct = 90, Fragments_pct = 20, Films_pct = 0, Beads_pct = 0)
  v <- validate_mp_data(x, "morphology")
  expect_true(any(v$status == "FAIL"))
})
