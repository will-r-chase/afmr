context("extract param")

load(file = "../test_data/afm_header.rda")

test_that("param not found", {
  expect_warning(afm_extract_param(afm_header, 4))
  expect_warning(afm_extract_param(afm_header, "cell1"))
  expect_equal(afm_extract_param(afm_header, 4), 4)
  expect_match(afm_extract_param(afm_header, "cell1"), "cell1")
})

test_that("extract param", {
  expect_equal(afm_extract_param(afm_header, "Scan Rate"), 0.497119)
  expect_match(afm_extract_param(afm_header, "Capture direction"), "Down")
})
