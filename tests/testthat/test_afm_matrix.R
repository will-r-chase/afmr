context("afm_matrix")

load(file = "../test_data/afm_data.rda")
vec <- runif(1000)
too_small <- data.frame("Height_Sensor(nm)" = runif(22000))

test_that("afm_matrix errors", {
  expect_error(afm_matrix(vec, 512, 512, "Height_Sensor(nm)"))
  expect_error(afm_matrix(vec, 600, 600, "Height_Sensor(nm)"))
  expect_error(afm_matrix(afm_data, 512, 512))
})

