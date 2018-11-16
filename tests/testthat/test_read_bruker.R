context("afm_read_bruker")

good_file <- "../test_data/500nm_1.txt"
bad_file <- "../test_data/500nm_1_bad.txt"

test_scan <- afm_read_bruker(good_file)
specific_maps <- afm_read_bruker(good_file, maps = c("Height(nm)", "Peak_Force_Error(nN)"))
opt_params <- afm_read_bruker(good_file, opt_params = list(scan_rate = "Scan Rate", capture_direction = "Capture direction", cell = "cell1"))
map_names <- c("Height_matrix", "Peak_Force_Error_matrix")


test_that("error for bad file", {
  expect_error(afm_read_bruker(bad_file))
})

test_that("output is afm_scan", {
  expect_match(class(test_scan), "afm_scan")
})

test_that("specifying maps works", {
  expect_length(specific_maps$maps, 2)
  expect_true(all(names(specific_maps$maps) %in% c("Height_matrix", "Peak_Force_Error_matrix")))
})

test_that("opt_params works", {
  expect_length(opt_params$opt_params, 3)
  expect_equal(opt_params$opt_params$scan_rate, 0.497119)
  expect_match(opt_params$opt_params$capture_direction, "Down")
  expect_match(opt_params$opt_params$cell, "cell1")
})
