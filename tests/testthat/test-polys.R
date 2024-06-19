
# check_poly --------------------------------------------------------------


test_that("check_poly finds basic bad shapes", {
  expect_equal(check_poly(c(1, 1, 3)), 1)
  expect_equal(check_poly(c(1, 1, 1, 4)), 1)
  expect_equal(check_poly(c(1, 9, 1, 4, 1)), 2)
})

test_that("check_poly finds basic ok shapes with 0 difference", {
  expect_equal(check_poly(c(1, 1, 2)), 0)
  expect_equal(check_poly(c(1, 1, 1, 3)), 0)
  expect_equal(check_poly(c(3, 9, 1, 4, 1)), 0)
})

test_that("check_poly finds basic ok shapes with negative difference", {
  expect_equal(check_poly(c(2, 1, 2)), -1)
  expect_equal(check_poly(c(1, 1, 2, 3)), -1)
  expect_equal(check_poly(c(3, 9, 5, 4, 1)), -4)
})



# adjust_poly -------------------------------------------------------------


