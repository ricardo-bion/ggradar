context("Perform test for ggradar")

test_that("Error correctly raised", {
  expect_error(ggradar(mtcars))
  expect_error(ggradar(mtcars, axis.labels = names(mtcars)[-c(1, 2)]))
})
