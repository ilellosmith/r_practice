library(testthat)
context('unit test caller')
test_dir(
  "./testthat",
  env = shiny::loadSupport(),
  reporter = c("progress", "fail")
)
