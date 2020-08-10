context("Check version of LanguageTool")

test_that("lato_check_online_version() works", {
  skip_if_not(check_is_online(), "Internet connection is needed")
  expect_silent(online_version <- lato_check_online_version())
  expect_is(online_version, "numeric_version")
  expect_true(online_version >= "5.0")
})
