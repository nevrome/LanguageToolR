test_that("lato_get_default_executable()", {

  expected_str <- paste0(
    "java -jar \"",
    path.expand("~"),
    "/LanguageTool-",
    languagetool_version,
    "/languagetool-commandline.jar\""
  )
  
  expect_silent({output <- lato_get_default_executable()})
  expect_is(output, "character")
  expect_length(output, 1)
  expect_equal(output, expected_str)
})

test_that("lato_get_version()", {
  
  expect_silent({output <- lato_get_version(current_executable)})
  expect_is(output, "character")
  expect_length(output, 1)
  expect_match(output, "^LanguageTool version")
})

test_that("lato_list_languages()", {
  
  expect_silent({output <- lato_list_languages(current_executable)})
  expect_is(output, "data.frame")
  expect_named(output, c("id", "name"))
})

test_that("lato_test_setup()", {
  
  expect_silent({output <- lato_test_setup(current_executable)})
  expect_is(output, "logical")
  expect_length(output, 1)
})


