test_that("get_default_executable()", {
  languagetool_version <- 4.6

  expected_str <- paste0(
    "java -jar \"D:/Dokumentai/LanguageTool-",
    languagetool_version,
    "/languagetool-commandline.jar\""
  )
  
  expect_silent({output <- get_default_executable()})
  expect_is(output, "character")
  expect_length(output, 1)
  expect_equal(output, expected_str)
})

test_that("LanguageToolR::version()", {
  
  expect_silent({output <- LanguageToolR::version()})
  expect_is(output, "character")
  expect_length(output, 1)
  expect_match(output, "^LanguageTool version")
})

test_that("LanguageToolR::languages()", {
  
  expect_silent({output <- LanguageToolR::languages()})
  expect_is(output, "data.frame")
  expect_named(output, c("id", "name"))
})

test_that("LanguageToolR::test_setup()", {
  
  expect_silent({output <- LanguageToolR::test_setup()})
  expect_is(output, "logical")
  expect_length(output, 1)
})


