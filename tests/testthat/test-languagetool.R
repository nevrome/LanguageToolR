# show version
print(lato_get_version())

test_that("languagetool output in is empty data.frame in case of empty input`", {
  expect_silent({output_empty <- languagetool("")})
  expect_is(output_empty, "data.frame")
  expect_equal(nrow(output_empty), 0)
})

# run test for same text from different sources
file_name <- system.file(package = "LanguageToolR", "test", "test_text.txt")
dir_name  <- system.file(package = "LanguageToolR", "test")
output_text <- languagetool(test_text)
output_file <- languagetool(input_file = file_name)
output_dir  <- languagetool(input_directory = dir_name, recursive = TRUE)

test_that("languagetool returns data.frames`", {
  expect_is(output_text, "data.frame")
  expect_is(output_file, "data.frame")
  expect_is(output_dir,  "data.frame")
})

comparison_func <- function(x, y) {nrow(x) == nrow(y) && ncol(x) == ncol(y)}

test_that("languagetool output with the same text but from different sources (text, file, directory) has same format`", {
  expect_true(
    all(sapply(list(output_text, output_dir), FUN = comparison_func, output_file))
  )
})

# store result: output_stored <- output_text; save(output_stored, file = "inst/expected_output/expected_output.RData")
load(system.file(package = "LanguageToolR", "expected_output", "expected_output.RData"))

test_that("languagetool JSON parsing works`", {
  expect_true(comparison_func(output_text, output_stored))
})

test_that("languagetool fails where needed`", {
  # No input
  expect_error(languagetool(), info = "Error in languagetool() : No input defined.")
  # The 'executable' command is incorrect.
  if (.Platform$OS.type != "windows") {
    expect_error(
      languagetool(test_text[1], tagger_only = TRUE, executable = "-")
    )
  } else {
    # Both warning (by `system()`) and error (by `languagetool()`) are returned
    # on Windows
    expect_warning(
      expect_error(
        languagetool(test_text[1], tagger_only = TRUE, executable = "-")
      )
    )
  }
})
