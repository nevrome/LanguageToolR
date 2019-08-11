test_that("languagetool works with text, file and directory input`", {
  
  file_name <- system.file(package = "LanguageToolR", "test", "test_text.txt")
  dir_name  <- system.file(package = "LanguageToolR", "test")
  
  # tagger_only = TRUE is used to get the output quicer
  expect_silent({output_text <- languagetool(test_text, tagger_only = TRUE)})
  expect_silent({output_file <- languagetool(file_name, tagger_only = TRUE)})
  expect_silent({output_dir  <- languagetool(dir_name,  tagger_only = TRUE)})
  
  expect_is(output,      "character")
  expect_is(output_file, "character")
  expect_is(output_dir,  "character")
  
  # TODO: More tests are needed here to investigate the contents of the output.
  # E.g., to check the equivalence of the output with the same text but from 
  # different sources (text, file, directory).

})


test_that("languagetool fails where needed`", {
  
  # No input
  expect_error(languagetool(), info = "Error in languagetool() : No input defined.")
  
  # The 'executable' command is incorrect.
  # Both warning (by `system()`) and error (by `languagetool()`) are returned.
  expect_warning(
    expect_error(
      languagetool(test_text[1], tagger_only = TRUE, executable = "-")
    )
  )
  
})


test_that("languagetool JSON parsing works`", {
  
  expect_silent({output <- languagetool(test_text[1], )})
  expect_is(output, "character")
  
  # TODO: More tests are needed here to investigate the contents of the output
  
})

