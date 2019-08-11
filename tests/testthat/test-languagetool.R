test_that("languagetool works with `tagger_only = TRUE`", {
  
  expect_silent({output <- languagetool(test_text, tagger_only = TRUE)})
  expect_is(output, "character")
  
})
