test_text <- readLines("data-raw/test_text.txt")

usethis::use_data(test_text, overwrite = T)
