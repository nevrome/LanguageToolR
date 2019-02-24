input <- c("LanguageTool offers spell and grammar checking.", 
"Just paste your text here and click the 'Check Text' button. ",
"Click the colored phrases for details on potential errors. ",
"or use this text too see an few of of the problems that LanguageTool can detecd. ",
"What do you thinks of grammar checkers? Please not that they are not perfect. ",
"Style issues get a blue marker: It's 5 P.M. in the afternoon. ",
"The weather was nice on Thursday, 27 June 2017.")

output <- system(
  command = paste("languagetool --json", "data-raw/languagetool_testext.txt"),
  intern = TRUE,
  input = input
)

output_list <- rjson::fromJSON(output)

output_list$matches -> ma

output_df_list <- lapply(
  ma,
  function(x) {
    tibble::tibble(
      message = x$message,
      short_message = x$shortMessage,
      replacements = ifelse(length(x$replacements) > 0, I(unlist(x$replacements)), NA),
      offset = x$offset,
      length = x$length,
      context_text = x$context$text,
      context_offset = x$context$offset,
      context_length = x$context$length,
      sentence = x$sentence,
      type_name = x$type$typeName,
      rule_id = x$rule$id,
      rule_description = x$rule$description,
      rule_issue_type = x$rule$issueType,
      rule_category_id = x$rule$category$id,
      rule_category_name = x$rule$category$name,
      ignore_for_incomplete_sentence = x$ignoreForIncompleteSentence
    )
  }
)

output_df <- dplyr::bind_rows(output_df_list)

