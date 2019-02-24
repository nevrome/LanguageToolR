langtool <- function( 
  x = c(),
  language = "en-GB",
  executable = "languagetool",
  encoding = "utf-8",
  auto_detect_language = FALSE,
  mothertongue = NA_character_,
  disabled_rules = c(),
  enabled_rules = c(),
  enabled_only = FALSE,
  disabled_categories = c(),
  enabled_categories = c(),
  profile = FALSE,
  verbose = FALSE,
  rule_file = NA_character_,
  false_friends_file = NA_character_,
  language_model_directory = NA_character_,
  word2vec_model_directory = NA_character_,
  neural_network_model = NA_character_,
  fast_text_model_file = NA_character_,
  fast_text_binary_file = NA_character_
) {

  input_file <- tempfile()
  writeLines(
    x,
    input_file
  )
  
  output_json <- system2(
    command = executable,
    args = c(
      "--json",
      input_file
    ),
    stdout = TRUE
  )
  
  output_list <- rjson::fromJSON(output_json)
  
  output_df_list <- lapply(
    output_list$matches,
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
  
  return(output_df)

}
