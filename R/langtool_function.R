langtool <- function( 
  x = c(),
  executable = "languagetool",
  encoding = "utf-8",
  language = "en-GB",
  auto_detect_language = FALSE,
  mothertongue = NA_character_,
  disabled_rules = c(),
  enabled_rules = c(),
  enabled_only = FALSE,
  disabled_categories = c(),
  enabled_categories = c(),
  bitext = FALSE,
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

  # write input text to temporary file
  input_file <- tempfile()
  writeLines(
    x,
    input_file
  )
  
  # call languagetool to get json result
  output_json <- system2(
    command = executable,
    args = c(
      ifelse(!is.na(encoding), paste("--encoding", encoding), ""),
      ifelse(!is.na(language), paste("--language", language), ""),
      ifelse(auto_detect_language, paste("--autoDetect"), ""),
      ifelse(!is.na(mothertongue), paste("--mothertongue", mothertongue), ""),
      ifelse(length(disabled_rules) != 0, paste("--disable", paste(disabled_rules, collapse = ",")), ""),
      ifelse(length(enabled_rules) != 0, paste("--enable", paste(enabled_rules, collapse = ",")), ""),
      ifelse(enabled_only, paste("--enabledonly"), ""),
      ifelse(length(disabled_categories) != 0, paste("--disable", paste(disabled_categories, collapse = ",")), ""),
      ifelse(length(enabled_categories) != 0, paste("--enable", paste(enabled_categories, collapse = ",")), ""),
      ifelse(bitext, paste("--bitext"), ""),
      ifelse(profile, paste("--profile"), ""),
      ifelse(verbose, paste("--verbose"), ""),
      ifelse(!is.na(rule_file), paste("--rulefile", rule_file), ""),
      ifelse(!is.na(false_friends_file), paste("--falsefriends", false_friends_file), ""),
      ifelse(!is.na(language_model_directory), paste("--languagemodel", language_model_directory), ""),
      ifelse(!is.na(word2vec_model_directory), paste("--word2vecmodel", word2vec_model_directory), ""),
      ifelse(!is.na(neural_network_model), paste("--neuralnetworkmodel", neural_network_model), ""),
      ifelse(!is.na(fast_text_model_file), paste("--fasttextmodel", fast_text_model_file), ""),
      ifelse(!is.na(fast_text_binary_file), paste("--fasttextbinary", fast_text_binary_file), ""),
      "--json",
      input_file
    ),
    stdout = TRUE,
    stderr = ""
  )
  
  # json output to R list
  output_list <- rjson::fromJSON(output_json)
  
  # R list to useful tibble
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
  
  # return output tibble
  return(output_df)

}
