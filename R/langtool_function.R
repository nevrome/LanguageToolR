#' LanguageTool API function
#' 
#' Provides an API for the 
#' \link[http://wiki.languagetool.org/command-line-options]{LanguageTool CLI tool} 
#' for spelling, grammar and language checking.
#'
#' @param x Character vector. Text to analyse
#' @param input_file Character. File to analyse (instead of x)
#' @param input_directory Character. Directory with files to analyse (instead of x). 
#' Maybe recursive = TRUE is necessary 
#' @param recursive Logical. Work recursively on directory, not on a single file
#' @param executable Character. Path to the languagetool executable on your system
#' @param encoding Character. Character set of the input text, e.g. utf-8 or latin1
#' @param linebreak_paragraph Logical. Assume that a single line break marks the end 
#' of a paragraph
#' @param language Character. The language code of the text, e.g. en for English, 
#' en-GB for British English
#' @param auto_detect_language Logical. Auto-detect the language of the input text - 
#' note this will not detect variants like 'English (US)', so you will not get spell 
#' checking for languages with variants
#' @param mothertongue Character. The language code of your first language, used to 
#' activate false-friend checking
#' @param disabled_rules Integer vector. List of rule ids to be disabled
#' @param enabled_rules Integer vector. List of rule ids to be enabled
#' @param enabled_only Logical. Disable all rules except those enabled explicitly
#' @param disabled_categories Integer vector. List of category ids to be disabled
#' @param enabled_categories Integer vector. List of category ids to be enabled
#' @param bitext Logical. Check bilingual texts with a tab-separated input file,
#' see http://languagetool.wikidot.com/checking-translations-bilingual-texts
#' @param profile Logical. Print performance measurements
#' @param verbose Logical. Print text analysis (sentences, part-of-speech tags)
#' @param rule_file Character. Use an additional grammar file; if the filename 
#' contains a known language code, it is used in addition of standard rules
#' @param false_friends_file Character. Use external false friend file to be used 
#' along with the built-in rules
#' @param bitext_rules_file Character. Use external bitext XML rule file (useful only in 
#' bitext mode)
#' @param language_model_directory Character. A directory with e.g. 'en' sub directory 
#' (i.e. a language code) that contains '1grams'...'3grams' sub directories with Lucene 
#' indexes with ngram occurrence counts; activates the confusion rule if supported
#' @param word2vec_model_directory Character. A directory with e.g. 'en' sub directory 
#' (i.e. a language code) that contains final_embeddings.txt and dictionary.txt; 
#' activates neural network based rules
#' @param neural_network_model_directory Character. A base directory for various saved 
#' neural network models
#' @param fast_text_model_file Character. fasttext language detection model, 
#' see https://fasttext.cc/docs/en/language-identification.html
#' @param fast_text_binary_file Character. fasttext executable, 
#' see https://fasttext.cc/docs/en/support.html
#'
#' @return Tibble (data.frame) with the output of languagetool parsed from json
#' 
#' @export
languagetool <- function( 
  x = c(),
  input_file = NA_character_,
  input_directory = NA_character_,
  recursive = FALSE,
  executable = "languagetool",
  encoding = "utf-8",
  linebreak_paragraph = FALSE,
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
  bitext_rules_file = NA_character_,
  language_model_directory = NA_character_,
  word2vec_model_directory = NA_character_,
  neural_network_model_directory = NA_character_,
  fast_text_model_file = NA_character_,
  fast_text_binary_file = NA_character_
) {

  # input selection
  if (!is.na(input_file)) {
    # one file input
    input <- input_file
  } else if (!is.na(input_directory) ) {
    # directory input
    input <- input_directory
  } else {
    # write input text to temporary file
    input <- tempfile()
    writeLines(x, input)
  }
  
  # call languagetool to get json result
  output_json <- system2(
    command = executable,
    args = c(
      ifelse(recursive, paste("--recursive"), ""),
      ifelse(!is.na(encoding), paste("--encoding", encoding), ""),
      ifelse(linebreak_paragraph, paste("-b"), ""),
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
      ifelse(!is.na(bitext_rules_file), paste("--bitextrules", bitext_rules_file), ""),
      ifelse(!is.na(language_model_directory), paste("--languagemodel", language_model_directory), ""),
      ifelse(!is.na(word2vec_model_directory), paste("--word2vecmodel", word2vec_model_directory), ""),
      ifelse(!is.na(neural_network_model_directory), paste("--neuralnetworkmodel", neural_network_model_directory), ""),
      ifelse(!is.na(fast_text_model_file), paste("--fasttextmodel", fast_text_model_file), ""),
      ifelse(!is.na(fast_text_binary_file), paste("--fasttextbinary", fast_text_binary_file), ""),
      "--json",
      input
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
