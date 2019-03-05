#' LanguageTool API function
#' 
#' Provides a wrapper for the 
#' \href{http://wiki.languagetool.org/command-line-options}{LanguageTool CLI tool} 
#' for spelling, grammar and language checking. \code{quick_setup} provides an easy
#' option to automatically download LanguageTool.
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
#' @param list_languages Logical. Print all available languages and exit
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
#' @param tagger_only Logical. Don't check, but only print text analysis (sentences, 
#' part-of-speech tags)
#' @param list_unknown Logical. Also print a summary of words from the input that 
#' LanguageTool doesn't know
#' @param bitext Logical. Check bilingual texts with a tab-separated input file,
#' see http://languagetool.wikidot.com/checking-translations-bilingual-texts
#' @param profile Logical. Print performance measurements
#' @param verbose Logical. Print text analysis (sentences, part-of-speech tags)
#' @param version Logical. Print LanguageTool version number
#' @param apply Logical. automatically apply suggestions if available, printing result. 
#' NOTE: only use with very robust rules, as this will otherwise introduce new errors
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
#' @param path Character. Directory where LanguageTool should be installed. 
#'
#' @return Tibble (data.frame) with the output of languagetool parsed from json. 
#' Some options yield special outputs
#' @rdname languagetool
#' 
#' @examples 
#' languagetool(test_text)
#' 
#' @export
languagetool <- function( 
  x = c(),
  input_file = NA_character_,
  input_directory = NA_character_,
  recursive = FALSE,
  executable = "java -jar ~/LanguageTool-4.4/languagetool-commandline.jar",
  encoding = "utf-8",
  linebreak_paragraph = FALSE,
  language = "en-GB",
  list_languages = FALSE,
  auto_detect_language = FALSE,
  mothertongue = NA_character_,
  disabled_rules = c(),
  enabled_rules = c(),
  enabled_only = FALSE,
  disabled_categories = c(),
  enabled_categories = c(),
  tagger_only = FALSE,
  list_unknown = FALSE,
  bitext = FALSE,
  profile = FALSE,
  verbose = FALSE,
  version = FALSE,
  apply = FALSE,
  rule_file = NA_character_,
  false_friends_file = NA_character_,
  bitext_rules_file = NA_character_,
  language_model_directory = NA_character_,
  word2vec_model_directory = NA_character_,
  neural_network_model_directory = NA_character_,
  fast_text_model_file = NA_character_,
  fast_text_binary_file = NA_character_
) {
  
  if (!test_setup(executable)) {
    stop(
      "The provided executable is not available or does not work correctly. ",
      "You can install LanguageTool with the quick_setup() function."
    )
  }
  
  #### input selection ####
  input <- NA_character_
  if (!is.na(input_file)) {
    # one file input
    input <- input_file
  } else if (!is.na(input_directory) ) {
    # directory input
    input <- input_directory
  } else if (length(x) > 0) {
    # write input text to temporary file
    input <- tempfile()
    writeLines(x, input)
  } else if (list_languages | version) {
    # special output without input
  } else {
    stop("No input defined.")
  }
  
  #### call languagetool ####
  output_json <- system(
    command = paste(
      executable,
      paste(
        ifelse(recursive, paste("--recursive"), ""),
        ifelse(!is.na(encoding), paste("--encoding", encoding), ""),
        ifelse(linebreak_paragraph, paste("-b"), ""),
        ifelse(!is.na(language), paste("--language", language), ""),
        ifelse(list_languages, paste("--list"), ""),
        ifelse(auto_detect_language, paste("--autoDetect"), ""),
        ifelse(!is.na(mothertongue), paste("--mothertongue", mothertongue), ""),
        ifelse(length(disabled_rules) != 0, paste("--disable", paste(disabled_rules, collapse = ",")), ""),
        ifelse(length(enabled_rules) != 0, paste("--enable", paste(enabled_rules, collapse = ",")), ""),
        ifelse(enabled_only, paste("--enabledonly"), ""),
        ifelse(length(disabled_categories) != 0, paste("--disable", paste(disabled_categories, collapse = ",")), ""),
        ifelse(length(enabled_categories) != 0, paste("--enable", paste(enabled_categories, collapse = ",")), ""),
        ifelse(tagger_only, paste("--taggeronly"), ""),
        ifelse(list_unknown, paste("--list-unknown"), ""),
        ifelse(bitext, paste("--bitext"), ""),
        ifelse(profile, paste("--profile"), ""),
        ifelse(verbose, paste("--verbose"), ""),
        ifelse(version, paste("--version"), ""),
        ifelse(apply, paste("--apply"), ""),
        ifelse(!is.na(rule_file), paste("--rulefile", rule_file), ""),
        ifelse(!is.na(false_friends_file), paste("--falsefriends", false_friends_file), ""),
        ifelse(!is.na(bitext_rules_file), paste("--bitextrules", bitext_rules_file), ""),
        ifelse(!is.na(language_model_directory), paste("--languagemodel", language_model_directory), ""),
        ifelse(!is.na(word2vec_model_directory), paste("--word2vecmodel", word2vec_model_directory), ""),
        ifelse(!is.na(neural_network_model_directory), paste("--neuralnetworkmodel", neural_network_model_directory), ""),
        ifelse(!is.na(fast_text_model_file), paste("--fasttextmodel", fast_text_model_file), ""),
        ifelse(!is.na(fast_text_binary_file), paste("--fasttextbinary", fast_text_binary_file), ""),
        # json
        ifelse(!list_languages & !tagger_only & !list_unknown & !apply, "--json", ""),
        # input
        ifelse(!is.na(input), input, "")
      )
    ),
    intern = TRUE
  )
  
  #### special output ####
  # special output if language list is requested
  if (list_languages) {
    languages <- lapply(
      strsplit(output_json, " "),
      function(x) {
        tibble::tibble(
          id = x[1],
          name = paste(x[-1], collapse = " ")
        )
      }
    )
    return(do.call(rbind, languages))
  }
  
  # special output if tagger_only == TRUE or list_unknown == TRUE or
  # version == TRUE or apply == TRUE
  if (tagger_only | list_unknown | version | apply) {
    return(output_json)
  }
  
  #### normal output ####
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
  output_df <- do.call(rbind, output_df_list)
  
  # return output tibble
  return(output_df)

}

#' @rdname languagetool
#' @export
languages <- function(x) {
  languagetool(list_languages = TRUE)
}

#' @rdname languagetool
#' @export
version <- function(x) {
  languagetool(version = TRUE)
}

#' @rdname languagetool
#' @export
test_setup <- function(
  executable = "java -jar ~/LanguageTool-4.4/languagetool-commandline.jar"
) {
  system(paste(executable, "--version"), ignore.stdout = TRUE) == 0
}

#' @rdname languagetool
#' @name test_text
NULL
