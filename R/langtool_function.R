#' LanguageTool API function
#' 
#' Provides a wrapper for the 
#' \href{http://wiki.languagetool.org/command-line-options}{LanguageTool CLI tool} 
#' for spelling, grammar and language checking. \code{lato_quick_setup} provides 
#' an easy option to automatically download LanguageTool.
#'
#' @param x Character vector. Text to analyse
#' @param input_file Path. File to analyse (instead of x)
#' @param input_directory Path. Directory with files to analyse (instead of x). 
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
#' @param disabled_rules Character vector. List of rule ids to be disabled
#' @param enabled_rules Character vector. List of rule ids to be enabled
#' @param enabled_only Logical. Disable all rules except those enabled explicitly
#' @param disabled_categories Character vector. List of category ids to be disabled
#' @param enabled_categories Character vector. List of category ids to be enabled
#' @param tagger_only Logical. Don't check, but only print text analysis (sentences, 
#' part-of-speech tags)
#' @param list_unknown Logical. Also print a summary of words from the input that 
#' LanguageTool doesn't know
#' @param bitext Logical. Check bilingual texts with a tab-separated input file,
#' see http://languagetool.wikidot.com/checking-translations-bilingual-texts
#' @param profile Logical. Print performance measurements
#' @param verbose Logical. Print text analysis (sentences, part-of-speech tags)
#' @param version Logical. Print LanguageTool version number
#' @param apply Logical. Automatically apply suggestions if available, printing result. 
#' NOTE: only use with very robust rules, as this will otherwise introduce new errors
#' @param rule_file Path. Use an additional grammar file; if the filename 
#' contains a known language code, it is used in addition of standard rules
#' @param remote_rules_file Path. Configure rules depending on external services 
#' via a JSON file (optional)
#' @param false_friends_file Path. Use external false friend file to be used 
#' along with the built-in rules
#' @param bitext_rules_file Path. Use external bitext XML rule file (useful only in 
#' bitext mode)
#' @param language_model_directory Path. A directory with e.g. 'en' sub directory 
#' (i.e. a language code) that contains '1grams'...'3grams' sub directories with Lucene 
#' indexes with ngram occurrence counts; activates the confusion rule if supported;
#' see https://dev.languagetool.org/finding-errors-using-n-gram-data
#' @param fast_text_model_file Path. fasttext language detection model (optional), 
#' see https://fasttext.cc/docs/en/language-identification.html
#' @param fast_text_binary_file Path. fasttext executable (optional), 
#' see https://fasttext.cc/docs/en/support.html
#' @param line_by_line Logical. Work on file line by line (for development, 
#' e.g. inside an IDE)
#' @param enable_temp_off Logical. Enable all temp_off rules (for testing and 
#' development)
#' @param clean_overlapping Logical. Clean overlapping matches (show only the 
#' highest priority match)
#' @param level Character. Enable the given level (currently only 'PICKY')
#' @param path Path. Directory where LanguageTool should be installed.
#' @param overwrite Logical. Should the user not be asked whether she would like to 
#' overwrite an already available LanguageTool installation? (Default: FALSE)
#' @param timeout Integer. Maximum download time in seconds. See ?utils::download.file
#' @param quiet Logical. Should the console output of languagetool be displayed or hidden? 
#'
#' @return A \code{\link[tibble]{tibble}} (\code{data.frame}) with the output of languagetool parsed from json. 
#' Some options yield special outputs
#' @rdname languagetool
#' 
#' @examples 
#' \donttest{
#' # install LanguageTool if it is not already available
#' if (!LanguageToolR::lato_test_setup()) {
#'   LanguageToolR::lato_quick_setup(overwrite = TRUE)
#' }
#' 
#' # apply LanguageTool checks to some test text
#' languagetool(test_text)
#' }
#' 
#' @export
languagetool <- function( 
  x = c(),
  input_file = NA_character_,
  input_directory = NA_character_,
  recursive = FALSE,
  executable = lato_get_default_executable(),
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
  remote_rules_file = NA_character_,
  false_friends_file = NA_character_,
  bitext_rules_file = NA_character_,
  language_model_directory = NA_character_,
  fast_text_model_file = NA_character_,
  fast_text_binary_file = NA_character_,
  line_by_line = FALSE,
  enable_temp_off = FALSE,
  clean_overlapping = FALSE,
  level = NA_character_,
  quiet = FALSE
) {
  
  if (!lato_test_setup(executable)) {
    stop(
      "The provided executable is not available or does not work correctly. ",
      "You can install LanguageTool with the lato_quick_setup() function."
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
    on.exit(unlink(input)) # ensure cleanup
  } else if (list_languages | version) {
    # special output without input
  } else {
    stop("No input defined.")
  }
  
  #### Construct languagetool command ####
  command <-
    paste0(
      executable,
      if (recursive)                                    " --recursive",
      if (!is.na(encoding))                       paste(" --encoding", encoding),
      if (linebreak_paragraph)                          " -b",
      if (!is.na(language))                       paste(" --language", language),
      if (list_languages)                               " --list",
      if (auto_detect_language)                         " --autoDetect",
      if (!is.na(mothertongue))                   paste(" --mothertongue", mothertongue),
      if (length(disabled_rules) != 0)            paste(" --disable", paste(disabled_rules, collapse = ",")),
      if (length(enabled_rules)  != 0)            paste(" --enable",  paste(enabled_rules,  collapse = ",")),
      if (enabled_only)                                 " --enabledonly",
      if (length(disabled_categories) != 0)       paste(" --disable", paste(disabled_categories, collapse = ",")),
      if (length(enabled_categories)  != 0)       paste(" --enable",  paste(enabled_categories,  collapse = ",")),
      if (tagger_only)                                  " --taggeronly",
      if (list_unknown)                                 " --list-unknown",
      if (bitext)                                       " --bitext", 
      if (profile)                                      " --profile",
      if (verbose)                                      " --verbose",
      if (version)                                      " --version",
      if (apply)                                        " --apply",  
      if (!is.na(rule_file))                      paste(" --rulefile", rule_file),
      if (!is.na(remote_rules_file))              paste(" --remoterules", remote_rules_file),
      if (!is.na(false_friends_file))             paste(" --falsefriends", false_friends_file),
      if (!is.na(bitext_rules_file))              paste(" --bitextrules", bitext_rules_file),
      if (!is.na(language_model_directory))       paste(" --languagemodel", language_model_directory),
      if (!is.na(fast_text_model_file))           paste(" --fasttextmodel",  fast_text_model_file), 
      if (!is.na(fast_text_binary_file))          paste(" --fasttextbinary", fast_text_binary_file),
      if (line_by_line)                                 " --line-by-line",
      if (enable_temp_off)                              " --enable-temp-off",
      if (clean_overlapping)                            " --clean-overlapping",
      if (!is.na(level))                          paste(" --level", level),
      # json
      if (!list_languages & !tagger_only & !list_unknown & !apply) " --json",
      # input
      if (!is.na(input)) paste0(' "', input, '"')
    )
  
  #### call languagetool ####
  langtool_output <- system(command, intern = TRUE, ignore.stderr = quiet)
  
  #### special output ####
  # special output if language list is requested
  if (list_languages) {
    languages <- lapply(
      strsplit(langtool_output, " "),
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
    return(langtool_output)
  }
  
  # If `quiet = FALSE` and warnings exist, output contains non-JSON strings 
  # with messages, that fail to be parsed. They should be removed.
  is_json <- sapply(langtool_output, jsonlite::validate, USE.NAMES = FALSE)
  langtool_output <- langtool_output[is_json]
  
  #### regular output ####
  output_df <- lato_parse_json(langtool_output)
  
  # return output tibble
  return(output_df)
}

# Internal function: JSON to tibble parser
lato_parse_json <- function(x) {
  
  #### regular output ####
  # json output to R list
  output_list <- rjson::fromJSON(x)
  
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
  
  # To have class-consistent output
  if (is.null(output_df)) {
    output_df <- tibble::tibble()
  }
  
  return(output_df)
}


#' @rdname languagetool
#' @export
lato_list_languages <- function(executable = lato_get_default_executable()) {
  languagetool(executable = executable, list_languages = TRUE)
}

#' @rdname languagetool
#' @export
lato_get_version <- function(executable = lato_get_default_executable()) {
  languagetool(executable = executable, version = TRUE)
}

#' @rdname languagetool
#' @export
lato_test_setup <- function(executable = lato_get_default_executable()) {
  system(paste(executable, "--version"), ignore.stdout = TRUE, ignore.stderr = TRUE) == 0
}

#' @rdname languagetool
#' @export
lato_get_default_executable <- function() {
  paste0(
    'java -jar "', 
    path.expand(paste0('~/LanguageTool-', languagetool_version, '/languagetool-commandline.jar')),
    '"')
}

#' @rdname languagetool
#' @name test_text
NULL
