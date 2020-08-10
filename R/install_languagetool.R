# The version of languagetool currently supported/recommended by this package
lato_default_version <- function() {
  "5.0"
}

lato_default_path <- function() {
  "~"
}

#' @rdname languagetool
#' @export
lato_quick_setup <- function(path = lato_default_path(), overwrite = FALSE) {
  
  should_the_download_happen <- TRUE
  
  # check if the correct java version is available
  if (!lato_is_java_64bit_available()) {
    warning("LanguageTool requires a 64-bit version of JAVA.")
  }
  
  # resolve "~" to appropriate directory
  path <- path.expand(path)
  
  # check for available versions and check if download useful
  available_versions <- dir(path, pattern = "LanguageTool")
  if (length(available_versions) > 0) {
    message(paste0("LanguageTool seems to be already available in ", path ,":"))
    message(paste(available_versions, collapse = ", "))
  }
  
  version_that_would_be_created <- paste0("LanguageTool-", lato_default_version())
  
  if (!overwrite && version_that_would_be_created %in% available_versions) {
    message(paste(
      "The version that would be downloaded is already there:", 
      version_that_would_be_created
    ))
    should_be_overwritten <- readline(
      prompt = "Overwrite? [y/n]: "
    )
    if (should_be_overwritten != "y") {
      should_the_download_happen <- FALSE
    }
  }
  
  # download languagetool
  if (should_the_download_happen) {
    message(paste("Now downloading:", version_that_would_be_created))
    lato_download(path)
  }
  
  # how to call languagetool
  jar_file <- file.path(
    path, 
    paste0("LanguageTool-", lato_default_version()),
    "languagetool-commandline.jar"
  )
  message("Path to the executable:")
  return(paste0('java -jar "', jar_file, '"'))
}

# Internal to download the tool
lato_download <- function(path) {
  temp <- tempfile()
  url <- paste0(
    "https://www.languagetool.org/download/",
    "LanguageTool-", lato_default_version(), ".zip"
  )
  
  utils::download.file(url, temp)
  message("Unpacking archive...")
  utils::unzip(temp, exdir = path, overwrite = TRUE)
  unlink(temp)
}

lato_is_java_64bit_available <- function() {
  java_version_output <- system2("java" , 
    c("-XshowSettings:properties", "-version"), stderr = TRUE, stdout = TRUE
  )
  return(any(grepl("sun.arch.data.model = 64", java_version_output)))
}

# Checks if computer (machine) has valid IP.
# If valid IP addess is detected in ip/if config, it is treated as machine
# has Internet connection. A better solution might be pingr::is_online()
check_is_online <- function() {
  ip_config_message <-
    if (.Platform$OS.type == "windows") {
      system2("ipconfig", stdout = TRUE, stderr = FALSE)
      
    } else {
      system2("ifconfig", stdout = TRUE, stderr = FALSE)
    }
  
  ip_digits_pattern <- "(25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)"
  valid_ip_pattern <- paste(rep(ip_digits_pattern, 4), collapse = "[.]")
  any(grep(valid_ip_pattern, ip_config_message))
}



extract_versions <- function(text, pattern) {
  matches <- regexpr(pattern = pattern, text = text, perl = TRUE)
  versions <- as.numeric_version(regmatches(text, m = matches))
  versions
}

# Finds the most current released version of LanguageTool.
# Internet connection is required.
# @examples 
# lato_get_online_version()
lato_get_online_version <- function() {
  if (!check_is_online()) {
    stop(
      "This function requires Internet connection but it seems that your ",
      "machine is offline. Please check the Internet connection and try again."
    )
  }
  versions <-
    extract_versions(
      text = readLines("https://www.languagetool.org/download/"),
      pattern = "(?<=LanguageTool-)\\d{1,2}[.]\\d{1,2}(?=[.]zip)"
    )
  max(versions)
}


