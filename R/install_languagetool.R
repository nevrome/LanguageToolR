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


# Gets LanguageTool versions installed in the same dir, which is used by
# lato_quick_setup() by default.
# @examples 
# lato_get_installed_versions()
lato_get_installed_versions <- function(path = lato_default_path()) {
  extract_versions(
    text = dir(path, pattern = "^LanguageTool-", full.names = TRUE),
    pattern = "(?<=LanguageTool-)\\d{1,2}[.]\\d{1,2}$"
  )
}

# Makes a more reliable path
make_path <- function(...) {
  normalizePath(file.path(...), winslash = "/", mustWork = FALSE)
}

# Removes current (or indicated) version of LanguageTool
lato_remove <- function(version = lato_default_version(), path = lato_default_path()) {
  tool_dir <- make_path(path, paste0("LanguageTool-", version))
  if (dir.exists(tool_dir)) {
    message("Removing: ", tool_dir)
    unlink(tool_dir, recursive = TRUE, force = TRUE)
    message("  DONE")
    
  } else {
    warning("Directory was not found: \n", tool_dir, call. = FALSE)
  }
}


# Removes older version of LanguageTool, if present
lato_remove_old_versions <- function(path = lato_default_path()) {
  v_installed <- lato_get_installed_versions(path = path)
  v_newest <- max(v_installed)
  v_old <- setdiff(as.numeric_version(v_installed), as.numeric_version(v_newest))
  
  if (length(v_old) == 0) {
    message("No old versions of LanguageTool were found.")
  } else {
    message(
      "Removing directories with old versions (",
      paste(v_old, collapse = ", "),
      ") of LanguageTool..."
    )
    lapply(as.list(v_old), lato_remove, path = path)
  }
}


# Checks for updates of LanguageTool
lato_check_for_updates <- function(path = lato_default_path()) {
  v_online <- lato_get_online_version()
  v_installed <- lato_get_installed_versions()
  
  if (v_online > max(v_installed)) {
   message("An update to LanguageTool-", v_online, " is available.")
    # TODO: ask if a user wants to update
    # message("Updating to LanguageTool-", v_online)
    # lato_quick_setup()
    
    # TODO: ask if a user wants to remove old versions of LanguageTool.
    # if (isTRUE(remove_old)) {
    #   lato_remove_old_versions()
    # }
    
  } else {
    message(
      "The most recent version (LanguageTool-", v_online, ") ",
      "is already present."
    )
  }
}

