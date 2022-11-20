# The version of languagetool currently supported/recommended by this package
languagetool_version <- "5.9"

#' @rdname languagetool
#' @export
lato_quick_setup <- function(path = "~", overwrite = FALSE) {
  
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
  
  version_that_would_be_created <- paste0("LanguageTool-", languagetool_version)
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
    paste0("LanguageTool-", languagetool_version),
    "languagetool-commandline.jar"
  )
  message("Path to the executable:")
  return(paste0('java -jar "', jar_file, '"'))
}

# Internal to download the tool
lato_download <- function(path){
  temp <- tempfile()
  url <- paste0("https://www.languagetool.org/download/LanguageTool-",
    languagetool_version,".zip")
  
  utils::download.file(url, temp)
  message("Unpacking archive...")
  utils::unzip(temp, exdir = path, overwrite = TRUE)
  unlink(temp)
}

lato_is_java_64bit_available <- function() {
  java_version_output <- system2("java" , c("-XshowSettings:properties", "-version"), stderr = TRUE, stdout = TRUE)
  return(any(grepl("sun.arch.data.model = 64", java_version_output)))
}
