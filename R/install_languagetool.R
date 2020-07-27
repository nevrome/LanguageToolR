# The version of languagetool currently supported/recommended by this package
languagetool_version <- "5.0"

#' @rdname languagetool
#' @export
lato_quick_setup <- function(path = "~") {
  
  # check if the correct java version is available
  if (!lato_is_java_64bit_available()) {
    warning("LanguageTool requires a 64-bit version of JAVA.")
  }
  
  # Resolve "~" to appropriate directory
  path <- path.expand(path)
  
  # download languagetool
  lato_download(path)
  
  # how to call languagetool
  jar_file <- file.path(path, paste0("LanguageTool-", languagetool_version),
    "languagetool-commandline.jar")
  message("Executable:")
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
