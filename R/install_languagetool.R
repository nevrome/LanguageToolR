# The version of languagetool currently supported/recommended by this package
languagetool_version <- 4.6

#' @rdname languagetool
#' @export
lato_quick_setup <- function(path = "~") {
  
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
