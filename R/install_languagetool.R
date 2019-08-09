#' @rdname languagetool
#' @export
quick_setup <- function(path = "~") {
  
  # Resolve "~" to appropriate directory
  path <- path.expand(path)
  
  # download languagetool
  download(path)
  
  # how to call languagetool
  jar_file <- file.path(path, "LanguageTool-4.6", "languagetool-commandline.jar")
  message("Executable:")
  return(paste0('java -jar "', jar_file, '"'))
}

download <- function(path){
  temp <- tempfile()
  utils::download.file("https://www.languagetool.org/download/LanguageTool-4.6.zip", temp)
  message("Unpacking archive...")
  utils::unzip(temp, exdir = path, overwrite = TRUE)
  unlink(temp)
}
