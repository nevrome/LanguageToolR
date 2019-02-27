#' @rdname languagetool
#' @export
quick_setup <- function(path = "~") {
  
  # check if languagetool is installed
  check_if_installed(path)

  # download languagetool
  download(path)
  
  # how to call languagetool
  jar_file <- file.path(path, "LanguageTool-4.4", "languagetool-commandline.jar")
  message("Executable:")
  return(paste0("java -jar ", jar_file))
}

check_if_installed <- function(path) {
  place <- file.path(path, "LanguageTool-4.4")
  exists <- dir.exists(place)
  if (exists) {
    stop("LanguageTool seems to be already installed at: ", place)
  } else {
    message("LanguageTool does not seem to be installed yet.")
  }
  return(exists)
}

download <- function(path){
  temp <- tempfile()
  utils::download.file("https://www.languagetool.org/download/LanguageTool-4.4.zip", temp)
  message("Unpacking archive...")
  utils::unzip(temp, exdir = path)
  unlink(temp)
}
