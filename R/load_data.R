#' Load a CSV file with optional checks
#'
#' @param path Character. Path to the CSV file.
#' @param na.strings Character vector of strings to interpret as NA. Default is c("", "NA").
#' @param stringsAsFactors Logical. Should character vectors be converted to factors? Default is FALSE.
#' @return A data.frame or tibble.
#' @examples
#' df <- load_data("data/raw/mydata.csv")
load_csv <- function(path, na.strings = c("", "NA"), stringsAsFactors = FALSE) {
  if (!file.exists(path)) {
    stop(paste("File does not exist:", path))
  }
  
  message(paste("Loading data from:", path))
  
  df <- tryCatch({
    read.csv(path, na.strings = na.strings, stringsAsFactors = stringsAsFactors)
  }, error = function(e) {
    stop(paste("Failed to load data:", e$message))
  })
  
  return(df)
}

#' Load a TXT file as a data frame
#'
#' @param path Character. Path to the TXT file.
#' @param sep Character. Field separator (default is tab: "\t" but this file is "").
#' @param header Logical. Does the file have a header row? Default is TRUE.
#' @param ... Additional arguments passed to read.table().
#' @return A data.frame.
#' @examples
#' df <- load_txt("data/raw/mydata.txt")
load_txt <- function(path, sep = "", header = TRUE, ...) {
  if (!file.exists(path)) {
    stop(paste("File does not exist:", path))
  }
  
  message(paste("Loading TXT data from:", path))
  
  df <- tryCatch({
    read.table(path, sep = sep, header = header, stringsAsFactors = FALSE, ...)
  }, error = function(e) {
    stop(paste("Failed to load TXT file:", e$message))
  })
  
  return(df)
}
