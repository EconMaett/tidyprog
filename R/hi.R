#' Print a welcome message
#' 
#' This function prints "Wow!", or a custom text, on the console.
#' 
#' @param text The text to print, "Wow!" by default.
#' 
#' @return Thee `text` argument, invisibly
#' 
#' @examples
#' hi()
#' hi("Hello!")
hi <- function(text = "Oops!") {
  print(text)
  invisible(text)
}