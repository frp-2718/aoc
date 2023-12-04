is.digit <- function(chr) {
  return(nchar(chr) == 1 && grepl("[0-9]", chr))
}