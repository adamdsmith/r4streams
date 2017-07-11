is_missing <- function(string) {
  is.na(string) | nchar(string) == 0 | grepl("^ +$", string)
}
