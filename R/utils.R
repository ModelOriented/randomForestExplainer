# Helpers to avoid warnings in computations
# Are all values NA?
all_na <- function(x) {
  if (!anyNA(x)) {
    return(FALSE)
  }
  all(is.na(x))
}
# Min but returns NA if only has NA
min_na <- function(x) {
  if (all_na(x)) {
    return(NA)
  }
  min(x, na.rm = TRUE)
}
# max but returns NA if only has NA
max_na <- function(x) {
  if (all_na(x)) {
    return(NA)
  }
  max(x, na.rm = TRUE)
}
