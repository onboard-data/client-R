#' Process Columns
#' 
#' Function to process all columns in a dataframe inside a list. Converts list type columns to character. Used in `get_staged_data` 
#' 
#' @param df dataframe to parse
#' 
#' @return Processed df
#' 
process_columns <- function(df) {
  df[] <- lapply(df, function(col) {
    if (is.list(col)) {
      sapply(col, function(x) paste(unlist(x), collapse = ", "))
    } else {
      as.character(col)
    }
  })
  return(df)
}
