#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import dplyr
#' @import httr
#' @import rrapply
#' @importFrom data.table rbindlist
#' @importFrom jsonlite fromJSON
#' @importFrom jsonlite toJSON
#' @importFrom lubridate as_datetime
#' @importFrom plyr rbind.fill
#' @importFrom purrr list_rbind
#' @importFrom purrr map
#' @importFrom rlist list.flatten
#' @importFrom rstudioapi askForSecret
#' @importFrom stringr str_split
#' @importFrom tibble rownames_to_column
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr separate_rows
#' @importFrom utils askYesNo
#' @importFrom utils type.convert
#' @importFrom utils URLencode
## usethis namespace: end
NULL

# turn off scientific notations
options(scipen = 999)
