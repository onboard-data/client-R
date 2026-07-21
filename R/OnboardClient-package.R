#' @keywords internal
"_PACKAGE"

# Suppress R CMD check NOTEs for column names used in dplyr expressions
utils::globalVariables("id")

# Namespace ---------------------------------------------------------------
## usethis namespace: start
#' @import dplyr
#' @import httr2
#' @importFrom curl form_file
#' @importFrom jsonlite fromJSON
#' @importFrom jsonlite toJSON
#' @importFrom lubridate as_datetime
#' @importFrom plyr rbind.fill
#' @importFrom purrr list_rbind
#' @importFrom purrr map
#' @importFrom rstudioapi askForSecret
#' @importFrom stringr str_split
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr separate_rows
#' @importFrom tidyr unite
#' @importFrom tidyr unnest
#' @importFrom utils askYesNo
#' @importFrom utils URLencode
## usethis namespace: end
NULL


# Notations ---------------------------------------------------------------
# turn off scientific notations
options(scipen = 999)