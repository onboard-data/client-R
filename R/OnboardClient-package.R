#' @keywords internal
"_PACKAGE"

# Namespace ---------------------------------------------------------------
## usethis namespace: start
#' @import dplyr
#' @import httr2
#' @importFrom curl form_file
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
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr separate_rows
#' @importFrom tidyr unite
#' @importFrom tidyr unnest
#' @importFrom utils askYesNo
#' @importFrom utils type.convert
#' @importFrom utils URLencode
## usethis namespace: end
NULL


# Notations ---------------------------------------------------------------
# turn off scientific notations
options(scipen = 999)

# Documentation for common args ----------------------------------------------------------

## building ----------------------------------------------------------------
#' building
#' @name building
#' @param building Character vector or integer corresponding to the building name or id.
NULL

## buildings ---------------------------------------------------------------
#' buildings
#' @name buildings
#' @param buildings Integer, character, or a mix of both. Use integers to search by `building_id`, and strings to search by `building name`. Multiple values are allowed.
NULL


## equipment_ids -----------------------------------------------------------
#' equipment_ids
#' @name equipment_ids
#' @param equipment_ids Integer or vector of integers. One or more equipment IDs.
NULL

## org ---------------------------------------------------------------------
#' org
#' @name org
#' @param org Organization ID (numeric) or name (character) 
NULL

## point_ids ---------------------------------------------------------------
#' point_ids
#' @name point_ids
#' @param point_ids Integer or vector of integers. One or more point IDs.
NULL

## proceed -----------------------------------------------------------------
#' proceed
#' @name proceed
#' @param proceed (Optional) Logical argument indicating whether to proceed operation without asking for explicit user input. Useful for scripting
NULL

## verbose -----------------------------------------------------------------
#' verbose
#' @name  verbose
#' @param verbose Logical; if `TRUE`, prints verbose output including headers and body content. Defaults to `TRUE`.
NULL

