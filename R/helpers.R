
# UNIX to POSIXct ---------------------------------------------------------

#' Convert UNIX timestamp to POSIXct
#' 
#' Converts a UNIX timestamp string (with or without milliseconds) to a POSIXct datetime object.
#' 
#' @param x A character or numeric vector of UNIX timestamps.
#' 
#' @return A POSIXct vector.
convert_to_datetime <- function(x) {
  x %>%
    mutate(across(
      any_of(
        c("created","modified","last_discovery","last_published",
          "last_updated","first_updated","last_heartbeat","password_reset","last_login")),
      ~ {
        suppressWarnings(
          dplyr::case_when(
          # Detect epoch timestamps
          grepl("^\\d{10,16}", .) ~ 
            as_datetime(as.numeric(.) / ifelse(nchar(.) > 10, 1000, 1), 
                        tz = Sys.timezone()),
          
          is.na(.) | . == "" ~ NA,
          
          # Otherwise try parsing standard datetime strings
          TRUE ~ lubridate::ymd_hms(., quiet = TRUE, tz = Sys.timezone())))
      }
      ))
  
}

# Nested List to DF -------------------------------------------------------

#' Nested List to DF
#' 
#' Vectorized approach to convert a deeply nested into a dataframe. This function is customized only to work with time-series output. Used in `get_timeseries_raw`
#' 
#' @param nested_list time-series output return from /timeseries API endpoint
#' 
#' @return Clean dataframe

nested_list_to_df <- function(nested_list) {

  # Use map to iterate over each list element and process
  df_list <- purrr::map(nested_list, function(item) {
    # Extract data
    point_id <- item$point_id
    topic <- item$topic
    display <- item$display
    columns <- item$columns
    values <- item$values
    #Check if attribute "clean" exists in columns list and remove it from columns list and values list as well. This is present in older time-series data
    if("clean" %in% columns){ 
    #Get index of "clean" in columns
    clean_index <- which(columns=="clean")
    
    columns <- columns[-clean_index]
    
    values <- lapply(values,function(x) x[-clean_index])
    }
    
    #If "raw" attribute exists in column, convert 3rd name in columns to "unit" else convert 2nd name in columns to "unit"

    if("raw" %in% columns){
    columns[[3]] <- "unit"
    } else{
      columns[[2]] <- "unit"
    }
    
    # Convert the values list into a data frame
    values_df <- as.data.frame(do.call(rbind, values))
    
    # Set the column names from the 'columns' field
    colnames(values_df) <- columns
    
    # Add point_id, topic, and display as new columns
    values_df <- values_df %>%
      mutate(point_id = point_id, 
             topic = topic, 
             display = display)
    
    return(values_df)
  })
  
  # Combine all data frames in the list using list_rbind from purrr
  df_combined <- purrr::list_rbind(df_list)

  return(df_combined)
}

# Shared/Generic Helpers ----------------------------------------------------
#
# Small internal utilities used across multiple R/ files (not tied to any
# one endpoint or data model). Kept here rather than duplicated per-file.

#' Resolve a Single Building
#'
#' Internal helper shared by `update_staging_points()`, `update_staging_equip()`,
#' `publish()`, and `unpublish()` (all in staging.R). Validates that exactly
#' one building was provided and resolves it via `search_buildings()`.
#'
#' @inheritParams building
#' @inheritParams verbose
#'
#' @return A one-row data.frame of building info (from `search_buildings()`).
#' @noRd
.resolve_single_building <- function(building, verbose = TRUE) {
  if (length(building) > 1)
    stop("Only one building ID or name is allowed.")

  search_buildings(buildings = building, verbose = verbose)
}

#' Confirm an Operation or Stop
#'
#' Internal helper shared by `update_staging_points()`, `update_staging_equip()`,
#' `publish()`, and `unpublish()` (all in staging.R). Prompts for confirmation
#' via `askYesNo()` when `proceed` is `NULL`, and stops if the operation isn't
#' confirmed.
#'
#' @inheritParams proceed
#' @param message Character. Confirmation prompt shown to the user.
#' @noRd
.confirm_or_stop <- function(proceed, message) {
  if (is.null(proceed)) {
    proceed <- askYesNo(message)
  }

  if (is.na(proceed) || !proceed) {
    stop("Operation canceled by user.")
  }
}

#' Require Columns to be Present
#'
#' Internal helper shared by `update_staging_points()` and `update_staging_equip()`
#' (staging.R). Stops with a descriptive error if any required column is missing.
#'
#' @param df_cols Character vector of column names present in the input data.frame.
#' @param required_cols Character vector of column names that must be present.
#' @param df_label Character. Name used in the error message (e.g. `"staging_points"`).
#' @noRd
.require_cols <- function(df_cols, required_cols, df_label) {
  if (!all(required_cols %in% df_cols)) {
    stop(sprintf(
      "%s is missing cols %s",
      df_label,
      paste(required_cols, collapse = " or ")
    ))
  }
}

#' Default a Confidence Column to 100
#'
#' Internal helper shared by `update_staging_points()` (used for both
#' `point_type_confidence` and `raw_unit_confidence`) and `update_staging_equip()`
#' (staging.R). Adds `confidence_col` set to 100 if it isn't already present.
#'
#' @param df A data.frame.
#' @param confidence_col Character. Name of the confidence column to default.
#' @noRd
.default_confidence_col <- function(df, confidence_col) {
  if (!confidence_col %in% names(df)) {
    df[[confidence_col]] <- 100
  }
  df
}

#' Strip a Prefix from Matching List Elements
#'
#' Internal helper shared by `update_staging_points()` (used for both
#' `point_type_` and `raw_unit_`) and `update_staging_equip()` (`equipment_type_`),
#' both in staging.R. Pulls out the elements of `row_list` whose names start
#' with `prefix`, and strips the prefix from their names.
#'
#' @param row_list A named list (one row of a data.frame, via `as.list()`).
#' @param prefix Character. Name prefix to match and strip, e.g. `"point_type_"`.
#'
#' @return A named list of the matching elements, with `prefix` removed from their names.
#' @noRd
.extract_prefixed_fields <- function(row_list, prefix) {
  fields <- row_list[grepl(paste0("^", prefix), names(row_list))]
  names(fields) <- sub(prefix, "", names(fields))
  fields
}

#' Convert "NULL" Strings to NA
#'
#' Internal helper shared by `update_staging_points()` and `update_staging_equip()`
#' (staging.R). Some staging endpoints round-trip missing values as the
#' literal string `"NULL"` rather than `NA`; this normalizes them back to `NA`.
#'
#' @param df A data.frame.
#' @noRd
.nullify_null_strings <- function(df) {
  df %>% mutate(across(everything(.), ~ ifelse(. == "NULL", NA, .)))
}

#' Wrap a Scalar in a List
#'
#' Internal helper shared across the package wherever a length-1 vector needs
#' to be wrapped in `list()` so `jsonlite`/`httr2` don't unbox it into a bare
#' scalar (the API expects arrays). Used by `publish()`/`unpublish()`
#' (staging.R), `get_equipment_by_ids()` (buildings.R), and
#' `get_timeseries_raw()` (timeseries.R).
#'
#' @param x A vector.
#'
#' @return `x` unchanged if `length(x) != 1`, otherwise `list(x)`.
#' @noRd
.as_list_if_scalar <- function(x) {
  if (length(x) == 1) list(x) else x
}

#' Filter a Data Frame by Organization
#'
#' Internal helper shared by `search_buildings()` (buildings.R), `get_users()`,
#' and `get_deployments()` (both misc.R). When `orgs` is provided, resolves it
#' via `search_orgs()` and filters `df` to matching `org_id`s; otherwise
#' returns `df` unchanged.
#'
#' @param df A data.frame containing an `org_id` column.
#' @inheritParams orgs
#' @inheritParams verbose
#'
#' @return `df`, filtered to the resolved orgs if `orgs` was provided.
#' @noRd
.filter_by_orgs <- function(df, orgs, verbose = TRUE) {
  if (is.null(orgs)) {
    return(df)
  }
  orgs <- search_orgs(orgs = orgs, verbose = verbose)
  df %>% filter(org_id %in% orgs$id)
}