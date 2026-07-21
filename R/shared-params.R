# Documentation for common args ----------------------------------------------------------
#
# Shared @param placeholders reused across the package via @inheritParams.
# Kept separate from OnboardClient-package.R, which is reserved for
# package-level infrastructure (the _PACKAGE sentinel, @import/@importFrom
# declarations, and globalVariables()).

## building ----------------------------------------------------------------
#' building
#' @name building
#' @param building Character or integer corresponding to a single building name or id. Only one value is allowed (functions using this parameter operate on a single building).
NULL

## building_ids ----------------------------------------------------------------
#' building_ids
#' @name building_ids
#' @param building_ids Integer or vector of integers corresponding to building IDs. Unlike `buildings`, these are used directly (e.g. in API endpoint paths) and are not resolved by name.
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

## equipment_types ---------------------------------------------------------
#' equipment_types
#' @name equipment_types
#' @param equipment_types Character vector. One or more equipment types (e.g. `"HVAC/AHU"`).
NULL

## orgs ---------------------------------------------------------------------
#' orgs
#' @name orgs
#' @param orgs (Optional) Integer, character, or a mix of both. Use integers to search by `org_id`, and strings to search by org name, short name, or abbreviation. Multiple values are allowed.
NULL

## point_ids ---------------------------------------------------------------
#' point_ids
#' @name point_ids
#' @param point_ids Integer or vector of integers. One or more point IDs.
NULL

## point_names -------------------------------------------------------------
#' point_names
#' @name point_names
#' @param point_names Character vector. Use exact text of the point name
NULL

## point_topics ------------------------------------------------------------
#' point_topics
#' @name point_topics
#' @param point_topics Character vector. One or more point topics
NULL

## point_types -------------------------------------------------------------
#' point_types
#' @name point_types
#' @param point_types Character vector. One or more point_types
NULL

## proceed -----------------------------------------------------------------
#' proceed
#' @name proceed
#' @param proceed (Optional) Logical argument indicating whether to proceed operation without asking for explicit user input. Useful for scripting
NULL

## updated_since -----------------------------------------------------------
#' updated_since
#' @name updated_since
#' @param updated_since A timestamp to query points updated after a certain time
NULL

## verbose -----------------------------------------------------------------
#' verbose
#' @name  verbose
#' @param verbose Logical; if `TRUE`, prints progress messages (and for `api.request()` specifically, full HTTP request/response details). Defaults to `TRUE`.
NULL
