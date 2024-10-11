#' Generate output databases for DESC_03
#'
#' @param VCP Current program name to be logged, default to be the function name
#' @param database_id Tag for file names
#'
#' @return Databases in OUTPUT_FOLDER


# BESD_DESC_03_04GO R version 1.00 - Biostat Global Consulting - 2024-09-09
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2024-09-09  1.00      Caitlin Clary   Original version (modified from VCQI v1.00)
# *******************************************************************************

BESD_DESC_03_04GO <- function(VCP = "BESD_DESC_03_04GO",
                              database_id = NA){
  besd_log_comment(VCP, 5, "Flow", "Starting")

  rm(list = c(paste0("DESC_03_labels_", DESC_03_COUNTER)), envir = .GlobalEnv) %>%
    suppressWarnings()

  make_DESC_0203_output_database(
    variable = paste0("desc03_", ANALYSIS_COUNTER, "_", DESC_03_COUNTER),
    label = DESC_03_TO_TITLE,
    vid = DESC_03_COUNTER,
    measureid = "DESC_03",
    database_id = database_id)

  besd_log_comment(VCP, 5, "Flow", "Exiting")

}
