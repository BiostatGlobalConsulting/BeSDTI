#' Generate output databases for DESC_03
#'
#' @param VCP Current program name to be logged, default to be the function name
#' @param database_id Custom tag for output file names (optional)
#'
#' @return Databases in OUTPUT_FOLDER


# BESD_DESC_03_04GO R version 1.00 - Biostat Global Consulting - 2024-09-09
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2024-09-09  1.00      Caitlin Clary   Original version (modified from VCQI v1.00)
# 2024-10-23  1.01      Caitlin Clary   Use variable list as label if TO_TITLE
#                                       isn't specified
# *******************************************************************************

BESD_DESC_03_04GO <- function(VCP = "BESD_DESC_03_04GO",
                              database_id = NA){

  besd_log_comment(VCP, 5, "Flow", "Starting")

  if (!besd_object_exists("DESC_03_TO_TITLE")){
    desc03_temptitle <- paste(DESC_03_VARIABLES, collapse = " ")
  } else {
    desc03_temptitle <- DESC_03_TO_TITLE
  }

  rm(list = c(paste0("DESC_03_labels_", DESC_03_COUNTER)), envir = .GlobalEnv) %>%
    suppressWarnings()

  make_DESC_0203_output_database(
    variable = paste0("desc03_", ANALYSIS_COUNTER, "_", DESC_03_COUNTER),
    label = desc03_temptitle,
    vid = DESC_03_COUNTER,
    measureid = "DESC_03",
    database_id = database_id)

  besd_log_comment(VCP, 5, "Flow", "Exiting")

}
