#' Response to multiple-choice question where the respondent may select more than one response option.
#'
#' @param VCP Current program name to be logged, default to be the function name
#' @param database_id Custom tag for output file names (optional)
#' @param cleanup Clean up DESC_03 global settings after run
#'
#' @return Derived variables, databases and tables
#' @export
#'
#' @examples
#' BESD_DESC_03()

# BESD_DESC_03 R version 1.00 - Biostat Global Consulting - 2024-09-09
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2024-09-09  1.00      Caitlin Clary   Original R version adapted from v1.00 of
#                                       DESC_03 in vcqiR
# *******************************************************************************

BESD_DESC_03 <- function(VCP = "BESD_DESC_03",
                         database_id = NA,
                         cleanup = FALSE){
  besd_log_comment(VCP, 5, "Flow", "Starting")

  print(paste0("Calculating ", VCP, " ..."))

  print("Checking global macros")
  BESD_DESC_03_00GC()

  if(PREPROCESS_DATA %in% 1){
    print("Pre-processing dataset")
    BESD_DESC_03_01PP(database_id = database_id)
  }

  if(GENERATE_DVS %in% 1){
    print("Calculating derived variables")
    BESD_DESC_03_03DV(database_id = database_id)
  }

  if(GENERATE_DATABASES %in% 1){
    print("Generating output databases")
    BESD_DESC_03_04GO(database_id = database_id)
  }

  if(EXPORT_TO_EXCEL %in% 1){
    print("Exporting to Excel")
    BESD_DESC_03_05TOST(database_id = database_id)
  }

  # * Clear out globals if specified
  if (cleanup == TRUE){
    rm(list = c(
      "DESC_03_DATASET", "DESC_03_VARIABLES", "DESC_03_WEIGHTED",
      "DESC_03_DENOMINATOR", "DESC_03_TO_TITLE", "DESC_03_TO_SUBTITLE",
      "DESC_03_SHORT_TITLE", "DESC_03_SELECTED_VALUE", "DESC_03_N_LABEL",
      "DESC_03_NTWD_LABEL"),
      envir = .GlobalEnv) %>% suppressWarnings()

    if (besd_object_exists("DESC_03_N_RELABEL_LEVELS")){
      for (i in 1:DESC_03_N_RELABEL_LEVELS){
        rm(list = c(
          paste0("DESC_03_RELABEL_LEVEL_", i),
          paste0("DESC_03_RELABEL_LABEL_", i)),
          envir = .GlobalEnv) %>% suppressWarnings()
      }
    }

    if (besd_object_exists("DESC_03_N_SUBTOTALS")){
      for (i in 1:DESC_03_N_SUBTOTALS){
        rm(list = c(
          paste0("DESC_03_SUBTOTAL_LEVELS_", i),
          paste0("DESC_03_SUBTOTAL_LABEL_", i),
          paste0("DESC_03_SUBTOTAL_LIST_", i)),
          envir = .GlobalEnv) %>% suppressWarnings()
      }
    }

    for (i in 1:100){
      rm(list = c(paste0("DESC_03_TO_FOOTNOTE_", i)),envir = .GlobalEnv) %>% suppressWarnings()
    }

    rm(list = c("DESC_03_N_SUBTOTALS","DESC_03_N_RELABEL_LEVELS","DESC_03_SHOW_SUBTOTALS_ONLY",
                "DESC_03_LIST_N_BEFORE_PCT","DESC_03_LIST_NWTD_BEFORE_PCT", "DESC_03_SUPPRESS_CI_OUTPUT"),
       envir = .GlobalEnv) %>% suppressWarnings()

  }

  besd_log_comment(VCP, 5, "Flow", "Exiting")
}
