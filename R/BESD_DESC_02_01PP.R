#' Pre-process dataset for BESD_DESC_02
#'
#' @param VCP Current program name to be logged, default to be the function name
#' @param database_id Custom tag for output file names (optional)
#'
#' @return A dataset (BESD_DESC_02_<ANALYSIS_COUNTER>_<BESD_DESC_02_COUNTER>)
#'
#' @import dplyr
#' @import tidyselect

# BESD_DESC_02_01PP R version 1.00 - Biostat Global Consulting - 2024-08-08
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2024-08-08  1.00      Caitlin Clary   Original R version adapted from v1.01 of
#                                       DESC_02_01PP in vcqiR
# *******************************************************************************

BESD_DESC_02_01PP <- function(VCP = "BESD_DESC_02_01PP", database_id = NA){
  besd_log_comment(VCP, 5, "Flow", "Starting")

  dat <- besd_read(paste0(OUTPUT_FOLDER, "/", DESC_02_DATASET))

  # The user may call this measure several times with different combinations of
  # inputs, so track a counter so each dataset gets saved for later examination,
  # if necessary

  if (besd_object_exists("DESC_02_COUNTER")){
    DESC_02_COUNTER = DESC_02_COUNTER + 1
    assign("DESC_02_COUNTER", DESC_02_COUNTER, envir = .GlobalEnv)
  }

  if (!besd_object_exists("DESC_02_COUNTER")){
    assign("DESC_02_COUNTER", 1, envir = .GlobalEnv)
  }

  # TO DO update the variables selected here
  dat <- dat %>%
    select(
      #level1id,level2id,level3id,
      Districtname, Clusternum, clusterid, respid, psweight,
      # HH02, HH04,
      all_of(OUTPUT_VARLIST),
      all_of(DESC_02_VARIABLES),
      all_of(BESDTI_PASS_THRU_VARLIST)
    )

  if (!is.na(database_id)){
    temp_filestub <- paste0("DESC_02_", ANALYSIS_COUNTER,
                            "_", database_id, ".rds")
  } else {
    temp_filestub <- paste0("DESC_02_", ANALYSIS_COUNTER,
                            "_", DESC_02_COUNTER, ".rds")
  }

  saveRDS(dat, file = paste0(OUTPUT_FOLDER, "/", temp_filestub))

  if (!besd_object_exists("DESC_02_TEMP_DATASETS")){
    DESC_02_TEMP_DATASETS <- NULL
  }

  besd_global(DESC_02_TEMP_DATASETS,
              c(DESC_02_TEMP_DATASETS, temp_filestub))

  rm(temp_filestub)

  besd_log_comment(VCP, 5, "Flow", "Exiting")

}
