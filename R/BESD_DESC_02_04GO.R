#' Generate output databases for BESD_DESC_02
#'
#' @param VCP Current program name to be logged, default to be the function name
#' @param database_id Custom tag for output file names (optional)
#'
#' @return Databases in OUTPUT_FOLDER


# BESD_DESC_02_04GO R version 1.00 - Biostat Global Consulting - 2024-08-08
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2024-08-08  1.00      Caitlin Clary   Original R version adapted from v1.00 of
#                                       DESC_02_04GO in vcqiR
# 2024-10-23  1.01      Caitlin Clary   Use variable name as label if the
#                                       variable itself is unlabeled
# *******************************************************************************


BESD_DESC_02_04GO <- function(VCP = "BESD_DESC_02_04GO", database_id = NA){
  besd_log_comment(VCP, 5, "Flow", "Starting")

  vid <- 1
  for (d in seq_along(DESC_02_VARIABLES)){
    rm(list = c(paste0("DESC_02_labels_", vid)), envir = .GlobalEnv) %>% suppressWarnings()

    # dat <- besd_read(
    #   paste0(OUTPUT_FOLDER, "/DESC_02_", ANALYSIS_COUNTER, "_", DESC_02_COUNTER, ".rds"))

    if (is.na(database_id)){
      dat <- besd_read(
        paste0(OUTPUT_FOLDER, "/DESC_02_", ANALYSIS_COUNTER, "_", DESC_02_COUNTER, ".rds"))
    } else {
      dat <- besd_read(
        paste0(OUTPUT_FOLDER, "/DESC_02_", ANALYSIS_COUNTER, "_", database_id, ".rds"))
    }

    tempvar <- get(DESC_02_VARIABLES[d], dat)
    varlabel <- attr(tempvar, "label", exact = TRUE)

    # Use the variable name for the table title if the variable is unlabeled
    if (is.null(varlabel)){
      varlabel <- DESC_02_VARIABLES[d]
    }

    make_DESC_0203_output_database(
      variable = DESC_02_VARIABLES[d],
      label = varlabel,
      vid = vid,
      measureid = "DESC_02",
      database_id = database_id)

    vid <- vid + 1
  }

  besd_log_comment(VCP, 5, "Flow", "Exiting")

}
