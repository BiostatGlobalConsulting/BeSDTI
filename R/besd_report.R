#' Make template BeSD survey report
#'
#' @param VCP Current program name to be logged, default to be the function name
#' @param filetype Report output format
#' @param analysis Type of BeSD-TI analysis ("child", "covid", etc.)
#'
#' @import dplyr
#' @import knitr
#' @import rmarkdown
#'
#' @return Template BeSD survey report

besd_report <- function(
    VCP = "besd_report",
    filetype = "docx",
    analysis
){

  inputs_working <- BESDTI_REPORT_INPUTS %>%
    mutate(
      rt_input_path = stringr::str_remove(database_path, "_database.rds"),
      rt_input_path = paste0(
        OUTPUT_FOLDER, "/Report Inputs/", rt_input_path,
        "_rt.rds"
      )
    )

  # saveRDS(inputs_working, paste0(OUTPUT_FOLDER, "/Report Inputs/Temp_Report_Inputs.rds"))

  # Include templates in package:
  # https://spencerschien.info/post/r_for_nonprofits/quarto_template/

  if (analysis == "child"){

  }

}
