#' Make template BeSD survey report
#'
#' @param VCP Current program name to be logged, default to be the function name
#' @param filetype Report output format
#' @param analysis Type of BeSD-TI analysis ("child", "covid", etc.)
#'
#' @import dplyr
#' @import knitr
#' @import rmarkdown
#' @import stringr
#'
#' @export
#' @return Template BeSD survey report
#'
#' @examples besd_report(analysis = "child")

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

  # Include templates in package:
  # https://spencerschien.info/post/r_for_nonprofits/quarto_template/

  if (analysis == "child"){

    report_filename <- paste0(
      "BeSD_Template_Report_Child_Vaccination_", ANALYSIS_COUNTER, "_",
      stringr::str_replace_all(Sys.Date(), "-", "_"), ".Rmd")

    # Copy template .Rmd files
    besd_report_setup(template_name = "besd_report_subsection_plot.Rmd",
                      outfile_name = "besd_report_subsection_plot.Rmd")

    besd_report_setup(template_name = "besd_report_subsection_table.Rmd",
                      outfile_name = "besd_report_subsection_table.Rmd")

    besd_report_setup(
      template_name = "besd_child_vx_report_template.Rmd",
      outfile_name = report_filename)

  }

  setwd(OUTPUT_FOLDER)
  rmarkdown::render(
    paste0("Report_Template/", report_filename))
  setwd(DATA_FOLDER)

}
