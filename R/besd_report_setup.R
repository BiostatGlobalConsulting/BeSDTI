#' Copy report template to output folder
#'
#' @param template_name Name of template to copy
#' @param outfile_name Name for copy of template
#'
#' @return Template files copied to output folder
#'
#' @export
#'

# besd_report_setup R version 1.00 - Biostat Global Consulting - 2024-10-11
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2024-10-11  1.00      Caitlin Clary   Original version
# *******************************************************************************


besd_report_setup <- function(
    template_name = NULL,
    outfile_name = NULL) {

  # Create folder for recursive copying into ahead of time
  if (!file.exists(paste0(OUTPUT_FOLDER, "/Report Template"))){
    dir.create(paste0(OUTPUT_FOLDER, "/Report Template"))
  }

  # Copy template file from internals
  file.copy(
    from = system.file(paste0("extdata/_extensions/", template_name),
                       package = "BeSDTI"),
    to = paste0(OUTPUT_FOLDER, "/Report Template/", outfile_name),
    overwrite = TRUE,
    recursive = TRUE,
    copy.mode = TRUE
  ) %>% suppressWarnings()

}
