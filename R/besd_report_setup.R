#' Copy report template to output folder
#'
#' @param template_path Path to template to copy
#' @param outfile_path Path where copy of template should be saved (usually in OUTPUT_FOLDER)
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
    template_path = NULL,
    outfile_path = NULL) {

  browser()

  # Create folder for recursive copying into ahead of time
  if (!file.exists(paste0(OUTPUT_FOLDER, "/Report Template"))){
    dir.create(paste0(OUTPUT_FOLDER, "/Report Template"))
  }

  # Copy template file from internals
  file.copy(
    from = system.file(paste0("extdata/_extensions/", template_path),
                       package = "BeSDTI"),
    to = paste0(OUTPUT_FOLDER, "/Report Template/", outfile_path),
    overwrite = TRUE,
    recursive = TRUE,
    copy.mode = TRUE
  )


  # if (is.null(file_name)) {
  #   stop("You must provide a valid file_name")
  # }
  #
  # out_dir <- file_name
  #
  # if(!dir.exists(out_dir)) {
  #   dir.create(out_dir)
  # }
  #
  # # check for available extensions
  # stopifnot("Extension not in package" = ext_name %in% c("quartotemplate"))
  #
  # # check for existing _extensions directory
  # if(!file.exists("_extensions")) dir.create("_extensions")
  # message("Created '_extensions' folder")
  #
  # # various reading of key-value pairs for reporting
  # ext_yml <- readLines(system.file("extdata/_extensions/quartotemplate/_extension.yml",
  #                                  package = "quartotemplate"))
  #
  # ext_ver <- gsub(
  #   x = ext_yml[grepl(x = ext_yml, pattern = "version:")],
  #   pattern = "version: ",
  #   replacement = ""
  # )
  #
  # ext_nm <- gsub(
  #   x = ext_yml[grepl(x = ext_yml, pattern = "title:")],
  #   pattern = "title: ",
  #   replacement = ""
  # )
  #

  #

  #
  # # logic check to make sure extension files were moved
  # n_files <- length(dir(paste0("_extensions/", ext_name)))
  #
  # if(n_files >= 2){
  #   message(paste(ext_nm, "v", ext_ver, "was installed to _extensions folder in current working directory."))
  # } else {
  #   message("Extension appears not to have been created")
  # }
  #
  # # create new qmd report based on skeleton
  # readLines("_extensions/quartotemplate/skeleton.qmd") |>
  #   writeLines(text = _,
  #              con = paste0(out_dir, "/", file_name, ".qmd", collapse = ""))
  #
  # # open the new file in the editor
  # file.edit(paste0(out_dir, "/", file_name, ".qmd", collapse = ""))
  #
  # # copy header.tex over as well
  # readLines("_extensions/quartotemplate/header.tex") |>
  #   writeLines(text = _, con = paste0(out_dir, "/header.tex"))

}
