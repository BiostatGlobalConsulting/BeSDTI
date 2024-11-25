#' Copy template files to a target folder
#'
#' @param folder Folder where file should be copied
#' @param overwrite Overwrite existing destination file?
#'
#' @import stringr
#' @import cli
#'
#' @return File copied to target folder
#'
#' @export

# besd_copy_multilingual R version 1.00 - Biostat Global Consulting - 2024-11-22
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2024-11-22  1.00      Caitlin Clary   Original version
# *******************************************************************************

besd_copy_multilingual <- function(
    folder,
    overwrite = TRUE
){

  outfolder <- folder
  finalchar <- stringr::str_sub(
    string = outfolder,
    start = stringr::str_length(outfolder), end = stringr::str_length(outfolder)
  )
  if (finalchar != "/"){outfolder <- paste0(outfolder, "/")}

  # Copy template file from internals
  out <- file.copy(
    from = system.file(
      "extdata/_extensions/Multi-Lingual Phrases - En Fr Es Pt - with BeSD-TI.xlsx",
      package = "BeSDTI"),
    to = paste0(outfolder, "BeSD-TI Multi-Lingual Phrases - En Fr Es Pt.xlsx"),
    overwrite = overwrite,
    # recursive = TRUE,
    copy.mode = TRUE
  )

  if (out == TRUE){
    cli::cli_inform(
      paste0("Multilingual strings file copied successfully to ", outfolder))
    } else {
    cli::cli_warn(
      paste0("Multilingual strings file was not copied successfully. Please check file path and the overwrite option and try again."))
  }
}
