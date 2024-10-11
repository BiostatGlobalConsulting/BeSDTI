#' Detect file type and read dataset
#'
#' @param file File to read
#'
#' @return A data frame or tibble
#'
#' @import haven
#' @import readr
#' @rawNamespace import(tools, except = makevars_user)

# besd_read R version 1.00 - Biostat Global Consulting - 2024-07-19
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2024-07-19  1.00      Caitlin Clary   Original version (modified from VCQI v1.01)
# *******************************************************************************

besd_read <- function(file){

  infile_ext <- tools::file_ext(file)

  # Read CM dataset OR error if file extension not supported
  if(infile_ext == "dta"){
    indat <- haven::read_dta(file)
  } else if(infile_ext == "csv"){
    indat <- readr::read_csv(file)
  } else if(infile_ext == "rds"){
    indat <- readRDS(file)
  } else {
    indat <- paste0("Error: file extension ", infile_ext, " is not supported. Supported file extensions are: rds, csv, dta")
  }

  if (is.data.frame(indat)){
    new_varnames <- stringr::str_replace_all(names(indat), "\\.", "_")
    names(indat) <- new_varnames
  }

  # Return:
  indat
}
