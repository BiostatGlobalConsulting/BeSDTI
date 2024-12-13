#' Get the specific string with the user required language
#'
#' @param language_use The global that stored the language the user choose
#' @param str The string to be retrieved from the language file
#' @param replaceq Option for rendering quotation marks
#'
#' @return A string
#'
#' @export
#'
#' @import openxlsx
#' @import stringr
#'
#' @examples language_string("ENGLISH", str = "OS_304")

# language_string R version 1.01 - Biostat Global Consulting - 024-09-03
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2023-08-12  1.00      Mia Yu          Original R package version
# 2024-09-03  1.01      Caitlin Clary   Modified for BeSD-TI: use language file
#                                       defined in besd_multi_lingual_strings
# *******************************************************************************

language_string <- function(language_use, str, replaceq = FALSE) {

  dat <- language_file

  names(dat) <- str_to_upper(names(dat))
  var <- get(language_use,dat)
  returnstr <- var[which(dat$STRING_NAME == str)]

  if (replaceq == TRUE){
    returnstr <- gsub("'", "\\\\'", returnstr)
  }

  return(returnstr)
}
