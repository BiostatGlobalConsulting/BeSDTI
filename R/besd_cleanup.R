#' Exit BeSD-TI gracefully
#'
#' @return Call to besd_halt_immediately
#'
#' @export
#'
#' @examples
#' besd_cleanup()

# besd_cleanup R version 1.00 - Biostat Global Consulting - 2024-09-10
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2024-09-10  1.00      Caitlin Clary   Original version (modified from VCQI v1.00)
# *******************************************************************************

besd_cleanup <- function(){

  # If the user was doing a check run, then unset this flag so VCQI will exit
  # gracefully
  besd_global(CHECK_INSTEAD_OF_RUN, 0)

  # Close the datasets that hold the results of hypothesis tests and put them
  # into the output spreadsheet; close the log file and put it into the output
  # spreadsheet; clean up extra files; send a message to the screen if there are
  # warnings or errors in the log
  besd_halt_immediately()
}
