#' Define and log a value in the global environment
#'
#' @param global The name of the global value
#' @param value The value to be assigned
#'
#' @return An R object in the global environment
#'
#' @export
#'
#' @examples
#' besd_global(x, 100)
#' besd_global(y, c("a", "b", "c"))
#'
# besd_global R version 1.00 - Biostat Global Consulting - 2024-07-19
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2024-07-19  1.00      Caitlin Clary   Original version (modified from VCQI v1.04) *******************************************************************************

# If the user calls besd_global(GLOBAL_NAME, value) then this program will
# a) assign GLOBAL_NAME <- value and then
# b) put the current value of GLOBAL_NAME into the besd log

besd_global <- function(global, value){

  assign(deparse(substitute(global)), value, envir = .GlobalEnv)
  do.call(besd_log_global, list(substitute(global)))

}
