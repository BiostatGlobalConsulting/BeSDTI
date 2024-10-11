#' Put the current value of a global into the BeSD-TI log
#'
#' @param global A global value to be logged
#'
#' @import stringr
#'
#' @return Call to besd_log_comment
#'
#' @export
#'
#' @examples
#' test_value <- 1
#' besd_log_global(test_value)

# besd_log_global R version 1.00 - Biostat Global Consulting - 2024-07-19
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2024-07-19  1.00      Caitlin Clary   Original version (modified from VCQI v1.04)
# *******************************************************************************

# NOTE Stata version includes process to split into multiple comments; not implemented here

besd_log_global <- function(global){

  # Global value
  gv <- try(get(deparse(substitute(global))), silent = TRUE)
  if(inherits(gv, "try-error")){gv <- try(get(global), silent = TRUE)}

  # Global name
  gn <- try(deparse(substitute(global)), silent = TRUE)
  if(str_detect(gn, "\"") == TRUE){
    gn <- global
  }

  # Handling list and c() values
  if (is.list(gv)) {
    gv <- lapply(seq_along(gv), function(x) paste(
      names(gv)[[x]],
      " = ",
      paste(gv[[x]], collapse = ""))
      )
    gv <- paste(gv, collapse = ", ")
  } else if (length(gv) > 1){
    gv <- paste(gv, collapse = ", ")
  }
  # Handling non-existent values
  if(length(gv) == 0){
    gv <- "null/not specified"
  }

  if(exists(gn) == FALSE){
    besd_log_comment(VCP, 3, "Global", paste0("Global value ", gn, " is not defined at this time."))
  } else {
    besd_log_comment(VCP, 3, "Global", paste0("Global value ", gn, " is ", gv))
  }
}
