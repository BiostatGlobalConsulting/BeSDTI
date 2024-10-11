#' Check if a string represents a valid R color
#'
#' @param color String to check

# besd_check_color R version 1.00 - Biostat Global Consulting - 2024-08-20
# ******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2024-08-20  1.00      Caitlin Clary   Original R version
# ******************************************************************************

# Logic via: https://stackoverflow.com/questions/13289009/check-if-character-string-is-a-valid-color-representation

besd_check_color <- function(color){
  # res <- try(col2rgb(color), silent = TRUE)
  # return(!"try-error" %in% class(res))

  sapply(color, function(X) {
    tryCatch(is.matrix(col2rgb(X)),
             error = function(e) FALSE)
  })
}

