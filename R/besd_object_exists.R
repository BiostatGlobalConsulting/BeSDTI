#' Check if an object is defined, not NULL, not NA, and not an empty vector
#'
#' @param object Name of object to be tested
#'
#' @return TRUE or FALSE
#'
#' @import stringr
#'
#' @export
#'
#' @examples
#' besd_object_exists("test_value")

# besd_object_exists R version 1.00 - Biostat Global Consulting - 2024-07-19
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2024-07-19  1.00      Caitlin Clary   Original version (modified from VCQI v1.02)
# *******************************************************************************

# Function checks if an object is in the global enviroment and
# not NULL and not NA and not an empty string

besd_object_exists <- function(object){

  # Set starting value, switch to FALSE if any checks fail
  existsval <- TRUE

  # Check 1: object in global environment
  if(exists(object, envir = globalenv()) == FALSE){
    existsval <- FALSE
  } else if(exists(object, envir = globalenv()) == TRUE){
    obj <- get(object)

    # Check 2: object null
    if(is.null(obj)){
      existsval <- FALSE

      # Check 3: object NA
    } else if(all(is.na(obj))){
      existsval <- FALSE

      # Checks for atomic vectors only:
    } else if(is.atomic(obj)){

      # Check 4: empty string
      if(all(obj == "")){
        existsval <- FALSE

        # Check 5: string of only spaces
      } else if(all(str_squish(obj) == "")){
        existsval <- FALSE
      }
    } # end checks when object is a vector

  } # end if object exists

  existsval
}
