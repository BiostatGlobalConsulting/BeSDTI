#' Reads in the COVID-19 BeSD survey and creates the derived variables
#'
#' @param VCP Current program name to be logged, default to be the function name
#'
#' @rawNamespace import(rlang, except = c(local_options,with_options))
#' @import dplyr
#' @import stringr
#' @import tidyselect
#' @import haven
#' @rawNamespace import(tools, except = makevars_user)
#'
#' @return a dataset
#'
#' @export
#'
#' @examples
#' gen_cov_dv()

# gen_cov_dv R version 1.00 - Biostat Global Consulting - 2024-11-12
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2024-11-12  1.00      Caitlin Clary   Original R package version
# *******************************************************************************

gen_cov_dv <- function(VCP = "gen_cov_dv"){

  besd_log_comment(VCP, 5, "Flow", "Starting")

  dat <- besd_read(paste0(OUTPUT_FOLDER, "/COV_with_ids.rds"))

  # Indicator: intention to get vaccinated
  # [COUNTRY NAME] has a schedule of vaccines for children. Do you want your
  # child to get none of these vaccines, some of these vaccines, or all of
  # these vaccines?
  dat <- dat %>%
    mutate(
      COV_intent_indicator = case_when(
        COV_intent %in% c(1, 2) ~ 0,
        COV_intent %in% c(3, 4) ~ 1,
        TRUE ~ NA
      ),
      COV_intent_indicator = haven::labelled(
        COV_intent_indicator,
        label = "Indicator: want to receive COVID-19 vaccine",
        labels = c(
          "No or not sure" = 0,
          "Yes or already vaccinated" = 1
        )
      )
    )

  # Indicator: confidence in vaccine benefits
  dat <- dat %>%
    mutate(
      COV_confb_indicator = case_when(
        COV_confb %in% c(1, 2) ~ 0,
        COV_confb %in% c(3, 4) ~ 1,
        TRUE ~ NA
      ),
      COV_confb_indicator = haven::labelled(
        COV_confb_indicator,
        label = "Indicator: importance of COVID-19 vaccine for your health",
        labels = c(
          "Not at all or a little important" = 0,
          "Moderately or very important" = 1
        )
      )
    )

  # Indicator: family norms
  dat <- dat %>%
    mutate(
      COV_normf_indicator = case_when(
        COV_normf %in% 0 ~ 0,
        COV_normf %in% 1 ~ 1,
        TRUE ~ NA
      ),
      COV_normf_indicator = haven::labelled(
        COV_normf_indicator,
        label = "Indicator: family/friends support COVID-19 vaccination",
        labels = c(
          "No" = 0,
          "Yes" = 1
        )
      )
    )

  # Indicator: know where to get vaccination
  dat <- dat %>%
    mutate(
      COV_where_indicator = case_when(
        COV_where %in% 0 ~ 0,
        COV_where %in% 1 ~ 1,
        TRUE ~ NA
      ),
      COV_where_indicator = haven::labelled(
        COV_where_indicator,
        label = "Indicator: know where to go for COVID-19 vaccination",
        labels = c(
          "No" = 0,
          "Yes" = 1
        )
      )
    )

  # Indicator: affordability
  dat <- dat %>%
    mutate(
      COV_afford_indicator = case_when(
        COV_afford %in% c(1, 2) ~ 0,
        COV_afford %in% c(3, 4) ~ 1,
        TRUE ~ NA
      ),
      COV_afford_indicator = haven::labelled(
        COV_afford_indicator,
        label = "Indicator: how easy to pay for COVID-19 vaccination",
        labels = c(
          "Not at all or a little easy" = 0,
          "Moderately or very easy" = 1
        )
      )
    )

  saveRDS(
    dat, file = paste0(OUTPUT_FOLDER, "/COV_dv.rds"))

}
