#' Reads in the child vaccination BeSD survey and creates the derived variables
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
#' gen_ch_dv()

# gen_ch_dv R version 1.00 - Biostat Global Consulting - 2024-07-24
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2024-07-24  1.00      Caitlin Clary   Original R package version
# *******************************************************************************

gen_ch_dv <- function(VCP = "gen_ch_dv"){

  besd_log_comment(VCP, 5, "Flow", "Starting")

  dat <- besd_read(paste0(OUTPUT_FOLDER, "/CH_with_ids.rds"))

  # Indicator: intention to get vaccinated
  # [COUNTRY NAME] has a schedule of vaccines for children. Do you want your
  # child to get none of these vaccines, some of these vaccines, or all of
  # these vaccines?
  dat <- dat %>%
    mutate(
      CHI_intent_indicator = case_when(
        CHI_intent %in% c(1, 2) ~ 0,
        CHI_intent %in% 3 ~ 1,
        TRUE ~ NA
      ),
      CHI_intent_indicator = haven::labelled(
        CHI_intent_indicator,
        label = "Indicator: intent for child to receive vaccines in schedule",
        labels = c(
          "None or some" = 0,
          "All" = 1
        )
      )
    )

  # Indicator: confidence in vaccine benefits
  dat <- dat %>%
    mutate(
      CHI_confb_indicator = case_when(
        CHI_confb %in% c(1, 2) ~ 0,
        CHI_confb %in% c(3, 4) ~ 1,
        TRUE ~ NA
      ),
      CHI_confb_indicator = haven::labelled(
        CHI_confb_indicator,
        label = "Indicator: importance of vaccines for child health",
        labels = c(
          "Not at all or a little important" = 0,
          "Moderately or very important" = 1
        )
      )
    )

  # Indicator: family norms
  dat <- dat %>%
    mutate(
      CHI_normf_indicator = case_when(
        CHI_normf %in% 0 ~ 0,
        CHI_normf %in% 1 ~ 1,
        TRUE ~ NA
      ),
      CHI_normf_indicator = haven::labelled(
        CHI_normf_indicator,
        label = "Indicator: family/friends support vaccination",
        labels = c(
          "No" = 0,
          "Yes" = 1
        )
      )
    )

  # Indicator: know where to get vaccination
  dat <- dat %>%
    mutate(
      CHI_where_indicator = case_when(
        CHI_where %in% 0 ~ 0,
        CHI_where %in% 1 ~ 1,
        TRUE ~ NA
      ),
      CHI_where_indicator = haven::labelled(
        CHI_where_indicator,
        label = "Indicator: know where to go for vaccination",
        labels = c(
          "No" = 0,
          "Yes" = 1
        )
      )
    )

  # Indicator: affordability
  dat <- dat %>%
    mutate(
      CHI_afford_indicator = case_when(
        CHI_afford %in% c(1, 2) ~ 0,
        CHI_afford %in% c(3, 4) ~ 1,
        TRUE ~ NA
      ),
      CHI_afford_indicator = haven::labelled(
        CHI_afford_indicator,
        label = "Indicator: how easy to pay for vaccination",
        labels = c(
          "Not at all or a little easy" = 0,
          "Moderately or very easy" = 1
        )
      )
    )

  saveRDS(
    dat, file = paste0(OUTPUT_FOLDER, "/CH_dv.rds"))

  besd_global(TEMP_DATASETS,
              c(TEMP_DATASETS, "CH_dv.rds"))


}
