#' Assign default formats for Excel
#'
#' @return Values that store Excel styles in the global environment
#'
#' @import openxlsx

# besd_basic_fmtids R version 1.00 - Biostat Global Consulting - 2024-07-19
# *******************************************************************************
# Change log

# Date 			  Version 	Name			      What Changed
# 2024-07-19  1.00      Caitlin Clary   Original version (modified from VCQI v1.00)
# *******************************************************************************

# Based on default vcqi_basic_fmtids in Stata VCQI

besd_basic_fmtids <- function(){

  # We define a set of format ids - create different cell formats
  col_header <- createStyle(
    fontName = "Calibri", fontSize = 11, fontColour = "black", halign = "right",
    wrapText = TRUE)

  assign("col_header", col_header, envir = .GlobalEnv)

  # Table_title - bold and left aligned
  table_title <- createStyle(
    fontName = "Calibri", fontSize = 11, fontColour = "black", halign = "left",
    textDecoration = "bold")

  assign("table_title", table_title, envir = .GlobalEnv)

  # Table_subtitle - regular and left aligned - standard name for user customization
  table_subtitle <- createStyle(
    fontName = "Calibri", fontSize = 11, fontColour = "black", halign = "left")

  assign("table_subtitle", table_subtitle, envir = .GlobalEnv)

  # Table_footnote - regular and left aligned - but has its own name in case the user wishes to customize it
  table_footnote <- createStyle(
    fontName = "Calibri", fontSize = 11, fontColour = "black", halign = "left")

  assign("table_footnote", table_footnote, envir = .GlobalEnv)

  # Table_header - regular, right aligned, wrapped  - format for the row that
  # holds the table column titles in columns 2 through the end
  table_header <- createStyle(
    fontName = "Calibri", fontSize = 11, fontColour = "black", halign = "right")

  assign("table_header", table_header, envir = .GlobalEnv)

  # ********************************************************************************
  # Bold_left and bold_right - sometimes used to report level 1 output
  bold_left <- createStyle(
    fontName = "Calibri", fontSize = 11, fontColour = "black", halign = "left",
    textDecoration = "bold")

  assign("bold_left", bold_left, envir = .GlobalEnv)

  bold_right <- createStyle(
    fontName = "Calibri", fontSize = 11, fontColour = "black", halign = "right",
    textDecoration = "bold")

  assign("bold_right", bold_right, envir = .GlobalEnv)

  # ********************************************************************************
  # Regular_left and regular_right - our default formats - used for many table rows
  regular_left <- createStyle(
    fontName = "Calibri", fontSize = 11, fontColour = "black", halign = "left")

  assign("regular_left", regular_left, envir = .GlobalEnv)

  regular_right <- createStyle(
    fontName = "Calibri", fontSize = 11, fontColour = "black", halign = "right")

  assign("regular_right", regular_right, envir = .GlobalEnv)

  # ********************************************************************************
  # Shaded_left and shaded_right - puts a lightgray shading in the table cell
  shaded_left <- createStyle(
    fontName = "Calibri", fontSize = 11, fontColour = "black", halign = "left",
    fgFill = "#e5e5e5")

  assign("shaded_left", shaded_left, envir = .GlobalEnv)

  shaded_right <- createStyle(
    fontName = "Calibri", fontSize = 11, fontColour = "black", halign = "right",
    fgFill = "#e5e5e5")

  assign("shaded_right", shaded_right, envir = .GlobalEnv)

  # ********************************************************************************
  # Italic_left and italic_right
  italic_left <- createStyle(
    fontName = "Calibri", fontSize = 11, fontColour = "black", halign = "left",
    textDecoration = "italic")

  assign("italic_left", italic_left, envir = .GlobalEnv)

  italic_right <- createStyle(
    fontName = "Calibri", fontSize = 11, fontColour = "black", halign = "right",
    textDecoration = "italic")

  assign("italic_right", italic_right, envir = .GlobalEnv)

  # ********************************************************************************

  # italic_left_indented3 is sometimes used for level4 stratum names; it simply
  # adds a small indent at the far left to show, perhaps, that these strata are
  # nested within whatever row appears above them

  italic_left_indented3 <- createStyle(
    fontName = "Calibri", fontSize = 11, fontColour = "black", halign = "left",
    textDecoration = "italic", indent = 3)

  assign("italic_left_indented3", italic_left_indented3, envir = .GlobalEnv)
}
