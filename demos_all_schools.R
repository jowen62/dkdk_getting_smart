#==========================================================================================#
# notes:
# purpose:
#==========================================================================================#

#=================================#
# load packages and clear objects #
#=================================#

  # clear objects
  rm(list = ls(pos = ".GlobalEnv"), pos = ".GlobalEnv")

  # turn off scientific notation
  options(scipen = 999)

  # clear log
  cat("\f")

  # load data.table
  library(data.table)
  library(googlesheets4)

#===========#
# set parms #
#===========#

  # set export toggle
  p_opt_exp <- F

  # set input directory
  p_dir_in <- ""

  #
  p_file_html <- "https://docs.google.com/spreadsheets/d/1I2mRjiAC4MGC3R5VubolOfEFvUx68GjPjE7qOtlEniE/edit?gid=956690954#gid=956690954"

  # set export directory
  p_dir_out <- ""

#===========#
# load data #
#===========#

  #
  as.data.table(read_sheet(p_file_html, sheet = "Data Collection Plan Grantees WITH Students", skip = 1))




