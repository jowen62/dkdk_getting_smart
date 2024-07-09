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
  p_file_attend <- "https://docs.google.com/spreadsheets/d/19otMzcKy5-s2rdkrYZzMeWbnelnGDVLdHMcZeIgboHE/edit?gid=0#gid=0"

  # set export directory
  p_dir_out <- ""

#===========#
# load data #
#===========#

  # load attendance
  in_attend <- as.data.table(read_sheet(p_file_attend))

  # we're counting "other" as excused because it's usually when kids miss class to do something else at school
  in_attend[, days_excused_sum := days_excused + days_other]

  # and now sum it all up
  in_attend[, days_absent := days_excused_sum + days_unexcused]

  # calculate attendance rate
  in_attend[, attend_rate := (180 - days_absent) / 180]

  # take the average
  in_attend[, mean(attend_rate)]


