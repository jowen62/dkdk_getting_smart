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
  library(ggplot2)

#===========#
# set parms #
#===========#

  # set export toggle
  p_opt_exp <- F

  # set input directory
  p_dir_file <- "https://docs.google.com/spreadsheets/d/1MNgx3HrIY60Nn8Xy2Ml9JWq1JvdY0YXkYkNyowqFXPc/edit?gid=89047361#gid=89047361"

  # set export directory
  p_dir_out <- "/Users/jowen/Documents/contract_work/dkdk_getting_smart/data/output/da_vinci/"

  # we have historical CAASP and current MAP data

# interim performance --------------------------------------------------------------------------------------------------

  # load NWEA data
  in_perf_interim <- as.data.table(read_sheet(p_dir_file, sheet = "Performance"))

  # we'll do a bar chart to show change over the first semester
  plot_perf_interim <- ggplot(in_perf_interim, aes(x = grade, y = `mean rit score`, fill = stringr::str_to_title(period))) +

    # add bars
    geom_col(position = 'dodge') +

    # change colors to match theme
    scale_fill_manual(values = c("#999999", "#38761d")) +

    geom_text(aes(label = round(`mean rit score`), y = `mean rit score` / 2),
              position = position_dodge(width = 0.9), color = 'white') +

    # split by subject
    facet_wrap(~ stringr::str_to_title(subject)) +

    # remove background
    theme_bw() +

    # prettify
    theme(legend.position  = 'bottom',
          strip.background = element_rect(fill = "#FFF2CC"),
          text             = element_text(size = 20)) +

    # make labels nice
    labs(x = "Grade Level", y = "Mean RIT Score", fill = "Test Date")

# school growth ---------------------------------------------------------------------------------------------------

  # load NWEA data
  in_school_growth <- as.data.table(read_sheet(p_dir_file, sheet = "Grade Level Norms"))


# student growth --------------------------------------------------------------------------------------------------

  # load NWEA data
  in_stud_growth <- as.data.table(read_sheet(p_dir_file, sheet = "Student Norms"))

# export ----------------------------------------------------------------------------------------------------------

  # save plots
  ggsave("nwea_map_performance.png", plot_perf_interim, path = p_dir_out, width = 8, height = 4)
