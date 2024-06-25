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
  p_dir_in <- ""

  # set export directory
  p_dir_out <- ""

# performance over time -------------------------------------------------------------------------------------------

  # set the sheet location
  p_file_summary <- "https://docs.google.com/spreadsheets/d/16RG3zeKHrEFqAtnK-RoLJFZhu11VcmUo8UDKNUWQ5V8/edit?gid=575332293#gid=575332293"

  # load the data - it's easiest if we pre-specify the sections of this first sheet that contain different data
  in_perf_summary <- as.data.table(read_sheet(p_file_summary, sheet = "Data Summary Overview", range = "A7:G11"))

  # make column names workable
  setnames(in_perf_summary, gsub(" ", "_", tolower(colnames(in_perf_summary))))

  # pull out subject
  in_perf_summary[, subject := tolower(stringr::str_remove(microschool_sp24_act_aspire, "EHS 9 "))]

  # melt
  perf_long <- melt.data.table(in_perf_summary, id.vars = 'subject',
                               measure.vars = c('needs_support_(mp)', 'close_(pp)', 'ready_(p)', 'exceeds_(hp)'),
                               variable.factor = F, variable.name = "perf_level", value.name = "pct_stud")

  # fix names
  perf_long[perf_level == 'needs_support_(mp)', perf_level := "1 - Needs Support"]
  perf_long[perf_level == 'close_(pp)',         perf_level := "2 - Close"]
  perf_long[perf_level == 'ready_(p)',          perf_level := "3 - Ready"]
  perf_long[perf_level == 'exceeds_(hp)',       perf_level := "4 - Exceeds"]

  # change to factor
  perf_long[, perf_level := as.factor(perf_level)]

  # plot
  plot_perf_levels <- ggplot(perf_long, aes(x = stringr::str_to_title(subject), y = pct_stud * 100, fill = perf_level)) +

    # add bars
    geom_col(position = 'dodge') +

    # add colors
    scale_fill_manual(values = c("#38761d", "#999999", "#FFF2CC", "#BF9000")) +

    # add labels
    geom_text(aes(label = pct_stud * 100, y = (pct_stud * 100) / 2), position = position_dodge(width = 0.9)) +

    # remove background
    theme_bw() +

    # make the text match - doesn't currently work and I'm not sure how important it is to pursue
    theme(text = element_text(size = 16)) +

    # add labels
    labs(x = "Subject", y = "Percentage of Students", fill = "Performance Level")


