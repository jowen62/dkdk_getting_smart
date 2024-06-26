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
  library(ggplot2)

#===========#
# set parms #
#===========#

  # set export toggle
  p_opt_exp <- F

  # set input directory
  p_dir_in <- ""

  # set export directory
  p_dir_out <- "/Users/jowen/Documents/contract_work/dkdk_getting_smart/data/output/gem_prep/"

#===========#
# load data #
#===========#

  # create dataset since all we had was a dataset
  perf_counts <- data.table(level = c(1:4),
                            ela   = c(9, 5, 14, 9),
                            math  = c(6, 10, 15, 6))

  # melt
  perf_counts_long <- melt.data.table(perf_counts, id.vars = 'level', variable.factor = F, variable.name = 'subject', value.name = 'n_stud')

  # make pretty
  perf_counts_long[subject == 'ela',  subject := "ELA"]
  perf_counts_long[subject == 'math', subject := "Math"]

  # create plot
  plot_perf_levels <- ggplot(perf_counts_long, aes(x = level, y = n_stud, fill = subject)) +

    # make bars, dodge them
    geom_col(position = 'dodge') +

    # add the numbers to the bars
    geom_text(aes(label = n_stud, y = n_stud / 2), position = position_dodge(width = 0.9), color = 'white') +

    # use pie chart colors
    scale_fill_manual(values = c("#38761d", "#999999")) +

    # add a box around the proficient levels
    annotate("rect", xmin = 2.5, xmax = 4.5, ymin = -0.25, ymax = 15.25,
             alpha = 0, color= "#BF9000") +

    # add label
    geom_text(label = 'Proficient', x = 3.5, y = 15.6, color = "#BF9000") +

    # clear background
    theme_bw() +

    # make the text match - doesn't currently work and I'm not sure how important it is to pursue
    theme(text = element_text(size = 16),
          legend.position = 'bottom') +

    # relabel
    labs(x = "Performance Level", y = "Number of Students", fill = "Subject",
         title = "ISAT Performance 2023-24, All Grades")

#=========#
# export #
#========#

  ggsave(filename = "isat_perf_levels.png",
         plot = plot_perf_levels,
         path = p_dir_out,
         width = 8,
         height = 6)
