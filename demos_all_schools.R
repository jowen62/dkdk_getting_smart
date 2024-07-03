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

  # set file location
  p_file_demos <- "https://docs.google.com/spreadsheets/d/1b3_nGCzH0gABBQCgTUN1-y_ojSjTHANhb0wJSQee_EQ/edit?pli=1&gid=577167601#gid=577167601"

  # set export directory
  p_dir_out <- "/Users/jowen/Documents/contract_work/dkdk_getting_smart/data/output/demos/"

# BIPOC -----------------------------------------------------------------------------------------------------------

  # load the sheet
  in_compiled_bipoc <- as.data.table(read_sheet(p_file_demos, sheet = "BIPOC", range = "A22:E29"))

  # rename one column to be more accurate
  setnames(in_compiled_bipoc, "Percentage of all enrolled learners who are BIPOC", 'school_name')

  # melt others long for pie charts
  long_bipoc <- melt.data.table(in_compiled_bipoc, id.vars = 'school_name')

  # test parm
  # p_school <- long_bipoc[, unique(school_name)][[7]]

  # loop through schools
  for (p_school in long_bipoc[, unique(school_name)]) {

    # we need one plot per school
    sub_bipoc <- long_bipoc[school_name == p_school]

    # need to switch this to cumulative to be the top of the bar
    sub_bipoc[, value_end := cumsum(value)]

    # calculate bottom of bar
    sub_bipoc[, value_start := c(0, head(value_end, n = -1))]

    # determine where the label should go
    sub_bipoc[, label_position := (value_start + value_end) / 2]

    # create pie chart that looks like the ones Excel makes
    plot_bipoc <- ggplot(sub_bipoc, aes(ymax = value_end, ymin = value_start, xmax=4, xmin=3, fill = variable)) +

      # create a stacked bar chart
      geom_rect() +

      # change to pie
      coord_polar(theta = "y") +

      # change to donut to match Excel formatting from before
      xlim(c(2, 4)) +

      # remove all background
      theme_void() +

      # reformat legend
      theme(legend.position = 'bottom',
            legend.direction = "vertical",
            legend.title = element_blank(),
            legend.text = element_text(size = 12),

            # and title
            plot.title = element_text(hjust = 0.5, size = 14)) +

      # add labels
      geom_text(aes(label = paste0(round(value * 100), "%"), y = label_position), x = 3.5, color = 'white') +

      # set colors
      scale_fill_manual(values = c("#38761D", "#999999", "#BF9000", "#B6D7A8")) +

      # add title
      labs(title = "BIPOC-Identifying \nStudents")

    # save plot
    ggsave("pie_bipoc.png", plot_bipoc, path = paste0(p_dir_out, p_school, '/'), width = 3, height = 4.5)

  }

# Neurodiversity -------------------------------------------------------------------------------------------------

  # load data
  in_compiled_neuro <- as.data.table(read_sheet(p_file_demos, sheet = "Neurodiverse", range = "A1:B8"))

  # rename for clarity
  setnames(in_compiled_neuro, c("Student population & student demographics: Neurodiversity", "...2"),
                              c("school_name", "Neurodivergent"))

  # calculate percentage of neurotypicals
  in_compiled_neuro[, Neurotypical := 1 - Neurodivergent]

  # melt long
  long_neuro <- melt.data.table(in_compiled_neuro, id.vars = 'school_name')

  # rename a couple to match BIPOC labeling (want the plots to go into the existing folders)
  long_neuro[school_name == 'Purdue Polytechnic High School (PPHS)', school_name := "Purdue Polytechnic HS"]
  long_neuro[grepl("Science Prep", school_name),                     school_name := "Science Prep Academy"]

  # test parm
  # p_school <- long_neuro[, unique(school_name)][[7]]

  # loop through schools
  for (p_school in long_neuro[, unique(school_name)]) {

    # we need one plot per school
    sub_neuro <- long_neuro[school_name == p_school]

    # need to switch this to cumulative to be the top of the bar
    sub_neuro[, value_end := cumsum(value)]

    # calculate bottom of bar
    sub_neuro[, value_start := c(0, head(value_end, n = -1))]

    # determine where the label should go
    sub_neuro[, label_position := (value_start + value_end) / 2]

    # if it's 100% one way or the other, only keep that row, otherwise the plot comes out weird
    if (any(sub_neuro[, value] == 1)) {sub_neuro <- sub_neuro[value == 1]}

    # create pie chart that looks like the ones Excel makes
    plot_neuro <- ggplot(sub_neuro, aes(ymax = value_end, ymin = value_start, xmax = 4, xmin = 3, fill = variable)) +

      # create a stacked bar chart
      geom_rect() +

      # change to pie
      coord_polar(theta = "y") +

      # change to donut to match Excel formatting from before
      xlim(c(2, 4)) +

      # remove all background
      theme_void() +

      # reformat legend
      theme(legend.position  = 'bottom',
            legend.direction = "vertical",
            legend.title     = element_blank(),
            legend.text      = element_text(size = 12),

            # and title
            plot.title = element_text(hjust = 0.5, size = 14)) +

      # add labels
      geom_text(aes(label = paste0(round(value * 100), "%"), y = label_position), x = 3.5, color = 'white') +

      # set colors
      scale_fill_manual(values = c("#38761D", "#999999")) +

      # add title
      labs(title = "Neurodivergent \nStudents")

    # save plot
    ggsave("pie_neurodiverse.png", plot_neuro, path = paste0(p_dir_out, p_school, '/'), width = 3, height = 4.5)

  }

# Econ Disadvantage -----------------------------------------------------------------------------------------------

  # load data
  in_compiled_econdis <- as.data.table(read_sheet(p_file_demos, sheet = "Econ-Dis", range = "A1:B7"))

  # change names
  setnames(in_compiled_econdis, c("Student population & student demographics: economically disadvantaged", "...2"),
                                c("school_name", "Students Qualifying for Free & Reduced Lunch"))

  # calculate the inverse
  in_compiled_econdis[, `All Other Students` := 1 - `Students Qualifying for Free & Reduced Lunch`]

  # melt
  long_econdis <- melt.data.table(in_compiled_econdis, id.vars = 'school_name')

  # change names to match
  long_econdis[grepl('Purdue', school_name),       school_name := "Purdue Polytechnic HS"]
  long_econdis[grepl("Science Prep", school_name), school_name := "Science Prep Academy"]

  # test parm
  p_school <- long_econdis[, unique(school_name)][[6]]

  # loop through schools
  for (p_school in long_econdis[, unique(school_name)]) {

    # we need one plot per school
    sub_econdis <- long_econdis[school_name == p_school]

    # need to switch this to cumulative to be the top of the bar
    sub_econdis[, value_end := cumsum(value)]

    # calculate bottom of bar
    sub_econdis[, value_start := c(0, head(value_end, n = -1))]

    # determine where the label should go
    sub_econdis[, label_position := (value_start + value_end) / 2]

    # if it's 100% one way or the other, only keep that row, otherwise the plot comes out weird
    if (any(sub_econdis[, value] == 1)) {sub_econdis <- sub_econdis[value == 1]}

    # create pie chart that looks like the ones Excel makes
    plot_econdis <- ggplot(sub_econdis, aes(ymax = value_end, ymin = value_start, xmax = 4, xmin = 3,
                                            fill = stringr::str_wrap(rev(variable), width = 24))) +

      # create a stacked bar chart
      geom_rect() +

      # change to pie
      coord_polar(theta = "y") +

      # change to donut to match Excel formatting from before
      xlim(c(2, 4)) +

      # remove all background
      theme_void() +

      # reformat legend
      theme(legend.position  = 'bottom',
            legend.direction = "vertical",
            legend.title     = element_blank(),
            legend.text      = element_text(size = 12),

            # and title
            plot.title = element_text(hjust = 0.5, size = 14)) +

      # add labels
      geom_text(aes(label = paste0(round(value * 100), "%"), y = label_position), x = 3.5, color = 'white') +

      # set colors
      scale_fill_manual(values = c("#38761D", "#999999")) +

      # add title
      labs(title = "Free & Reduced Lunch \nEligible Students")

    # save plot
    ggsave("pie_econdis.png", plot_econdis, path = paste0(p_dir_out, p_school, '/'), width = 3, height = 4.5)

  }
