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
  p_file_psat <- "https://docs.google.com/spreadsheets/d/1aS9C9xAEJzHUH9-Aar-MYT7XVEPBiVXwvBMENPTrGos/edit?gid=0#gid=0"
  p_file_star <- "https://docs.google.com/spreadsheets/d/1Dm0JalPjy6SzPBL1h94RaEmxuYy-5_aC2kVfzU-ZJ34/edit?gid=0#gid=0"

  # set export directory
  p_dir_out <- "/Users/jowen/Documents/contract_work/dkdk_getting_smart/data/output/pphs/"

# PSAT ------------------------------------------------------------------------------------------------------------

  # load PSAT data
  in_psat_9  <- as.data.table(read_sheet(p_file_psat, range = "A3:D9"))
  in_psat_10 <- as.data.table(read_sheet(p_file_psat, range = "A12:D18"))

  # stack
  psat_stack <- rbind(in_psat_9[, grade_level := 9], in_psat_10[, grade_level := 10])

  # rename for clarity
  setnames(psat_stack, "...1", "time")

  # clarify
  psat_stack[, num_type := ifelse(is.na(time), "pct", "n")]

  # fill info downward
  psat_stack <- as.data.table(tidyr::fill(psat_stack, "time", .direction = "down"))

  # I think we want time period wide, so let's first melt subjects long
  psat_long <- melt.data.table(psat_stack, measure.vars = c("ERW", "Math"), variable.factor = F, variable.name = 'subject')

  # now cast time period wide
  # dcast.data.table(psat_long[num_type == 'pct'], grade_level + subject ~ time, value.var = "value")

  # add n-size manually
  psat_long[grade_level == 9, n_size := 8]
  psat_long[grade_level == 10, n_size := 4]

  # make the plot of Percentages, though we don't know what they mean
  plot_psat_cb <- ggplot(psat_long[time != "EOY Difference" & num_type == "pct"],
         aes(x = subject, y = value * 100, fill = time)) +

    # add columns
    geom_col(position = 'dodge') +

    # split by grade level
    facet_grid( ~ paste("Grade", grade_level), scales = "free") +

    # change bar colors
    scale_fill_manual(values = c("#999999", "#38761d")) +

    # remove background
    theme_bw() +

    # some more beautifying
    theme(text             = element_text(size = 20),
          strip.background = element_rect(fill = "#FFF2CC")) +

    # relabel
    labs(x = "Subject", y = "Percentage", fill = "Period", title = "PSAT Performance, 2023-24") +

    # add value labels
    geom_text(aes(label = round(value * 100, 1), y = (value * 100) / 2), position = position_dodge(width = 0.9), color = 'white')


# STAR ------------------------------------------------------------------------------------------------------------

  # load STAR data
  in_star_9 <- as.data.table(read_sheet(p_file_star, range = "A3:C9"))
  in_star_10 <- as.data.table(read_sheet(p_file_star, range = "A12:C18"))

  # slightly different subject names, so let's melt before stacking
  long_star_9 <- melt.data.table(in_star_9, id.vars = '...1', variable.factor = F, variable.name = "subject")
  long_star_10 <- melt.data.table(in_star_10, id.vars = '...1', variable.factor = F, variable.name = "subject")

  # now stack
  star_stack <- rbind(long_star_9[, grade_level := 9], long_star_10[, grade_level := 10])

  # change names
  setnames(star_stack, "...1", "time")

  # clarify
  star_stack[is.na(time), time := "EOY Difference PR"]
  star_stack[time == "EOY Difference", time := "EOY Difference GE"]

  # break apart the var
  star_stack[, ":=" (variable = stringr::str_extract(time, "GE|PR"),
                     time     = stringr::str_remove(time, " GE| PR"))]

  # add n-size manually
  star_stack[grade_level == 9, n_size := 8]
  star_stack[grade_level == 10, n_size := 3]

  # recode ERW and Reading as ELA
  star_stack[subject %chin% c("ERW", "Reading"), subject := "ELA"]

  # expand abbreviations
  star_stack[variable == 'GE', variable := "Grade Equivalency"]
  star_stack[variable == 'PR', variable := "Percentile Rank"]

  # make the plot
  plot_star <- ggplot(star_stack[time != "EOY Difference"], aes(x = subject, y = value, fill = time)) +

    # add columns
    geom_col(position = 'dodge') +

    # split by grade level
    facet_grid(variable ~ paste("Grade", grade_level), scales = "free") +

    # change bar colors
    scale_fill_manual(values = c("#999999", "#38761d")) +

    # remove background
    theme_bw() +

    # some more beautifying
    theme(text             = element_text(size = 20),
          strip.background = element_rect(fill = "#FFF2CC")) +

    # relabel
    labs(x = "Subject", y = "Value", fill = "Period", title = "STAR Performance, 2023-24") +

    # add value labels
    geom_text(aes(label = round(value, 1), y = value / 2), position = position_dodge(width = 0.9), color = 'white')

# export ----------------------------------------------------------------------------------------------------------

  # star
  ggsave("plot_star.png", plot_star, path = p_dir_out, width = 10, height = 6)
  ggsave("plot_psat_cb_pct.png", plot_psat_cb, path = p_dir_out, width = 10, height = 4)
