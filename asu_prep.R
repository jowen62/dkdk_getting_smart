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

  # set the sheet location
  # p_file_summary <- "https://docs.google.com/spreadsheets/d/16RG3zeKHrEFqAtnK-RoLJFZhu11VcmUo8UDKNUWQ5V8/edit?gid=575332293#gid=575332293"
  p_file_summary <- "https://docs.google.com/spreadsheets/d/14iU_8-_AW9XHWx-ZBKpXRxNb-oQijSohhEe1bNm9Vhw/edit?gid=575332293#gid=575332293"

  # set export directory
  p_dir_out <- "/Users/jowen/Documents/contract_work/dkdk_getting_smart/data/output/asu_prep/"

# Exact Path ------------------------------------------------------------------------------------------------------

  # load data - ugh, wish I still had my multi-level header function
  in_ep_ss_change <- as.data.table(read_sheet(p_file_summary, sheet = "Data Summary Overview", range = "A1:G5"))
  in_ep_npr       <- as.data.table(read_sheet(p_file_summary, sheet = "Data Summary Overview", range = "I1:O5"))

  # spread subjects to the next column
  setnames(in_ep_ss_change, c("...3", "...5", "...7"), c("Math", "Reading", "ELA"))
  setnames(in_ep_npr, c("...3", "...5", "...7"), c("Math", "Reading", "ELA"))

  # complete the info
  setnames(in_ep_ss_change, paste(colnames(in_ep_ss_change), unlist(in_ep_ss_change[1]), sep = " - "))
  setnames(in_ep_npr, paste(colnames(in_ep_npr), unlist(in_ep_npr[1]), sep = " - "))

  # melt
  ep_ss_change_long <- melt.data.table(in_ep_ss_change[2:4], id.vars = "Exact Path - Grade", variable.factor = F)
  ep_npr_long <- melt.data.table(in_ep_npr[2:4], id.vars = "Exact Path - Grade", variable.factor = F)

  # separate the variable
  ep_ss_change_long[, ":=" (value   = as.numeric(value),
                            grade   = as.numeric(gsub("th", "", `Exact Path - Grade`)),
                            subject = stringr::str_extract(variable, ".*(?= - )"),
                            type    = stringr::str_extract(variable, 'Microschool|Recommended'))]

  ep_npr_long[, ":=" (value   = as.numeric(value),
                      grade   = as.numeric(gsub('th', '', `Exact Path - Grade`)),
                      subject = stringr::str_extract(variable, ".*(?= - )"),
                      type    = stringr::str_extract(variable, "Beginning of Year|End of Year"))]

  # recode
  ep_ss_change_long[type == 'Microschool', type := "Actual"]

  # plot
  plot_ep_change <- ggplot(ep_ss_change_long, aes(x = subject, y = value, fill = type)) +

    # add columns
    geom_col(position = 'dodge') +

    # change colors
    scale_fill_manual(values = c("#38761d", "#999999")) +

    # add column labels
    geom_text(aes(label = value, y = value / 2), position = position_dodge(width = 0.9), colour = 'white') +

    # split by grade
    facet_wrap( ~ grade) +

    # remove background
    theme_bw() +

    # make pretty
    theme(strip.background = element_rect(fill = "#FFF2CC"),
          text             = element_text(size = 16)) +

    # make labels cleaner
    labs(x = "Subject", y = "Score", fill = '', title = "Exact Path Scale Score Change")

  # plot
  plot_ep_npr <- ggplot(ep_npr_long, aes(x = subject, y = value, fill = type)) +

    # add columns
    geom_col(position = 'dodge') +

    # change colors
    scale_fill_manual(values = c("#999999", "#38761d")) +

    # add column labels
    geom_text(aes(label = value, y = value / 2), position = position_dodge(width = 0.9), colour = 'white') +

    # split by grade
    facet_wrap( ~ grade) +

    # remove background
    theme_bw() +

    # make pretty
    theme(strip.background = element_rect(fill = "#FFF2CC"),
          text             = element_text(size = 16)) +

    # make labels cleaner
    labs(x = "Subject", y = "Score", fill = '', title = "Exact Path National Percentile Rank")

# ACT ASPIRE (Grade 9) ---changedFiles()# ACT ASPIRE (Grade 9) -------------------------------------------------------------------------------------------

  # load the data - it's easiest if we pre-specify the sections of this first sheet that contain different data
  in_act_aspire <- as.data.table(read_sheet(p_file_summary, sheet = "Data Summary Overview", range = "A7:G11"))

  # make column names workable
  setnames(in_act_aspire, gsub(" ", "_", tolower(colnames(in_act_aspire))))

  # pull out subject
  in_act_aspire[, subject := tolower(stringr::str_remove(microschool_sp24_act_aspire, "EHS 9 "))]

  # melt
  act_aspire_long <- melt.data.table(in_act_aspire, id.vars = 'subject',
                               measure.vars = c('needs_support_(mp)', 'close_(pp)', 'ready_(p)', 'exceeds_(hp)'),
                               variable.factor = F, variable.name = "perf_level", value.name = "pct_stud")

  # fix names
  act_aspire_long[perf_level == 'needs_support_(mp)', perf_level := "1 - Needs Support"]
  act_aspire_long[perf_level == 'close_(pp)',         perf_level := "2 - Close"]
  act_aspire_long[perf_level == 'ready_(p)',          perf_level := "3 - Ready"]
  act_aspire_long[perf_level == 'exceeds_(hp)',       perf_level := "4 - Exceeds"]

  # change to factor
  act_aspire_long[, perf_level := as.factor(perf_level)]

  # plot
  plot_act_aspire <- ggplot(act_aspire_long, aes(x = stringr::str_to_title(subject), y = pct_stud * 100, fill = perf_level)) +

    # add bars
    geom_col(position = 'dodge') +

    # add colors
    scale_fill_manual(values = c("#38761d", "#999999", "#FFF2CC", "#BF9000")) +

    # add labels
    geom_text(aes(label = pct_stud * 100, y = (pct_stud * 100) / 2), position = position_dodge(width = 0.9)) +

    # remove background
    theme_bw() +

    # make the text match - doesn't currently work and I'm not sure how important it is to pursue
    theme(text = element_text(size = 20)) +

    # add labels
    labs(x = "Subject", y = "Percentage of Students", fill = "Performance Level",
         title = "ACT Aspire Performance", subtitle = "Grade 9, 2023-24 School Year")


# ACT (Grade 11) --------------------------------------------------------------------------------------------------

  # load the data - it's easiest if we pre-specify the sections of this first sheet that contain different data
  in_act_math <- as.data.table(read_sheet(p_file_summary, sheet = "Data Summary Overview", range = "A13:H15"))
  in_act_ela  <- as.data.table(read_sheet(p_file_summary, sheet = "Data Summary Overview", range = "A17:H19"))
  in_act_avg  <- as.data.table(read_sheet(p_file_summary, sheet = "Data Summary Overview", range = "A21:I22"))

  # remove identifiers from names
  setnames(in_act_math, gsub("Math ", "", colnames(in_act_math)))
  setnames(in_act_ela,  gsub("ELA ", "", colnames(in_act_ela)))

  # stack
  act_perf <- rbind(in_act_math[, subject := 'Math'], in_act_ela[, subject := 'ELA'])[, `...2` := NULL]

  # change names
  setnames(act_perf, 'Microschool ACT Proficiency', 'school_year')

  # recode school year
  act_perf[school_year == 'SY 23', school_year := "2022-23"]
  act_perf[school_year == 'SY 24', school_year := "2023-24"]

  # melt performance levels long
  act_perf_levels <- melt.data.table(act_perf,
                                     id.vars = c('school_year', 'subject'),
                                     measure.vars = c('MP', 'PP', 'P', 'HP'),
                                     variable.name = "perf_level",
                                     variable.factor = F,
                                     value.name = "pct_stud")

  # change to numeric levels
  act_perf_levels[perf_level == 'MP', perf_level := "1 - Needs Support"]
  act_perf_levels[perf_level == 'PP', perf_level := "2 - Close"]
  act_perf_levels[perf_level == 'P',  perf_level := "3 - Ready"]
  act_perf_levels[perf_level == 'HP', perf_level := "4 - Exceeds"]

  # plot
  plot_act_levels <- ggplot(act_perf_levels, aes(x = perf_level, y = pct_stud * 100, fill = subject)) +

    # add bars
    geom_col(position = 'dodge') +

    # add colors
    scale_fill_manual(values = c("#38761d", "#999999")) +

    # add labels
    geom_text(aes(label = pct_stud * 100, y = (pct_stud * 100) / 2),
              position = position_dodge(width = 0.9), color = 'white') +

    # put school years on two different lines
    facet_wrap( ~ school_year, nrow = 2) +

    # add a box around the proficient levels
    annotate("rect", xmin = 2.5, xmax = 4.5, ymin = -1, ymax = 55,
             alpha = 0, color= "#BF9000") +

    # add label
    geom_text(label = 'Proficient', x = 3.5, y = 57, color = "#BF9000") +

    # remove background
    theme_bw() +

    # make the text match - doesn't currently work and I'm not sure how important it is to pursue
    theme(text = element_text(size = 20),
          legend.position = 'bottom',
          strip.background = element_rect(fill = "#FFF2CC")) +

    # add labels
    labs(x = "Subject", y = "Percentage of Students", fill = "Performance Level",
         title = "ACT Performance", subtitle = "Grade 11, 2023-24 School Year")

  # melt long, removing writing because it doesn't contribute to the composite score
  act_avg <- melt.data.table(in_act_avg,
                             id.vars = 'Microschool ACT Average Scores',
                             measure.vars = grep("of 36", colnames(in_act_avg), value = T),
                             variable.name = "subject",
                             variable.factor = F,
                             value.name = 'score')

  # abbreviate names
  act_avg[, subject := stringr::str_remove(subject, " \\(of 36\\)")]

  # plot,
  plot_act_scores <- ggplot(act_avg[subject != "Composite"], aes(x = subject, y = score)) +

    # add bars
    geom_col(fill = "#38761d") +

    # add labels
    geom_text(aes(label = score, y = score / 2), color = 'white') +

    # add composite
    geom_hline(yintercept = 22.2, color = "#BF9000") +

    # add label
    geom_text(aes(x = "Science", y = 24, label = "Composite \n2023-24"), color = "#BF9000") +

    # add the state average in 2023
    geom_hline(yintercept = 17.7, color = "#999999") +

    # label state average - could not get this to work with annotate(), despite that being what the error calls for
    geom_text(aes(x = "Science", y = 20, label = "State Avg. \n2022-23"), color = "#999999") +

    # remove background
    theme_bw() +

    # make text larger for reports
    theme(text = element_text(size = 20)) +

    # add labels
    labs(x = "ACT Subject", y = "Avg. Score", title = "ACT Avg. Scores", subtitle = "Grade 11, 2023-24 School Year")

# export ----------------------------------------------------------------------------------------------------------

  # Exact Path score change
  ggsave(filename = "ep_score_change.png",
         plot     = plot_ep_change,
         path     = p_dir_out,
         width    = 12,
         height   = 4)

  # Exact Path NPR
  ggsave(filename = "ep_npr.png",
         plot     = plot_ep_npr,
         path     = p_dir_out,
         width    = 12,
         height   = 4)

  # ACT ASPIRE performance levels
  ggsave(filename = "act_aspire_perf_levels.png",
         plot     = plot_act_aspire,
         path     = p_dir_out,
         width    = 10,
         height   = 6)

  # ACT performance levels
  ggsave(filename = "act_perf_levels.png",
         plot     = plot_act_levels,
         path     = p_dir_out,
         width    = 7,
         height   = 8)

  # ACT average scores
  ggsave(filename = "act_avg_scores.png",
         plot     = plot_act_scores,
         path     = p_dir_out,
         width    = 8,
         height   = 5)
