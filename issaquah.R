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
  p_file_grades <- "/Users/jowen/Documents/contract_work/dkdk_getting_smart/data/raw_data/ellemercito/Grading Data-23-24.xlsx - Sheet1.csv"

  # set export directory
  p_dir_out <- ""

# attendance ------------------------------------------------------------------------------------------------------

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

# grades ----------------------------------------------------------------------------------------------------------

  # not really sure what we can do with this, but let's see if we can do some summaries
  in_course <- fread(p_file_grades, colClasses = 'character')

  # create better names
  setnames(in_course, gsub("2024_", "", gsub(" ", "_", tolower(colnames(in_course)))))
  setnames(in_course, "v2", "student_id")

  # how many students is this? all 16
  in_course[, uniqueN(student_id)]

  # melt semesters long
  course_long <- melt.data.table(in_course, measure.vars = c('s1', 's2'), variable.name = "semester", na.rm = T)

  # recode
  course_long[semester == 's1', semester := "Fall"]
  course_long[semester == 's2', semester := "Spring"]

  # should we try to determine subjects?
  course_long[, grade_course_desc := tolower(grade_course_desc)]
  course_long[grepl("span|lang", grade_course_desc),                         subject := 'Language']
  course_long[grepl("geom|alg|math", grade_course_desc),                     subject := 'Math']
  course_long[grepl("lit|eng", grade_course_desc),                           subject := 'ELA']
  course_long[grepl("hist|human geo", grade_course_desc),                    subject := 'Social Studies']
  course_long[grepl("astro|chem|bio|phy lab|med|enviro", grade_course_desc), subject := 'Science']
  course_long[grepl("tv|song|art|photo|sculpture", grade_course_desc),       subject := 'Art']
  course_long[grepl("fit|train|yoga|health", grade_course_desc),             subject := 'PE']

  # bad practice, but I don't have a good way to group these
  course_long[is.na(subject), subject := "Misc."]

  # change letter grades to factors for order
  course_long[, value := factor(value, levels = c('A', 'A-',
                                                  'B+', 'B',
                                                  'C+', 'C', 'C-',
                                                  'D+', 'D',
                                                  'P', 'F'))]

  # summarize
  course_summary <- course_long[!is.na(value) & subject %chin% c("ELA", "Math", "Science", "Social Studies"),
                                .(.N), by = c('subject', 'value', 'semester')]

  # remove blanks and limit to core subjects
  plot_grades <- ggplot(course_summary, aes(x = value, y = N, fill = semester)) +

    # add bars
    geom_col(position = 'dodge') +

    # add colors
    scale_fill_manual(values = c("#999999", "#38761d")) +

    # force axis to use integers
    scale_y_continuous(breaks = seq(0, 12, 2)) +

    # add labels
    geom_text(aes(label = N, y = N / 2), position = position_dodge(width = 0.9), color = 'white') +

    # split by semester & subject
    facet_wrap( ~ subject, scales = 'free') +

    # remove grey background
    theme_bw() +

    # some more beautifying
    theme(text             = element_text(size = 20),
          strip.background = element_rect(fill = "#FFF2CC"),
          panel.grid.minor = element_blank()) +

    labs(x = "Course Grade", y = "N Students", fill = 'Semester', title = 'Core Subject Grade Distribution')

# export ----------------------------------------------------------------------------------------------------------

  ggsave("plot_course_grades.png", plot_grades, path = p_dir_out, width = 10, height = 5)

