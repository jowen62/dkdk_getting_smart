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
  library(readxl)
  library(googlesheets4)

#===========#
# set parms #
#===========#

  # set export toggle
  p_opt_exp <- F

  # set location for raw data to save
  p_dir_in <- "/Users/jowen/Documents/contract_work/dkdk_getting_smart/data/raw_data/sci_prep_acad/"

  # set export directory
  p_dir_out <- ""

# attendance ------------------------------------------------------------------------------------------------------

  # set the file name since we'll need to use it multiple times
  p_file_attend <- 'General Attendance Data 2023-2024 (without student names).xlsx'

  # get the sheet names
  p_sheets_attend <- excel_sheets(paste0(p_dir_in, p_file_attend))

  # load all months of data
  l_attend <- lapply(p_sheets_attend,
                     function(x) {

                       # load data
                       in_data <- as.data.table(read_excel(paste0(p_dir_in, p_file_attend), sheet = x, col_types = 'text'))

                       # change names to match
                       setnames(in_data, '# of students out', 'Students Out', skip_absent = T)

                       return(in_data)

                     })

  # add identifiers
  names(l_attend) <- p_sheets_attend

  # stack
  in_attend <- rbindlist(l_attend, idcol = 'month_year', fill = T)

  # calculate the mean (it doesn't particularly matter that the dates came in weird because we just need a simple avg)
  in_attend[, mean(as.numeric(`Overall Percentage`), na.rm = T)]

    #qc: let's look at how many entries there were per month to spot check against the raw data
    # in_attend[, .N, by = 'month_year']

# test data - fall & winter 2023 with growth ------------------------------------------------------------------------

  # set the html
  p_file_test_23 <- "https://docs.google.com/spreadsheets/d/1JwTaeu77NaZpRK2kFh65svo4nIXpJloBvU5gJI185S4/edit?gid=0#gid=0"

  # load
  in_read_23 <- as.data.table(read_sheet(p_file_test_23, sheet = "Reading scores 2023"))
  # in_alg_23  <- as.data.table(read_sheet(p_file_test, sheet = "Algebra scores 2023"))

  # this data is really messy, so we're going to have to assign some stuff
  setnames(in_read_23, gsub(" ", "_", tolower(colnames(in_read_23))))

  # before we fiddle with anything, we need to idenitfy HS vs. MS since that's just colors in the workbook
  in_read_23[date_tested == 'Date Tested', school_level := "ms"]

  # fill downward
  in_read_23 <- as.data.table(tidyr::fill(in_read_23, "school_level", .direction = 'down'))

  # the rest are HS
  in_read_23[is.na(school_level), school_level := "hs"]

  # stack and label the proper time periods
  read_23 <- rbind(in_read_23[, .(test_term = 'fall',
                                  date_tested,
                                  student_name,
                                  score              = reading_score,
                                  lexile             = lexile...4,
                                  grade_average      = grade_average...5,
                                  npr                = npr...6,
                                  growth_performance = growth_performance...7)],
                   in_read_23[, .(test_term          = 'winter',
                                  date_tested        = `...11`,
                                  student_name,
                                  score              = winter_testing_score,
                                  lexile             = lexile...14,
                                  grade_average      = grade_average...15,
                                  npr                = npr...16,
                                  growth_performance = growth_performance...17)])[date_tested != 'Date Tested' &
                                                                                    !is.na(student_name)][, subject := 'reading']

  # change everything to lowercase
  read_23[, c('npr', 'growth_performance') := lapply(.SD, tolower), .SDcols = c('npr', 'growth_performance')]


# test data - 2023-24 raw scores ----------------------------------------------------------------------------------

  # set location
  p_file_test_24 <- "/Users/jowen/Documents/contract_work/dkdk_getting_smart/data/raw_data/sci_prep_acad/Copy of 2023-2024 , 2022 - 2023 Scantron Testing Data for Data Collection Plan C1.xlsx"

  # load only the current year data (can come back for prior year in the next set of columns if needed)
  in_test_hs_24 <- as.data.table(readxl::read_excel(p_file_test_24, sheet = 'Blue Team (HS)', col_types = 'text', range = "A2:F67"))

  # pull out the student identifiers
  in_test_hs_24[grepl("^ST", `ST 10`), student_id := `ST 10`]

  # fill downward
  in_test_hs_24 <- as.data.table(tidyr::fill(in_test_hs_24, 'student_id', .direction = 'down'))

  # now add the student who was in the top row
  in_test_hs_24[is.na(student_id), student_id := 'ST10']

  # now remove extra rows
  sub_test_hs_24 <- in_test_hs_24[!grepl("^ST", `ST 10`) & !is.na(`ST 10`)]

  # change column names to be more workable
  setnames(sub_test_hs_24, gsub(" ", "_", tolower(colnames(sub_test_hs_24))))

  # change one more
  setnames(sub_test_hs_24, "st_10", "subject")

  # change text to lowercase
  sub_test_hs_24[, subject := tolower(subject)]

  # standardize student id
  sub_test_hs_24[, student_id := tolower(gsub(" ", "", student_id))]

  # melt scores long
  long_test_hs_24 <- melt.data.table(sub_test_hs_24,
                                     id.vars         = c('student_id', 'subject', 'current_grade_average'),
                                     measure.vars    = c('fall_2023', 'winter_2023', 'spring_2024'),
                                     variable.name   = 'test_term',
                                     value.name      = 'score',
                                     variable.factor = F)

  # change to numeric
  long_test_hs_24[, c('current_grade_average', 'score') := lapply(.SD, as.numeric), .SDcols = c('current_grade_average', 'score')]

  # next would be to find the cut scores and see if we can do any summary analysis with that


