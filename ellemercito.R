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
  p_file_scores <- "https://docs.google.com/spreadsheets/d/1NTEKjUsgCiaKBrOVRZh__manS_g1PrDfDcZiOxfxue8/edit?gid=0#gid=0"

  # set export directory
  p_dir_out <- "/Users/jowen/Documents/contract_work/dkdk_getting_smart/data/output/ellemercito/"

# test scores -----------------------------------------------------------------------------------------------------

  # load the scores
  in_scores <- as.data.table(read_sheet(p_file_scores))

  # add 2023-24 placeholders
  scores_stacked <- rbind(in_scores,
                          data.table(`School Year`   = "2023 - 2024",
                                     Subject         = paste("Total", c("Language", "Math", "Reading", "Score")),
                                     `Average Score` = 100))

  # subset to totals
  sub_scores <- scores_stacked[grepl("Total", Subject)]

  # recode subjects
  sub_scores[, Subject := gsub("Total ", "", Subject)]
  sub_scores[Subject == "Score", Subject := "Composite"]

  # create labels
  sub_scores[, col_label := ifelse(`School Year` == '2023 - 2024', "?", as.character(round(`Average Score`, 1)))]

  # plot only the totals
  plot_scores <- ggplot(sub_scores, aes(x = Subject, y = `Average Score`, fill = `School Year`, linetype = `School Year`)) +

    geom_col(position = 'dodge', color = 'black') +

    # add colors
    scale_fill_manual(values = c("#38761d", "#999999", "white")) +

    # add borders around 2023-24 to show empty bars
    scale_linetype_manual(values = c("blank", "blank", "dashed")) +

    # add labels
    geom_text(aes(label = col_label, y = `Average Score` / 2), position = position_dodge(width = 0.9)) +

    # remove background
    theme_bw() +

    # make the text match - doesn't currently work and I'm not sure how important it is to pursue
    theme(text = element_text(size = 20)) +

    # add title
    labs(title = "Seton Subject Totals")

# export ----------------------------------------------------------------------------------------------------------

  # save plot
  ggsave("plot_seton_scores.png", plot_scores, path = p_dir_out, width = 8, height = 4)

