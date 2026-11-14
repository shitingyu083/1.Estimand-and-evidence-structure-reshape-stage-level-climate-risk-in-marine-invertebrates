lines <- readLines("R/22_rebuild_submission_main_figures.R",warn=FALSE)
end <- grep('save_figure\\(fig1,',lines)[1]
eval(parse(text=lines[seq_len(end)]),envir=.GlobalEnv)
