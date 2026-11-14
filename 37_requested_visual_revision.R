# Update requested panels without recomputing or replacing subgroup outputs.
local({
  retained <- list.files("figures/main",pattern="^Fig[24]_",full.names=TRUE)
  backup <- tempfile("subgroup_figure_backup_")
  dir.create(backup)
  file.copy(retained,backup,overwrite=TRUE)
  on.exit(file.copy(file.path(backup,basename(retained)),retained,overwrite=TRUE),add=TRUE)
  source("R/00_setup.R")
  source("R/09_reference_deviation.R")
  source("R/22_rebuild_submission_main_figures.R")
})
