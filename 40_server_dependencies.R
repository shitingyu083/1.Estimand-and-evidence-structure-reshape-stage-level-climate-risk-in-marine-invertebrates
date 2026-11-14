options(repos=c(CRAN="https://cloud.r-project.org"), timeout=300)
task_lib <- path.expand("~/R/ER_boundary_revision_library")
dir.create(task_lib, recursive=TRUE, showWarnings=FALSE)
.libPaths(c(task_lib, .libPaths()))
required <- c("clubSandwich", "robumeta", "metafor", "readr", "dplyr")
missing <- required[!vapply(required, requireNamespace, quietly=TRUE, FUN.VALUE=logical(1))]
if(length(missing)) install.packages(missing, lib=task_lib, Ncpus=2)
for(p in required) {
  if(!requireNamespace(p,quietly=TRUE)) stop("Dependency remains unavailable: ",p)
  cat(p, as.character(packageVersion(p)), find.package(p), "\n")
}
sessionInfo()
