packages <- c('readr','dplyr','tidyr','stringr','forcats','ggplot2',
              'patchwork','RColorBrewer','metafor','scales','svglite','ragg',
              'igraph','ape','ggrepel')
cat('Runtime check:', format(Sys.time(), usetz=TRUE), '\n')
cat(R.version.string, '\n')
print(Sys.getlocale())
print(.libPaths())
available <- vapply(packages, requireNamespace, logical(1), quietly=TRUE)
print(available)
stopifnot(all(available))
print(sessionInfo())
cat('All final-render dependencies loaded successfully.\n')
