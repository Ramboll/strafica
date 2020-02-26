# -*- coding: us-ascii-unix -*-
DIRECTORY = dirname(parent.frame(2)$ofile)
suppressMessages(library(strafica, warn.conflicts=FALSE))
invisible(lapply(Sys.glob(sprintf("%s/R/*.R", DIRECTORY)), source))
