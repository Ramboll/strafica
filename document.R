# -*- coding: us-ascii-unix -*-
if (!("roxygen2" %in% rownames(installed.packages())))
    install.packages("roxygen2", repos="http://cran.rstudio.com/")
library(roxygen2)
unlink("man", recursive=TRUE)
unlink("NAMESPACE")
roxygenize()
