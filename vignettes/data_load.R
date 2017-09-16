

#################  Data Loading in Logolas   #######################


## this script checks if the loading of the data required for running the apps.Rmd
## vignette in Logolas works correctly or not.

data <- get(load(system.file("extdata", "ALL.2.Rdata",
                             package = "Logolas")))
dat <- read.table("inst/extdata/sig_16.txt")
save(dat, file = "inst/extdata/sig_16.rda")
