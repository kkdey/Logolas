

#################  Data Loading in Logolas   #######################


## this script checks if the loading of the data required for running the apps.Rmd
## vignette in Logolas works correctly or not.

data <- get(load(system.file("extdata", "ALL.2.Rdata",
                             package = "Logolas")))
