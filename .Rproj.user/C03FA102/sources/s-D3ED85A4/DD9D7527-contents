#### Data Preprocessing Meta Script ####

# Execute this script to run all data preprocessing steps
source("./data-raw/ElicitationResults.R")
source("./data-raw/WetzelsEtAl.R")
source("./data-raw/BoscoEtAl.R") # this takes 3-5 minutes to run

# Make data available in the package
usethis::use_data(BoscoEtAl,
                  WetzelsEtAl,
                  ExpertsPriors_tTest,
                  ExpertsPriors_cor,
                  internal=TRUE,
                  overwrite = TRUE)

