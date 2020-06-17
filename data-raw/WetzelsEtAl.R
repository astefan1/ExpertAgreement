#### Code to prepare WetzelsEtAl dataset ####

library(tibble)

# Read in 3 separate datasets

Wetzels1 <- read.table("../Wetzels_ES-data/data_onesample.txt", header = T)
Wetzels2 <- read.table("../Wetzels_ES-data/data_pairedsample.txt", header = T)
Wetzels3 <- read.table("../Wetzels_ES-data/data_twosample.txt", header = T)

# Add second sample size column to one-sided tests to align data structure
N2 <- rep(NULL, 85)
Wetzels1 <- tibble::add_column(Wetzels1, N2=0, .after = "N")
colnames(Wetzels1)[1] <- "N1"

# Add t-test type to datasets
Wetzels1$type <- "onesample"
Wetzels2$type <- "pairedsample"
Wetzels3$type <- "twosample"

# Combine datasets to one big dataset
WetzelsEtAl <- rbind(Wetzels1, Wetzels2, Wetzels3)
WetzelsEtAl$is.twosample <- ifelse(WetzelsEtAl$type == "twosample", 1, 0)
