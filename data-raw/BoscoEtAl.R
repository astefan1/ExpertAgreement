#### Code to prepare `BoscoEtAl` dataset ####

# Download dataset from http://www.frankbosco.com/data/CorrelationalEffectSizeBenchmarks.html
# Dataset version: 2.08
# Save in the same folder as code project

# Read in dataset
library("readxl")
BoscoEtAl <- read_excel("../Ver2-08_MasterDB_JAP-PPsych_1980-2010.xlsx", sheet = 2)
BoscoEtAl <- as.data.frame(BoscoEtAl[,1:9])
BoscoEtAl$RowID <- BoscoEtAl$RowID-1

# Define a study vector that identifies single studies (as opposed to articles)
corNameRows <- which(is.na(BoscoEtAl$VariableName))
newstudy <- c(1,corNameRows[diff(corNameRows) > 1]+1,nrow(BoscoEtAl)+1)
BoscoEtAl$studyID <- rep(seq_along(diff(newstudy)), diff(newstudy))

# Rename variable names where capitalization was inconsistent
  # study 381
  BoscoEtAl[23907,6] <- "Level 2: Group Performance"
  BoscoEtAl[23920,8] <- "Level 2: Group Performance"
  BoscoEtAl[23906,6] <- "Level 2: Trust for leader"
  BoscoEtAl[23920,7] <- "Level 2: Trust for leader"

  # study 613
  BoscoEtAl[39600,6] <- "Interaction Adjustment: Hong Kong Sample"
  BoscoEtAl[39601,6] <- "Work Adjustment: Hong Kong Sample"
  BoscoEtAl[39603,6] <- "Contextual Performance: Hong Kong Sample"
  BoscoEtAl[39604,6] <- "Task Performance: Hong Kong Sample"

  # study 1095
  BoscoEtAl[69671,6] <- "direct report ratings 2000"

  # study 3143
  BoscoEtAl[194199,6] <- "Job level"

  # study 3223
  BoscoEtAl[197217:197231,6] <- c("Company Policies and Practices", "Compensation",
                                  "Co-workers", "Creativity", "Independence",
                                  "Job Security", "Moral Values", "Recognition",
                                  "Responsibility", "Social Service",
                                  "Social Status", "Supervision-technical competence",
                                  "Supervision-human relations", "Variety",
                                  "Working Conditions"
  )

  BoscoEtAl[197233:197236,6] <- c("Ability Utilization", "Achievement",
                                  "Activity", "Advancement")

  # study 1200
  BoscoEtAl[77690,6] <- "WTS Conflict"
  BoscoEtAl[77698,7] <- "WTS Conflict"

  # study 1634
  BoscoEtAl[109360,8] <- "Teamwork"
  BoscoEtAl[109362,8] <- "Planning"
  BoscoEtAl[109364,8] <- "Planning"

  # study 1635
  BoscoEtAl[109370,8] <- "Teamwork"
  BoscoEtAl[109372,8] <- "Planning"
  BoscoEtAl[109374,8] <- "Planning"

  # study 1636
  BoscoEtAl[109380,8] <- "Teamwork"
  BoscoEtAl[109382,8] <- "Planning"
  BoscoEtAl[109384,8] <- "Planning"

  # study 1807
  BoscoEtAl[c(122721:122723,122727,122729),8] <- "Job satisfaction"

  # study 2210
  BoscoEtAl[145995,6] <- "Preference for Numerical Information - B"

  # study 2228
  BoscoEtAl[147012,6] <- "Assertive Job-Hunting"

  # study 2229
  BoscoEtAl[147029,6] <- "Assertive Job-Hunting"

  # study 3050
  BoscoEtAl[189481,8] <- "Leisure ethic"

  # study 3051
  BoscoEtAl[189487,8] <- "Leisure ethic"

  # study 3415
  BoscoEtAl[205959,6] <- "kcal"
  BoscoEtAl[205960,6] <- "mets"

# Find N1 and N2 (sample size for X1 and X2) and taxonomy ID
BoscoEtAl[,11:14] <- NA
names(BoscoEtAl)[11:14] <- c("N1", "N2", "TaxID1", "TaxID2")
for(i in unique(BoscoEtAl$studyID)){
  if(i %in% seq(1, 3441, by = 50)) print(i)
  studyData <- BoscoEtAl[BoscoEtAl$studyID==i,]
  for(j in as.character(na.omit(studyData$VariableName))){
    studyData$N1[studyData$X1==j] <- studyData$N[studyData$VariableName==j & is.na(studyData$r)]
    studyData$N2[studyData$X2==j] <- studyData$N[studyData$VariableName==j & is.na(studyData$r)]
    studyData$TaxID1[studyData$X1==j] <- studyData$TaxonomyID[studyData$VariableName==j & is.na(studyData$r)]
    studyData$TaxID2[studyData$X2==j] <- studyData$TaxonomyID[studyData$VariableName==j & is.na(studyData$r)]
  }
  BoscoEtAl[BoscoEtAl$studyID==i,] <- studyData
}

# Remove unnecessary rows and columns
noCor <- which(is.na(BoscoEtAl$r))
BoscoEtAl <- BoscoEtAl[-noCor,] # remove variable definition sections
BoscoEtAl <- BoscoEtAl[,-c(4:6)] # remove empty columns

# Check if N1 and N2 are always the same, if not remove row
BoscoEtAl <- BoscoEtAl[BoscoEtAl$N1 == BoscoEtAl$N2,]

# Check if all SS are integers, if not remove
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) abs(x-round(x)) < tol
BoscoEtAl <- BoscoEtAl[is.wholenumber(BoscoEtAl$N1),]

# Remove all perfect correlations (r == 1 | r == -1)
BoscoEtAl <- BoscoEtAl[!BoscoEtAl$r %in% c(-1,1), ]

# Remove correlations with unrealistically large sample size
BoscoEtAl <- BoscoEtAl[!BoscoEtAl$N1 > 500, ]

# Remove correlations with very small sample sizes
BoscoEtAl <- BoscoEtAl[BoscoEtAl$N1 >= 10, ]

# Rename column N1 to N and remove unnecessary column N2
colnames(BoscoEtAl)[8] <- "N"
BoscoEtAl <- BoscoEtAl[, -9]
