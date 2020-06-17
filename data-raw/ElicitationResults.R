#### Read in Elicitation Results ####

# MATCH Tool

MATCH_tTestFiles <- list.files("../elicitation_data/", pattern = "^Expert")[c(F,T)]
MATCH_tTestFilePath <- paste0("../elicitation_data/", MATCH_tTestFiles)

MATCH_corFiles <- list.files("../elicitation_data/", pattern = "^Expert")[c(T,F)]
MATCH_corFilePath <- paste0("../elicitation_data/", MATCH_corFiles)

ExpertsPriors_tTest <- MATCH_extract_tTest(MATCH_tTestFilePath)
ExpertsPriors_cor <- MATCH_extract_cor(MATCH_corFilePath)

# Shiny
# elicitation results from ../elicitation_data/best_fitted_parameters.txt

ShinytTest <- matrix(c(0.1, 0.1218545, 0.1, 0.12, 3,
                       0.5500002, 0.102205, 0.5500025, 0.08142563, 3,
                       0.599999, 0.1361149, 0.6000279, 0.1076721, 13,
                       0.5875679, 0.1342709, 0.5853842, 0.1073029, 3,
                       0.4049102, 0.1216028, 0.4052126, 0.1167314, 13,
                       0.3176305, 0.08394974, 0.31064931, 0.07525807, 9),
                     nrow=6, byrow=T)

ShinyCor <- matrix(c(0.626195, 22.44102,
                     5.3268021, 18.58188,
                     5.355801, 15.69369,
                     10.7009, 22.98733,
                     3.834643, 8.76432,
                     8.651692, 12.393982),
                   nrow=6, byrow=T)

ExpertsPriors_tTest[, 29:33] <- ShinytTest
colnames(ExpertsPriors_tTest)[29:33] <- c("Shiny_mu_norm", "Shiny_sigma_norm",
                                          "Shiny_mu_t", "Shiny_scale_t",
                                          "Shiny_df_t")

ExpertsPriors_cor[26:27] <- ShinyCor
colnames(ExpertsPriors_cor)[26:27] <- c("Shiny_beta_a", "Shiny_beta_b")
