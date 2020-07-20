#### Bayes Factor Agreement Plot: Direction ####

#' Function to plot the agreement in direction between Bayes factors for the Bosco data
#' @param BFBosco Dataset containing Bayes factors for Bosco data
#' @param elicit.stage Either "MATCH" or "Shiny"
#' @import ggplot2

plotBFDirectionBosco <- function(BFBosco, elicit.stage){

  # Reduce Bayes factor object
  BFred <- BFBosco[, c(grep(elicit.stage, colnames(BFBosco)),
                       grep("default", colnames(BFBosco)))]

  # Compare direction
  n <- seq_len(ncol(BFred))
  id <- expand.grid(n, n)
  out <- matrix(colSums((((BFred[ , id[,1] ] > 1) & (BFred[ , id[,2] ] > 1) ) | ((BFred[ , id[,1] ] < 1) & (BFred[ , id[,2] ] < 1) ))), ncol = length(n) )
  out <- out/nrow(BFred)

  # Prepare data for plot
  colnames(out) <- c(paste0("Expert", c(1:6)), "Default")
  rownames(out) <- colnames(out)
  out <- round(out, 2) # round percentages to 2 decimals
  out[lower.tri(out)] <- NA
  melted_out <- reshape2::melt(out, na.rm=TRUE)

  # Do the actual plotting
  p <- ggplot(data=melted_out, aes(Var2, Var1, fill=value)) +
    geom_tile(color="white") +
    scale_fill_gradient2(low="red", high="darkgreen", mid="lightgrey", name="Same Direction", limit=c(0,1), midpoint=0.5) +
    theme_void() +
    theme(axis.text.x = element_text(angle=45, vjust=1, size=12, hjust=1))+
    coord_fixed() +
    geom_text(aes(Var2, Var1, label = value), color = "white", size = 4)

  ggsave(filename=paste0("plot_BF_DirectionBosco_", elicit.stage,".pdf"),
         plot=p,
         device="pdf",
         path="../Figures/")
}


#' Function to plot the agreement in direction between Bayes factors for the Wetzels data
#' @param BFWetzels Dataset containing Bayes factors for Wetzels data
#' @param elicit.stage Either "MATCH" or "Shiny"
#' @param disttype Either "t" or "norm"
#' @import ggplot2

plotBFDirectionWetzels <- function(BFWetzels, elicit.stage, disttype){

  # Reduce Bayes factor object
  BFselect <- paste0("BF_", elicit.stage, "_", disttype, "_BFplus0")
  BFred <- BFWetzels[, c(grep(BFselect, colnames(BFWetzels)),
                       grep("BFplus0_default", colnames(BFWetzels)))]
  BFred <- BFred[!apply(BFred, 1, function(x){any(is.na(x))}), ]

  # Compare direction
  n <- seq_len(ncol(BFred))
  id <- expand.grid(n, n)
  out <- matrix(colSums((((BFred[ , id[,1] ] > 1) & (BFred[ , id[,2] ] > 1) ) | ((BFred[ , id[,1] ] < 1) & (BFred[ , id[,2] ] < 1) ))), ncol = length(n) )
  out <- out/nrow(BFred)

  # Prepare data for plot
  colnames(out) <- c(paste0("Expert", c(1:6)), "Default")
  rownames(out) <- colnames(out)
  out <- round(out, 2) # round percentages to 2 decimals
  out[lower.tri(out)] <- NA
  melted_out <- reshape2::melt(out, na.rm=TRUE)

  # Do the actual plotting
  p <- ggplot(data=melted_out, aes(Var2, Var1, fill=value)) +
    geom_tile(color="white") +
    scale_fill_gradient2(low="red", high="darkgreen", mid="lightgrey", name="Same Direction", limit=c(0,1), midpoint=0.5) +
    theme_void() +
    theme(axis.text.x = element_text(angle=45, vjust=1, size=12, hjust=1))+
    coord_fixed() +
    geom_text(aes(Var2, Var1, label = value), color = "white", size = 4)

  ggsave(filename=paste0("plot_BF_DirectionWetzels_", elicit.stage, "_", disttype, ".pdf"),
         plot=p,
         device="pdf",
         path="../Figures/")
}

#' Function to plot the agreement in evidence strength between Bayes factors for the Bosco data
#' @param BFBosco Dataset containing Bayes factors for Bosco data
#' @param elicit.stage Either "MATCH" or "Shiny"
#' @param threshold BF threshold
#' @import ggplot2

plotEvidenceChangeBosco <- function(BFBosco, elicit.stage, threshold){

  # Reduce Bayes factor object
  BFred <- BFBosco[, c(grep(elicit.stage, colnames(BFBosco)),
                       grep("default", colnames(BFBosco)))]

  # Compare direction
  n <- seq_len(ncol(BFred))
  id <- expand.grid(n, n)
  out <- matrix(colSums((
    ((BFred[ , id[,1] ] > threshold) & (BFred[ , id[,2] ] > threshold)) | # both are larger upper threshold OR
    ((BFred[ , id[,1] ] < 1/threshold) & (BFred[ , id[,2] ] < 1/threshold)) | # both are smaller lower threshold OR
    (((BFred[ , id[,1] ] > 1/threshold) & (BFred[ , id[,1] ] < threshold)) & ((BFred[ , id[,2] ] > 1/threshold) & (BFred[ , id[,2] ] < threshold))) # both are between thresholds
    )) , ncol = length(n) )
  out <- out/nrow(BFred)

  # Prepare data for plot
  colnames(out) <- c(paste0("Expert", c(1:6)), "Default")
  rownames(out) <- colnames(out)
  out <- round(out, 2) # round percentages to 2 decimals
  out[lower.tri(out)] <- NA
  melted_out <- reshape2::melt(out, na.rm=TRUE)

  # Do the actual plotting
  p <- ggplot(data=melted_out, aes(Var2, Var1, fill=value)) +
    geom_tile(color="white") +
    scale_fill_gradient2(low="red", high="darkgreen", mid="lightgrey", name="Same Evidence Category", limit=c(0,1), midpoint=0.5) +
    theme_void() +
    theme(axis.text.x = element_text(angle=45, vjust=1, size=12, hjust=1))+
    coord_fixed() +
    geom_text(aes(Var2, Var1, label = value), color = "white", size = 4)

  ggsave(filename=paste0("plot_EvidenceChange_Bosco_", elicit.stage,".pdf"),
         plot=p,
         device="pdf",
         path="../Figures/")
}

#' Function to plot the agreement in evidence strength between Bayes factors for the Wetzels data
#' @param BFBosco Dataset containing Bayes factors for Bosco data
#' @param elicit.stage Either "MATCH" or "Shiny"
#' @param disttype Either "t" or "norm"
#' @param threshold BF threshold
#' @import ggplot2

plotEvidenceChangeWetzels <- function(BFWetzels, elicit.stage, threshold, disttype){

  # Reduce Bayes factor object
  BFselect <- paste0("BF_", elicit.stage, "_", disttype, "_BFplus0")
  BFred <- BFWetzels[, c(grep(BFselect, colnames(BFWetzels)),
                         grep("BFplus0_default", colnames(BFWetzels)))]
  BFred <- BFred[!apply(BFred, 1, function(x){any(is.na(x))}), ]

  # Compare direction
  n <- seq_len(ncol(BFred))
  id <- expand.grid(n, n)
  out <- matrix(colSums((
    ((BFred[ , id[,1] ] > threshold) & (BFred[ , id[,2] ] > threshold)) | # both are larger upper threshold OR
    ((BFred[ , id[,1] ] < 1/threshold) & (BFred[ , id[,2] ] < 1/threshold)) | # both are smaller lower threshold OR
    (((BFred[ , id[,1] ] > 1/threshold) & (BFred[ , id[,1] ] < threshold)) & ((BFred[ , id[,2] ] > 1/threshold) & (BFred[ , id[,2] ] < threshold))) # both are between thresholds
  )) , ncol = length(n) )

  out <- out/nrow(BFred)

  # Prepare data for plot
  colnames(out) <- c(paste0("Expert", c(1:6)), "Default")
  rownames(out) <- colnames(out)
  out <- round(out, 2) # round percentages to 2 decimals
  out[lower.tri(out)] <- NA
  melted_out <- reshape2::melt(out, na.rm=TRUE)

  # Do the actual plotting
  p <- ggplot(data=melted_out, aes(Var2, Var1, fill=value)) +
    geom_tile(color="white") +
    scale_fill_gradient2(low="red", high="darkgreen", mid="lightgrey", name="Same Evidence Category", limit=c(0,1), midpoint=0.5) +
    theme_void() +
    theme(axis.text.x = element_text(angle=45, vjust=1, size=12, hjust=1))+
    coord_fixed() +
    geom_text(aes(Var2, Var1, label = value), color = "white", size = 4)

  ggsave(filename=paste0("plot_EvidenceChange_Wetzels_", elicit.stage, "_", disttype, ".pdf"),
         plot=p,
         device="pdf",
         path="../Figures/")
}


