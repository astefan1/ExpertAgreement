#### Bayes Factor Agreement Plot: Direction ####

#' Function to plot the agreement in direction between Bayes factors for the Bosco data
#' @param BFBosco Dataset containing Bayes factors for Bosco data
#' @param elicit.stage Either "MATCH" or "Shiny"
#' @import ggplot2
#' @importFrom rlang .data

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
  out[lower.tri(out)] <- NA
  melted_out <- reshape2::melt(out, na.rm=TRUE)
  melted_out$label <- paste0(round(melted_out$value*100, 1), "%")

  # Do the actual plotting
  p <- ggplot(data=melted_out, aes(.data$Var2, .data$Var1, fill=.data$value)) +
    geom_tile(color="white") +
    scale_fill_gradient2(low="red", high="darkgreen", mid="gold2",
                         name="Agreement on Direction", limit=c(0,1), midpoint=0.5,
                         labels=c("0%", "25%", "50%", "75%", "100%"), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle=45, vjust=1, size=18, hjust=1),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size=18),
          plot.title = element_text(size=20),
          legend.position = c(0.11,0.82),
          legend.text = element_text(size=16),
          legend.title = element_text(size=18),
          legend.spacing.y = unit(10, "pt"),
          legend.key.height = unit(25, "pt"))+
    coord_fixed() +
    geom_text(aes(.data$Var2, .data$Var1, label = .data$label), color = "white", size = 7) +
    scale_y_discrete(position="right") +
    ggtitle("Data: Bosco et al. (2015)")

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
#' @importFrom rlang .data

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
  out[lower.tri(out)] <- NA
  melted_out <- reshape2::melt(out, na.rm=TRUE)
  melted_out$label <- paste0(round(melted_out$value*100, 1), "%")

  # Do the actual plotting
  p <- ggplot(data=melted_out, aes(.data$Var2, .data$Var1, fill=.data$value)) +
    geom_tile(color="white") +
    scale_fill_gradient2(low="red", high="darkgreen", mid="gold2",
                         name="Agreement on Direction", limit=c(0,1), midpoint=0.5,
                         labels=c("0%", "25%", "50%", "75%", "100%"), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle=45, vjust=1, size=18, hjust=1),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size=18),
          plot.title = element_text(size=20),
          legend.position = c(0.11,0.82),
          legend.text = element_text(size=16),
          legend.title = element_text(size=18),
          legend.spacing.y = unit(10, "pt"),
          legend.key.height = unit(25, "pt"))+
    coord_fixed() +
    geom_text(aes(.data$Var2, .data$Var1, label = .data$label), color = "white", size = 7) +
    scale_y_discrete(position="right") +
    ggtitle("Data: Wetzels et al. (2011)")

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
#' @importFrom rlang .data

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
  out[lower.tri(out)] <- NA
  melted_out <- reshape2::melt(out, na.rm=TRUE)
  melted_out$label <- paste0(round(melted_out$value*100, 1), "%")

  # Do the actual plotting
  p <- ggplot(data=melted_out, aes(.data$Var2, .data$Var1, fill=.data$value)) +
    geom_tile(color="white") +
    scale_fill_gradient2(low="red", high="darkgreen", mid="gold2",
                         name="Agreement on Evidence Category", limit=c(0,1), midpoint=0.5,
                         labels=c("0%", "25%", "50%", "75%", "100%"), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle=45, vjust=1, size=18, hjust=1),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size=18),
          plot.title = element_text(size=20),
          legend.position = c(0.25,0.82),
          legend.text = element_text(size=16),
          legend.title = element_text(size=18),
          legend.spacing.y = unit(10, "pt"),
          legend.key.height = unit(25, "pt"))+
    coord_fixed() +
    geom_text(aes(.data$Var2, .data$Var1, label = .data$label), color = "white", size = 7) +
    scale_y_discrete(position="right") +
    ggtitle("Data: Bosco et al. (2015)")

  ggsave(filename=paste0("plot_EvidenceChange_Bosco_", elicit.stage,".pdf"),
         plot=p,
         device="pdf",
         path="../Figures/")
}

#' Function to plot the agreement in evidence strength between Bayes factors for the Wetzels data
#' @param BFWetzels Dataset containing Bayes factors for Bosco data
#' @param elicit.stage Either "MATCH" or "Shiny"
#' @param disttype Either "t" or "norm"
#' @param threshold BF threshold
#' @import ggplot2
#' @importFrom rlang .data

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
  out[lower.tri(out)] <- NA
  melted_out <- reshape2::melt(out, na.rm=TRUE)
  melted_out$label <- paste0(round(melted_out$value*100, 1), "%")

  # Do the actual plotting
  p <- ggplot(data=melted_out, aes(.data$Var2, .data$Var1, fill=.data$value)) +
    geom_tile(color="white") +
    scale_fill_gradient2(low="red", high="darkgreen", mid="gold2",
                         name="Agreement on Evidence Category", limit=c(0,1), midpoint=0.5,
                         labels=c("0%", "25%", "50%", "75%", "100%"), breaks = c(0, 0.25, 0.5, 0.75, 1)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle=45, vjust=1, size=18, hjust=1),
          panel.background = element_blank(),
          panel.grid.major = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size=18),
          plot.title = element_text(size=20),
          legend.position = c(0.25,0.82),
          legend.text = element_text(size=16),
          legend.title = element_text(size=18),
          legend.spacing.y = unit(10, "pt"),
          legend.key.height = unit(25, "pt"))+
    coord_fixed() +
    geom_text(aes(.data$Var2, .data$Var1, label = .data$label), color = "white", size = 7) +
    scale_y_discrete(position="right") +
    ggtitle("Data: Wetzels et al. (2011)")

  ggsave(filename=paste0("plot_EvidenceChange_Wetzels_", elicit.stage, "_", disttype, ".pdf"),
         plot=p,
         device="pdf",
         path="../Figures/")
}


