

########   Logolas plot for the bird distribution  ##########################

library(HimalayanBirdsAbundance)
data("HimalayanBirdsAbundance")
new_counts <- t(exprs(HimalayanBirdsAbundance));
metadata <- pData(HimalayanBirdsAbundance);

elevation_metadata=metadata$Elevation;
east_west_dir = metadata$WorE;


library(Logolas)

out <- CountClust::FitGoMpool(new_counts, K=2, tol=0.1,
                              burn_trials = 100)

theta <- out$topic_fit$theta

species_names <- rownames(theta)
families <- unlist(lapply(species_names, function(x) return(strsplit(x, "[_]")[[1]][1])))

signature_prob <- matrix(0, length(unique(families)), dim(theta)[2])

for(l in 1:dim(theta)[2]){
    signature_prob[,l] <- tapply(theta[,l], factor(families), sum)
}

colSums(signature_prob)

signature_prob[signature_prob < 0.01] = 0
signature_prob <- apply(signature_prob, 2, function(x) return(x/sum(x)))

rownames(signature_prob) <- levels(factor(families))
colnames(signature_prob) <- c("Cluster1", "Cluster 2")

logomaker(signature_prob,
          cols= sample(RColorBrewer::brewer.pal(10,name = "Spectral"),
                       dim(signature_prob)[1], replace=TRUE),
          frame_width = 1,
          ic.scale = TRUE,
          pop_name = "Bird family abundance across clusters",
          xlab = "Clusters",
          ylab = "Information content")


out <- CountClust::FitGoMpool(new_counts, K=3, tol=0.1,
                              burn_trials = 100)

theta <- out$topic_fit$theta

species_names <- rownames(theta)
families <- unlist(lapply(species_names, function(x) return(strsplit(x, "[_]")[[1]][1])))

signature_prob <- matrix(0, length(unique(families)), dim(theta)[2])

for(l in 1:dim(theta)[2]){
  signature_prob[,l] <- tapply(theta[,l], factor(families), sum)
}

colSums(signature_prob)

signature_prob[signature_prob < 0.01] = 0
signature_prob <- apply(signature_prob, 2, function(x) return(x/sum(x)))

rownames(signature_prob) <- levels(factor(families))
colnames(signature_prob) <- c("Cluster1", "Cluster 2", "Cluster 3")

logomaker(signature_prob,
          cols= sample(RColorBrewer::brewer.pal(10,name = "Spectral"),
                       dim(signature_prob)[1], replace=TRUE),
          frame_width = 1,
          ic.scale = TRUE,
          pop_name = "Bird family abundance across clusters",
          xlab = "Clusters",
          ylab = "Information content")





