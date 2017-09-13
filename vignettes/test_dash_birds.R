
################  Testing dash as a pre-processing step before GoM model  ############################

########  Here we apply dash on the bird abundance data due to A. White et al (2017)  #################



library(devtools)

#install_github("kkdey/ecostructure")
library(ecostructure)
library(Biobase)
library(Logolas)

data <- get(load(system.file("extdata", "HimalayanBirdsData.rda",
                             package = "ecostructure")))
taxonomic_counts <- t(exprs(data))

rowSums(taxonomic_counts)

taxonomic_counts_1 <- taxonomic_counts +1

m1 <- colMeans(taxonomic_counts_1)
m2 <- colMeans(taxonomic_counts)

system.time(out <- dash(comp_data = taxonomic_counts,
                        optmethod = "mixEM",
                        mode = m2,
                        def_positions = list("center" = Inf, "null" = 1, "corner" = 1),
                        concentration = c(Inf, 100, 50, 20, 10, 5, 2, 1),
                        weight = list("center" = 1, "null" = 1, "corner" = 1),
                        bf = TRUE,
                        verbose=TRUE))

plot(out$posterior_weights[,1], rowSums(taxonomic_counts))


grid_metadata <- pData(phenoData(data))
head(grid_metadata)

elevation_metadata=grid_metadata$Elevation;
east_west_dir = grid_metadata$WorE;
gom_fit <- CountClust::FitGoM(taxonomic_counts, K=2:4, tol=0.1)

omega <- gom_fit[[2]]$omega
theta <- gom_fit[[2]]$theta

BlockStructure(omega, blocker_metadata = east_west_dir,
               order_metadata = elevation_metadata,
               yaxis_label = "Elevation",
               levels_decreasing = FALSE)


idx <- which(out$posterior_weights[,1] < 0.01)
tmp <- matrix(0, dim(out$datamean)[1], dim(out$datamean)[2])
tmp[idx, ] <- out$datamean[idx, ]
tmp[-idx, ] <- out$posmean[-idx, ]

mod_counts <- round((rowSums(taxonomic_counts) %*% t(rep(1, dim(taxonomic_counts)[2])))*tmp)
rownames(mod_counts) <- rownames(taxonomic_counts)
colnames(mod_counts) <- colnames(taxonomic_counts)

gom_fit_2 <- CountClust::FitGoM(mod_counts, K=2:4, tol=0.1)


omega2 <- gom_fit_2[[2]]$omega
theta2 <- gom_fit_2[[2]]$theta

BlockStructure(omega2, blocker_metadata = east_west_dir,
               order_metadata = elevation_metadata,
               yaxis_label = "Elevation",
               levels_decreasing = FALSE)
