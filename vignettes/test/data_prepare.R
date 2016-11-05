

#############  Data preparation  ###################################


himalayan_fauna_3_clusters <- get(load("../../data/himalayan_fauna_3_clusters.rda"))
devtools::use_data(himalayan_fauna_3_clusters, overwrite=TRUE)


himalayan_fauna_2_clusters <- get(load("../../data/himalayan_fauna_2_clusters.rda"))
devtools::use_data(himalayan_fauna_2_clusters, overwrite=TRUE)
