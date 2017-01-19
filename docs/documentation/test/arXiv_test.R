


############   aRxiv data 

library(aRxiv)
rec1 <- arxiv_search('au:"Matthew Stephens"', limit=50)
rec2 <- arxiv_search('au:"John Lafferty"', limit=50)
rec3 <- arxiv_search('au:"Wei Biao Wu"', limit=50)
rec4 <- arxiv_search('au:"Peter Mccullagh"', limit=50)

primary_categories_1 <- toupper(rec1$primary_category)
primary_categories_2 <- toupper(rec2$primary_category)
primary_categories_3 <- toupper(rec3$primary_category)
primary_categories_4 <- toupper(rec4$primary_category)

factor_levels <- unique(c(unique(primary_categories_1),
                   unique(primary_categories_2),
                   unique(primary_categories_3),
                   unique(primary_categories_4)))

primary_categories_1 <- factor(primary_categories_1, levels=factor_levels)
primary_categories_2 <- factor(primary_categories_2, levels=factor_levels)
primary_categories_3 <- factor(primary_categories_3, levels=factor_levels)
primary_categories_4 <- factor(primary_categories_4, levels=factor_levels)


tab_data <- cbind(table(primary_categories_1),
                  table(primary_categories_2),
                  table(primary_categories_3),
                  table(primary_categories_4))

colnames(tab_data) <- c("Matthew Stephens",
                        "John Lafferty",
                        "Wei Biao Wu",
                        "Peter McCullagh")

tab_data <- as.matrix(tab_data)

logomaker(tab_data,
          cols= RColorBrewer::brewer.pal(dim(tab_data)[1],name = "Spectral"),
          frame_width = 1,
          ic.scale = TRUE,
          pop_name = "arXiv field categories of UChicago STAT professors",
          xlab = "Professors",
          ylab = "Information content")






