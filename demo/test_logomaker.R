

##############  test new logomaker   #######################

sequence <- c("CTATTGT", "CTCTTAT", "CTATTAA", "CTATTTA", "CTATTAT", "CTTGAAT",
              "CTTAGAT", "CTATTAA", "CTATTTA", "CTATTAT", "CTTTTAT", "CTATAGT",
              "CTATTTT", "CTTATAT", "CTATATT", "CTCATTT", "CTTATTT", "CAATAGT",
              "CATTTGA", "CTCTTAT", "CTATTAT", "CTTTTAT", "CTATAAT", "CTTAGGT",
              "CTATTGT", "CTCATGT", "CTATAGT", "CTCGTTA", "CTAGAAT", "CAATGGT")

logomaker(sequence, type = "Logo")
logomaker(sequence, type = "EDLogo")


counts_mat <- rbind(c(0, 10, 100, 60, 20),
                    c(40, 30, 30, 35, 20),
                    c(100, 0, 15, 25, 75),
                    c(10, 30, 20, 50, 70))
colnames(counts_mat) <- c("2012", "2013", "2014", "2015", "2016")
rownames(counts_mat) <- c("MAN", "MAIL", "LAWN", "CAR")

logomaker(counts_mat, type = "Logo")
logomaker(counts_mat, type = "EDLogo")

p <- data("seqlogo_example")
logomaker(p, type = "Logo", return_heights = TRUE)
logomaker(p, type = "EDLogo", return_heights = TRUE)

logomaker(p, color_type = "per_row", colors = c("#7FC97F", "#BEAED4", "#FDC086", "#386CB0"), type = "Logo")
logomaker(p, color_type = "per_row", colors = c("#7FC97F", "#BEAED4", "#FDC086", "#386CB0"), type = "EDLogo")


##################  ELF1 data  ###############################

data("ELF1_PWM")
Logolas::get_viewport_logo(4, 2, heights.val = 15)
library(grid)
names <- c("Homo sapiens",
           "Drosophila melanogaster",
           "Neurospora crassa",
           "Toxoplasma gondii",
           "Oryza sativa",
           "Ustilago maydis",
           "Saccharomyces cerevisiae")
for(m in 1:length(ELF1_PWM)){
  seekViewport(paste0("plotlogo", m))
  Logolas::logomaker(ELF1_PWM[[m]], type = "EDLogo", 
            color_type = "per_row", colors = c("#7FC97F", "#BEAED4", "#FDC086", "#386CB0"),
            logo_control = list(newpage = FALSE,
                                pop_name = paste0(names[m]),
                                control = list(negbins = 2, posbins = 3),
                                xlab = ""))
}

Logolas::get_viewport_logo(4, 2, heights.val = 15)
for(m in 1:length(ELF1_PWM)){
  seekViewport(paste0("plotlogo", m))
  Logolas::logomaker(ELF1_PWM[[m]], type = "Logo", 
                     color_type = "per_row", colors = c("#7FC97F", "#BEAED4", "#FDC086", "#386CB0"),
                     logo_control = list(newpage = FALSE,
                                         pop_name = paste0(names[m]),
                                         control = list(totbins = 3),
                                         xlab = ""))
}




###############  save this image as 13 by 13 pdf   #######################




