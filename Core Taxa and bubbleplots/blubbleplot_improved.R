library(ggplot2)
library(dplyr)
library(ggnewscale)
library(scales)
library(sysfonts)
library(tidyverse)
library(glue)
library(ggtext)
library(showtext)
font_paths()
font_files() %>% tibble() %>% filter(str_detect(family,"Trebu"))
font_add(family = "Trebuchet", regular = "trebuc.ttf")
showtext_auto()
plot_bubble <- function(file = NULL, file2 = NULL, file3 = NULL, file4 = NULL, file5 = NULL, file6 = NULL, file7 = NULL, file8 = NULL, file9 = NULL, file10 = NULL, esv_lower_lim = 3, esv_upper_lim = 200000, ra_lower_lim = 0.001, ra_upper_lim = 1, con_unid = NULL, b1 = NULL, b2 = NULL, b3 = NULL, b4 = NULL, b5 = NULL, b6 = NULL, b7 = NULL) {
  bubble_data <- read.csv(file, header=TRUE)
  bubble_data <- bubble_data %>%
    filter(ESVs >= esv_lower_lim, ESVs <= esv_upper_lim,
           REL_ABUND >= ra_lower_lim, REL_ABUND <= ra_upper_lim,
           grepl("^unid", CLADE, ignore.case = TRUE) == con_unid)
  p2 <- bubble_data %>%
    ggplot() + 
    geom_point(aes(x = REL_ABUND, y = CLADE, size = ESVs, color = ESVs), 
               alpha = 0.8, shape = 15, stroke = 1.25, na.rm = TRUE) + 
    theme_classic() + 
    xlab("Relative Abundance") + 
    ylab("Clades") + 
#Bubble Plot of core unidentified species families of bacteria fungi in conventional and organic management; Bubble Plot of core unidentified species families of bacteria fungi in Capsicum annuum, C. baccatum, C. chinense and hybrids AxA and AxCh; Bubble Plot of core unidentified species families of bacteria fungi in Bola, BOL-58, Cali. Red, ECU-994, Piquillo, Serrano and hybrids SerXBola and CaliXECU
    ggtitle(paste("Bubble Plot of core species of fungi in conventional and organic management", (if (con_unid == FALSE) {"(no unid)"} else  {"only unid"}))) +
    theme(text = element_text(family = "Trebuchet", size = 12)) +
    theme(plot.title = element_text(family = "Trebuchet",hjust = 0.5, size = 11)) +
    theme(axis.title = element_text(family = "Trebuchet",color = "black", size = 8, face = "bold")) +
    theme(axis.text = element_text(family = "Trebuchet",color = "black", size = 8, face = "bold")) +
    theme(legend.text = element_text(family = "Trebuchet",colour = "black", size = 10, face = "bold")) +
    theme(legend.title = element_text(family = "Trebuchet",colour = "black", size = 10, face = "bold")) +
    scale_x_continuous(limits = c(0.0, 0.4), labels = percent_format()) + #fungi: limits = c(0.0, 0.4); bacteria: limits = c(0.0, 0.25)
    scale_color_gradientn(name = (paste("ESVs of", gsub("_BUBBLEPLOT.csv", "", file))),
                          breaks = c(b1, b2, b3, b4, b5, b6, b7), #fungi:5000, 15000, 30000, 60000, 120000, 240000, 480000; bacteria:500, 1500, 3000, 6000, 12000, 24000, 48000
                          colours = c("#6d4408", "#6d4408", "#6d4408","#6d4408", "#6d4408", "#6d4408")) +
    scale_size_continuous(name = "Number of ESVs",
                          #trans = "log10",
                          breaks = c(b1, b2, b3, b4, b5, b6, b7),
                          range = c(0, 8))
  if (!is.null(file2)) {
    bubble_data2 <- read.csv(file2, header=TRUE)
    bubble_data2 <- bubble_data2 %>%
      filter(ESVs >= esv_lower_lim, ESVs <= esv_upper_lim,
             REL_ABUND >= ra_lower_lim, REL_ABUND <= ra_upper_lim,
             grepl("^unid", CLADE, ignore.case = TRUE) == con_unid)
    p2 <- p2 +
      new_scale_color() +
      geom_point(data=bubble_data2, aes(x = REL_ABUND, y = (reorder (CLADE, ESVs)), size = ESVs, color = ESVs),
                 alpha = 0.8, shape = 16, stroke = 1.25, na.rm = TRUE) +
      scale_color_gradientn(name = (paste("ESVs of", gsub("_BUBBLEPLOT.csv", "", file2))),
                            colours = c("#006d2c", "#006d2c", "#006d2c","#006d2c", "#006d2c", "#006d2c"))
  }
  if (!is.null(file3)) {
    bubble_data2 <- read.csv(file3, header=TRUE)
    bubble_data2 <- bubble_data2 %>%
      filter(ESVs >= esv_lower_lim, ESVs <= esv_upper_lim,
             REL_ABUND >= ra_lower_lim, REL_ABUND <= ra_upper_lim,
             grepl("^unid", CLADE, ignore.case = TRUE) == con_unid)
    p2 <- p2 +
      new_scale_color() +
      geom_point(data=bubble_data2, aes(x = REL_ABUND, y = (reorder (CLADE, ESVs)), size = ESVs, color = ESVs),
                 alpha = 0.8, shape = 1, stroke = 1.25, na.rm = TRUE) +
      scale_color_gradientn(name = (paste("ESVs of", gsub("_BUBBLEPLOT.csv", "", file3))),
                            colours = c("#98df8a", "#98df8a", "#98df8a","#98df8a", "#98df8a", "#98df8a"))
  }
  if (!is.null(file4)) {
    bubble_data2 <- read.csv(file4, header=TRUE)
    bubble_data2 <- bubble_data2 %>%
      filter(ESVs >= esv_lower_lim, ESVs <= esv_upper_lim,
             REL_ABUND >= ra_lower_lim, REL_ABUND <= ra_upper_lim,
             grepl("^unid", CLADE, ignore.case = TRUE) == con_unid)
    p2 <- p2 +
      new_scale_color() +
      geom_point(data=bubble_data2, aes(x = REL_ABUND, y = (reorder (CLADE, ESVs)), size = ESVs, color = ESVs),
                 alpha = 0.8, shape = 2, stroke = 1.25, na.rm = TRUE) +
      scale_color_gradientn(name = (paste("ESVs of", gsub("_BUBBLEPLOT.csv", "", file4))),
                            colours = c("#0b5fa5", "#0b5fa5", "#0b5fa5","#0b5fa5", "#0b5fa5", "#0b5fa5"))
  }
  if (!is.null(file5)) {
    bubble_data2 <- read.csv(file5, header=TRUE)
    bubble_data2 <- bubble_data2 %>%
      filter(ESVs >= esv_lower_lim, ESVs <= esv_upper_lim,
             REL_ABUND >= ra_lower_lim, REL_ABUND <= ra_upper_lim,
             grepl("^unid", CLADE, ignore.case = TRUE) == con_unid)
    p2 <- p2 +
      new_scale_color() +
      geom_point(data=bubble_data2, aes(x = REL_ABUND, y = (reorder (CLADE, ESVs)), size = ESVs, color = ESVs),
                 alpha = 0.8, shape = 3, stroke = 1.25, na.rm = TRUE) +
      scale_color_gradientn(name = (paste("ESVs of", gsub("_BUBBLEPLOT.csv", "", file5))),
                            colours = c("#9e0d0f", "#9e0d0f", "#9e0d0f","#9e0d0f", "#9e0d0f", "#9e0d0f"))
  }
  if (!is.null(file6)) {
    bubble_data2 <- read.csv(file6, header=TRUE)
    bubble_data2 <- bubble_data2 %>%
      filter(ESVs >= esv_lower_lim, ESVs <= esv_upper_lim,
             REL_ABUND >= ra_lower_lim, REL_ABUND <= ra_upper_lim,
             grepl("^unid", CLADE, ignore.case = TRUE) == con_unid)
    p2 <- p2 +
      new_scale_color() +
      geom_point(data=bubble_data2, aes(x = REL_ABUND, y = (reorder (CLADE, ESVs)), size = ESVs, color = ESVs),
                 alpha = 0.8, shape = 4, stroke = 1.25, na.rm = TRUE) +
      scale_color_gradientn(name = (paste("ESVs of", gsub("_BUBBLEPLOT.csv", "", file6))),
                            colours = c("#7e7e7e", "#7e7e7e", "#7e7e7e","#7e7e7e", "#7e7e7e", "#7e7e7e"))
  }
  if (!is.null(file7)) {
    bubble_data2 <- read.csv(file7, header=TRUE)
    bubble_data2 <- bubble_data2 %>%
      filter(ESVs >= esv_lower_lim, ESVs <= esv_upper_lim,
             REL_ABUND >= ra_lower_lim, REL_ABUND <= ra_upper_lim,
             grepl("^unid", CLADE, ignore.case = TRUE) == con_unid)
    p2 <- p2 +
      new_scale_color() +
      geom_point(data=bubble_data2, aes(x = REL_ABUND, y = (reorder (CLADE, ESVs)), size = ESVs, color = ESVs),
                 alpha = 0.8, shape = 5, stroke = 1.25, na.rm = TRUE) +
      scale_color_gradientn(name = (paste("ESVs of", gsub("_BUBBLEPLOT.csv", "", file7))),
                            colours = c("#6a51a3", "#6a51a3", "#6a51a3","#6a51a3", "#6a51a3", "#6a51a3"))
  }
  if (!is.null(file8)) {
    bubble_data2 <- read.csv(file8, header=TRUE)
    bubble_data2 <- bubble_data2 %>%
      filter(ESVs >= esv_lower_lim, ESVs <= esv_upper_lim,
             REL_ABUND >= ra_lower_lim, REL_ABUND <= ra_upper_lim,
             grepl("^unid", CLADE, ignore.case = TRUE) == con_unid)
    p2 <- p2 +
      new_scale_color() +
      geom_point(data=bubble_data2, aes(x = REL_ABUND, y = (reorder (CLADE, ESVs)), size = ESVs, color = ESVs),
                 alpha = 0.8, shape = 6, stroke = 1.25, na.rm = TRUE) +
      scale_color_gradientn(name = (paste("ESVs of", gsub("_BUBBLEPLOT.csv", "", file8))),
                            colours = c("#8abed6", "#8abed6", "#8abed6","#8abed6", "#8abed6", "#8abed6"))
  }
  if (!is.null(file9)) {
    bubble_data2 <- read.csv(file9, header=TRUE)
    bubble_data2 <- bubble_data2 %>%
      filter(ESVs >= esv_lower_lim, ESVs <= esv_upper_lim,
             REL_ABUND >= ra_lower_lim, REL_ABUND <= ra_upper_lim,
             grepl("^unid", CLADE, ignore.case = TRUE) == con_unid)
    p2 <- p2 +
      new_scale_color() +
      geom_point(data=bubble_data2, aes(x = REL_ABUND, y = (reorder (CLADE, ESVs)), size = ESVs, color = ESVs),
                 alpha = 0.8, shape = 7, stroke = 1.25, na.rm = TRUE) +
      scale_color_gradientn(name = (paste("ESVs of", gsub("_BUBBLEPLOT.csv", "", file9))),
                            colours = c("#f8bf4c", "#f8bf4c", "#f8bf4c","#f8bf4c", "#f8bf4c", "#f8bf4c"))
  }
  if (!is.null(file10)) {
    bubble_data2 <- read.csv(file10, header=TRUE)
    bubble_data2 <- bubble_data2 %>%
      filter(ESVs >= esv_lower_lim, ESVs <= esv_upper_lim,
             REL_ABUND >= ra_lower_lim, REL_ABUND <= ra_upper_lim,
             grepl("^unid", CLADE, ignore.case = TRUE) == con_unid)
    p2 <- p2 +
      new_scale_color() +
      geom_point(data=bubble_data2, aes(x = REL_ABUND, y = (reorder (CLADE, ESVs)), size = ESVs, color = ESVs),
                 alpha = 0.8, shape = 8, stroke = 1.25, na.rm = TRUE) +
      scale_color_gradientn(name = (paste("ESVs of", gsub("_BUBBLEPLOT.csv", "", file10))),
                            colours = c("#fff299", "#fff299", "#fff299","#fff299", "#fff299", "#fff299"))
  }
  ggsave(paste(sep = "_", gsub("_BUBBLEPLOT.csv", "", file), "and", gsub("_BUBBLEPLOT.csv", "", file2),  "and", gsub("_BUBBLEPLOT.csv", "", file3),  "and", gsub("_BUBBLEPLOT.csv", "", file4),  "and", gsub("_BUBBLEPLOT.csv", "", file5), (if (con_unid == FALSE) {"no_unid"} else  {"unid"}), ".svg"), plot = p2, device = "svg", width = 10, height = 10, units = "in")
  print(p2)
}
plot_bubble("HONGOS_FAM_ORG_BUBBLEPLOT.csv", "HONGOS_FAM_CON_BUBBLEPLOT.csv", , , , , , , , , 27, 480000, 0.0001, , FALSE, 5000, 15000, 30000, 60000, 120000, 240000, 480000)
