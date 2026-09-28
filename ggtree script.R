
########### guide ###########

# This script allows you to create phylogenetic trees with visuals, colours and such.

# Inputs:
#   a tree file (for example from Clustal). The only one required.
#   a dot file, containing for each leaf a categoric factor (e.g. protein origin or E.C. code)
#   a heatmap file, containing for each leaf a numerical factor (e.g. activity or Tm)

# The tree can be simple, or contain coloured dots to indicate the categoric factor, and
# a heatmap to indicate the numerical factor. All of this will be described by a legend.

# More complex options, like labeling of a single leaf or adding a second heatmap, are possible.

# Each tree type is symbolised by a letter, and complex trees are based on simple ones:
#   p -> q (add labels) -> r (add dots) -> s (add heatmap)

# Each step adds a layer of complexity, this means that to modify certain characteristic
# of the s tree, you have to modify the p, the q or the r.

# If you want a tree with only labels and a heatmap, use s created from q (i.e. skip r):
#   p -> q (add labels) -> s (add heatmap)



########### install libraries/fonts ###########

# To run only the first time:

# if (!require("BiocManager", quietly = TRUE))
# install.packages("BiocManager")
# BiocManager::install("ggtree")
# BiocManager::install("treeio")
# BiocManager::install("ggtreeExtra")
# install.packages("tidyverse")
# install.packages("ape")
# install.packages('extrafont')
# install.packages("grid")
# font_import()



########### load libraries ###########

library(tidyverse) # contains ggplot2 for data visualization and dplyr for data manipulation
library(ggtree) # contains ggtree
library(ggtreeExtra) # contains extra functions for ggtree
library(treeio) # makes it easier to import and store phylogenetic tree data
library(ape) # analyses of phylogenetics and evolution
library(extrafont) # for the use of different fonts from the standard ones
library(grid) # for grid adjustment



########### set-up ###########

setwd("Y:/Andrea/enzymes data/UMG_SP_2/new urethanases") # set directory

cols <- c('#8ED973', '#156082', '#FFE600', "#AF5943") # define colours for dots

tree <- read.tree("tree.dnd") # call tree file

#dots <- read.csv("2025_08_13_MC_sequences_copia dots.csv", sep = ";") # call dots data (.CSV)

#heatmap <- read.csv("seq_id.csv", sep = ";", row.names=1) # call heatmap data (.CSV)
#heatmap2 <- read.csv("seq_id_2.csv", sep = ";", row.names=1) # call more heatmap

#tree$edge.length[tree$edge.length <= 0] <- 0.001 # replace negative values branch lenght values with 0.001
#tree$edge.length <- tree$edge.length ^ 0.2 # increases difference between branches

#+ xlim(0, 5.5) #sets limit for x and y axes



########### definition of tree types ###########

### tree with only branches ###
p <- ggtree(tree,
            size = 0.2, #tickness of the branches
            layout = "circular", #layout type
            branch.length='none') # more layouts at https://yulab-smu.top/treedata-book/chapter4.html figure 4.2


### tree with labels ###
q <- p +
  geom_tiplab(offset=0.2, #offset of the labels
              family = "Montserrat", #font type
              size = 1, #font size
              fontface="plain") # font face (bold, italic...)
# more at https://rdrr.io/github/GuangchuangYu/ggtree/man/geom_tiplab.html


### tree with labels + one highlighted ###
### to add more highlighted labels, just add another geom_tiplab ###
q2 <- p +
  geom_tiplab(data = subset(p$data, !label %in% c("UMG-SP-1", "UMG-SP-2", "UMG-SP-3", "Urethanase-1", "Urethanase-2", "Urethanase-3", "Urethanase-4", "Urethanase-5", "Urethanase-6", "Urethanase-7", "Urethanase-8","met","Tm", "pQR3137", "pQR3138", "pQR3139", "pQR3140", "pQR3141", "pQR3142", "pQR3143", "pQR3144", "pQR3145", "AbPURase")), # select the labels you want to highlight
              offset=0.2, #offset of the labels
              family = "Montserrat", #font type
              size = 1, #font size
              fontface="plain") + # font face (bold, italic...)
  geom_tiplab(
    data = subset(p$data, label %in% c("UMG-SP-1", "UMG-SP-2", "UMG-SP-3", "Urethanase-1", "Urethanase-2", "Urethanase-3", "Urethanase-4", "Urethanase-5", "Urethanase-6", "Urethanase-7", "Urethanase-8","met","Tm", "pQR3137", "pQR3138", "pQR3139", "pQR3140", "pQR3141", "pQR3142", "pQR3143", "pQR3144", "pQR3145", "AbPURase")),  # highlight only these labels
    geom = "label",
    fill = "white", # select colour
    label.size=0, # select thickness of border
    alpha = 0.3, # transparency 0 to 1
    offset = 0.2, # offset of the highlighted labels
    family = "Montserrat", # font of the highlighted labels
    size = 1.5, # font size of highlighted labels
    fontface = "bold")


### tree with dots ###
r <- q %<+% dots +
    geom_tippoint(aes(color = bababa), # select column in the CSV for dots and select size
                  size = 6) +  # change size of the dots
  theme(legend.position = "right", # add legend
    legend.text = element_text(family = "Montserrat", face="italic"), # legend entries font
    legend.title = element_text(family = "Montserrat", face="bold"), # legend title font
    plot.margin = margin(5, 5, 5, 5)) + # margins size
  scale_color_manual(values=cols, name = "class") + xlim(0, 16) # pick title of the legend


### tree with a heatmap ###
s <- gheatmap(r, heatmap,
              offset = 3, # change offset of the heatmap
              width = 0.06, # width of heatmap
              low = "#AEDADC", # low value colour
              high = "#0E2841", # high value colour
              color = "white", # border colour
              colnames = TRUE, # to add name to each heatmap
              colnames_position = "top", # position of the name of the heatmap
              colnames_angle = 0, # sets angle of the heatmap name
              colnames_level = NULL, # sets the order of the different heatmaps
              colnames_offset_x = 0, # x-axis offset of the heatmap name
              colnames_offset_y = 0.5, # y-axis offset of the heatmap name
              font.size = 2.5, # heatmap name font size
              hjust = 0.2, # adjust horizontal justification
              legend_title = "similarity", # name of the legend
              family = "Montserrat SemiBold") + # font of the heatmap names
  theme(legend.position = "right", # add legend
        legend.text = element_text(family = "Montserrat", face="plain"), # legend entries font
        legend.title = element_text(family = "Montserrat", face="bold.italic")) # legend title font
# more info at https://rdrr.io/github/GuangchuangYu/ggtree/man/gheatmap.html


### tree with a second heatmap ###
### to add more heatmaps, replicate this  script a second/third time (s3, s4, etc...) ###
s_temp <- s + ggnewscale::new_scale_fill() #Need to load more than one heatmap
s2 <- gheatmap(s_temp, heatmap2,
               offset = 5,  # change offset of the heatmap. Set > than the offset in s
               width = 0.06, # width of heatmap
               low = "#FFE2A9", # low value colour
               high = "#A94D00", # high value colour
               color = "white", # border colour
               colnames = FALSE, # to add name to each heatmap
               colnames_position = "top", # position of the name of the heatmap
               colnames_offset_x = 0, # x-axis offset of the heatmap name
               colnames_offset_y = 0.5, # y-axis offset of the heatmap name
               font.size = 2.5, # heatmap name font size
               hjust = 0.2, # adjust horizontal justification
               legend_title = "score", # name of the legend
               family = "Montserrat SemiBold") + # font of the heatmap names
  theme(legend.position = "right", # add legend
        legend.text = element_text(family = "Montserrat", face = "plain"), # legend entries font
        legend.title = element_text(family = "Montserrat", face = "bold.italic")) # legend title font



########### save ###########
# This is the command to save the plot.
# Can adjust name of the file, the plot, the path and so on and so forth

sav <- ggsave("tree.png", plot = q, device = NULL, path = NULL, scale = 1, width = NA,height = NA, units = c("in", "cm", "mm", "px"), dpi = 600, limitsize = TRUE, bg = NULL,create.dir = FALSE,)

