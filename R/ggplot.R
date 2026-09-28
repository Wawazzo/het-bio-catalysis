
########## install and load packages ##########
#install.packages("tidyverse")
#install.packages("ggtext")
# CTRL + SHIFT + C

library(tidyverse)
library(data.table)
library(ggtext)

########## single data import ##########

# data <- read_csv("C:/Users/andrearizzo/Desktop/carpeta/Work/Spectrometer/20260422_ABTS_EZ28_COPPER/EZ28 ABTS + CuSO4 test_EZ63 + H2O2.csv",
#                    skip=2)
#   
# data <- data %>%
#   rename_with(~c("column1", "column2"))


# ---- mutiple data import ----

### load ###
filelist <- list.files(
  path = "C:/Users/andrearizzo/Desktop/carpeta/Work/Spectrometer/20260422_ABTS_EZ28_COPPER/",
  pattern = "*.csv",
  full.names = TRUE
)

### change columns names ###
read_spectrum <- function(location) {
  read_csv(location, skip = 2) %>%
    rename_with(~c("column1", "column2")) %>%
    mutate(samplename = basename(location))
}

### import ###
data_set <- map_dfr(filelist, read_spectrum)

### change samples names ###
file_key <- tibble::tibble(
  samplename = basename(filelist),
  sample = c(
    "Blank",
    "EZ28 + H<sub>2</sub>O<sub>2</sub>",
    "EZ28",
    "EZ63 + CuSO<sub>4</sub>",
    "EZ63 + H<sub>2</sub>O<sub>2</sub>",
    "EZ63"
  )
)
data_set <- data_set %>%
  left_join(file_key, by = "samplename")

### add groups ###
# data_set <- data_set %>%
#   mutate(group = case_when(
#     sample == unique(sample)[1] ~ "blank",
#     sample %in% unique(sample)[2:3] ~ "EZ28",
#     sample %in% unique(sample)[4:6] ~ "EZ63"
#   ))


# ---- plotting ----

data_set$sample <- factor(data_set$sample, levels = unique(data_set$sample))

#singleplot
g <- ggplot(data_set, aes(x = column1/60, y = column2)) +
  geom_line(aes(color = sample),
            linetype = 1,
            linewidth = 1.5,
            alpha = 0.4) +
  geom_point(
    aes(color = sample, shape = sample),
    alpha = 1,
    size = 4.5,
    data = data_set %>% filter(row_number() %% 10 == 0)
  ) +
  scale_color_manual(values = c("black", "orange", "red", "aquamarine2", "deepskyblue", "blue")) +
  scale_shape_manual(values = c(15, 17, 17, 16, 16, 16)) +
  guides(
    color = guide_legend(override.aes = list(shape = c(15, 17, 17, 16, 16, 16))),
    shape = "none"
  ) +
  labs(
    x = "Time (m)",
    y = "Signal (a.u.)",
    color = "Sample",
    shape = "Sample"
  ) +
  xlim(0, 19.5) +
  ylim(-0.05, 0.5) +
  theme_classic() + #https://ggplot2.tidyverse.org/reference/ggtheme.html
  theme(legend.text = element_markdown()
        #,axis.line = element_line(size = 0.5),
        #,panel.grid.major = element_line(color = "grey85")
        #,panel.grid.minor = element_line(color = "grey90")
        ,plot.background = element_rect(fill = "transparent", color = NA)
        #,panel.background = element_rect(fill = "white", color = "NA")
        )

#faceting
f <- ggplot(data_set, aes(x = column1, y = column2)) +
  geom_line() +
  facet_wrap(~ sample)


# ---- saving ----

savefile <- ggsave(
  filename = "C:/Users/andrearizzo/Desktop/carpeta/Work/Spectrometer/20260422_ABTS_EZ28_COPPER/plot_spectra.svg",
  plot = g,
  width = 7,
  height = 5,
  dpi = 600
)


