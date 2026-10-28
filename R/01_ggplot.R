
# ---- install and load packages --------------------------------------------------

install.packages("tidyverse")
install.packages("ggtext")
install.packages("readxl")

library(tidyverse)
library(data.table)
library(ggtext)
library(readxl)
library(hms)
library(scales)


# ---- single data import --------------------------------------------------

data <- read_excel("K:/gfernando/Andrea/OD reads/OD600 read 2026 10 26 - test EGTDHMAE.xlsx",
                   sheet = "Hoja2",
                   skip=0)
# you want the names of the columns in excel have the following criteria: same of the same | space | variable | space | replica number
# for example, for multiple samples at different pH, the names should be: "met 5 1" "met 5 2" "met 6 1" "met 6 2" "blank 5 1", etc...

data <- data.frame(sample_type = c("A", "B"), value = c(0, 0)) # to create from scratch

data <- edit(data) # to manipulate manually


## ---- data manipulation --------------------------------------------------

data <- data %>% rename("Time" = 1) # rename a title based on number

data <- data %>% 
  rename_with(~ str_replace(., regex("."), " ")) # replace text

data <- data %>% select(-2) # delete columns

data <- data %>% slice(-2) # delete rows

colnames(data) <- as.character(data[1, ]) # change header

data <- data %>%
  mutate(Time = format(as_hms(Time), "%M:%S")) # change format for Time. Change name of the column accordingly.

data <- data %>%
  mutate(
    Time = as.numeric(as_hms(Time)), # needed for calculations
  )

data <- data %>%
  mutate(sample = factor(sample, levels = c("CONTROL", "1st extraction", "2nd extraction", "3rd extraction"))) # to change order


## ---- long_data creation --------------------------------------------------

long_data <- data %>% # change to long columns, better suited for data analysis
  pivot_longer(
    cols = -1, # doesn't take into account column 1 for analysis, which is usually the x-axis.
    names_to = "original_column",
    values_to = "RFU" # unit of measurement. Change accordingly in the script.
  ) %>%
  mutate(
    clean_col = sub("\\..*", "", original_column), # cleans the headers names
    sample = sub(" [0-9]+$", "", clean_col), # removes last numbers
    sample = if_else(sample == clean_col & grepl(" ", clean_col), clean_col, sample),
     replica = ave(RFU, Time, sample, FUN = seq_along) # change columns names. The second one is the x-axis. E.g. Time / Wavelegth
  ) %>%
  select(-original_column, -clean_col)

long_data <- long_data %>%
  mutate(original_column = fct_relevel(original_column, "blank")) # change the fist ocurrency

long_data <- long_data %>%
  mutate(original_column = gsub("H2O", "H<sub>2</sub>O", original_column)) # add pedices for words like H2O or NH3


# ---- mutiple data import --------------------------------------------------

## ---- load --------------------------------------------------

filelist <- list.files(
  path = "C:/Users/andrearizzo/Desktop/carpeta/Work/Spectrometer/20260422_ABTS_EZ28_COPPER/",
  pattern = "*.csv",
  full.names = TRUE
)


## ---- change columns names --------------------------------------------------

read_spectrum <- function(location) {
  read_csv(location, skip = 2) %>%
    rename_with(~c("column1", "column2")) %>%
    mutate(samplename = basename(location))
}


## ---- import --------------------------------------------------

data_set <- map_dfr(filelist, read_spectrum)


## ---- change samples names --------------------------------------------------

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


## ---- add groups --------------------------------------------------

data_set <- data_set %>%
  mutate(group = case_when(
    sample == unique(sample)[1] ~ "blank",
    sample %in% unique(sample)[2:3] ~ "EZ28",
    sample %in% unique(sample)[4:6] ~ "EZ63"
 ))


# ---- linear slope --------------------------------------------------

## ---- calculate slope from long_data --------------------------------------------------

find_linear_slope <- function(data, r2_threshold = 0.98, n_min_start = 1, n_max_start = 4, n_min_points = 3) {
  required_cols <- c("Time", "RFU", "sample", "replica")   # Validate input columns
  if (!all(required_cols %in% names(data))) {
    stop(paste("Input table must contain the following columns:", paste(required_cols, collapse = ", ")))
  }
  fit_single_curve <- function(x, y) { # Internal function to analyze a single curve (vectors x = Time, y = RFU)
    N <- length(x)
    if (N < 2) return(NULL) # If the curve has fewer than 2 points total, regression is not possible
    min_s <- max(1, n_min_start) # Define valid bounds for the starting point index
    max_s <- min(n_max_start, N - 1)
    if (min_s > max_s) return(NULL) # If the lower bound exceeds the upper bound, return NULL
    grid <- expand.grid(start = min_s:max_s, end = 2:N) %>% # Generate all valid combinations of (start, end) respecting n_min_points
      filter(end > start, (end - start + 1) >= n_min_points)
    if (nrow(grid) == 0) return(NULL)
    results <- map_df(seq_len(nrow(grid)), function(k) { # Evaluate each interval
      i <- grid$start[k]
      j <- grid$end[k]
      sub_x <- x[i:j]
      sub_y <- y[i:j]
      fit <- lm(sub_y ~ sub_x) # Fit linear model
      fit_sum <- summary(fit) # Fit linear model
      r2 <- fit_sum$r.squared # Fit linear model
      coefs <- coef(fit) # Extract parameters
      intercept <- unname(coefs[1]) # Extract parameters
      slope <- unname(coefs[2]) # Extract parameters
      tibble(
        initial_point = i,
        last_point = j,
        points_number = (j - i + 1),
        slope = slope,
        intercept = intercept,
        r_squared = r2
      )
    })
    best_fit <- results %>% # 1. R^2 >= r2_threshold
      filter(r_squared >= r2_threshold) %>% # 2. Maximum number of points (points_number DESC)
      arrange(desc(points_number), desc(r_squared)) %>% # 3. Maximum R^2 for ties in point count (r_squared DESC)
      slice(1)
    if (nrow(best_fit) == 0) { # If no combination meets the criteria, return a row with NAs
      return(tibble(
        initial_point = NA_integer_,
        last_point = NA_integer_,
        points_number = NA_integer_,
        slope = NA_real_,
        intercept = NA_real_,
        r_squared = NA_real_
      ))
    }
    return(best_fit)
  }
  summary_table <- data %>% # Process the table grouping by sample and replica
    group_by(sample, replica) %>%
    arrange(Time, .by_group = TRUE) %>%
    reframe(
      fit_single_curve(Time, RFU)
    ) %>%
    ungroup()
  summary_table <- summary_table %>% # Reorder columns in the exact requested format
    select(
      sample,
      replica,
      initial_point,
      last_point,
      points_number,
      slope,
      intercept,
      r_squared
    )
  
  return(summary_table)
}

results_slope <- find_linear_slope(
  data = long_data, 
  r2_threshold = 0.97, # Minimum R2 value. Standard value: 0.99
  n_min_start = 1,     # Minimum start. Standard value: 1
  n_max_start = 5,     # Maximum start. Standard value: 5
  n_min_points = 4     # Minimum number of points. Standard value: 8
)

results_slope <- results_slope %>% slice(-7) # delete rows of outliers

results_slope <- results_slope %>% mutate(across(where(is.numeric), ~ if_else(row_number() == 7, 0, .x))) # converts all numerican in a row in 0

results_slope <- results_slope %>% mutate(across(where(is.numeric), ~ replace_na(.x, 0))) # Converts all numerical NA in 0


### ---- creation of data summaries --------------------------------------------------

# 01 data summary
df_summary <- results_slope %>%
  separate(sample, into = c("sample_type", "MeOH"), sep = " ", remove = FALSE) %>%
  mutate(MeOH = as.numeric(MeOH)) %>% # necessary to correctly format numbers
  # mutate(MeOH = case_when(is.na(MeOH) ~ "Control", TRUE ~ MeOH)) %>% # "TRUE ~ MeOH" keeps the rest intact
  group_by(sample_type, MeOH, sample) %>%
  summarise(
    slope_media = mean(slope, na.rm = TRUE),
    slope_sd = sd(slope, na.rm = TRUE),
    mU_media = mean(mU, na.rm = TRUE),
    mU_sd = sd(mU, na.rm = TRUE),
    n_repliche = n(),
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~ replace_na(.x, 0))) # Converts all numerical NA in 0

# change df_summary to relative
df_summary <- df_summary %>%
  mutate(
    slope_media_rel = (slope_media / max(slope_media, na.rm = TRUE)) * 100, # automatic
    slope_sd_rel = (slope_sd / max(slope_media, na.rm = TRUE)) * 100,
    # 
    # slope_media_rel = (slope_media / slope_media[5]) * 100, # manual
    # slope_sd_rel = (slope_sd / slope_media[5]) * 100,
  )

# separate df_summary in 2
df_summary_EG <- df_summary %>%
  filter(solvent %in% c("Ethylene glycol", "Control"))

# 02 data summary for multiple variables in X
df_summary <- results_slope %>%
  separate(sample, into = c("sample_type", "variable"), sep = " ", remove = FALSE) %>%
  extract(col = variable, into = c("solvent", "concentration"), regex = "^([A-Za-z]+)(\\d+)$", remove = FALSE) %>%
  mutate(concentration = as.numeric(concentration)) %>%
  mutate(solvent = case_when(
    is.na(solvent) ~ "Control",
    solvent == "EG" ~ "Ethylene glycol",
    solvent == "G" ~ "Glycerol",
    TRUE ~ solvent)) %>% # Keeps the rest intact
  group_by(sample_type, solvent, concentration, sample) %>% # change columns to see
  summarise(
    slope_media = mean(slope, na.rm = TRUE),
    slope_sd = sd(slope, na.rm = TRUE),
    # mU_media = mean(mU, na.rm = TRUE),
    # mU_sd = sd(mU, na.rm = TRUE),
    n_repliche = n(),
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~ replace_na(.x, 0))) # Converts all numerical NA in 0


## ---- data analysis for U calculation --------------------------------------------------

standard_curve <- 821.65 # as RFU = standard_curve * [substrate]
reaction_volume <- 0.1 # in mL

results_slope <- results_slope %>%
  mutate(mU = slope * 60 * reaction_volume / standard_curve)


## ---- check values --------------------------------------------------

long_data_check <- long_data %>%
  mutate(id_curva = paste(sample, "Replica", replica))
for (curva in unique(long_data_check$id_curva)) {
  df_sub <- long_data_check %>% filter(id_curva == curva)
  p <- ggplot(df_sub, aes(x = Time, y = RFU)) +
    geom_point(color = "blue", size = 2) +
    geom_line(color = "blue", alpha = 0.5) +
    theme_minimal() +
    labs(
      title = paste("Controllo Cinetica:", curva),
      x = "Time",
      y = "RFU"
    ) + coord_cartesian(ylim = c(0, 35000)) # pick y axis values
  print(p)
}


### ---- faceting --------------------------------------------------

ggplot(long_data_check, aes(x = Time, y = RFU)) +
  geom_point(color = "blue", size = 1) +
  geom_line(color = "blue", alpha = 0.5) +
  theme_minimal() +
  facet_wrap(~ id_curva) + 
  coord_cartesian(ylim = c(0, 50000)) + 
  scale_x_time(labels = time_format("%M:%S")) + # Cambia la visualizzazione in mm:ss al volo!
  labs(
    title = "All data",
    x = "Time (mm:ss)",
    y = "RFU"
  )


# ---- plotting --------------------------------------------------

# Transforms "sample" column in a categorical variable, and fixes levels order
data_set$sample <- factor(data_set$sample, levels = unique(data_set$sample))

# replicas manipulation
long_data_replicas <- long_data %>%
  # separate(sample, into = c("sample_type", "temperature"), sep = " ", remove = FALSE) %>%
  group_by(Time, sample) %>%
  summarise(
    RFU_media = mean(RFU, na.rm = TRUE),
    RFU_sd = sd(RFU, na.rm = TRUE),
    n_repliche = n(),
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~ replace_na(.x, 0))) # Converts all numerical NA in 0


## ---- single plot --------------------------------------------------

# LINES + POINTS + RIBBON
ggplot(data, aes(x = Time, y = solvent_A)) +
  geom_line(aes(color = "blue"), linetype = 1, linewidth = 1.5,alpha = 1) +
  # geom_point(aes(color = sample, shape = sample), alpha = 1, size = 2.5,
    # data = long_data_replicas %>% filter(row_number() %% 10 == 0) # to select only a few points, for very large datasets.
  # ) +
  # geom_ribbon(aes(ymin = RFU_media - RFU_sd, ymax = RFU_media + RFU_sd, fill = sample), alpha = 0.2, colour = NA, show.legend = FALSE) +
  scale_fill_brewer(palette = "Set2") + # colour palette. More at https://jmsallan.netlify.app/blog/the-brewer-palettes/
  scale_color_brewer(palette = "Dark2") +
  scale_y_continuous(
    breaks = seq(0, 100, by = 20), # manual breaks selection
    # labels = label_scientific() # to have scientific annotations
  ) +
  scale_x_continuous(
    breaks = seq(0, 15, by = 2), # manual breaks selection
    # limits = c(18, 52),            # forces the limits of the axis if they don't work. Remove to make automatic
    # labels = scales::label_number(suffix = "%")) + # addition of % to the y-axis scale
  ) +
  coord_cartesian(xlim = c(NULL, NULL), ylim = c(0, 100)) + # to pick coordinates
  labs(
    x = "Time (m)",
    y = "% of A",
    title = "uHPLC gradient",
    color = "Sample",
    # subtitle = "Samples were incubated at room temperature over the weekend"
  ) +
  # xlim(0, 19.5) +
  # ylim(-0.05, 0.5) +
  theme_bw() + # More at https://ggplot2.tidyverse.org/reference/ggtheme.html
  theme(legend.text = element_markdown(),
        # legend.position = c(1, 0.55),
        legend.position = "none",
        legend.background = element_rect(fill = "white", color = "grey80", linewidth = 0.1),
        legend.title = element_text(face = "bold", size = 12),

        plot.margin = margin(15,15,15,15),
        plot.background = element_rect(fill = "transparent", color = NA),
        
        text = element_text(family = "Montserrat"), # text
        plot.title = element_markdown(face = "bold", size = 14),
        plot.subtitle = element_markdown(face = "plain", size = 9),
        axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0), face = "bold", size = 15),
        axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0),face = "bold", size = 15),
        strip.text = element_text(face = "bold", size = 10),
        axis.text.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0), size = 12),
        axis.text.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0), size = 12),
        
        panel.grid.major.y = element_line(color = "grey85", linewidth = 0.2), # grid lines
        panel.grid.minor.y = element_line(color = "grey95", linewidth = 0.02),
        panel.grid.major.x = element_line(color = "grey85", linewidth = 0.2),
        panel.grid.minor.x = element_blank()
        )

# BAR + ERRORBARS + POINTS + LABELS
ggplot(data, aes(x = sample, y = weight)) +
  geom_col(aes(fill = sample, color = sample), width = 0.6, alpha = 1, position = position_dodge(0.6), linewidth = 0.2) +
  # geom_errorbar(aes(ymin = mU_media - mU_sd, ymax = mU_media + mU_sd, color = sample), 
                # width = 0.25, linewidth = 1, position = position_dodge(0.6)) +
  # geom_point(data = results_slope, aes(x = sample, y = mU, color = sample), shape = 1, size = 3, stroke = 1.5) +
  geom_text(aes(label = weight), vjust = 3, size = 4.5, fontface = "plain") +
  # coord_cartesian(ylim = c(0, 0.7)) + # to pick coordinates
  theme_bw() +
  scale_fill_brewer(palette = "Set2") + # colour palette. More at https://jmsallan.netlify.app/blog/the-brewer-palettes/
  scale_color_brewer(palette = "Dark2") +
  theme(
    plot.margin = margin(15,15,15,15),
    plot.background = element_rect(fill = "transparent", color = NA),
    
    text = element_text(family = "Montserrat"), # text
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(face = "plain", size = 9),
    axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0), face = "bold", size = 15),
    axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0),face = "bold", size = 15),
    strip.text = element_text(face = "bold", size = 10),
    axis.text.x = element_markdown(margin = margin(t = 5, r = 0, b = 0, l = 0), size = 12),
    axis.text.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0), size = 12),
    
    panel.grid.major.y = element_line(color = "grey85", linewidth = 0.2), # grid lines
    panel.grid.minor.y = element_line(color = "grey95", linewidth = 0.02),
    panel.grid.major.x = element_blank(),
    
    legend.position = "none") +
  labs(
    title = "Polyols extraction",
    subtitle = "8.16 mg (10 uL) of polyols in 0.5 mL of water. Extractions with 0.5 mL chloroform",
    x = NULL,
    y = "mg",
    fill = "Sample"
  )


## ---- multiple X plot --------------------------------------------------

# LINES + POINTS + ERRORBAR + FACET
ggplot(df_summary, aes(x = factor(concentration), y = slope_media_rel, group = sample_type, colour = sample_type)) +
  geom_line(linewidth = 0.1, linetype = 2) +
  geom_point(aes(fill = sample_type), shape = 21, size = 5, stroke = 2) +
  geom_errorbar(aes(ymin = slope_media_rel - slope_sd_rel, ymax = slope_media_rel + slope_sd_rel), 
                width = 0.2, linewidth = 0.8) +
  facet_grid(~ solvent, scales = "free_x", space = "free_x", switch = "x") + # here the second x-axis
  theme_bw() +
  coord_cartesian(ylim = c(NA, NA)) + # to pick coordinates
  scale_fill_brewer(palette = "Set2") + # colour palette. More at https://jmsallan.netlify.app/blog/the-brewer-palettes/
  scale_color_brewer(palette = "Dark2") +
  scale_y_continuous(
    breaks = seq(0, 100, by = 50), # manual breaks selection
    labels = scales::label_percent(scale = 1)) + # addition of % to the y-axis scale
  # scale_x_continuous(
  #   breaks = seq(0, 30, by = 5), # manual breaks selection
  #   limits = c(0, 52),            # forces the limits of the axis if they don't work. Remove to make automatic
  #   labels = scales::label_number(suffix = "%")) + # addition of % to the y-axis scale
  theme(
    plot.margin = margin(15,15,15,15),
    plot.background = element_rect(fill = "transparent", color = NA),
    
    text = element_text(family = "Montserrat"), # text
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(face = "plain", size = 9),
    axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0), face = "bold", size = 15),
    axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0),face = "bold", size = 15),
    strip.text = element_text(face = "bold", size = 10),
    axis.text.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0),size = 17),
    axis.text.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0),size = 10),
    
    panel.grid.major.y = element_line(color = "grey85", linewidth = 0.2), # grid lines
    panel.grid.minor.y = element_line(color = "grey95", linewidth = 0.02),
    panel.grid.major.x = element_blank(),
    
    legend.text = element_text(),
    legend.position = c(0.41, 0.79),
    legend.background = element_rect(fill = "white", color = "grey80", linewidth = 0.1),
  ) +
  labs(
    title = "Enzymes activity at different % of solvents",
    x = "solvent %",
    y = "Relative activity"
  )  


# BARS + ERRORBARS + FACET
ggplot(df_summary, aes(x = factor(concentration), y = mU_media, fill = sample_type)) +
  geom_col(width = 0.6, alpha = 1, position = position_dodge(0.6)) +
  geom_errorbar(aes(ymin = mU_media - mU_sd, ymax = mU_media + mU_sd), 
                width = 0.15, linewidth = 0.6, color = "grey20", 
                position = position_dodge(0.6)) +
  facet_grid(~ solvent, scales = "free_x", space = "free_x", switch = "x") + # here the second x-axis
  theme_light() +
  theme(
    plot.margin = margin(15, 15, 15, 15),
    text = element_text(family = "Montserrat"),
    plot.title = element_text(face = "bold", size = 14),
   strip.placement = "outside",
    strip.background = element_rect(fill = "grey95", color = "grey80"),
    strip.text = element_text(color = "black", face = "bold", size = 11),
    panel.spacing = unit(0.2, "lines"),
    axis.text.x = element_text(size = 10)
  ) +
  labs(
    title = "Enzymes activity",
    subtitle = "mU in 100 uL reaction, 100 uM substrate CAMC and 2 ug/mL enzyme",
    x = "Concentration (% v/v)",
    y = "mU",
    fill = "Sample"
  )


# ---- saving --------------------------------------------------

savefile <- ggsave(
  filename = "K:/gfernando/Andrea/Scripts/ggplot/pictures/enzymes activity.png",
  width = 7,
  height = 5,
  dpi = 600
)

savefile <- ggsave(
  filename = "K:/gfernando/Andrea/Scripts/ggplot/pictures/2026_10_27_uHPLC_gradient.svg",
  width = 7,
  height = 5
)

