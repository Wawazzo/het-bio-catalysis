# ---- install and load packages ----
library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(tidyverse)
library(data.table)
library(ggtext)
library(zoo)
library(plotly)

# ---- load data ----

# 1. Setup
file_path <- "chromatogram3.csv"
lines <- readLines(file_path)

# 2. Groups identification
is_sample_start <- str_detect(lines, "^[\" ]*#") & !str_detect(lines, "#Point")
group_id <- cumsum(is_sample_start)
is_any_header <- str_detect(lines, "^[\" ]*#")

# 3. Mapping. Use it to change names to samples
mapping <- tibble(
  raw = c("EI TIC Scan No O2 -.d ",
          "EI TIC Scan No O2 +.d ",
          "EI TIC Scan 15 min EZ63 + PE.d ",
          "EI TIC Scan 15 min EZ63.d ",
          "EI TIC Scan 60 min EZ63 + PE.d ",
          "EI TIC Scan 60 min EZ63.d ",
          "EI TIC Scan 60 min H2O + PE.d ",
          "EI TIC Scan ENZ003 SAMPLE 110 1.d ",
          "EI TIC Scan ENZ003 SAMPLE 110 2.d ",
          "EI TIC Scan ENZ003 SAMPLE 110 3.d ",
          "EI TIC Scan RED SHIFT.d ",
          "EI TIC Scan EZ63 + H2O2 1.d ",
          "EI TIC Scan EZ63 + H2O2 2.d ",
          "EI TIC Scan H2O + H2O2 1.d ",
          "EI TIC Scan H2O + H2O2 2.d ",
          "EI TIC Scan 15 min H2O + PE.d ",
          "EI TIC Scan EZ63 EtOAc + o-xylene.d ",
          "EI TIC Scan EZ63 EtOAc.d "),
  nice = c("O2 negative",
           "O2 positive",
           "15 min EZ63 + PE",
           "15 min EZ63",
           "60 min EZ63 + PE",
           "60 min EZ63",
           "60 min H2O + PE",
           "ENZ003 I",
           "ENZ003 II",
           "ENZ003 III",
           "RED SHIFT",
           "EZ63 + H2O2 1",
           "EZ63 + H2O2 2",
           "H2O + H2O2 1",
           "H2O + H2O2 2",
           "15 min H2O + PE",
           "O-XYLENE POSITIVE",
           "O-XYLENE NEGATIVE")
)

cat(paste0('"', unique(data_set$Sample), '"'), sep = ",\n") # to print raw values

  
# 4. Elaboration
data_set <- tibble(lines = lines, group = group_id) %>%
  mutate(is_any_header = is_any_header) %>%
  group_by(group) %>%
  mutate(raw_name = lines[1]) %>% 
  mutate(raw_name = str_remove(raw_name, '^[#"+ ]+')) %>% # Toglie # " + all'inizio
  mutate(raw_name = str_remove(raw_name, '"$')) %>%       # Toglie la virgoletta finale
  ungroup() %>%
 filter(!is_any_header, str_count(lines, ",") == 2) %>%
  separate(lines, into = c("Point", "Time", "Signal"), sep = ",", convert = TRUE) %>%
  left_join(mapping, by = c("raw_name" = "raw")) %>%
  mutate(Sample = coalesce(nice, raw_name)) %>%
  filter(!str_detect(Sample, "TCC")) %>%
  select(Point, Time, Signal, Sample)

# 5. Verify
print(unique(data_set$Sample))

# ---- baseline correction ----


window_size <- 200 #reduce value to increase correction

data_set <- data_set %>%
  group_by(Sample) %>%
  mutate(
    Baseline = rollapply(Signal, width = window_size, FUN = min, fill = "extend", align = "center"),
    Corrected_Signal = Signal - Baseline
  ) %>%
  ungroup()

#quick check
ggplot(data_set, aes(x = Time)) +
  geom_line(aes(y = Signal, color = "Original"), alpha = 0.5) +
  geom_line(aes(y = Baseline, color = "Baseline"), linewidth = 1) +
  geom_line(aes(y = Corrected_Signal, color = "Correct")) +
  facet_wrap(~Sample) +
  ylim(-1000, 1000000)

# ---- filter ----

levels(data_set$Sample)

data_filtered <- data_set %>%
  filter(as.numeric(factor(Sample, levels = unique(Sample))) %in% c(17,18)) #decide which samples to plot

print(unique(data_set$Sample)) #to check

# ---- plotting ----

p <- ggplot(data_filtered, aes(x = Time, y = Corrected_Signal)) +
  geom_line(aes(color = Sample),
            linetype = 1,
            linewidth = 0.5,
            alpha = 1) +
  scale_color_manual(values = c("darkblue","brown","aquamarine","pink")
  ) +
  labs(
    x = "Time (m)",
    y = "Signal (%)",
    color = "Sample"
  ) +
  # xlim(0, 19.5) +
  # ylim(0, 25) +
  theme_classic() + #https://ggplot2.tidyverse.org/reference/ggtheme.html
  theme(legend.text = element_markdown()
        ,plot.background = element_rect(fill = "transparent", color = NA)
  )

# ---- plotly ----

p_interactive <- ggplotly(p)

# 3. Salvalo come file HTML
htmlwidgets::saveWidget(p_interactive, "graph o-xylene.html")


# ---- peak integration ----

target_molecules <- tibble(
  Molecule = c("Tetradecane", "Methyl-6,8-dodecadienyl-ether", "3-methyl-pentadecane", "3-methyl-heptadecane", "1-octadecanol", "3-methyl-nonedecane", "2-methyl-7-nonadecene", "4-methyl-heneicosane"
),
  ExpectedRT = c(33.869, 35.414, 38.972, 43.735, 45.652, 47.850, 49.678, 52.201
),
  Tolerance = 0.03
)

noise_threshold <- 0.5

integration_set <- data_set %>%
  group_by(Sample) %>%
  group_modify(~ {
    target_molecules %>%
      rowwise() %>%
      mutate(
        MaxSignal = {
          window_data <- .x %>% filter(
            Time >= ExpectedRT - Tolerance & 
              Time <= ExpectedRT + Tolerance
          )
          if(nrow(window_data) > 0) max(window_data$Corrected_Signal) else 0
        },
        Detected = MaxSignal > noise_threshold
      )
  }) %>%
  ungroup()

print(integration_set, n = nrow(integration_set))

# ---- table graph ----

# to check: unique(integration_set$Sample)

integration_filtered <- integration_set %>%
  filter(as.numeric(factor(Sample, levels = unique(Sample))) %in% c(1,2,3))

t <- ggplot(integration_set, aes(x = Molecule, y = MaxSignal, fill = Sample)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(
    title = "Peaks comparison",
    subtitle = "",
    x = "Molecule",
    y = "Signal"
  ) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1), # Spin labels if too long
    legend.position = "right"
  )

