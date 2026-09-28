
library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(tidyverse)
library(data.table)
library(ggtext)

vacuumtest <- tibble(
  Condition = c("control", "pre-vacuum", "post-vacuum", "pre and post vacuum"
  ),
  Read = c(3.761, 3.683, 3.673, 3.654
  )
)

vacuumtest_perc <- vacuumtest %>%
  mutate(Read_norm = Read / max(Read) * 100)

v <- ggplot(vacuumtest_perc, aes(x = Condition, y = Read_norm)) +
  geom_bar(stat = "identity", position = "dodge", fill = "steelblue") +
  geom_text(aes(label = sprintf("%.2f", Read)),
            vjust = -0.3) +
  geom_text(aes(label = sprintf("%.1f%%", Read_norm)),
            vjust = 5,
            color = "white",
            size = 5) +
  labs(
    title = "Comparison with vacuum conditions",
    x = "condition",
    y = "Signal"
  ) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5), # Spin labels if too long
    legend.position = "right"
  )

savefile <- ggsave(
  filename = "C:/Users/andrearizzo/Desktop/carpeta/Work/Spectrometer/20260424_vacuum test.svg",
  plot = v,
  width = 7,
  height = 5,
  dpi = 600
)