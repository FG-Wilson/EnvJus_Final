#Environmental Justice Final Assignment 

#Loading packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)

#Loading Green Spaces list
green_spaces <- read_delim("/Users/federicowilson/Desktop/Hertie/Semester 4/Env Justice/EnvJus_Final/Zonas_Verdes_2024.csv", delim = ";")


green_spaces <- green_spaces %>% #creating a column with the clean number for area
  mutate(
    area = as.numeric(gsub(" m2", "", Solar))
  )

#summary district by count of GS and size

distrito_summary <- green_spaces %>%
  group_by(Distrito) %>%
  summarise(
    total_area_km2 = sum(area, na.rm = TRUE) / 1000000,
    green_space_count = n()
  ) %>%
  arrange(desc(total_area_km2))

#plot of previous summary 

library(ggplot2)

ggplot(distrito_summary, aes(x = reorder(Distrito, total_area_km2), y = total_area_km2)) +
  geom_col(fill = "forestgreen") +
  geom_text(aes(label = green_space_count), 
            hjust = -0.2, 
            size = 3.5) +
  coord_flip() +
  labs(title = "Total Green Space area by district with count",
       x = "District",
       y = "Area (km²)") +
  theme_bw()

#now working with socio-economic data

