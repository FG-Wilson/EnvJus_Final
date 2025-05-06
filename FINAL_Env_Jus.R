#Environmental Justice Final Assignment 

#Loading packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(sf)
library(stringr)

#Loading Green Spaces list
green_spaces <- read_delim("/Users/federicowilson/Desktop/Hertie/Semester 4/Env Justice/EnvJus_Final/Zonas_Verdes_2024.csv", delim = ";")


green_spaces <- green_spaces %>% #creating a column with the clean number for area
  mutate(
    area = as.numeric(gsub(" m2", "", Solar))
  )

#summary district by count of GS and size in ha

distrito_summary <- green_spaces %>%
  group_by(Distrito) %>%
  summarise(
    total_area_km2 = sum(area, na.rm = TRUE) / 1000000,
    total_area_ha = total_area_km2 *100,
    green_space_count = n()
  ) %>%
  arrange(desc(total_area_km2))

#plot of previous summary 

library(ggplot2)

ggplot(distrito_summary, aes(x = reorder(Distrito, total_area_ha), y = total_area_ha)) +
  geom_col(fill = "forestgreen") +
  geom_text(aes(label = green_space_count), 
            hjust = -0.2, 
            size = 3.5) +
  coord_flip() +
  labs(title = "Total Green Space area by district with count",
       x = "District",
       y = "Area (hectare)") +
  theme_bw()

#toying with shp of districts

madrid_map <- st_read("/Users/federicowilson/Desktop/Hertie/Semester 4/Env Justice/EnvJus_Final/Distritos/DISTRITOS.shp")

names(madrid_map)

#cleaning names 
madrid_map <- madrid_map %>%
  mutate(NOMBRE_CLEAN = str_to_title(str_trim(NOMBRE)))

distrito_summary <- distrito_summary %>%
  mutate(Distrito_CLEAN = str_to_title(str_trim(Distrito)))

setdiff(madrid_map$NOMBRE_CLEAN, distrito_summary$Distrito_CLEAN) #shows that 3 districts did not match/can delete after

# fixing districts that did not match 

distrito_summary <- distrito_summary %>%
  mutate(Distrito_CLEAN = case_when(
    Distrito_CLEAN == "Moncloa-Aravaca" ~ "Moncloa - Aravaca",
    Distrito_CLEAN == "Fuencarral-El Pardo" ~ "Fuencarral - El Pardo",
    Distrito_CLEAN == "San Blas-Canillejas" ~ "San Blas - Canillejas",
    TRUE ~ Distrito_CLEAN
  ))


#joined by name map for plotting map with areas
madrid_map_joined <- madrid_map %>%
  left_join(distrito_summary, by = c("NOMBRE_CLEAN" = "Distrito_CLEAN"))

#plotting map 
ggplot(madrid_map_joined) +
  geom_sf(aes(fill = total_area_ha), color = "white", size = 0.3) +
  geom_sf_text(aes(label = NOMBRE_CLEAN), size = 2, color = "black") +
  scale_fill_steps(name = "Green Space (ha)", 
                   breaks = c(0,20,50,100,200,500,2000), #check what a good visualisation can be
                   low = "lightgreen", high = "darkgreen") +
  labs(
    title = "Green Space Area by District in Madrid",
    subtitle = "Measured in hectares (ha)",
    fill = "Total Area"
  ) +
  theme_minimal()

#remember moncloa-aravaca has Casa de Campo of 1405 ha - in my list shows 1800, could also use the other file with mayor superficie to check if its the same after

#now working with socio-economic data

