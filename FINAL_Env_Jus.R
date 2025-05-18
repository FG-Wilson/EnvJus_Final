#Environmental Justice Final Assignment - CHECK ALL CODE AND START CLEANING

#Github folder -> https://github.com/FG-Wilson/EnvJus_Final 

#Loading packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(sf)
library(stringr)
library(readxl)
library(purrr)

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

distrito_summary %>%
  summarise(
    total_hectares = sum(total_area_ha, na.rm = TRUE),
    total_green_space_count = sum(green_space_count, na.rm = TRUE),
    total_km2 = sum(total_area_km2, na.rm = TRUE)
  )

#plot of previous summary 

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

#top 5 parks by size
green_spaces <- green_spaces %>%
  rename(address = `Situación Destino`)

green_spaces %>% 
  arrange (desc(area)) %>% 
  select(address, area) %>% 
  head(5)

#df without top 5 & plot

top5_gs <- green_spaces %>% 
  slice_max(order_by = area, n = 5)

green_spaces_no_top_5 <- green_spaces %>% 
  anti_join(top5_gs, by = 'address')

distrito_summary_no_top_5 <- green_spaces_no_top_5 %>%
  group_by(Distrito) %>%
  summarise(
    total_area_km2 = sum(area, na.rm = TRUE) / 1000000,
    total_area_ha = total_area_km2 *100,
    green_space_count = n()
  ) %>%
  arrange(desc(total_area_km2))

distrito_summary_no_top_5 %>%
  summarise(
    total_hectares = sum(total_area_ha, na.rm = TRUE),
    total_green_space_count = sum(green_space_count, na.rm = TRUE),
    total_km2 = sum(total_area_km2, na.rm = TRUE)
  )

ggplot(distrito_summary_no_top_5, aes(x = reorder(Distrito, total_area_ha), y = total_area_ha)) +
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

distrito_summary_no_top_5 <- distrito_summary_no_top_5 %>%
  mutate(Distrito_CLEAN = str_to_title(str_trim(Distrito)))

setdiff(madrid_map$NOMBRE_CLEAN, distrito_summary_no_top_5$Distrito_CLEAN) #shows that 3 districts did not match/can delete after

# fixing districts that did not match 

distrito_summary_no_top_5 <- distrito_summary_no_top_5 %>%
  mutate(Distrito_CLEAN = case_when(
    Distrito_CLEAN == "Moncloa-Aravaca" ~ "Moncloa - Aravaca",
    Distrito_CLEAN == "Fuencarral-El Pardo" ~ "Fuencarral - El Pardo",
    Distrito_CLEAN == "San Blas-Canillejas" ~ "San Blas - Canillejas",
    TRUE ~ Distrito_CLEAN
  ))

#joined by name for plotting map with areas total and without top 5
madrid_map_joined <- madrid_map %>%
  left_join(distrito_summary_no_top_5, by = c("NOMBRE_CLEAN" = "Distrito_CLEAN"))

madrid_map_joined_total_area <- madrid_map %>%
  left_join(distrito_summary, by = c("NOMBRE_CLEAN" = "Distrito_CLEAN"))

#map with all parks
ggplot(madrid_map_joined_total_area) +
  geom_sf(aes(fill = total_area_ha), color = "white", size = 0.3) +
  geom_sf_text(aes(label = NOMBRE_CLEAN), size = 2, color = "black") +
  scale_fill_steps(name = "Green Space (ha)", 
                   breaks = c(0,25,50,100,200,500,2000), #trying to adjust numbers for better visualisation 
                   low = "lightgreen", high = "darkgreen") +
  labs(
    title = "Green Space Area by District in Madrid (all parks included)",
    subtitle = "Measured in hectares (ha)",
    fill = "Total Area"
  ) +
  theme_bw()

#plotting map without outliers
ggplot(madrid_map_joined) +
  geom_sf(aes(fill = total_area_ha), color = "white", size = 0.3) +
  geom_sf_text(aes(label = NOMBRE_CLEAN), size = 2, color = "black") +
  scale_fill_steps(name = "Green Space (ha)", 
                   breaks = c(0,25,50,100,200), #check what a good visualisation can be
                   low = "lightgreen", high = "darkgreen") +
  labs(
    title = "Green Space Area by District in Madrid (no outliers)",
    subtitle = "Measured in hectares (ha)",
    fill = "Total Area"
  ) +
  theme_bw()

#remember moncloa-aravaca has Casa de Campo of 1405 ha - in my list shows 1800, could also use the other file with mayor superficie to check if its the same after

#For the socio-economic data, the values were previously manually extracted to work only with relevant data as the excel is not formatted in a compatible way for working in R

socio_econ <- read_excel ("/Users/federicowilson/Desktop/Hertie/Semester 4/Env Justice/EnvJus_Final/Madrid_district_socioecon_data.xlsx")

#working the variables
socio_econ <- socio_econ %>% 
  mutate(terciarios_percentage = round((`Estudios Terciarios` / `Población`) * 100, 2)) %>% 
  mutate(foreigners_percentage = round((`Extranjeros` / `Población`) * 100, 2)) %>% 
  mutate(vulnerable_foreigners = round(`Extranjeros no EU ni OCDE` * `Población`)) %>% 
  mutate(vulnerable_foreigners_percentage = round((vulnerable_foreigners / `Población`) * 100, 2))

socio_econ <- socio_econ %>% 
  mutate(Distrito_CLEAN = str_to_title(str_squish(Distrito)))

#Fixing mistakes and merging problems
socio_econ <- socio_econ %>%
  mutate(Distrito_CLEAN = case_when(
    Distrito_CLEAN == "Chamaratín" ~ "Chamartín",
    TRUE ~ Distrito_CLEAN
  ))

socio_econ <- socio_econ %>%
  mutate(Distrito_CLEAN = case_when(
    Distrito_CLEAN == "San Blas-Canillejas" ~ "San Blas - Canillejas",
    Distrito_CLEAN == "Fuencarral-El Pardo" ~ "Fuencarral - El Pardo",
    Distrito_CLEAN == "Moncloa-Aravaca" ~ "Moncloa - Aravaca",
    TRUE ~ Distrito_CLEAN
  ))

#joining socio_econ and map for visualisation of satisfaction - bit useless in the end

madrid_map_joined <- madrid_map_joined %>% 
  left_join(socio_econ, by = c("NOMBRE_CLEAN" = "Distrito_CLEAN"))

ggplot(madrid_map_joined) +
  geom_sf(aes(fill = `Satisfaccion con el barrio`), color = "white", size = 0.3) +
  geom_sf_text(aes(label = NOMBRE_CLEAN), size = 2, color = "black") +
  scale_fill_steps(name = "Satisfaction (1–10)",
                   breaks = seq(6, 8, by = 0.5),
                   low = "lightblue", high = "darkblue") +
  labs(
    title = "District Satisfaction in Madrid",
    subtitle = "Resident ratings from 1 (low) to 10 (high)",
    fill = "Satisfaction"
  ) +
  theme_bw()

satisfaction_long <- madrid_map_joined %>%
  select(Distrito.y, `Satisfaccion con el barrio`, `Satisfaccion espacios verdes`) %>%
  pivot_longer(cols = c(`Satisfaccion con el barrio`, `Satisfaccion espacios verdes`),
               names_to = "type", values_to = "score")

ggplot(satisfaction_long, aes(x = score, y = reorder(Distrito.y, score), color = type)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("steelblue", "forestgreen"),
                     labels = c("District", "Green Space")) +
  labs(
    title = "Comparison of District and Green Space Satisfaction",
    x = "Satisfaction (1–10)",
    y = "District",
    color = "Satisfaction with"
  ) +
  theme_minimal()

#joined data without outliers
combined_data <- left_join(distrito_summary_no_top_5, socio_econ, by = 'Distrito_CLEAN')

#plots on vulnerable foreigners 
ggplot(combined_data, aes(x = vulnerable_foreigners_percentage, y = total_area_ha)) +
  geom_point(color = "darkgreen", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "gray") +
  labs(
    title = "Total Green Space vs. % Vulnerable Foreigners by District",
    x = "% Vulnerable Foreigners (non-EU/non-OECD)",
    y = "Total Green Space Area (ha)"
  ) +
  theme_bw()

ggplot(combined_data, aes(x = vulnerable_foreigners_percentage, y = green_space_count)) +
  geom_point(color = "darkgreen", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "gray") +
  labs(
    title = "Green Space Count vs. % Vulnerable Foreigners by District",
    x = "% Vulnerable Foreigners (non-EU/non-OECD)",
    y = "Green Space Count (n)"
  ) +
  theme_bw()

#plots on general foreigners 
ggplot(combined_data, aes(x = foreigners_percentage, y = total_area_ha)) +
  geom_point(color = "darkgreen", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "gray") +
  labs(
    title = "Total Green Space vs. % Foreigners by District",
    x = "% Foreigners",
    y = "Total Green Space Area (ha)"
  ) +
  theme_bw()

ggplot(combined_data, aes(x = foreigners_percentage, y = green_space_count)) +
  geom_point(color = "darkgreen", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "gray") +
  labs(
    title = "Green Space Count vs. % Foreigners by District",
    x = "% Foreigners",
    y = "Green Space Count (n)"
  ) +
  theme_bw()

#now controlling for population density 

combined_data <- combined_data %>%
  mutate(
    green_area_per_1000 = round((total_area_ha / Población) * 1000, 2)
  )

ggplot(combined_data, aes(x = vulnerable_foreigners_percentage, y = green_area_per_1000)) +
  geom_point(color = "darkgreen", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "gray") +
  labs(
    title = "Green Space per 1,000 Residents vs. % Vulnerable Foreigners",
    x = "% Vulnerable Foreigners (non-EU/non-OECD)",
    y = "Green Space (ha per 1,000 residents)"
  ) +
  theme_minimal()

#Trying the same with the complete list of parks INCOMPLETE, NEED TO COPY CODE FROM UP
combined_data_full_area <- left_join(distrito_summary, socio_econ, by = 'Distrito_CLEAN')

#running regression now 

combined_data <- combined_data %>%
  rename(renta_media = `Renta Neta media anual (Urban audit)`)

#model 1 is with total area, foreigners controlling by studies and avg rent
model_1 <- lm(total_area_ha ~ vulnerable_foreigners_percentage + terciarios_percentage + renta_media, data = combined_data)

summary(model_1)

plot(model_1)

model_1_log <- lm(log(total_area_ha) ~ vulnerable_foreigners_percentage + terciarios_percentage + renta_media, data = combined_data)
summary(model_1_log)
plot(model_1_log)

#per capita - EVERTHING UNDER HERE IS A BIT CONFUSING

combined_data <- combined_data %>%
  mutate(green_space_per_1000 = round((total_area_ha / Población) * 1000, 2))

model_percap <- lm(green_space_per_1000 ~ vulnerable_foreigners_percentage + terciarios_percentage + renta_media, data = combined_data)
summary(model_percap)

combined_data <- combined_data %>%
mutate(log_green_per_1000 = log(green_space_per_1000 + 1))
model_log_percap <- lm(log_green_per_1000 ~ vulnerable_foreigners_percentage + terciarios_percentage + renta_media, data = combined_data)

summary(model_log_percap)
plot(model_log_percap)

#everything points to more education less green space lolz

#using satisfaction data now
combined_data <- combined_data %>%
  rename(
    satisfaction_n = `Satisfaccion con el barrio`,
    satisfaction_gs = `Satisfaccion espacios verdes`)

model_sat_gs <- lm(satisfaction_gs ~ green_space_per_1000 + renta_media + vulnerable_foreigners_percentage, data = combined_data)
summary(model_sat_gs)
plot(model_sat_gs)

#seems like more green space means more satisfied people - with GS, should check with neighbourhood too

ggplot(combined_data, aes(x = green_space_per_1000, y = satisfaction_gs)) +
  geom_point(color = "forestgreen", size = 3) +
  geom_smooth(method = "lm", color = "gray") +
  labs(
    title = "Do Greener Districts Report Higher Satisfaction?",
    x = "Green Space per 1,000 Residents (ha)",
    y = "Satisfaction with Green Spaces (1–10)"
  ) +
  theme_minimal()

model_sat_n <- lm(satisfaction_n ~ green_space_per_1000 + renta_media + vulnerable_foreigners_percentage, data = combined_data)
summary(model_sat_n)

#people might be racist, whats new? income and foreigner drive satisfaction with the neighbourhood

#trying log models and removing outlier districts - ALL THIS I SHOIULD IGNORE AND WORK WITHOUT TOP 5
combined_data$log_green_space <- log(combined_data$green_space_per_1000 + 1)
model_log <- lm(log_green_space ~ vulnerable_foreigners_percentage + terciarios_percentage + renta_media, data = combined_data)
summary(model_log)

model_log_filtered <- lm(log_green_space ~ vulnerable_foreigners_percentage + terciarios_percentage + renta_media, data = combined_data_filtered)
summary(model_log_filtered)

#visualising
#outliers
ggplot(combined_data, aes(x = vulnerable_foreigners_percentage, y = log_green_space)) +
  geom_point(color = "darkred", size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(
    title = "Full Model: Log Green Space vs % Vulnerable Foreigners (All Districts)",
    x = "% Vulnerable Foreigners",
    y = "Log(Green Space per 1,000 Residents)"
)+
  theme_bw()
#no outliers
ggplot(combined_data_filtered, aes(x = vulnerable_foreigners_percentage, y = log_green_space)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    title = "Log Green Space per 1,000 Residents vs % Vulnerable Foreigners",
    x = "% Vulnerable Foreigners",
    y = "Log(Green Space per 1,000 Residents)"
  ) +
  theme_bw()
