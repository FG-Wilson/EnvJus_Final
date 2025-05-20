#Environmental Justice Final Assignment

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
library(stargazer)
library(broom)
library(modelsummary)
library(flextable)
library(janitor)
library(pandoc)

#Loading Green Spaces list
green_spaces <- read_delim("/Users/federicowilson/Desktop/Hertie/Semester 4/Env Justice/EnvJus_Final/Data/Zonas_Verdes_2024.csv", delim = ";")


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

#plot of previous summary ALL UGS

ggplot(distrito_summary, aes(x = reorder(Distrito, total_area_ha), y = total_area_ha)) +
  geom_col(fill = "forestgreen") +
  geom_text(aes(label = green_space_count), 
            hjust = -0.2, 
            size = 3.5) +
  coord_flip() +
  labs(title = "Urban Green Space (UGS) area and count by district",
       x = "District",
       y = "Area (ha)") +
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
  labs(title = "UGS area and count by district, without top 5 largest",
       x = "District",
       y = "Area (ha)") +
  theme_bw()
  
#Shapefile for districts to create maps

madrid_map <- st_read("/Users/federicowilson/Desktop/Hertie/Semester 4/Env Justice/EnvJus_Final/Data/Distritos_Shp/DISTRITOS.shp")

names(madrid_map)

#cleaning names 
madrid_map <- madrid_map %>%
  mutate(NOMBRE_CLEAN = str_to_title(str_trim(NOMBRE)))

distrito_summary_no_top_5 <- distrito_summary_no_top_5 %>%
  mutate(Distrito_CLEAN = str_to_title(str_trim(Distrito)))

setdiff(madrid_map$NOMBRE_CLEAN, distrito_summary_no_top_5$Distrito_CLEAN) #shows that 3 districts did not match/can delete after

#fixing districts that did not match 

distrito_summary_no_top_5 <- distrito_summary_no_top_5 %>%
  mutate(Distrito_CLEAN = case_when(
    Distrito_CLEAN == "Moncloa-Aravaca" ~ "Moncloa - Aravaca",
    Distrito_CLEAN == "Fuencarral-El Pardo" ~ "Fuencarral - El Pardo",
    Distrito_CLEAN == "San Blas-Canillejas" ~ "San Blas - Canillejas",
    TRUE ~ Distrito_CLEAN
  ))

#joined by name for plotting map with areas total and without top 5
#joined map no top 5
madrid_map_joined <- madrid_map %>%
  left_join(distrito_summary_no_top_5, by = c("NOMBRE_CLEAN" = "Distrito_CLEAN"))

#Getting data on full list of parks to then plot

#joined data with top 5, repeating same process as before for top 5 UGS removed
distrito_summary <- distrito_summary %>%
  mutate(Distrito_CLEAN = str_to_title(str_trim(Distrito)))

setdiff(madrid_map$NOMBRE_CLEAN, distrito_summary$Distrito_CLEAN) #shows that 3 districts did not match/can delete after

distrito_summary <- distrito_summary %>%
  mutate(Distrito_CLEAN = case_when(
    Distrito_CLEAN == "Moncloa-Aravaca" ~ "Moncloa - Aravaca",
    Distrito_CLEAN == "Fuencarral-El Pardo" ~ "Fuencarral - El Pardo",
    Distrito_CLEAN == "San Blas-Canillejas" ~ "San Blas - Canillejas",
    TRUE ~ Distrito_CLEAN
  ))

madrid_map_joined_complete <- madrid_map %>%
  left_join(distrito_summary, by = c("NOMBRE_CLEAN" = "Distrito_CLEAN"))

#map with all parks -> madrid_map_joined_complete df with all parks
ggplot(madrid_map_joined_complete) +
  geom_sf(aes(fill = total_area_ha), color = "white", size = 0.2) +
  geom_sf_text(aes(label = NOMBRE_CLEAN), size = 2.5, color = "black") +
  scale_fill_distiller(palette = "Greens", direction = 1, name = "Green Space (ha)", breaks = c(0,100,500,1000,2000)) +
  labs(
    title = "Green Space Area by District in Madrid",
    subtitle = "Measured in hectares (ha)",
    fill = "Total Area"
  ) +
  theme_bw()+ 
  theme(axis.text = element_blank(),
                    axis.ticks = element_blank(),
                    axis.title = element_blank())

#plotting map without outliers
ggplot(madrid_map_joined) +
  geom_sf(aes(fill = total_area_ha), color = "white", size = 0.3) +
  geom_sf_text(aes(label = NOMBRE_CLEAN), size = 2, color = "black") +
  scale_fill_distiller(palette = "Greens", direction = 1, name = "Green Space (ha)") +
  labs(
    title = "Green Space Area by District in Madrid (no outliers)",
    subtitle = "Measured in hectares (ha)",
    fill = "Total Area"
  ) +
  theme_bw() +
  theme(axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank())


#Moncloa-Aravaca has Casa de Campo of 1405 ha according to Google while ion my list shows 1800. Discrepancy. 

#For the socio-economic data, the values were previously manually extracted to work only with relevant data as the excel is not formatted in a compatible way for working in R

socio_econ <- read_excel ("/Users/federicowilson/Desktop/Hertie/Semester 4/Env Justice/EnvJus_Final/Data/Madrid_district_socioecon_data.xlsx")

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

#joining socio_econ and map for visualisation of satisfaction - NOT USED

madrid_map_joined <- madrid_map_joined %>% 
  left_join(socio_econ, by = c("NOMBRE_CLEAN" = "Distrito_CLEAN"))

ggplot(madrid_map_joined) +
  geom_sf(aes(fill = `Satisfaccion con el barrio`), color = "white", size = 0.3) +
  geom_sf_text(aes(label = NOMBRE_CLEAN), size = 2.5, color = "darkgreen") +
  scale_fill_viridis_c(
    option = "magma", 
    name = "Satisfaction (1–10)",
    breaks = seq(6, 8, by = 0.5))+
  labs(
    title = "District Satisfaction in Madrid",
    subtitle = "Resident ratings from 1 (low) to 10 (high)",
    fill = "Satisfaction"
  ) +
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank())

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
  theme_bw()

ggplot(madrid_map_joined, aes(x = `Satisfaccion espacios verdes`, y = `Satisfaccion con el barrio`)) +
  geom_point(color = "darkgreen", size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
  labs(
    title = "Relationship Between Green Space and District Satisfaction",
    x = "Green Space Satisfaction (1–10)",
    y = "District Satisfaction (1–10)"
  ) +
  theme_bw()

#joined data without outliers
combined_data <- left_join(distrito_summary_no_top_5, socio_econ, by = 'Distrito_CLEAN')

#joined data WITH outliers

combined_data_complete <- left_join(distrito_summary, socio_econ, by = 'Distrito_CLEAN')

combined_data_complete <- combined_data_complete %>%
  mutate(total_area_ha_log = log(total_area_ha)) %>% 
  rename(renta_media = `Renta Neta media anual (Urban audit)`)

#plots on vulnerable foreigners using MAIN DF
ggplot(combined_data, aes(x = vulnerable_foreigners_percentage, y = total_area_ha)) +
  geom_point(color = "darkgreen", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "gray") +
  labs(
    title = "Total Green Space vs. % Global South Foreigners by District",
    x = "% Global South Foreigners (non-EU/non-OECD)",
    y = "Total Green Space Area (ha)"
  ) +
  theme_bw()

ggplot(combined_data, aes(x = vulnerable_foreigners_percentage, y = green_space_count)) +
  geom_point(color = "darkgreen", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "gray") +
  labs(
    title = "Green Space Count vs. % Global South Foreigners by District",
    x = "% Global South Foreigners (non-EU/non-OECD)",
    y = "Green Space Count (n)"
  ) +
  theme_bw()

#plots on general foreigners - DID NOT USE
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
    green_area_per_1000 = round((total_area_ha / Población) * 1000, 2),
    green_count_per_1000 = ((green_space_count / Población) * 1000))

#plot for area
ggplot(combined_data, aes(x = vulnerable_foreigners_percentage, y = green_area_per_1000)) +
  geom_point(color = "darkgreen", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "gray") +
  labs(
    title = "Green Space per 1,000 Residents vs. % Global South Foreigners",
    x = "% Global South Foreigners (non-EU/non-OECD)",
    y = "Green Space (ha per 1,000 residents)"
  ) +
  theme_bw()

#plot for count 

ggplot(combined_data, aes(x = vulnerable_foreigners_percentage, y = green_count_per_1000)) +
  geom_point(color = "darkgreen", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "gray") +
  labs(
    title = "Green Space count per 1,000 Residents vs. % Global South Foreigners",
    x = "% Global South Foreigners (non-EU/non-OECD)",
    y = "Green Space count (1 UGS per 1,000 residents)"
  ) +
  theme_bw()

#running regression now 

combined_data <- combined_data %>%
  rename(renta_media = `Renta Neta media anual (Urban audit)`)

#model 1 is with total area, foreigners controlling by studies and avg rent
model_1 <- lm(total_area_ha ~ vulnerable_foreigners_percentage + terciarios_percentage + renta_media, data = combined_data)

summary(model_1)

#model 2 is same as 1 but log
model_2 <- lm(log(total_area_ha) ~ vulnerable_foreigners_percentage + terciarios_percentage + renta_media, data = combined_data)
summary(model_1_log)

#model 3 is linear on green area per cap

model_3 <- lm(green_area_per_1000 ~ vulnerable_foreigners_percentage + terciarios_percentage + renta_media, data = combined_data)
summary(model_3)

#model 4 is same a 3 but log if green_per_cap
combined_data <- combined_data %>%
mutate(log_green_per_1000 = log(green_area_per_1000))

model_4 <- lm(log_green_per_1000 ~ vulnerable_foreigners_percentage + terciarios_percentage + renta_media, data = combined_data)

summary(model_4)

#using satisfaction data now - NOT USED, insignificant 
combined_data <- combined_data %>%
  rename(
    satisfaction_n = `Satisfaccion con el barrio`,
    satisfaction_gs = `Satisfaccion espacios verdes`)

model_sat_gs <- lm(satisfaction_gs ~ green_area_per_1000 + renta_media + vulnerable_foreigners_percentage, data = combined_data)
summary(model_sat_gs)
plot(model_sat_gs)

model_sat_n <- lm(formula = satisfaction_n ~ green_area_per_1000 + renta_media + 
     vulnerable_foreigners_percentage, data = combined_data)
summary(model_sat_n)

#Used for last part on satisfaction exploration 
ggplot(combined_data, aes(x = green_area_per_1000, y = satisfaction_gs)) +
  geom_point(color = "forestgreen", size = 3) +
  geom_smooth(method = "lm", color = "gray") +
  labs(
    title = "Do Greener Districts Report Higher Satisfaction?",
    x = "Green Space per 1,000 Residents (ha)",
    y = "Satisfaction with Green Spaces (1–10)"
  ) +
  theme_bw()

#more evidence on higher education less UGS

#plotting on data without top 5 parks 
ggplot(combined_data, aes(x = vulnerable_foreigners_percentage, y = total_area_ha_log)) +
  geom_point(color = "darkred", size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(
    title = "Full Model: Log Green Space vs % Vulnerable Foreigners (All Districts)",
    x = "% Vulnerable Foreigners",
    y = "Log(Green Space per 1,000 Residents)"
)+
  theme_bw()

#plot with complete UGS data 
ggplot(combined_data_complete, aes(x = vulnerable_foreigners_percentage, y = total_area_ha_log)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    title = "Log Green Space area % Global South Foreigners (GSF)",
    x = "% GSF",
    y = "Log Green Space area in ha"
  ) +
  theme_bw()

#model 5 is log of total area but on complete data
model_5 <- lm(log(total_area_ha) ~ vulnerable_foreigners_percentage + 
                            terciarios_percentage + renta_media,
                          data = combined_data_complete)

summary(model_5)

#plot without Moncloa-Aravaca and Hortaleza

no_MA_H_data <- combined_data_complete %>% 
  filter(total_area_ha < 500) %>% 
  rename(renta_media = `Renta Neta media anual (Urban audit)`) #might be done earlier

ggplot(no_MA_H_data, aes(x = vulnerable_foreigners_percentage, y = total_area_ha_log)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE, color = "yellow") +
  labs(
    title = "Log Green Space area % Global South Foreigners (GSF) - top two districts removed",
    x = "% GSF",
    y = "Log Green Space area in ha"
  ) +
  theme_bw()

#model 6 removes the 2 districts with total UGS over 500 hectares
model_6 <- lm(log(total_area_ha) ~ vulnerable_foreigners_percentage + 
     terciarios_percentage + renta_media,
   data = no_MA_H_data)

summary(model_6)

#model 7, controlling for population density 
combined_data <- combined_data %>% 
  clean_names() %>% 
  rename(district_size = tamano_ha,
         population = poblacion)

combined_data <- combined_data %>% 
  mutate(
    population = as.numeric(gsub("[^0-9\\.]", "", population)),
    district_size = as.numeric(gsub("[^0-9\\.]", "", district_size))
  ) %>% 
  mutate(pop_density = population/district_size)

model_7 <- lm(log_green_per_1000 ~ vulnerable_foreigners_percentage +
    terciarios_percentage +
    renta_media +
    pop_density,
  data = combined_data)

summary(model_7)

#Model 8 - log pop density 
combined_data <- combined_data %>%
  mutate(log_pop_density = log(pop_density))

model_8 <- lm(log_green_per_1000 ~ vulnerable_foreigners_percentage + 
                          terciarios_percentage + renta_media + log_pop_density, data = combined_data)

summary(model_8)

# Summary of all models
ft <- modelsummary(
  list(
  "Model 1" = model_1,
  "Model 2" = model_2, 
  "Model 3" = model_3,
  "Model 4" = model_4
),
  output = "flextable",
  estimate = "{estimate}{stars}",    
  statistic = "std.error (p = {p.value})", 
  gof_omit = "AIC|BIC|Log.Lik|RMSE|Deviance",
  fmt = 3,
  coef_map = c(
    "(Intercept)" = "Intercept",
    "vulnerable_foreigners_percentage" = "% Global South Foreigners",
    "terciarios_percentage" = "% University Education Completed",
    "renta_media" = "District Median Income"
  ),
  notes = "* p < 0.05, ** p < 0.01, *** p < 0.001",)

ft

ft_controls <- modelsummary(
  list(
    "Model 5" = model_5,
    "Model 6" = model_6,
    "Model 7" = model_7,
    "Model 8" = model_8
  ),
  output = "flextable",
  estimate = "{estimate}{stars}",    
  statistic = "std.error (p = {p.value})", 
  gof_omit = "AIC|BIC|Log.Lik|RMSE|Deviance",
  fmt = 3,
  coef_map = c(
    "(Intercept)" = "Intercept",
    "vulnerable_foreigners_percentage" = "% Global South Foreigners",
    "terciarios_percentage" = "% University Education Completed",
    "renta_media" = "District Median Income",
    "pop_density" = "Population Density",
    "log_pop_density" = "Log(Population Density)"
  ),
  notes = "* p < 0.05, ** p < 0.01, *** p < 0.001",)

ft_controls

ft_controls <- ft_controls %>% 
  fontsize(size = 12, part = "all") %>%
  autofit()

save_as_image(ft_controls, path = "model_table_controls.png")

#Summary of satisfaction models
modelsummary(
  list(
    "Green Space Satisfaction" = model_sat_gs,
    "District Satisfaction" = model_sat_n 
  ),
  output = "flextable",
  estimate = "{estimate}{stars}",   
  statistic = "std.error (p = {p.value})",  # show std. error and p-value
  gof_omit = "AIC|BIC|Log.Lik|RMSE|Deviance",
  fmt = 3,
  coef_map = c(
    "(Intercept)" = "Intercept",
    "green_area_per_1000" = "Green Space per 1000 residents",
    "vulnerable_foreigners_percentage" = "% Global South Foreigners",
    "renta_media" = "District Median Income"
  ),
  notes = "* p < 0.05, ** p < 0.01, *** p < 0.001",)