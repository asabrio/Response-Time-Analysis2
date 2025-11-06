# -----------------------------------------------------------------------------
# Code for: "Dispatch Delays and Geographic Disparities in Police Response Times in New Orleans"
# Authors: Alexandra Sabrio, Debashis Mondal
# Description: This script cleans the 2018 Calls for Service data, runs GLM
# analyses, and generates figures corresponding to the paper.
# -----------------------------------------------------------------------------

# -----------------------------
# 1. Libraries
# -----------------------------
library(dplyr)
library(readxl)
library(sf)
library(ggplot2)
library(spdep)
library(data.table)
library(effects)
library(car)
library(cowplot)

# -----------------------------
# 2. File Paths
# -----------------------------
data_dir <- "~/Desktop/Research"       # Put your Excel and shapefiles here
fig_dir <- "~/Desktop/Research"     # Folder to save plots

# -----------------------------
# 3. Load Data
# -----------------------------
my_data <- read_excel(file.path(data_dir, "Calls_for_Service_2018.xlsx"))

# -----------------------------
# 4. Data Cleaning
# -----------------------------
# Keep only non-self-initiated calls and valid coordinates
my_data <- my_data %>%
  filter(SelfInitiated == "N" & Location != "(0.0, 0.0)")

# -----------------------------
# 5. Time Variables
# -----------------------------
# Convert character times to POSIXct
my_data_NoZero <- my_data_NoZero %>%
  mutate(
    TimeCreate = as.POSIXct(TimeCreate, format = "%Y-%m-%dT%H:%M:%S"),
    TimeDispatch = as.POSIXct(TimeDispatch, format = "%Y-%m-%dT%H:%M:%S"),
    TimeArrive = as.POSIXct(TimeArrive, format = "%Y-%m-%dT%H:%M:%S")
  ) %>%
  mutate(
    ResponseTime = as.numeric(difftime(TimeArrive, TimeCreate, units = "mins")),
    DispatchTime = as.numeric(difftime(TimeDispatch, TimeCreate, units = "mins")),
    WaitTime = as.numeric(difftime(TimeDispatch, TimeCreate, units = "mins")),
    HandlingTime = as.numeric(difftime(TimeArrive, TimeDispatch, units = "mins"))
  ) %>%
  na.omit() %>%
  filter(ResponseTime > 0)

# -----------------------------
# 6. Extract Latitude/Longitude
# -----------------------------
my_data_NoZero <- my_data_NoZero %>%
  mutate(
    Latitude = as.numeric(sub("\\((.*?),.*", "\\1", Location)),
    Longitude = as.numeric(sub(".*,\\s*(.*?)\\)", "\\1", Location))
  ) %>%
  filter(!is.na(Latitude) & !is.na(Longitude))

# Convert to sf object
crime_sf <- st_as_sf(my_data_NoZero, coords = c("Longitude", "Latitude"), crs = 4326)

# -----------------------------
# 7. Load Neighborhood Shapefile
# -----------------------------
no.B <- st_read(file.path(data_dir, "harvard_NOLA.shp")) %>%
  st_transform(crs = 4326)

# Neighborhood adjacency matrix
no.mat <- nb2mat(poly2nb(no.B), style = "B")

# -----------------------------
# 8. Assign Neighborhood Labels
# -----------------------------
lst <- st_intersects(crime_sf$geometry, no.B$geometry)
n <- nrow(crime_sf)
crime_sf$nbhd <- as.numeric(lst[1:n])
crime_sf$label <- no.B$gnocdc_lab[as.numeric(lst)]

crime_sf_final <- subset(crime_sf, crime_sf$nbhd> 0)

# -----------------------------
# 9. Recode Priority
# -----------------------------
zeros <- c("0B", "0C", "0E", "0H", "0Z")
ones <- c("1Z")
twos <- c("2G", "2H", "2J", "2Q")
threes <- c("3A", "3C")

crime_sf_final <- crime_sf_final %>%
  mutate(InitialPriority = case_when(
    InitialPriority %in% zeros ~ "0Other",
    InitialPriority %in% ones  ~ "1C",
    InitialPriority %in% twos  ~ "2Other",
    InitialPriority %in% threes ~ "3",
    TRUE ~ InitialPriority
  ))


# -----------------------------
# 10. Extract Hour
# -----------------------------
crime_sf_final$Hour <- as.character(format(crime_sf_final$TimeCreate, "%H"))
crime_sf_final$Hour <- as.numeric(crime_sf_final$Hour)
class(crime_sf_final$Hour) = "character"

# add hour of day data for Handling Time
crime_sf_final$HandlingHour <- as.character(format(crime_sf_final$TimeDispatch, "%H"))
crime_sf_final$HandlingHour <- as.numeric(crime_sf_final$HandlingHour)
class(crime_sf_final$HandlingHour) = "character"

# -----------------------------
# 11. Prepare Variables for Analysis
# -----------------------------
yRT= log(crime_sf_final$ResponseTime)
yWT= log(crime_sf_final$WaitTime)
yHT= log(crime_sf_final$HandlingTime)

dispatch=crime_sf_final$WaitTime
dispatch= as.numeric(dispatch>0)
dispatch0=which(dispatch==0)
dispatch1=which(dispatch==1)

y0= yRT[dispatch0]
y00= yRT[dispatch1]
y1= yWT[dispatch1]
y2= yHT[dispatch1]

priority0=factor(crime_sf_final$InitialPriority[dispatch0])
priority1=factor(crime_sf_final$InitialPriority[dispatch1])

type0=crime_sf_final$InitialTypeText[dispatch0]
type1=crime_sf_final$InitialTypeText[dispatch1]

nbhd0=factor(crime_sf_final$nbhd[dispatch0])
nbhd1=factor(crime_sf_final$nbhd[dispatch1])

label0=factor(crime_sf_final$label[dispatch0])
label1=factor(crime_sf_final$label[dispatch1])

police0=factor(crime_sf_final$PoliceDistrict[dispatch0])
police1=factor(crime_sf_final$PoliceDistrict[dispatch1])

hour0=factor(crime_sf_final$Hour[dispatch0])
hour1=factor(crime_sf_final$Hour[dispatch1])

# -----------------------------
# 12. Run AFT Models
# -----------------------------
# We set neighborhood reference level to LAKEVIEW as this neighborhood has fastest average response time 
label0 <- factor(label0)
label0 <- relevel(label0, ref = 'LAKEVIEW')

police0 <- factor(police0)
police0 <- relevel(police0, ref = 4)

label1 <- factor(label1)
label1 <- relevel(label1, ref = 'LAKEVIEW')

# Immediate
RespTimeANOVA0 <- lm(y0 ~priority0+ police0+ label0+ label0:police0+ hour0)

# NonImmediate
RespTimeANOVA00 <- lm(y00 ~priority1+ police1+ label1+ label1:police1 + hour1)

# Wait Time
RespTimeANOVA1 <- lm(y1 ~priority1+ police1+ label1+ label1:police1 + hour1)


# -----------------------------
# 13. ANOVA Tables for Each AFT Models
# -----------------------------
Anova(RespTimeANOVA0)
Anova(RespTimeANOVA00)
Anova(RespTimeANOVA1)


# -----------------------------
# 14. Further Data Cleaning for Handling Time Model
# -----------------------------
my_data_NoZero <- subset(my_data_NoZero, my_data_NoZero$HandlingTime > 0)

# -----------------------------
# 14.5 Re-run Steps 6-11 for Handling Time Data
# -----------------------------
# -----------------------------
# 5. Time Variables
# -----------------------------
# Convert character times to POSIXct
my_data_NoZero <- my_data_NoZero %>%
  mutate(
    TimeCreate = as.POSIXct(TimeCreate, format = "%Y-%m-%dT%H:%M:%S"),
    TimeDispatch = as.POSIXct(TimeDispatch, format = "%Y-%m-%dT%H:%M:%S"),
    TimeArrive = as.POSIXct(TimeArrive, format = "%Y-%m-%dT%H:%M:%S")
  ) %>%
  mutate(
    ResponseTime = as.numeric(difftime(TimeArrive, TimeCreate, units = "mins")),
    DispatchTime = as.numeric(difftime(TimeDispatch, TimeCreate, units = "mins")),
    WaitTime = as.numeric(difftime(TimeDispatch, TimeCreate, units = "mins")),
    HandlingTime = as.numeric(difftime(TimeArrive, TimeDispatch, units = "mins"))
  ) %>%
  na.omit() %>%
  filter(ResponseTime > 0)

# -----------------------------
# 6. Extract Latitude/Longitude
# -----------------------------
my_data_NoZero <- my_data_NoZero %>%
  mutate(
    Latitude = as.numeric(sub("\\((.*?),.*", "\\1", Location)),
    Longitude = as.numeric(sub(".*,\\s*(.*?)\\)", "\\1", Location))
  ) %>%
  filter(!is.na(Latitude) & !is.na(Longitude))

# Convert to sf object
crime_sf <- st_as_sf(my_data_NoZero, coords = c("Longitude", "Latitude"), crs = 4326)

# -----------------------------
# 7. Load Neighborhood Shapefile
# -----------------------------
no.B <- st_read(file.path(data_dir, "harvard_NOLA.shp")) %>%
  st_transform(crs = 4326)

# Neighborhood adjacency matrix
no.mat <- nb2mat(poly2nb(no.B), style = "B")

# -----------------------------
# 8. Assign Neighborhood Labels
# -----------------------------
lst <- st_intersects(crime_sf$geometry, no.B$geometry)
n <- nrow(crime_sf)
crime_sf$nbhd <- as.numeric(lst[1:n])
crime_sf$label <- no.B$gnocdc_lab[as.numeric(lst)]
crime_sf <- subset(crime_sf, nbhd > 0)

crime_sf_final <- subset(crime_sf, crime_sf$nbhd> 0)

# -----------------------------
# 9. Recode Priority
# -----------------------------
zeros <- c("0B", "0C", "0E", "0H", "0Z")
ones <- c("1Z")
twos <- c("2G", "2H", "2J", "2Q")
threes <- c("3A", "3C")

crime_sf_final <- crime_sf_final %>%
  mutate(InitialPriority = case_when(
    InitialPriority %in% zeros ~ "0Other",
    InitialPriority %in% ones  ~ "1C",
    InitialPriority %in% twos  ~ "2Other",
    InitialPriority %in% threes ~ "3",
    TRUE ~ InitialPriority
  ))

# -----------------------------
# 10. Extract Hour
# -----------------------------
crime_sf_final$Hour <- as.character(format(crime_sf_final$TimeCreate, "%H"))
crime_sf_final$Hour <- as.numeric(crime_sf_final$Hour)
class(crime_sf_final$Hour) = "character"

# add hour of day data for Handling Time
crime_sf_final$HandlingHour <- as.character(format(crime_sf_final$TimeDispatch, "%H"))
crime_sf_final$HandlingHour <- as.numeric(crime_sf_final$HandlingHour)
class(crime_sf_final$HandlingHour) = "character"

# -----------------------------
# 11. Prepare Variables for Analysis
# -----------------------------
yRT= log(crime_sf_final$ResponseTime)
yWT= log(crime_sf_final$WaitTime)
yHT= log(crime_sf_final$HandlingTime)

dispatch=crime_sf_final$WaitTime
dispatch= as.numeric(dispatch>0)
dispatch0=which(dispatch==0)
dispatch1=which(dispatch==1)

y0= yRT[dispatch0]
y00= yRT[dispatch1]
y1= yWT[dispatch1]
y2= yHT[dispatch1]

priority0=factor(crime_sf_final$InitialPriority[dispatch0])
priority1=factor(crime_sf_final$InitialPriority[dispatch1])

type0=crime_sf_final$InitialTypeText[dispatch0]
type1=crime_sf_final$InitialTypeText[dispatch1]

nbhd0=factor(crime_sf_final$nbhd[dispatch0])
nbhd1=factor(crime_sf_final$nbhd[dispatch1])

label0=factor(crime_sf_final$label[dispatch0])
label1=factor(crime_sf_final$label[dispatch1])

police0=factor(crime_sf_final$PoliceDistrict[dispatch0])
police1=factor(crime_sf_final$PoliceDistrict[dispatch1])

hour0=factor(crime_sf_final$Hour[dispatch0])
hour1=factor(crime_sf_final$Hour[dispatch1])

# Need different Hour for handling time because we calculate not at hour of call, but at hour of dispatch
handlinghour=factor(crime_sf_final$HandlingHour[dispatch1])

label0 <- factor(label0)
label0 <- relevel(label0, ref = 'LAKEVIEW')

police0 <- factor(police0)
police0 <- relevel(police0, ref = 4)

label1 <- factor(label1)
label1 <- relevel(label1, ref = 'LAKEVIEW')

# -----------------------------
# 16. Handling Time AFT Model
# -----------------------------
# Handling Time
RespTimeANOVA2 <- lm(y2 ~priority1+ police1+ label1+ label1:police1 + handlinghour)

# -----------------------------
# 17. Handling Time AFT Model ANOVA Table
# -----------------------------
Anova(RespTimeANOVA2)



# -----------------------------
# 18. Ridge Plot Creation
# -----------------------------
my_data_NoZero$DispatchTimeCategory <- 0

my_data_NoZero <- my_data_NoZero %>%
  mutate(DispatchTimeCategory = case_when(
    WaitTime == 0 ~ '0',
    WaitTime > 0 & WaitTime <= 10 ~ '0 to 10',
    WaitTime > 10 & WaitTime <= 20 ~ '10 to 20',
    WaitTime > 20 & WaitTime <= 30 ~ '20 to 30',
    WaitTime > 30 ~ '30 up'
  ))

my_data_NoZero$DispatchTimeCategory <- factor(my_data_NoZero$DispatchTimeCategory, levels = c("0", "0 to 10", "10 to 20", "20 to 30", "30 up"))

gg_ridge <- ggplot(my_data_NoZero, aes(x = log(ResponseTime),
                                       y = DispatchTimeCategory,
                                       fill = after_stat(x))) +
  geom_density_ridges_gradient(scale = 0.9) +
  scale_fill_viridis_c(option = "C") +
  labs(x = "log(ResponseTime) in minutes", y = "Dispatch Time Grouping in minutes", fill = '') +
  theme_minimal() +
  theme(#legend.justification = c(1, 0), legend.position = c(1, 0),
    legend.background = element_rect(colour = NA, fill = "white"),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 10),
    legend.key.width = unit(0.4, "cm"),
    legend.key.height = unit(0.7, "cm"),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    panel.grid.major = element_line(linewidth = 0.3),
    panel.grid.minor = element_line(linewidth = 0.3)
  )


# -----------------------------
# 19. HoneyComb Plot of Police Response Time
# -----------------------------
mapping <- no.B 
city_combined <- st_union(mapping)
city_outline <- st_boundary(city_combined)

no.B_projected <- st_transform(city_combined, crs = 26915)

hexgrid <- st_make_grid(no.B_projected, cellsize = 200, what = 'polygons',
                        square = FALSE) |> st_as_sf()

hexgrid_NO <- hexgrid[st_intersects(hexgrid, no.B_projected, sparse = FALSE),]
hexgrid_NO$CallCount 

my_data <- st_as_sf(my_data_NoZeroDT, coords = c("Longitude", "Latitude"), crs = 4326)

calls_sf <- st_transform(my_data, crs = st_crs(no.B_projected))
calls_sf$x <- calls_sf$geometry

hexgrid_with_calls <- st_join(hexgrid_NO, calls_sf, join = st_intersects)

hexgrid_with_counts <- hexgrid_with_calls %>%
  group_by(x.x) %>%
  summarize(avg_response_time = mean(log(ResponseTime), na.rm = FALSE))

hexgrid_with_counts_df <- as.data.frame(hexgrid_with_counts)

gg_honey_time <- ggplot() +
  geom_sf(data = hexgrid_with_counts,
          aes(fill = avg_response_time),
          color = NA,
          size = 0.1) +
  geom_sf(data = no.B_projected, fill = NA, color = 'black', size = 0.1) +
  scale_fill_viridis_c(option = "C") +
  labs(fill = "", size = 5) +
  theme_minimal() +
  theme(legend.justification = c(1, 0), legend.position = c(1, 0),
        legend.background = element_rect(colour = NA, fill = "white"),
        legend.title = element_text(size = 7),
        legend.text = element_text(size = 9),
        legend.key.width = unit(0.4, "cm"),
        legend.key.height = unit(0.7, "cm"),
        axis.text.x = element_blank(), #element_text(size = 5),
        axis.text.y = element_blank(), #element_text(size = 5),
        axis.title.x = element_text(size = 7),
        axis.title.y = element_text(size = 7),
        plot.title = element_text(size = 8, face = "bold", vjust = 1.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
  )
gg_honey_time




# -----------------------------
# 20. Plot of AFT Model Priority Coefficients 
# -----------------------------
# Immediate ANOVA
coef_names0 <- rownames(summary(RespTimeANOVA0)$coefficients)

coef_priority0 <- data.frame(
  Variable = names(RespTimeANOVA0$coefficients[
    grep("^priority0", names(RespTimeANOVA0$coefficients))]),
  Coefficient = RespTimeANOVA0$coefficients[
    grep("^priority0", names(RespTimeANOVA0$coefficients)) %>%
      intersect(grep(":", names(RespTimeANOVA0$coefficients), invert = TRUE))],
  SE = summary(RespTimeANOVA0)$coefficients[grep("^priority0", names(RespTimeANOVA0$coefficients)) %>%
                                              intersect(grep(":", names(RespTimeANOVA0$coefficients), invert = TRUE)), "Std. Error"]
)

coef_priority0$CleanVariable <- gsub("^priority0", "", coef_priority0$Variable)

coef_priority0$CI_Lower <- coef_priority0$Coefficient - 1.96 * coef_priority0$SE
coef_priority0$CI_Upper <- coef_priority0$Coefficient + 1.96 * coef_priority0$SE

# NonImmediate ANOVA
coef_names00 <- rownames(summary(RespTimeANOVA00)$coefficients)

coef_priority00 <- data.frame(
  Variable = names(RespTimeANOVA00$coefficients[
    grep("^priority1", names(RespTimeANOVA00$coefficients))]),
  Coefficient = RespTimeANOVA00$coefficients[
    grep("^priority1", names(RespTimeANOVA00$coefficients)) %>%
      intersect(grep(":", names(RespTimeANOVA00$coefficients), invert = TRUE))],
  SE = summary(RespTimeANOVA00)$coefficients[grep("^priority1", names(RespTimeANOVA00$coefficients)) %>%
                                               intersect(grep(":", names(RespTimeANOVA00$coefficients), invert = TRUE)), "Std. Error"]
)

coef_priority00$CleanVariable <- gsub("^priority1", "", coef_priority00$Variable)

coef_priority00$CI_Lower <- coef_priority00$Coefficient - 1.96 * coef_priority00$SE
coef_priority00$CI_Upper <- coef_priority00$Coefficient + 1.96 * coef_priority00$SE

# Wait Time ANOVA
coef_names1 <- rownames(summary(RespTimeANOVA1)$coefficients)

coef_priority1 <- data.frame(
  Variable = names(RespTimeANOVA1$coefficients[
    grep("^priority1", names(RespTimeANOVA1$coefficients))]),
  Coefficient = RespTimeANOVA1$coefficients[
    grep("^priority1", names(RespTimeANOVA1$coefficients)) %>%
      intersect(grep(":", names(RespTimeANOVA1$coefficients), invert = TRUE))],
  SE = summary(RespTimeANOVA1)$coefficients[grep("^priority1", names(RespTimeANOVA1$coefficients)) %>%
                                              intersect(grep(":", names(RespTimeANOVA1$coefficients), invert = TRUE)), "Std. Error"]
)

coef_priority1$CleanVariable <- gsub("^priority1", "", coef_priority1$Variable)

coef_priority1$CI_Lower <- coef_priority1$Coefficient - 1.96 * coef_priority1$SE
coef_priority1$CI_Upper <- coef_priority1$Coefficient + 1.96 * coef_priority1$SE

# Handling Time ANOVA
coef_names2 <- rownames(summary(RespTimeANOVA2)$coefficients)

coef_priority2 <- data.frame(
  Variable = names(RespTimeANOVA2$coefficients[
    grep("^priority1", names(RespTimeANOVA2$coefficients))]),
  Coefficient = RespTimeANOVA2$coefficients[
    grep("^priority1", names(RespTimeANOVA2$coefficients)) %>%
      intersect(grep(":", names(RespTimeANOVA2$coefficients), invert = TRUE))],
  SE = summary(RespTimeANOVA2)$coefficients[grep("^priority1", names(RespTimeANOVA2$coefficients)) %>%
                                              intersect(grep(":", names(RespTimeANOVA2$coefficients), invert = TRUE)), "Std. Error"]
)

coef_priority2$CleanVariable <- gsub("^priority1", "", coef_priority2$Variable)

coef_priority2$CI_Lower <- coef_priority2$Coefficient - 1.96 * coef_priority2$SE
coef_priority2$CI_Upper <- coef_priority2$Coefficient + 1.96 * coef_priority2$SE

coef_priority0$Model <- "Immediate"
coef_priority00$Model <- "Non-Immediate"
coef_priority1$Model <- "Wait"
coef_priority2$Model <- "Travel"

combined_priority <- bind_rows(
  coef_priority0,
  coef_priority00,
  coef_priority1,
  coef_priority2
)

combined_priority_plot <- ggplot(combined_priority, 
                                 aes(x = CleanVariable, 
                                     y = Coefficient,
                                     color = Model, shape = Model)) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper),
                width = 0.2,
                position = position_dodge(width = 0.3)) +
  scale_shape_manual(values = c("Immediate" = 16,
                                "Non-Immediate" = 17,
                                "Wait" = 15,
                                "Travel" = 18)) +  # customize as needed
  labs(x = "Priority Level", y = "Coefficient Estimate") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 14, face = "bold"),
    legend.title = element_blank()
  )
combined_priority_plot

ggsave("...", combined_priority_plot, width = 12, height = 5, dpi = 300)



# -----------------------------
# 21. Plot of AFT Model Neighborhood Coefficients 
# -----------------------------
# Immediate
coef_names0 <- rownames(summary(RespTimeANOVA0)$coefficients)

coef_nbhds0 <- RespTimeANOVA0$coefficients[
  grep("^label0", names(RespTimeANOVA0$coefficients)) %>%
    intersect(grep(":", names(RespTimeANOVA0$coefficients), invert = TRUE))]

coef_df0 <- data.frame(
  gnocdc_lab = names(coef_nbhds0),
  Estimate = coef_nbhds0
)

coef_df0$gnocdc_lab <- sub("label0", "", coef_df0$gnocdc_lab)

coef_sf0 <- no.B %>%
  left_join(coef_df0, by = c("gnocdc_lab"))

# NonImmediate
coef_names00 <- rownames(summary(RespTimeANOVA00)$coefficients)

coef_nbhds00 <- RespTimeANOVA00$coefficients[
  grep("^label1", names(RespTimeANOVA00$coefficients)) %>%
    intersect(grep(":", names(RespTimeANOVA00$coefficients), invert = TRUE))]

coef_df00 <- data.frame(
  gnocdc_lab = names(coef_nbhds00),
  Estimate = coef_nbhds00
)

coef_df00$gnocdc_lab <- sub("label1", "", coef_df00$gnocdc_lab)

coef_sf00 <- no.B %>%
  left_join(coef_df00, by = c("gnocdc_lab"))

# Wait Time
coef_names1 <- rownames(summary(RespTimeANOVA1)$coefficients)

coef_nbhds1 <- RespTimeANOVA1$coefficients[
  grep("^label1", names(RespTimeANOVA1$coefficients)) %>%
    intersect(grep(":", names(RespTimeANOVA1$coefficients), invert = TRUE))]

coef_df1 <- data.frame(
  gnocdc_lab = names(coef_nbhds1),
  Estimate = coef_nbhds1
)

coef_df1$gnocdc_lab <- sub("label1", "", coef_df1$gnocdc_lab)

coef_sf1 <- no.B %>%
  left_join(coef_df1, by = c("gnocdc_lab"))

# Handling Time
coef_names2 <- rownames(summary(RespTimeANOVA2)$coefficients)

coef_nbhds2 <- RespTimeANOVA2$coefficients[
  grep("^label1", names(RespTimeANOVA2$coefficients)) %>%
    intersect(grep(":", names(RespTimeANOVA2$coefficients), invert = TRUE))]

coef_df2 <- data.frame(
  gnocdc_lab = names(coef_nbhds2),
  Estimate = coef_nbhds2
)

coef_df2$gnocdc_lab <- sub("label1", "", coef_df2$gnocdc_lab)

coef_sf2 <- no.B %>%
  left_join(coef_df2, by = c("gnocdc_lab"))

# change inputs for different models
T0_nbhd_estimate_plot <- ggplot(data = coef_sf0) +
  geom_sf(aes(fill = Estimate)) +
  scale_fill_viridis_c(option = "C") +
  labs(fill = "", size = 5) +
  theme_minimal() +
  theme(legend.justification = c(1, 0), legend.position = c(1, 0),
        legend.background = element_rect(colour = NA, fill = "white"),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8),
        legend.key.width = unit(0.4, "cm"),   
        legend.key.height = unit(0.7, "cm"),
        axis.text.x = element_blank(), #element_text(size = 8),
        axis.text.y = element_blank(), #element_text(size = 8),
        axis.title.x = element_blank(), #element_text(size = 9),
        axis.title.y = element_blank(), #element_text(size = 9),
        plot.title = element_text(size = 8, face = "bold", vjust = 1.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
  )

T00_nbhd_estimate_plot <- ggplot(data = coef_sf00) +
  geom_sf(aes(fill = Estimate)) +
  scale_fill_viridis_c(option = "C") +
  labs(fill = "", size = 5) +
  theme_minimal() +
  theme(legend.justification = c(1, 0), legend.position = c(1, 0),
        legend.background = element_rect(colour = NA, fill = "white"),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8),
        legend.key.width = unit(0.4, "cm"),   
        legend.key.height = unit(0.7, "cm"),
        axis.text.x = element_blank(), #element_text(size = 8),
        axis.text.y = element_blank(), #element_text(size = 8),
        axis.title.x = element_blank(), #element_text(size = 9),
        axis.title.y = element_blank(), #element_text(size = 9),
        plot.title = element_text(size = 8, face = "bold", vjust = 1.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
  )

T1_nbhd_estimate_plot <- ggplot(data = coef_sf1) +
  geom_sf(aes(fill = Estimate)) +
  scale_fill_viridis_c(option = "C") +
  labs(fill = "", size = 5) +
  theme_minimal() +
  theme(legend.justification = c(1, 0), legend.position = c(1, 0),
        legend.background = element_rect(colour = NA, fill = "white"),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8),
        legend.key.width = unit(0.4, "cm"),   
        legend.key.height = unit(0.7, "cm"),
        axis.text.x = element_blank(), #element_text(size = 8),
        axis.text.y = element_blank(), #element_text(size = 8),
        axis.title.x = element_blank(), #element_text(size = 9),
        axis.title.y = element_blank(), #element_text(size = 9),
        plot.title = element_text(size = 8, face = "bold", vjust = 1.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
  )

T2_nbhd_estimate_plot <- ggplot(data = coef_sf2) +
  geom_sf(aes(fill = Estimate)) +
  scale_fill_viridis_c(option = "C") +
  labs(fill = "", size = 5) +
  theme_minimal() +
  theme(legend.justification = c(1, 0), legend.position = c(1, 0),
        legend.background = element_rect(colour = NA, fill = "white"),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8),
        legend.key.width = unit(0.4, "cm"),   
        legend.key.height = unit(0.7, "cm"),
        axis.text.x = element_blank(), #element_text(size = 8),
        axis.text.y = element_blank(), #element_text(size = 8),
        axis.title.x = element_blank(), #element_text(size = 9),
        axis.title.y = element_blank(), #element_text(size = 9),
        plot.title = element_text(size = 8, face = "bold", vjust = 1.5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
  )


# Combine Immediate and Non-Immediate spatial coefficient plots
T0_nbhd_estimate_plot <- T0_nbhd_estimate_plot + coord_sf(expand = FALSE)
T00_nbhd_estimate_plot <- T00_nbhd_estimate_plot + coord_sf(expand = FALSE)

T0_nbhd_estimate_plot <- T0_nbhd_estimate_plot + theme(plot.margin = margin(0, 0, 0, 0))
T00_nbhd_estimate_plot <- T00_nbhd_estimate_plot + theme(plot.margin = margin(0, 0, 0, 0))

T0_nbhd_estimate_plot <- T0_nbhd_estimate_plot +
  theme(
    legend.position = c(0.95, 0.05),
    legend.justification = c("right", "bottom"),
    legend.background = element_rect(fill = alpha('white', 0.7), color = NA)
  )

T00_nbhd_estimate_plot <- T00_nbhd_estimate_plot + theme(legend.position = "none")

combined_plot <- plot_grid(
  T0_nbhd_estimate_plot,
  T00_nbhd_estimate_plot,
  ncol = 2,
  rel_widths = c(1, 1)
) 

ggsave("...", combined_plot, width = 12, height = 5, dpi = 300)


# Combine Wait and Travel Time coefficient plots
T1_nbhd_estimate_plot <- T1_nbhd_estimate_plot + coord_sf(expand = FALSE)
T2_nbhd_estimate_plot <- T2_nbhd_estimate_plot + coord_sf(expand = FALSE)

T1_nbhd_estimate_plot <- T1_nbhd_estimate_plot + theme(plot.margin = margin(0, 0, 0, 0))
T2_nbhd_estimate_plot <- T2_nbhd_estimate_plot + theme(plot.margin = margin(0, 0, 0, 0))

T1_nbhd_estimate_plot <- T1_nbhd_estimate_plot +
  theme(
    legend.position = c(0.95, 0.05),
    legend.justification = c("right", "bottom"),
    legend.background = element_rect(fill = alpha('white', 0.7), color = NA)
  )

T2_nbhd_estimate_plot <- T2_nbhd_estimate_plot + theme(legend.position = "none")

combined_plotWT <- plot_grid(
  T1_nbhd_estimate_plot,
  T2_nbhd_estimate_plot,
  ncol = 2,
  rel_widths = c(1, 1)
)

ggsave("...", combined_plotWT, width = 12, height = 5, dpi = 300)



# -----------------------------
# 22. Plots of Nested Police and Neighborhood Effects 
# -----------------------------
E0=effect("police0:label0",RespTimeANOVA0)

E1=effect("police1:label1",RespTimeANOVA1)

E2=effect("police1:label1",RespTimeANOVA2)

E3=effect("police1:label1",RespTimeANOVA00)

plot_df0 <- as.data.frame(E0)
plot_df00 <- as.data.frame(E1)
plot_df1 <- as.data.frame(E2)
plot_df2 <- as.data.frame(E3)

# immediate AFT effects
policenbhd0 <- ggplot(plot_df0, aes(x = label0, y = fit, group = police0, color = police0)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, 
                                   size = 9), 
        legend.position = "top",
        legend.direction = "horizontal",
        legend.box = "horizontal") +
  labs(
    x = "Neighborhood",
    y = "Effect",
    color = ""
  )

# non-immediate AFT effects
policenbhd00 <- ggplot(plot_df00, aes(x = label1, y = fit, group = police1, color = police1)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, 
                                   size = 9), 
        legend.position = "top",
        legend.direction = "horizontal",
        legend.box = "horizontal") +
  labs(
    x = "Neighborhood",
    y = "Effect",
    color = ""
  )

# wait time AFT effects
policenbhd1 <- ggplot(plot_df1, aes(x = label1, y = fit, group = police1, color = police1)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, 
                                   size = 9), 
        legend.position = "top",
        legend.direction = "horizontal",
        legend.box = "horizontal") +
  labs(
    x = "Neighborhood",
    y = "Effect",
    color = ""
  )

# handling time AFT effects 
policenbhd2 <- ggplot(plot_df2, aes(x = label1, y = fit, group = police1, color = police1)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, 
                                   size = 9), 
        legend.position = "top",
        legend.direction = "horizontal",
        legend.box = "horizontal") +
  labs(
    x = "Neighborhood",
    y = "Effect",
    color = ""
  )

