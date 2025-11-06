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
my_data <- my_data %>%
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
my_data <- my_data %>%
  mutate(
    Latitude = as.numeric(sub("\\((.*?),.*", "\\1", Location)),
    Longitude = as.numeric(sub(".*,\\s*(.*?)\\)", "\\1", Location))
  ) %>%
  filter(!is.na(Latitude) & !is.na(Longitude))

# Convert to sf object
crime_sf <- st_as_sf(my_data, coords = c("Longitude", "Latitude"), crs = 4326)

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

# -----------------------------
# 9. Recode Priority
# -----------------------------
zeros <- c("0B", "0C", "0E", "0H", "0Z")
ones <- c("1Z")
twos <- c("2G", "2H", "2J", "2Q")
threes <- c("3A", "3C")

crime_sf <- crime_sf %>%
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
crime_sf$Hour <- as.character(format(crime_sf$TimeCreate, "%H"))

# -----------------------------
# 11. Prepare Variables for Analysis
# -----------------------------
yRT <- log(crime_sf$ResponseTime)
yWT <- log(crime_sf$WaitTime)
yHT <- log(crime_sf$HandlingTime)

dispatch <- as.numeric(crime_sf$WaitTime > 0)
dispatch0 <- which(dispatch == 0)
dispatch1 <- which(dispatch == 1)

# -----------------------------
# 12. Logistic Regression for Dispatch Selection
# -----------------------------
z <- 1 - dispatch

priority <- crime_sf$InitialPriority
nbhd <- factor(crime_sf$nbhd)
label <- factor(crime_sf$label)
label <- relevel(label, ref = "LAKEVIEW")
police <- factor(crime_sf$PoliceDistrict)
hour <- factor(crime_sf$Hour)

DispatchGLM <- glm(z ~ priority + police + label + hour, family = binomial)
Anova(DispatchGLM)

# -----------------------------
# 13. Logistic Priority Plot
# -----------------------------
coef_priority <- DispatchGLM$coefficients[grep("priority", names(DispatchGLM$coefficients))]
se_priority <- summary(DispatchGLM)$coefficients[grep("priority", rownames(summary(DispatchGLM)$coefficients)), 2]

coef_df <- data.frame(
  Level = sub("priority", "", names(coef_priority)),
  Estimate = coef_priority,
  SE = se_priority
)
coef_df <- coef_df %>%
  mutate(
    CI_Lower = Estimate - 1.96 * SE,
    CI_Upper = Estimate + 1.96 * SE,
    Level = factor(Level, levels = Level)
  )

logistic_priority_plot <- ggplot(coef_df, aes(x = Level, y = Estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = CI_Lower, ymax = CI_Upper), width = 0.2) +
  labs(x = "Priority", y = "Estimate") +
  theme_minimal()

ggsave(file.path(fig_dir, "logistic_priority_plot.png"), logistic_priority_plot, width = 6, height = 5)

# -----------------------------
# 14. Logistic Neighborhood Plot
# -----------------------------
coef_nbhds <- DispatchGLM$coefficients[grep("label", names(DispatchGLM$coefficients)) %>% 
                                         intersect(grep(":", names(DispatchGLM$coefficients), invert = TRUE))]

coef_df_nbhd <- data.frame(gnocdc_lab = sub("label", "", names(coef_nbhds)), Estimate = coef_nbhds)
coef_sf <- no.B %>%
  left_join(coef_df_nbhd, by = "gnocdc_lab")

logistic_nbhd_plot <- ggplot(data = coef_sf) +
  geom_sf(aes(fill = Estimate)) +
  scale_fill_viridis_c(option = "C") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

ggsave(file.path(fig_dir, "logistic_nbhd_plot.png"), logistic_nbhd_plot, width = 12, height = 5)

