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
# 4. Time Variables and Data Cleaning
# -----------------------------
my_data <- subset(my_data, my_data$SelfInitiated == "N")
my_data <- subset(my_data, my_data$Location != "(0.0, 0.0)")

time_dispatch <- (my_data$TimeDispatch)
time_arrive <- (my_data$TimeArrive)
# time_arrive <- (my_data$TimeArrival)
time_create <- (my_data$TimeCreate)
time_close <- (my_data$TimeClosed)
time = data.frame()
time <- data.frame(
  time_arrive = time_arrive,
  time_create = time_create,
  time_dispatch = time_dispatch,
  time_close = time_close
)

time$time_create <- as.POSIXct(time$time_create, format = "%Y-%m-%dT%H:%M:%S")
time$time_arrive <- as.POSIXct(time$time_arrive, format = "%Y-%m-%dT%H:%M:%S")
time$time_dispatch <- as.POSIXct(time$time_dispatch, format = "%Y-%m-%dT%H:%M:%S")
time$time_close <- as.POSIXct(time$time_close, format = "%Y-%m-%dT%H:%M:%S")

my_data$ResponseTime <- as.numeric(difftime(time$time_arrive, time$time_create, 
                                            units = "mins"))
my_data$WaitTime <- as.numeric(difftime(time$time_dispatch, time$time_create, 
                                        units = "mins"))
my_data$HandlingTime <- as.numeric(difftime(time$time_arrive, time$time_dispatch, 
                                            units = "mins"))
# -----------------------------
# 5. Data Cleaning
# -----------------------------
my_data <- subset(my_data, my_data$WaitTime >= 0)

my_data <- na.omit(my_data)

my_data <- subset(my_data, my_data$ResponseTime >= 0)

my_data_NoZero <- subset(my_data, my_data$ResponseTime > 0)

my_data_NoZero <- setDT(my_data_NoZero) # needs to be a data table to reformat time

my_data_NoZero[, TimeCreate := as.POSIXct(TimeCreate, format="%Y-%m-%dT%H:%M:%S")]

my_data_NoZero[, TimeDispatch := as.POSIXct(TimeDispatch, format="%Y-%m-%dT%H:%M:%S")]

# -----------------------------
# 6. Extract Latitude/Longitude
# -----------------------------
my_data_NoZero$Latitude <- as.numeric(sub("\\((.*?),.*", "\\1", my_data_NoZero$Location))

my_data_NoZero$Longitude <- as.numeric(sub(".*,\\s*(.*?)\\)", "\\1", my_data_NoZero$Location))

# Keep only rows with valid coordinates
my_data_NoZero <- my_data_NoZero[!is.na(my_data_NoZero$Latitude) & !is.na(my_data_NoZero$Longitude), ]

crime_sf <- st_as_sf(my_data_NoZero, coords = c("Longitude", "Latitude"), crs = 4326)

# -----------------------------
# 7. Load Neighborhood Shapefile
# -----------------------------
no.B <- st_read(file.path(data_dir, "harvard_NOLA.shp")) %>%
  st_transform(crs = 4326)

no.B<-st_transform(no.B,crs=4326)
no.B <- st_make_valid(no.B)
no.mat<-nb2mat(poly2nb(no.B), style = "B")

g0 <- graph_from_adjacency_matrix((no.mat+t(no.mat))/2, mode="undirected")

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
crime_sf_final$Hour <- 0
crime_sf_final$Hour <- format(crime_sf_final$TimeCreate, "%H")
crime_sf_final$Hour <- as.numeric(crime_sf_final$Hour)
class(crime_sf_final$Hour) = "character"

# -----------------------------
# 11. Prepare Variables for Analysis
# -----------------------------
yRT <- log(crime_sf_final$ResponseTime)
yWT <- log(crime_sf_final$WaitTime)
yHT <- log(crime_sf_final$HandlingTime)

dispatch <- as.numeric(crime_sf_final$WaitTime > 0)
dispatch0 <- which(dispatch == 0)
dispatch1 <- which(dispatch == 1)

# -----------------------------
# 12. Logistic Regression for Dispatch Selection
# -----------------------------
z <- 1 - dispatch

priority <- crime_sf_final$InitialPriority
nbhd <- factor(crime_sf_final$nbhd)
label <- factor(crime_sf_final$label)
label <- relevel(label, ref = "LAKEVIEW")
police <- factor(crime_sf_final$PoliceDistrict)
hour <- factor(crime_sf_final$Hour)

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

