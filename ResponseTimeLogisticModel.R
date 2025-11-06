## libraries 
library(dplyr)
library(readxl)
library(sf)
library(ggplot2)
library(spdep)
library(data.table)
library(effects)
library(car)
library(cowplot)

# load in data file and clean
my_data <- read_excel("~/Calls_for_Service_2018.xlsx")
my_data <- subset(my_data, my_data$SelfInitiated == "N")
my_data <- subset(my_data, my_data$Location != "(0.0, 0.0)")

dim(my_data)


time_dispatch <- (my_data$TimeDispatch)
time_arrive <- (my_data$TimeArrive)
time_create <- (my_data$TimeCreate)
time = data.frame()
time <- data.frame(
  time_arrive = time_arrive,
  time_create = time_create,
  time_dispatch = time_dispatch
)

time$time_create <- as.POSIXct(time$time_create, format = "%Y-%m-%dT%H:%M:%S")
time$time_arrive <- as.POSIXct(time$time_arrive, format = "%Y-%m-%dT%H:%M:%S")
time$time_dispatch <- as.POSIXct(time$time_dispatch, format = "%Y-%m-%dT%H:%M:%S")


### Calculate the time differences in minutes ###

my_data$ResponseTime <- as.numeric(difftime(time$time_arrive, time$time_create, 
                                            units = "mins"))
my_data$DispatchTime <- as.numeric(difftime(time$time_dispatch, time$time_create, 
                                            units = "mins"))

my_data$WaitTime <- as.numeric(difftime(time$time_dispatch, time$time_create, 
                                        units = "mins"))
my_data$HandlingTime <- as.numeric(difftime(time$time_arrive, time$time_dispatch, 
                                            units = "mins"))

my_data <- na.omit(my_data)

my_data_NoZero <- subset(my_data, my_data$ResponseTime > 0)

my_data_NoZero <- setDT(my_data_NoZero) # needs to be a data table to reformat time
my_data_NoZero[, TimeCreate := as.POSIXct(TimeCreate, format="%Y-%m-%dT%H:%M:%S")]
my_data_NoZero[, TimeDispatch := as.POSIXct(TimeDispatch, format="%Y-%m-%dT%H:%M:%S")]


## locations data
my_data_NoZero$Latitude <- as.numeric(sub("\\((.*?),.*", "\\1", my_data_NoZero$Location))

my_data_NoZero$Longitude <- as.numeric(sub(".*,\\s*(.*?)\\)", "\\1", my_data_NoZero$Location))

# Keep only rows with valid coordinates
my_data_NoZero <- my_data_NoZero[!is.na(my_data_NoZero$Latitude) & !is.na(my_data_NoZero$Longitude), ]

crime_sf <- st_as_sf(my_data_NoZero, coords = c("Longitude", "Latitude"), crs = 4326)



## shape file for 72 neighborhoods
no.B=st_read("~/harvard_NOLA.shp")

no.B<-st_transform(no.B,crs=4326)
no.mat<-nb2mat(poly2nb(no.B), style = "B")

## join shape file with crime data
lst = st_intersects(crime_sf$geometry,no.B$geometry)
n=dim(crime_sf)[1]
crime_sf$nbhd=as.numeric(lst[1:n])
crime_sf$label=no.B$gnocdc_lab[as.numeric(lst)]

crime_sf_final <- subset(crime_sf, crime_sf$nbhd> 0)
n=dim(crime_sf_final)[1]

# priority re-categorization
# get counts of priorities for "Other" categories
filtered_data <- crime_sf_final %>%
  group_by(InitialPriority) %>%
  count()

# for 2018 data
zeros <- c("0B", "0C", "0E", "0H", "0Z")
ones <- c("1Z")
twos <- c("2G", "2H", "2J", "2Q")
threes <- c("3A", "3C")
for (i in 1:length(crime_sf_final$InitialPriority)){
  if (crime_sf_final$InitialPriority[i] %in% zeros) {
    crime_sf_final$InitialPriority[i] <- "0Other"
    }
  if (crime_sf_final$InitialPriority[i] %in% ones){
    crime_sf_final$InitialPriority[i] = "1C"
  }
  if (crime_sf_final$InitialPriority[i] %in% twos){
    crime_sf_final$InitialPriority[i] = "2Other"
  }
  if (crime_sf_final$InitialPriority[i] %in% threes){
    crime_sf_final$InitialPriority[i] = "3"
  }
}

crime_sf_final$Hour <- 0
crime_sf_final$Hour <- format(crime_sf_final$TimeCreate, "%H")
crime_sf_final$Hour <- as.numeric(crime_sf_final$Hour)
class(crime_sf_final$Hour) = "character"



# analysis with final data ...

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

# Logistic Regression for dispatch selection
# Running GLM with NO interaction terms
z=1-dispatch

priority=crime_sf_final$InitialPriority
nbhd=factor(crime_sf_final$nbhd)
label=factor(crime_sf_final$label)
label <- relevel(label, ref = 'LAKEVIEW')
police=factor(crime_sf_final$PoliceDistrict)
hour=factor(crime_sf_final$Hour)

DispatchGLM<- glm(z ~priority + police + label + hour, family=binomial)

Anova(DispatchGLM)


# Logistic Plots 
# priority plot logistic
coef_initial_priority <- DispatchGLM$coefficients[grep("priority", names(DispatchGLM$coefficients))]

se_initial_priority <- summary(DispatchGLM)$coefficients[grep("priority", rownames(summary(DispatchGLM)$coefficients)), 2]

coef_df <- data.frame(
  Level = names(coef_initial_priority),
  Estimate = coef_initial_priority,
  se = se_initial_priority
)
coef_df$Level <- sub("priority", "", coef_df$Level)
coef_df$CI_Lower <- coef_df$Estimate - 1.96 * coef_df$se
coef_df$CI_Upper <- coef_df$Estimate + 1.96 * coef_df$se
coef_df$Level <- factor(coef_df$Level, levels = coef_df$Level)

logistic_priority_estimate_plot <- ggplot(coef_df, aes(x = Level, y = Estimate)) +
  geom_point() +
  labs(x = "Priority", 
       y = "Estimate") +
  theme_minimal() +
  theme(legend.justification = c(1, 0), legend.position = c(1, 0),
        legend.background = element_rect(colour = NA, fill = "white"),
        legend.title = element_text(size = 3.7),
        legend.text = element_text(size = 3.7),
        legend.key.width = unit(0.15, "cm"),
        legend.key.height = unit(0.2, "cm"),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        plot.title = element_text(size = 8, face = "bold", vjust = 1.5),
        panel.grid.major = element_line(linewidth = 0.3),
        panel.grid.minor = element_line(linewidth = 0.3)
  )
logistic_priority_estimate_plot

ggsave("...", 
       logistic_priority_estimate_plot, width = 6, height = 5)




# spatial plot logistic
coef_names <- rownames(summary(DispatchGLM)$coefficients)

coef_nbhds <- DispatchGLM$coefficients[
  grep("label", names(DispatchGLM$coefficients)) %>%
    intersect(grep(":", names(DispatchGLM$coefficients), invert = TRUE))]

coef_df <- data.frame(gnocdc_lab = names(coef_nbhds), Estimate = coef_nbhds)

coef_df$gnocdc_lab <- sub("label", "", coef_df$gnocdc_lab)

coef_sf <- no.B %>%
  left_join(coef_df, by = c("gnocdc_lab"))

logistic_nbhd_estimate_plot <- ggplot(data = coef_sf) +
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

ggsave("...", 
       logistic_nbhd_estimate_plot, width = 12, height = 5)
