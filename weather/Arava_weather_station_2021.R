#===================================================================
#=== Task: plot radiation plot from weather station data ===========
#===================================================================

# Tutorial to process time series data: https://www.neonscience.org/resources/learning-hub/tutorials/dc-convert-date-time-posix-r

# Load libraries
library(lubridate)  #work with dates
library(ggplot2)
library(readxl)
library(xlsx) # Write excel
library(data.table)
library(export)

# Set Working Directory
setwd("C:/Users/Kaining/BGU ZKN/R/MR data analysis with R/Pepper_2021/pepper2021/weather")

# Load data
weather <- read_excel("Arava_weather_station_2021.xlsx", 
                                         sheet = "Weather station PARLight_Weathe")
View(weather)

# Process data
Arava_PAR_raw <- weather[,c(1,2)] # choose the first two columns interest us

colnames(Arava_PAR_raw) <- c("Date","PAR") # rename colnames for this data

Arava_PAR_raw$Date <- ymd_hms(Arava_PAR_raw$Date) # change date format from package "lubridate"

# Convert column class
dt <- setDT(Arava_PAR_raw) # change the data to data.table

dt2=dt[,.(PAR=mean(PAR,na.rm=TRUE)), by=.(Date=round_date(Date, "3 minute"))]
dt3=dt2[date(Date) %between% c("2021-09-10", "2021-09-20")] # Select interested period of time

# Make plot
ggplot(dt3, aes(x=Date, y=PAR))+
  geom_jitter(
    color=4,
    alpha=0.6
  )+
  scale_x_datetime(
    date_breaks = "1 day",
    date_labels = "%d-%b"
    )+
  theme_classic()+
  theme(
    axis.title = element_text(
      size=20
    ),
    axis.text = element_text(
      size=20, 
      color="black"
    ),
    axis.text.x = element_text(
      size=20, 
      color="black",
      angle = 60
    ),
    axis.title.x = element_text(
      margin = margin(
        t = 10, 
        r = 0, 
        b = 0, 
        l = 0)
    ),
    axis.title.y = element_text(
      margin = margin(
        t = 0, 
        r = 10, 
        b = 0, 
        l = 0)
    ),
    axis.line = element_line(
      size = 1, 
      colour = "black"
    ),
    axis.ticks.length = unit(0.2, "cm"),
    axis.ticks = element_line(
      size = 1
    )
  )+
  labs(x = "Date",
       y=~paste("PAR (", mu, "mol m"^-2,"s"^-1,")")
  )

# Export graph
graph2ppt(file="Arava_PAR.pptx", width=8, height=6, append = F)
