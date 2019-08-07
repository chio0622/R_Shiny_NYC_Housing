library(shiny)
library(lubridate)
library(tidyr)
library(dplyr)
library(sp)
library(rgdal)
library(plyr)
library(leaflet)
library(ggplot2)
library(ggiraph)



Emaster = read.csv(file="Emaster.csv", stringsAsFactors = FALSE)
Wmaster = read.csv(file="Wmaster.csv", stringsAsFactors = FALSE)
Hmaster = read.csv(file="Hmaster.csv", stringsAsFactors = FALSE)

shpf <- readOGR("./Map of NYCHA Developments/geo_export_f45f7888-f586-47b1-aeea-6afde3956c89.shp", stringsAsFactors=FALSE )
shpf2 <- spTransform(shpf, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
latlong <- coordinates(shpf2)



# HeatingGas <- read.csv("./Heating_Gas_Consumption_And_Cost__2010_-_March_2019_.csv")
# Water <- read.csv("./Water_Consumption_And_Cost__2013_-_March_2019_.csv")
# Electric <- read.csv("./Electric_Consumption_And_Cost__2010_-_March_2019_.csv")
# 
# #Electric subset columns
# Electric.subset <- Electric[, c("Development.Name","TDS..","Borough", "Revenue.Month", "Current.Charges", "Consumption..KWH." )]
# #rename columns
# colnames(Electric.subset)[2]<- "TDSNumber"
# colnames(Electric.subset)[6]<- "Consumption.KWH"
# #Borough has some additional factors that should be removed:
# Electric.subset <- Electric.subset[grep("MANHATTAN|QUEENS|BROOKLYN|BRONX|STATEN ISLAND", Electric.subset$Borough),]
# #Revenue>Month has a strange format---use RE to get rid of AM/PM
# Electric.subset$Revenue.Month.clean <- gsub("AM","",Electric.subset$Revenue.Month)
# #Current.Charges has a dollar sign in front, remove it. Also strip of comma
# Electric.subset$Current.Charges <- gsub("\\$","",Electric.subset$Current.Charges) #success
# Electric.subset$Current.Charges <- gsub("\\,","",Electric.subset$Current.Charges)
# #drop column
# Electric.subset$Revenue.Month <- NULL
# 
# #Datetime object column
# Electric.subset$DateTime <- as.POSIXct(Electric.subset$Revenue.Month.clean, format="%m/%d/%Y %I:%M:%S") #POSIXct 
# Electric.subset$Year <- year(Electric.subset$DateTime)
# 
# #change current.charges to numeric
# Electric.subset$Current.Charges <-as.numeric(as.character(Electric.subset$Current.Charges))
# Electric.subset$Borough <- as.character(Electric.subset$Borough)
# Electric.subset <- na.omit(Electric.subset) #314102 obs
# 
# #Master Data for Electric
# Emaster <- Electric.subset %>% filter(DateTime>="2013-01-01" & DateTime<="2017-12-01",Current.Charges !=0,Consumption.KWH !=0)
# 
# 
# 
# #WATER: subset columns
# water.subset <- Water[, c("Development.Name","TDS..","Borough", "Revenue.Month", "Current.Charges", "Consumption..HCF." )]
# #rename columns
# colnames(water.subset)[2]<- "TDSNumber"
# colnames(water.subset)[6]<- "Total.HCF" #Total HCF (Hundred Cubic Feet)
# #Borough has some additional factors that should be removed:
# water.subset <- water.subset[grep("MANHATTAN|QUEENS|BROOKLYN|BRONX|STATEN ISLAND", water.subset$Borough),]
# #Revenue>Month has a strange format---use RE to get rid of AM/PM
# water.subset$Revenue.Month.clean <- gsub("AM","",water.subset$Revenue.Month)
# #Current.Charges has a dollar sign in front, remove it. Also strip of comma
# water.subset$Current.Charges <- gsub("\\$","",water.subset$Current.Charges) #success
# water.subset$Current.Charges <- gsub("\\,","",water.subset$Current.Charges)
# #drop column
# water.subset$Revenue.Month <- NULL
# #Datetime object column
# water.subset$DateTime <- as.POSIXct(water.subset$Revenue.Month.clean, format="%m/%d/%Y %I:%M:%S") #POSIXct 
# water.subset$Year <- year(water.subset$DateTime)
# #change current.charges to numeric
# water.subset$Current.Charges <-as.numeric(as.character(water.subset$Current.Charges))
# water.subset$Borough <- as.character(water.subset$Borough)
# water.subset$Total.HCF <- as.integer(water.subset$Total.HCF)
# water.subset <- na.omit(water.subset) #314102 obs
# 
# #Master Data for Water
# Wmaster <- water.subset %>% filter(DateTime>="2013-01-01" & DateTime<="2017-12-01",Current.Charges !=0,Total.HCF !=0)
# 
# 
# 
# #HEATING GAS subset columns
# Heat.subset <- HeatingGas[, c("Development.Name","TDS..","Borough", "Revenue.Month", "Current.Charges", "Consumption..Therms." )]
# #rename columns
# colnames(Heat.subset)[2]<- "TDSNumber"
# colnames(Heat.subset)[6]<- "Total.Therm"
# #Borough has some additional factors that should be removed:
# Heat.subset <- Heat.subset[grep("MANHATTAN|QUEENS|BROOKLYN|BRONX|STATEN ISLAND", Heat.subset$Borough),]
# #Revenue>Month has a strange format---use RE to get rid of AM/PM
# Heat.subset$Revenue.Month.clean <- gsub("AM","",Heat.subset$Revenue.Month)
# #Current.Charges has a dollar sign in front, remove it. Also strip of comma
# Heat.subset$Current.Charges <- gsub("\\$","",Heat.subset$Current.Charges) #success
# Heat.subset$Current.Charges <- gsub("\\,","",Heat.subset$Current.Charges)
# #drop column
# Heat.subset$Revenue.Month <- NULL
# #Datetime object column
# Heat.subset$DateTime <- as.POSIXct(Heat.subset$Revenue.Month.clean, format="%m/%d/%Y %I:%M:%S") #POSIXct 
# Heat.subset$Year <- year(Heat.subset$DateTime)
# #change current.charges to numeric
# Heat.subset$Current.Charges <-as.numeric(as.character(Heat.subset$Current.Charges))
# Heat.subset$Borough <- as.character(Heat.subset$Borough)
# Heat.subset <- na.omit(Heat.subset) #314102 obs
# 
# #Master Data for Heating Gas
# Hmaster <- Heat.subset %>% filter(DateTime>="2013-01-01" & DateTime<="2017-12-01",Current.Charges !=0,Total.Therm !=0)
# 
# 
# #LEAFLET MAP
# shpf <- readOGR("./Map of NYCHA Developments/geo_export_f45f7888-f586-47b1-aeea-6afde3956c89.shp", stringsAsFactors=FALSE )
# proj4string(shpf) #latlong
# shpf2 <- spTransform(shpf, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
# latlong <- coordinates(shpf2)
# 
