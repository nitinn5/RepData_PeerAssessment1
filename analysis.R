
unzip("./exdata_data_NEI_data.zip", exdir = "./data")
data <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")

# PLOT 1
# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
# Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for 
# each of the years 1999, 2002, 2005, and 2008.


aggr <- aggregate(data$Emissions,by = list(data$year),FUN=sum)
names(aggr) <- c("year","emissions")
plot(aggr$year,aggr$emissions, type = "b", xlab = "year", ylab = "total emissions", main = "Total PM 2.5 emissions in the US over time", lty = 15, pch = 19)
dev.copy(png, "plot_1.png",  width = 480, height = 480, units = "px")
dev.off()

# PLOT 2
# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008? 
# Use the base plotting system to make a plot answering this question.

sub <- data[data$fips == "24510",]
aggr <- aggregate(sub$Emissions, by = list(sub$year), FUN = sum)
names(aggr) <- c("year","emissions")
with( aggr, plot(year, emissions, type = "b",lty = 15, pch = 19, main = "Total PM 2.5 emission in Baltimore Maryland over time"))
dev.copy(png, "plot_2.png",  width = 480, height = 480, units = "px")
dev.off()


# PLOT 3
# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) 
# variable, which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? 
# Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system

library(dplyr)
sub <- data[data$fips == "24510",]

sub_point <- sub[sub$type == "POINT",]
aggr_point = sub_point %>% group_by(year) %>% summarize(sum(Emissions, na.rm = TRUE))
names(aggr_point) = c("year","point")

sub_nonpoint <- sub[sub$type == "NONPOINT",]
aggr_nonpoint = sub_nonpoint %>% group_by(year) %>% summarize(sum(Emissions, na.rm = TRUE))
names(aggr_nonpoint) = c("year","nonpoint")

sub_onroad <- sub[sub$type == "ON-ROAD",]
aggr_onroad = sub_onroad %>% group_by(year) %>% summarize(sum(Emissions, na.rm = TRUE))
names(aggr_onroad) = c("year","onroad")

sub_nonroad <- sub[sub$type == "NON-ROAD", ]
aggr_nonroad = sub_nonroad %>% group_by(year) %>% summarize(sum(Emissions, na.rm = TRUE))
names(aggr_nonroad) = c("year","nonroad")

aggr <- cbind(aggr_point, aggr_nonpoint[,2], aggr_onroad[,2] , aggr_nonroad[,2])
newdata <- data.frame(matrix(rnorm(48),nrow=16,ncol=3))
names(newdata) <- c("year","emissions","type")
newdata$year <- c(rep("1999",4),rep("2002",4),rep("2005",4),rep("2008",4))
newdata$emissions <- c(aggr$point,aggr$nonpoint,aggr$onroad,aggr$nonroad)
newdata$type <- as.factor(c(1,2,3,4,1,2,3,4,1,2,3,4,1,2,3,4))
newdata <- transform(newdata, type = revalue(type, c("1"="point", "2" = "nonpoint", "3" = "onroad", "4" = "nonroad")))
library("ggplot2")
newdata %>% ggplot(aes(x=year,y=emissions,group=type, color = type))+geom_line(size = 1)+geom_point(size = 4)
dev.copy(png, "plot_3.png",  width = 480, height = 480, units = "px")
dev.off()


# PlOT 4 code

unzip("./exdata_data_NEI_data.zip", exdir = "./data")
data <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")

scc_coal <- as.vector(SCC[grepl("coal",SCC$EI.Sector,ignore.case = TRUE)>0 ,1])
sub <- data[data$SCC %in% scc_coal,]
aggr <- aggregate(sub$Emissions,by = list(sub$year),FUN=sum)
names(aggr) <- c("year","CoalEmissions")
plot(aggr$year,aggr$CoalEmissions, type = "b", xlab = "year", ylab = "total coal emissions", main = "Total Coal Related PM 2.5 Emissions in the US over time", lty = 15, pch = 19)
dev.copy(png, "plot_4.png",  width = 480, height = 480, units = "px")
dev.off()


# PLOT 5 code

unzip("./exdata_data_NEI_data.zip", exdir = "./data")
data <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")

sub <- data[data$fips == "24510",]
scc_vehicles <- as.vector(SCC[grepl("vehicles",SCC$EI.Sector,ignore.case = TRUE)>0 ,1])
sub <- sub[sub$SCC %in% scc_vehicles , ]
aggr <- aggregate(sub$Emissions,by = list(sub$year),FUN=sum)
names(aggr) <- c("year","VehicleEmissions")
plot(aggr$year,aggr$VehicleEmissions, type = "b", xlab = "year", ylab = "total motor vehicle emissions", main = "Total Motor Vehicle Related PM 2.5 Emissions in Baltimore over time", lty = 15, pch = 19)
dev.copy(png, "plot_5.png",  width = 480, height = 480, units = "px")
dev.off()


# PLOT 6 code

unzip("./exdata_data_NEI_data.zip", exdir = "./data")
data <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")

scc_vehicles <- as.vector(SCC[grepl("vehicles",SCC$EI.Sector,ignore.case = TRUE)>0 ,1])

baltimore <- data[data$fips == "24510",]
baltimore <- baltimore[baltimore$SCC %in% scc_vehicles, ]
losangeles <- data[data$fips == "06037",]
losangeles <- losangeles[losangeles$SCC %in% scc_vehicles, ]

aggr_baltimore <- aggregate(baltimore$Emissions,by = list(sub$year), FUN = sum)
aggr_baltimore$city <- "Baltimore"
aggr_losangeles <- aggregate(losangeles$Emissions, by = list(losangeles$year), FUN = sum)
aggr_losangeles$city <- "Los Angeles"
aggr <- rbind(aggr_baltimore,aggr_losangeles)
names(aggr) <- c("year","emissions","city")

library("ggplot2")
aggr %>% ggplot(aes(x=year,y=emissions,group=city, color = city))+geom_line(size = 1)+geom_point(size = 4)
dev.copy(png, "plot_6.png",  width = 480, height = 480, units = "px")
dev.off()



