## ---------------------------
##
## Script name: plot6.R
##
## Purpose of script: To create a plot examining change vehicle emissions by 
## year, in the city of Baltimore vs Los Angeles County over the time frame of 
## 1999-2008.
##
## Author: Shane Hesprich
##
## Date Created: 2022-09-15
##
## ---------------------------

library(ggplot2)
library(gridExtra)

# Load Data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Subset data to only include data from Baltimore
BA <- subset(NEI, fips == "24510")

# Subset data to only include data from Los Angeles County
LA <- subset(NEI, fips == "06037")

# Search SCC for codes associated with vehicle emissions
vehicles <- grep('[Vv]ehicles', SCC$EI.Sector)

# Subset SCC and extract codes for vehicle emissions
vehicle_SCC <- as.character(SCC[vehicles, ]$SCC)

# Extract NEI records with SCC codes associated with vehicle emissions
BA_V <- subset(BA, SCC %in% vehicle_SCC)
LA_V <- subset(LA, SCC %in% vehicle_SCC)

# Calculate sum by year
BAyearSum <- with(BA_V, tapply(Emissions, year, sum, na.rm = TRUE))
# Calculate sum by year
LAyearSum <- with(LA_V, tapply(Emissions, year, sum, na.rm = TRUE))
years <- unique(NEI$year)

# Convert to Data Frame
LAyearSum <- as.data.frame(cbind(LAyearSum, region = rep('Los Angeles County', 4), year = years))
BAyearSum <- as.data.frame(cbind(BAyearSum, region = rep('Baltimore', 4), year = years))
names(BAyearSum)[1] <- "Emissions"
names(LAyearSum)[1] <- "Emissions"
LAyearSum$Emissions <- as.numeric(LAyearSum$Emissions)
BAyearSum$Emissions <- as.numeric(BAyearSum$Emissions)

# Calculate percent change
LA99 <- LAyearSum$Emissions[1]
change <- with(LAyearSum, sapply(Emissions, function(x) (x - LA99) / LA99 ))
LAyearSum$change <- change

BA99 <- BAyearSum$Emissions[1]
change <- with(BAyearSum, sapply(Emissions, function(x) (x - BA99) / BA99 ))
BAyearSum$change <- change

# Merge Data Frames
mrg <- rbind(BAyearSum, LAyearSum)

# Plot Data
p1 <- ggplot(data = mrg, aes(x = year, y = Emissions))
p1 <- p1 + geom_point(shape = 18, color = "blue", size = 4) 
p1 <- p1 +  facet_wrap( ~ region, scales = "free", nrow = 1)

p2 <- ggplot(data = mrg, aes(x = year, y = change))
p2 <- p2 + geom_bar(stat = "identity") 
p2 <- p2 +  facet_wrap( ~ region, scales = "free", nrow = 1)

grid.arrange(p1, p2, nrow = 2)