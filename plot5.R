## ---------------------------
##
## Script name: plot5.R
##
## Purpose of script: To create a plot examining change vehicle emissions by 
## year, in the city of Baltimore over the time frame of 1999-2008. The plot 
## will examine not only total change in emissions, but also primary 
## contributors of that change based on EI Sector designations.
##
## Author: Shane Hesprich
##
## Date Created: 2022-09-13
##
## ---------------------------

library(ggplot2)
library(gridExtra)

# Load Data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Subset data to only include data from Baltimore
baltimore <- subset(NEI, fips == 24510)

# Search SCC for codes associated with vehicle emissions
vehicles <- grep('[Vv]ehicles', SCC$EI.Sector)

# Subset SCC and extract codes for vehicle emissions
vehicle_SCC <- as.character(SCC[vehicles, ]$SCC)
vehicle_EI.Sector <- as.character(SCC[vehicles, ]$EI.Sector)
vehicle_SCC <- cbind(vehicle_SCC, vehicle_EI.Sector)
vehicle_SCC <- as.data.frame(vehicle_SCC)

# Extract NEI records with SCC codes associated with vehicle emissions
vehicle_NEI <- subset(baltimore, SCC %in% vehicle_SCC$vehicle_SCC)

# Calculate sum by year
yearSum <- with(vehicle_NEI, tapply(Emissions, year, sum, na.rm = TRUE))
years <- unique(NEI$year) 

# Substitute SCC code for EI.Sector
for(code in seq_along(vehicle_SCC$vehicle_SCC)) { 
  index <- grep(vehicle_SCC$vehicle_SCC[code], vehicle_NEI$SCC)
  vehicle_NEI$SCC[index] <- vehicle_SCC$vehicle_EI.Sector[code]
}

# Calculate sum by year and type
spm25 <- data.frame()
for(years in c(1999, 2002, 2005, 2008)) { 
  x <- subset(vehicle_NEI, year == years)
  s <- with(x, tapply(Emissions, SCC, sum, na.rm = TRUE))
  s <- cbind(s, year = years, type = row.names(s))
  spm25 <- rbind(spm25, s)
}

# Fix naming and classes
row.names(spm25) <- 1:16
names(spm25)[1] <- "Emissions"
spm25$Emissions <- as.numeric(spm25$Emissions)
spm25$year <- factor(spm25$year)

years <- years <- unique(NEI$year)
total <- as.data.frame(cbind(yearSum, years))
names(total) <- c("Emissions", "year")

# Plot Data
## Parameters
color <- "blue"
shape <- 18
size <- 4
fill <- color
scales <- "free"

## Plots
p1 <- ggplot(total, aes(x = year, y = Emissions))
p1 <- p1 + geom_point(shape = shape, color = color, size = size)
p1 <- p1 + geom_line()
p1 <- p1 + ggtitle("Total Emissions")

p2 <- ggplot(data = spm25, aes(x = year, y = Emissions))
p2 <- p2 + geom_point(shape = shape, color = color, size = size)
p2 <- p2 + geom_line(group = type)
p2 <- p2 + facet_wrap(. ~ type, scales = scales, nrow = 2)
p2 <- p2 + ggtitle("Emissions by EI Sector")

png(file = "plot5.png")
grid.arrange(p1, p2, nrow = 2)
dev.off()
