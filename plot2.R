## ---------------------------
##
## Script name: plot2.R
##
## Purpose of script: To create a plot examining total tonnage of PM 2.5
## pollutants expelled into the air by Baltimore by year over the time frame
## of 1999-2008.
##
## Author: Shane Hesprich
##
## Date Created: 2022-09-13
##
## ---------------------------

# Load Data
NEI <- readRDS("summarySCC_PM25.rds")

# Subset data to only include data from Baltimore
baltimore <- subset(NEI, fips == 24510)

# Calculate sum by year
yearSum <- with(baltimore, tapply(Emissions, year, sum, na.rm = TRUE))
years <- unique(NEI$year) 

# Gen plot
png(file = "plot2.png")
main <- "Total PM 2.5 Emissions by Year"
xlab <- "Year"
ylab <- "Total Emissions (Tons)"
col <- "blue"
pch <- 18
cex <- 2
plot(years, yearSum, xlab = xlab, ylab = ylab, main = main, col = col, 
     pch = pch, cex = cex)
dev.off()
