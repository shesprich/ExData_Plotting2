## ---------------------------
##
## Script name: plot1.R
##
## Purpose of script: To create a plot examining total tonnage of PM 2.5
## pollutants expelled into the air by the US by year over the time frame
## of 1999-2008.
##
## Author: Shane Hesprich
##
## Date Created: 2022-09-13
##
## ---------------------------

# Load Data
NEI <- readRDS("summarySCC_PM25.rds")

# Calculate sum by year
yearSum <- with(NEI, tapply(Emissions, year, sum, na.rm = TRUE))
years <- unique(NEI$year) 

# Gen plot
png(file = "plot1.png")
main <- "Total PM 2.5 Emissions by Year"
xlab <- "Year"
ylab <- "Total Emissions (Tons)"
col <- "blue"
pch <- 18
cex <- 2
plot(years, yearSum, xlab = xlab, ylab = ylab, main = main, col = col, 
     pch = pch, cex = cex)
dev.off()
