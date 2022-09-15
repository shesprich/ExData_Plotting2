## ---------------------------
##
## Script name: plot3.R
##
## Purpose of script: To create a plot examining total tonnage of PM 2.5
## pollutants expelled by source, into the air by the city of Baltimore by
## year over the time frame of 1999-2008.
##
## Author: Shane Hesprich
##
## Date Created: 2022-09-13
##
## ---------------------------

library(ggplot2)
library(RColorBrewer)

# Load Data
NEI <- readRDS("summarySCC_PM25.rds")

# Subset data to only include data from Baltimore
baltimore <- subset(NEI, fips == 24510)

# Calculate sum by year and type
spm25 <- data.frame()
for(years in c(1999, 2002, 2005, 2008)) { 
  x <- subset(baltimore, year == years)
  s <- with(x, tapply(Emissions, type, sum, na.rm = TRUE))
  s <- cbind(s, year = years, type = row.names(s))
  spm25 <- rbind(spm25, s)
}

# Fix naming and classes
row.names(spm25) <- 1:16
names(spm25)[1] <- "Emissions"
spm25$Emissions <- as.numeric(spm25$Emissions)
spm25$year <- factor(spm25$year)

png(file = "plot3.png")
# Plot Data
g <- ggplot(data = spm25, aes(x = type, y = Emissions, fill = year))
g <- g + geom_bar(stat = "identity", position = "dodge") +
scale_fill_brewer(type = "qual", palette = "Set2")
print(g)
dev.off()

