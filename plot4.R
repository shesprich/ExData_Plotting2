## ---------------------------
##
## Script name: plot4.R
##
## Purpose of script: To create a plot examining change in coal combustion
## related pollution emissions across the US from the time period 1999-2008.
## The plot will examine not only total change in emissions, but also primary
## contributors of that change based on SCC Level Two designations.
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

# Search SCC for codes associated with coal combustion
coal <- grep('[Cc]oal', SCC$EI.Sector)

# Subset SCC and extract codes for coal combustion
coal_SCC <- as.character(SCC[coal, ]$SCC)
coal_SCC.Level.Two <- as.character(SCC[coal, ]$SCC.Level.Two)
coal_SCC <- cbind(coal_SCC, coal_SCC.Level.Two)
coal_SCC <- as.data.frame(coal_SCC)

# Extract NEI records with SCC codes associated with coal combustion
coal_NEI <- subset(NEI, SCC %in% coal_SCC$coal_SCC)

# Calculate sum by year
yearSum <- with(coal_NEI, tapply(Emissions, year, sum, na.rm = TRUE))
years <- unique(NEI$year) 

# Substitute SCC code for SCC.Level.Two
for(code in seq_along(coal_SCC$coal_SCC)) { 
  index <- grep(coal_SCC$coal_SCC[code], coal_NEI$SCC)
  coal_NEI$SCC[index] <- coal_SCC$coal_SCC.Level.Two[code]
}

# Calculate sum by year and type
spm25 <- data.frame()
for(years in c(1999, 2002, 2005, 2008)) { 
  x <- subset(coal_NEI, year == years)
  s <- with(x, tapply(Emissions, SCC, sum, na.rm = TRUE))
  s <- cbind(s, year = years, type = row.names(s))
  spm25 <- rbind(spm25, s)
}

# Fix naming and classes
row.names(spm25) <- 1:21
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
p2 <- p2 + ggtitle("Emissions by SCC Level Two")

png(file = "plot4.png")
grid.arrange(p1, p2, nrow = 2)
dev.off()

