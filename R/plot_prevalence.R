install.packages("rworldmap")
library(rworldmap)

prevalence <- read.csv("./HIV_prev.csv", header = TRUE)
matched <- joinCountryData2Map(prevalence,joinCode = "NAME", nameJoinColumn="Country")

jpeg("HIV_prevalence.jpg")
mapCountryData(matched, mapRegion = "Africa", nameColumnToPlot="Prevalence", mapTitle="HIV prevalence", catMethod = "fixedWidth", colourPalette = "heat")
dev.off()