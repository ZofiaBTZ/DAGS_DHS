install.packages("rworldmap")
library(rworldmap)

country_codes <- read.csv("./country_codes.csv", header = TRUE)

readRDS("../Output/Females/OR_all_countriesF.RDS")
View(names(ORs))

#countries_values <- data.frame(matrix(NA, nrow = dim(ORs)[3], ncol = 2))
#colnames(countries_values) <- c("country", "value")
for (i in 1:dim(ORs)[1]){
  for (j in (i+1):dim(ORs)[2]){
    countries_values <- data.frame(matrix(NA, nrow = dim(ORs)[3], ncol = 2))
    colnames(countries_values) <- c("country", "value")
    for (k in 1:dim(ORs)[3]){
      countries_values[k,1] <- as.character(country_codes[country_codes$Code ==names(ORs[i,j,])[k],2])  
      countries_values[k,2] <- ORs[i,j,k]
    }
      View(countries_values)
      matched <- joinCountryData2Map(countries_values,joinCode = "NAME", nameJoinColumn="country")
      name <- paste("OR_", names(ORs[i,,k])[j], names(ORs[,j,k])[i], ".jpg",sep="")
      jpeg(name)
      mapCountryData(matched, mapRegion = "Africa", nameColumnToPlot="value", mapTitle=name, catMethod = "pretty", colourPalette = "heat")
      dev.off()
    
  }
  
}

matched <- joinCountryData2Map(countries_values,joinCode = "NAME", nameJoinColumn="country")
mapCountryData(matched, mapRegion = "Africa", nameColumnToPlot="value", mapTitle="Met Collection Country Sample", catMethod = "pretty", colourPalette = "heat")

plot_Africa <- function(countries_values){
  # Get the shape file of Africa

  # You can do the same for afr_cartogram_df
  
}

data("countryExData",envir=environment(),package="rworldmap")

sPDF <- joinCountryData2Map(countryExData
                            , joinCode = "ISO3"
                            , nameJoinColumn = "ISO3V10"
)
mapCountryData( sPDF
                , nameColumnToPlot="BIODIVERSITY" 
)
