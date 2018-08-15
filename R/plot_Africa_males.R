install.packages("rworldmap")
library(rworldmap)

country_codes <- read.csv("./country_codes.csv", header = TRUE)

readRDS("../Output/Males/OR_all_countries.RDS")
View(names(ORs))

#countries_values <- data.frame(matrix(NA, nrow = dim(ORs)[3], ncol = 2))
#colnames(countries_values) <- c("country", "value")
for (i in (1:(dim(ORs)[1]-1))){
  for (j in (i+1):dim(ORs)[2]){
    countries_values <- data.frame(matrix(NA, nrow = dim(ORs)[3], ncol = 2))
    colnames(countries_values) <- c("country", "value")
    for (k in 1:dim(ORs)[3]){
      countries_values[k,1] <- as.character(country_codes[country_codes$Code ==names(ORs[i,j,])[k],2])  
      countries_values[k,2] <- ORs[i,j,k]
    }
      if (nrow(countries_values) < 28){
        View(countries_values)
      }
      matched <- joinCountryData2Map(countries_values,joinCode = "NAME", nameJoinColumn="country")
      name <- paste("Males_scaled_OR_", dimnames(ORs)[[1]][j], dimnames(ORs)[[1]][i], ".jpg",sep="")
      my_catMethod=c(seq(0.0,1,length.out = 10),1/seq(0.9,0.1,length.out = 8) )
      jpeg(name)
      mapCountryData(matched, mapRegion = "Africa", nameColumnToPlot="value", mapTitle=name, catMethod = my_catMethod, colourPalette = "heat")
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
