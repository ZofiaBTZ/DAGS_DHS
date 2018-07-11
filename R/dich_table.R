#dichotomization_table
countries <- list.files(path = "../Output/Females/",
                        pattern = "to_analyze.csv", 
                        recursive = TRUE,
                        full.names = TRUE)

full_name <- function(country_code){
  country_codes$Country.Name[country_codes$Code == country_code]
}

country_codes <- read.csv("./country_codes.csv", header = TRUE, stringsAsFactors=FALSE)

countries_variables <- data.frame(matrix(0, nrow = 12*2, ncol = 28))

var_levels <- expand.grid(c("(level 0)", "(level 1)"), colnames(my_analyze) )
var_levels$name <- paste(var_levels[,2], var_levels[,1],sep = "_")

rownames(countries_variables) <- var_levels$name[-c(1,2)]
codes <- (substr(countries, 28,29))
full_names <- ((lapply(codes, full_name)))
colnames(countries_variables) <- full_names

for (k in 1:length(countries)){
  my_analyze <- read.csv(countries[k])
    for (i in 2:ncol(my_analyze)){
      
      countries_variables[(2*(i-1)-1):(2*(i-1)), k] <- (table(my_analyze[,i])/nrow(my_analyze))
    }
}

med_variables <- round(apply(countries_variables, 1 , median), digits = 2)
min_variables <- round(apply(countries_variables, 1, min), digits = 2)
max_variables <- round(apply(countries_variables, 1, max), digits = 2) 
View(cbind(med_variables, min_variables, max_variables))
output_variables <- paste(med_variables, "(", min_variables, "-", max_variables, ")", sep="")
names(output_variables) <- names(med_variables)
View(output_variables)

