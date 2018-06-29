#dichotomization_table
countries <- list.files(path = "../Output/Females/",
                        pattern = "to_analyze.csv", 
                        recursive = TRUE,
                        full.names = TRUE)

tab_countries <- matrix(0, nrow = length(countries), ncol=ncol(my_analyze))
for (k in 1:length(countries)){
  my_analyze <- read.csv(countries[k])
    for (i in 1:ncol(my_analyze))
      print(table(my_analyze[,i]))
  }