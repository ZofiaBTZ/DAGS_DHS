
countries <- list.files(path = "../Output/Males/",
                        pattern = "DAG_matrix.rds", 
                        recursive = TRUE,
                        full.names = TRUE)

m0 <- 0*readRDS(countries[1])
for (i in 1:length(countries)){
  m0 <-m0 +readRDS(countries[i])
}
plot_weigthed_amat(floor(m0/6))


countries <- list.files(path = "../Output/Females/",
                        pattern = "DAG_matrix.rds", 
                        recursive = TRUE,
                        full.names = TRUE)

f0 <- 0*readRDS(countries[1])
for (i in 1:length(countries)){
  f0 <-f0 +readRDS(countries[i])
}
plot_weigthed_amat(floor(f0/6))
View(floor(f0/6))