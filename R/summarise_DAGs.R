summarise_DAGs <- function(DAGS_list)

  countries <- list.files(path = "../Output/Males/", pattern = "*/DAG_matrix",
                          all.files = FALSE,
                          full.names = FALSE, recursive = TRUE,
                          ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
  
  countries <- list.files(path = "../Output/Males/",
                          pattern = "DAG_matrix.rds", 
                          recursive = TRUE,
                          full.names = TRUE)
  
  sum_DAGs <- 0* readRDS("../Output/Males/Output_BFMR62FL/Models/DAG_matrix.rds")
  causal_edges <- 0* readRDS("../Output/Males/Output_BFMR62FL/Models/DAG_matrix.rds")
  for (i in 1:length(countries)){
    print(countries[i])
    tmp_adj <- readRDS(countries[i])

    
#    plot_amat(sum_DAGs)
    names(sum_DAGs)
    names(tmp_adj)
    sum_DAGs <- sum_DAGs + tmp_adj
    causal_edges <- causal_edges + tmp_adj - tmp_adj*t(tmp_adj)
  } 
  write.csv(sum_DAGs, "../Output/sum_DAGs.csv")
  male_sum_DAGs <- floor(((sum_DAGs)/6))
  plot_weigthed_amat(male_sum_DAGs)
  plot_weigthed_amat((causal_edges))
  View(causal_edges)
  #plot_weigthed_amat(floor(sqrt(sum_DAGs)))
 
  
  
