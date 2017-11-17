#install.packages("scatterplot3d", repos="http://R-Forge.R-project.org")
library("scatterplot3d")
#install.packages("poLCA")
library("poLCA")
#install.packages("varhandle")
library(varhandle)
# By the way, for all examples in this article, you?ll need some more packages:
library("reshape2")
library("plyr")
library("dplyr")
library("poLCA")
library("ggplot2")
library("ggparallel")
library("igraph")
library("abind")
#library("tidyr")
library("knitr")
#install.packages("tidyr")
library(maps)       # Provides functions that let us plot the maps
library(mapdata) 
library(foreign)
library(readstata13)
library(fields)#
#source("https://bioconductor.org/biocLite.R")
#biocLite("RBGL")
library(RBGL)
#source("https://bioconductor.org/biocLite.R")
#biocLite("graph")
library(graph)
library(pcalg)
#source("https://bioconductor.org/biocLite.R")
#biocLite("Rgraphviz")
library("Rgraphviz")
library(survey)


# TODO: the same analysis inc. same variables across the countries.
# generalized --- concentrated comparison
# 
print("the right place")


 prepare_df_DAG <- function(df, var_list, DHS_indicators, w_var){ 
  to_analyze <- df[,c(w_var, unlist(var_list[,'var']))]
  to_analyze <- unfactor(to_analyze)
  View(to_analyze)
  no_NA = na.omit(to_analyze)
  description <-data.frame(var_dhs = character(), zero = character(), one = character(), label_dhs = character(), stringsAsFactors = FALSE)
  for (i in seq(1:length(DHS_indicators))) { 
 #   if (!(DHS_indicators[[i]]$var =="v005")){ #don't change weights
    dhs_var <- DHS_indicators[[i]]$var
    dhs_level <- DHS_indicators[[i]]$level
    #print(dhs_var)
    #print(dhs_level)
    name2one <- names(table(to_analyze[,dhs_var]))[dhs_level]
    name2zero <- names(table(to_analyze[,dhs_var]))[-dhs_level]
    print(name2zero)
    print(name2one)
    print("utils: ")
    print(nrow(to_analyze))
    print(nrow(no_NA))
    num_rec = sum(!is.na(to_analyze[,dhs_var]))
    num_1 = sum(to_analyze[,dhs_var]%in% name2one)
    num_0 = sum(to_analyze[,dhs_var]%in% name2zero)
    #to_analyze = na.omit(to_analyze)
  
    
    no_NA[no_NA[,dhs_var] %in% name2one , dhs_var] <- "DHS_one"  #100 ## change the name to DHS_one; DHS_zero
    no_NA[no_NA[,dhs_var] %in% name2zero , dhs_var] <- "DHS_zero" # 1000
   # View(no_NA)
    
    # print(setting ones and zeros)
    no_NA[no_NA[,dhs_var] =="DHS_one" , dhs_var] <- 1
    no_NA[no_NA[,dhs_var] =="DHS_zero" , dhs_var] <- 0
    num_noNA_1 <- sum(no_NA[,dhs_var] == 1)
    num_noNA_0 <- sum(no_NA[,dhs_var] == 0)
    print("rbind description")
   
    description <- rbind(description, data.frame(var_dhs = dhs_var, zero = paste(name2zero, collapse = ' , '), 
                                                one = paste(name2one, collapse = ' , '), 
                                                label_dhs =  DHS_indicators[[i]]$label, 
                                                number_records = num_rec, 
                                                number_one = num_1,
                                                number_zero = num_0,
                                                number_one_noNA = num_noNA_1,#num_noNA_1,
                                                number_zero_noNA = num_noNA_0)) 
    #}
  }
  print("created description")
  View(description)
  #write.csv(description, "desc.csv")  # save in the Output/Output_data
  #nrow(to_analyze)
  to_analyze <- no_NA
  #nrow(to_analyze)
  #View(to_analyze)
  weights_dhs <- as.numeric(to_analyze[, w_var]) 
  print("ẅeights created")
  strata_dhs <- weights_dhs
  levels(strata_dhs) <- c(1:length(unique(weights_dhs)))
  to_analyze[,w_var] <- NULL
  return(list(to_analyze = to_analyze, weights = weights_dhs, strata = strata_dhs, description = description))
  }




numGedges <- function(amat) {
  dimnames(amat) <- NULL # speed
  A <- (amat + t(amat)) != 0
  n <- nrow(A)
  if(n <= 40) ## faster
    sum(A[lower.tri(A)])
  else
    sum(A[indTri(n)])
}


plot_rfci <- function(fci_output){
  amat <- fci_output@amat
  #amat <- fci_output
  g <- as(amat,"graphNEL")
  nn <- nodes(g)
  p <- numNodes(g)
  ## n.edges <- numEdges(g) -- is too large:
  ## rather count edges such that  "<-->" counts as 1 :
  n.edges <- numGedges(amat)
  ahs <- ats <- rep("none", n.edges)
  nms <- character(n.edges)
  cmat <- array(c("0" = "none",   "1" = "odot",
                  "2" = "normal", "3" = "none")[as.character(amat)],
                dim = dim(amat), dimnames = dimnames(amat))
  iE <- 0L
  for (i in seq_len(p-1)) {
    x <- nn[i]
    for (j in (i+1):p) {
      y <- nn[j]
      if (amat[x,y] != 0) {
        iE <- iE + 1L
        ahs[[iE]] <- cmat[x,y]
        ats[[iE]] <- cmat[y,x]
        nms[[iE]] <- paste0(x,"~",y)
      }
    }
  }
  names(ahs) <- names(ats) <- nms
  edgeRenderInfo(g) <- list(arrowhead = ahs, arrowtail = ats)
  #print(nodeRenderInfo(g)) <- list(shape ='box')
  nodeRenderInfo(g) <- list(shape =c("box"), fontsize = c("30"))
  ## XXX Sep/Oct 2010  --- still current -- FIXME ??
  ## XXX undid change by MM, since edge marks didn't work anymore
  ## XXX "known bug in Rgraphviz, but not something they may fix soon"
  ## Rgraphviz::plot(g, main = main, ...)
  Rgraphviz::renderGraph(Rgraphviz::layoutGraph(g))
  #plot(g,attrs=list(node=list( fillcolor="white", 
   #                                 shape = "box",fixedsize=FALSE)))#,
#                         edge=list(color="black"),   graph=list(rankdir="LR")) )    
 }



plot_DAG <- function(df, weights, strata, my_alpha, df_labels, df_plot_name){
#  df <- to_analyze
#  df_labels <- as.vector(unlist(labels_DHS))
  fdag_cor <- cor(data.matrix(df))
#  my_alpha <- 0.0005
  
  #suffStat_gauss <- list(C=fdag_cor, n = nrow(df))
  #fci.fit_gauss <- fci(suffStat_gauss, indepTest = gaussCItest, alpha = 0.001, labels = as.vector(unlist(labels_DHS)))
  
  suffStat_dis <- list(dm = df, adaptDF = FALSE)
  print("start rfci")
  View(suffStat_dis)
  rfci.fit_dis <- rfci(suffStat_dis, indepTest = disCItest, alpha = my_alpha, labels = df_labels, skel.method = "stable",
                       conservative = TRUE)
  print("end rfci")
   plot_rfci(rfci.fit_dis)
  plot_name = paste('./plots/', df_plot_name, 'rfci_dis_unw_alpha', toString(my_alpha), '.png', sep = '')
  model_name = paste('./models/', df_plot_name, 'rfci_dis_unw_alpha', toString(my_alpha), '.rds', sep = '')
  saveRDS(rfci.fit_dis, model_name)
  dev.copy(png,plot_name)
  dev.off() 
  
  
  for (f in seq(1:length(df))){
    df[,f] <- as.numeric(df[,f]) 
  }
  
  suffStat_dis_w <- list(dm = df, adaptDF = FALSE, weights = as.numeric(weights_dhs/1000000) )
  rfci.fit_dis_w <- rfci(suffStat_dis_w, indepTest = disCItest_w, alpha = my_alpha, labels = df_labels, skel.method = "stable",
                         conservative = TRUE)
  plot_rfci(rfci.fit_dis_w)
  plot_name = paste('./plots/', df_plot_name, 'rfci_dis_w_alpha', toString(my_alpha), '.png', sep = '')
  model_name = paste('./models/', df_plot_name, 'rfci_dis_w_alpha', toString(my_alpha), '.rds', sep = '')
  saveRDS(rfci.fit_dis_w, model_name)
  dev.copy(png,plot_name)
  dev.off()  
  
  # read Sonja's comparison of discrete vs. logistic
  
  #suffStat_logistic_unw <- df#, strata = strata_dhs, weights = weights_dhs) 
  #rfci.fit_logistic_unw <- rfci(suffStat_logistic_unw, indepTest = logisticCItest_unw, alpha = my_alpha, labels = df_labels)
  #plot_rfci(rfci.fit_logistic_unw)
  #plot_name = paste('./plots/', df_plot_name, 'rfci_logistic_unw_alpha', toString(my_alpha), '.png', sep = '')
  #model_name = paste('./models/', df_plot_name, 'rfci_logistic_unw_alpha', toString(my_alpha), '.rds', sep = '')
  #saveRDS(rfci.fit_logistic_unw, model_name)
  #dev.copy(png,plot_name)
  #dev.off() 
  
  
  #suffStat_logistic <- list(dm =df, strata = as.numeric(strata_dhs), weights = as.numeric(weights_dhs)) 
  #rfci.fit_logistic <- rfci(suffStat_logistic, indepTest = logisticCItest, alpha = my_alpha, labels = df_labels)
  #plot_rfci(rfci.fit_logistic)
  #plot_name = paste('./plots/', df_plot_name, 'rfci_logistic_w_alpha', toString(my_alpha), '.png', sep = '')
  #model_name = paste('./models/', df_plot_name, 'rfci_logistic_w_alpha', toString(my_alpha), '.rds', sep = '')
  #saveRDS(rfci.fit_logistic, model_name)
  #dev.copy(png,plot_name)
  #dev.off() 
  
}


