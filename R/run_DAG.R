source('./pcalg_DHS_utils.R')
#source("DHS_rec5_males_Togo_data.R")
source("./DHS_rec7_males_Anogla_data.R")
source("./bnlearn_bootstrap.R")
require(tools)
#data_name <- "Malawi_male_2010_all"
#data_path <- "//home//b//zbaran/Documents/Genf/Malawi-SNF/Malawi_DHS//MWMR61DT//MWMR61FL.DTA"

data_name <- "Tanzania_male_2015_all"

data_path<- list()
#data_path[1] <- "//home//b//zbaran/Documents/Genf/Malawi-SNF/Malawi_DHS//TZMR7HDT/TZMR7HFL.DTA"
data_path[2] <- "//home//b//zbaran/Documents/Genf/Malawi-SNF/Malawi_DHS//AO_2015-16_DHS/AOMR71DT/AOMR71FL.DTA"
data_path[3] <- "//home//b//zbaran/Documents/Genf/Malawi-SNF/Malawi_DHS//BF_2010_DHS/BFMR62DT/BFMR62FL.DTA"
data_path[4] <- "//home//b//zbaran/Documents/Genf/Malawi-SNF/Malawi_DHS//BJ_2011-12_DHS/BJMR61DT/BJMR61FL.DTA"
data_path[5] <- "//home//b//zbaran/Documents/Genf/Malawi-SNF/Malawi_DHS//BU_2010_DHS/BUMR61DT/BUMR61FL.DTA"
data_path[6] <- "//home//b//zbaran/Documents/Genf/Malawi-SNF/Malawi_DHS//BF_2010_DHS/BFMR62DT/BFMR62FL.DTA"
data_path[7] <- "//home//b//zbaran/Documents/Genf/Malawi-SNF/Malawi_DHS//CD_2013-14_DHS/CDMR61DT/CDMR61FL.DTA"
data_path[8] <- "//home//b//zbaran/Documents/Genf/Malawi-SNF/Malawi_DHS/CG_2011-12_DHS/CGMR60DT/CGMR60FL.DTA"
data_path[9] <- "//home//b//zbaran/Documents/Genf/Malawi-SNF/Malawi_DHS//CI_2011-12_DHS/CIMR62DT/CIMR62FL.DTA"
data_path[10] <- "//home//b//zbaran/Documents/Genf/Malawi-SNF/Malawi_DHS//CM_2011_DHS/CMMR61DT/CMMR61FL.DTA"
data_path[11] <- "//home//b//zbaran/Documents/Genf/Malawi-SNF/Malawi_DHS//ET_2016_DHS/ETMR70DT/ETMR70FL.DTA"
data_path[12] <- "//home//b//zbaran/Documents/Genf/Malawi-SNF/Malawi_DHS//GA_2012_DHS/GAMR60DT/GAMR60FL.DTA"
data_path[13] <- "//home//b//zbaran//Documents/Genf/Malawi-SNF/Malawi_DHS/GH_2014_DHS/GHMR71DT/GHMR71FL.DTA"
data_path[14] <- "//home//b//zbaran//Documents/Genf/Malawi-SNF/Malawi_DHS/GM_2013_DHS/GMMR60DT/GMMR60FL.DTA"
#data_path[15] <- "//home//b//zbaran//Documents/Genf/Malawi-SNF/Malawi_DHS/GN_2012_DHS/GNMR62DT/GNMR62FL.DTA"
data_path[16] <- "//home//b//zbaran//Documents/Genf/Malawi-SNF/Malawi_DHS/KE_2014_DHS/KEMR70DT/KEMR70FL.DTA"
data_path[17] <- "//home//b//zbaran//Documents/Genf/Malawi-SNF/Malawi_DHS/LB_2013_DHS/LBMR6ADT/LBMR6AFL.DTA"
data_path[17] <- "//home//b//zbaran//Documents/Genf/Malawi-SNF/Malawi_DHS/LS_2014_DHS/LSMR71DT/LSMR71FL.DTA"
data_path[18] <- "//home//b//zbaran//Documents/Genf/Malawi-SNF/Malawi_DHS/MW_2015-16_DHS/MWMR7HDT/MWMR7HFL.DTA"
data_path[19] <- "//home//b//zbaran//Documents/Genf/Malawi-SNF/Malawi_DHS/ML_2012-13_DHS/MLMR6HDT/MLMR6HFL.DTA"
data_path[20] <- "//home//b//zbaran//Documents/Genf/Malawi-SNF/Malawi_DHS/MZ_2011_DHS/MZMR62DT/MZMR62FL.DTA"
data_path[21] <- "//home//b//zbaran//Documents/Genf/Malawi-SNF/Malawi_DHS/NG_2013_DHS/NGMR6ADT/NGMR6AFL.DTA"
data_path[22] <- "//home//b//zbaran//Documents/Genf/Malawi-SNF/Malawi_DHS/NI_2012_DHS/NIMR61DT/NIMR61FL.DTA"
data_path[23] <- "//home//b//zbaran//Documents/Genf/Malawi-SNF/Malawi_DHS/NM_2013_DHS/NMMR61DT/NMMR61FL.DTA"
data_path[24] <- "//home//b//zbaran//Documents/Genf/Malawi-SNF/Malawi_DHS/RW_2014-15_DHS/RWMR70DT/RWMR70FL.DTA"
data_path[25] <- "//home//b//zbaran//Documents/Genf/Malawi-SNF/Malawi_DHS/SL_2013_DHS/SLMR61DT/SLMR61FL.DTA"
  data_path[26] <- "//home//b//zbaran//Documents/Genf/Malawi-SNF/Malawi_DHS/SN_2010-11_DHS/SNMR61DT/SNMR61FL.DTA"
 # data_path[27] <- "//home//b//zbaran//Documents/Genf/Malawi-SNF/Malawi_DHS/ST_2008-09_DHS/STMR50DT/STMR50FL.DT"
#  data_path[28] <- "//home//b//zbaran//Documents/Genf/Malawi-SNF/Malawi_DHS/SZ_2006-07_DHS/SZMR51DT/szmr51fl.dta"
  data_path[29] <- "//home//b//zbaran//Documents/Genf/Malawi-SNF/Malawi_DHS/TD_2014-15_DHS/TDMR71DT/TDMR71FL.DTA"
  data_path[30] <- "//home//b//zbaran//Documents/Genf/Malawi-SNF/Malawi_DHS/TG_2013-14_DHS/TGMR61DT/TGMR61FL.DTA"
 # data_path[31] <- "//home//b//zbaran//Documents/Genf/Malawi-SNF/Malawi_DHS/TZ_2015-16_DHS/TZMR7HDT/TZMR7HFL.DTA"
  data_path[15] <- "//home//b//zbaran//Documents/Genf/Malawi-SNF/Malawi_DHS/UG_2011_DHS/UGMR60DT/UGMR60FL.DTA"
  data_path[27] <- "//home//b//zbaran//Documents/Genf/Malawi-SNF/Malawi_DHS/ZM_2013-14_DHS/ZMMR61DT/ZMMR61FL.DTA"
  data_path[28] <- "//home//b//zbaran//Documents/Genf/Malawi-SNF/Malawi_DHS/ZW_2015_DHS/ZWMR71DT/ZWMR71FL.DTA"

#data_path <- "//home//b//zbaran/Documents/Genf/Malawi-SNF/Malawi_DHS//TZ_2015-16_DHS_01212018_1622_73102/TZMR7HDT/TZMR7HFL.DTA"

#data_path <- "//home//b//zbaran/Documents/Genf/Malawi-SNF/Malawi_DHS//TZ_2015-16_DHS_01212018_1622_73102/TZMR7HDT/TZMR7HFL.DTA"
#data_path <- "//home//b//zbaran/Documents/Genf/Malawi-SNF/Malawi_DHS//ZM_2013-14_DHS_01212018_1625_73102/ZMMR61DT/ZMMR61FL.DTA"

#data_path <- "//home//b//zbaran/Documents/Genf/Malawi-SNF/Malawi_DHS//ZW_2015_DHS_01212018_1624_73102/ZWMR71DT/ZWMR71FL.DTA"

#data_path <- "//home//b//zbaran/Documents/Genf/Malawi-SNF/Malawi_DHS//UG_2011_DHS_01212018_1626_73102/UGMR60DT/UGMR60FL.DTA"

#data_name <- "togo_2013_males"
#data_path <- "//home//b//zbaran/Documents/Genf/Malawi-SNF/Malawi_DHS//TG_2013-14_DHS/TGMR61DT/TGMR61FL.DTA"
data_function <-  "DHS_rec7_males_Angola_data"
#togo <- read.dta(data_path)
#data_DHS <- data.frame(row.names = c("data_name", "data_path")) 


for (i in seq(2,30)){
  print(data_path[i])
  data_name <- file_path_sans_ext(basename(as.character(data_path[i])))
  output <- paste("../Output/Males/Output", data_name, sep="_") # handle warnings when the directory exists
  output_plots  <- paste(output, "Plots", sep = "/")
  output_models  <- paste(output, "Models", sep = "/")

if (!file.exists(output)){
  dir.create(output)
}
if (!file.exists(output_plots)){
  dir.create(output_plots)
}
if (!file.exists(output_models)){
  dir.create(output_models)
}
 
{  
  tryCatch({ clean_data <- do.call(data_function, list((as.character(data_path[i])))) 
       }, error = function(cond) {
        message(cond)
        print(paste("No good data for ", substr(data_path[i], 57, 67) )) 
        print(i)
        next()
        } 
  )
  #readline(prompt=paste(substr(data_path[i], 57, 67)," Press [enter] to continue"))
}

to_analyze <- clean_data$to_analyze
description <- clean_data$description
#View(description)

write.csv(to_analyze, paste(output, "to_analyze.csv", sep="/"))
write.csv(description, paste(output, "description.csv", sep="/"))

#output_plot <- paste(output_plots,  data_name, sep = "") #change the name of the plot
######## only to_analyze. create a new file from the part below

#model <- plot_bn_strength(to_analyze)
#DAG_matrix <- model$DAG_matrix
#DAG_graph <- model$DAG_graph
#strength_bn <- model$strength_bn

#saveRDS(DAG_matrix, paste(output_models, "DAG_matrix.rds", sep="/"))
#saveRDS(DAG_graph, paste(output_models, "DAG_graph.rds", sep="/"))
#saveRDS(strength_bn, paste(output_models, "strength_bn.rds", sep="/"))

#png(paste(output_plots, 'DAG.png', sep="/"))
###graphviz.plot(DAG_graph, shape="rectangle")
#plot_amat(DAG_matrix)
#dev.off()
}