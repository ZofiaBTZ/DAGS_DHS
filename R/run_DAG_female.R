source('./pcalg_DHS_utils.R')
#source("DHS_rec5_males_Togo_data.R")
source("./DHS_rec7_females_Anogla_data.R")
source("./bnlearn_bootstrap.R")
require(tools)
#data_name <- "Malawi_male_2010_all"
#data_path <- "//home//b//zbaran/Documents/Genf/Malawi-SNF/Malawi_DHS//MWIR61DT//MWIR61FL.DTA"



data_path<- list()
data_path[2] <- "../../../Malawi_DHS/AO_2015-16_DHS/AOIR71DT/AOIR71FL.DTA"
data_path[3] <- "../../../Malawi_DHS/BF_2010_DHS/BFIR62DT/BFIR62FL.DTA"
data_path[4] <- "../../../Malawi_DHS/BJ_2011-12_DHS/BJIR61DT/BJIR61FL.DTA"
data_path[5] <- "../../../Malawi_DHS/BU_2010_DHS/BUIR61DT/BUIR61FL.DTA"
data_path[6] <- "../../../Malawi_DHS/BF_2010_DHS/BFIR62DT/BFIR62FL.DTA"
data_path[7] <- "../../../Malawi_DHS/CD_2013-14_DHS/CDIR61DT/CDIR61FL.DTA"
data_path[8] <- "../../../Malawi_DHS/CG_2011-12_DHS/CGIR60DT/CGIR60FL.DTA"
data_path[9] <- "../../../Malawi_DHS/CI_2011-12_DHS/CIIR62DT/CIIR62FL.DTA"
data_path[10] <- "../../../Malawi_DHS/CM_2011_DHS/CMIR61DT/CMIR61FL.DTA"
data_path[11] <- "../../../Malawi_DHS/ET_2016_DHS/ETIR70DT/ETIR70FL.DTA"
data_path[12] <- "../../../Malawi_DHS/GA_2012_DHS/GAIR60DT/GAIR60FL.DTA"
data_path[13] <- "../../../Malawi_DHS/GH_2014_DHS/GHIR72DT/GHIR72FL.DTA"
data_path[14] <- "../../../Malawi_DHS/GM_2013_DHS/GMIR60DT/GMIR60FL.DTA"
#data_path[15] <- "../../../Malawi_DHS/Malawi_DHS/GN_2012_DHS/GNIR62DT/GNIR62FL.DTA"
data_path[16] <- "../../../Malawi_DHS/KE_2014_DHS/KEIR70DT/KEIR70FL.DTA"
data_path[17] <- "../../../Malawi_DHS/LB_2013_DHS/LBIR6ADT/LBIR6AFL.DTA"
data_path[30] <- "../../../Malawi_DHS/LS_2014_DHS/LSIR71DT/LSIR71FL.DTA"
data_path[18] <- "../../../Malawi_DHS/MW_2015-16_DHS/MWIR7HDT/MWIR7HFL.DTA"
data_path[19] <- "../../../Malawi_DHS/ML_2012-13_DHS/MLIR6HDT/MLIR6HFL.DTA"
data_path[20] <- "../../../Malawi_DHS/MZ_2011_DHS/MZIR62DT/MZIR62FL.DTA"
data_path[21] <- "../../../Malawi_DHS/NG_2013_DHS/NGIR6ADT/NGIR6AFL.DTA"
data_path[22] <- "../../../Malawi_DHS/NI_2012_DHS/NIIR61DT/NIIR61FL.DTA"
data_path[23] <- "../../../Malawi_DHS/NM_2013_DHS/NMIR61DT/NMIR61FL.DTA"
data_path[24] <- "../../../Malawi_DHS/RW_2014-15_DHS/RWIR70DT/RWIR70FL.DTA"
data_path[25] <- "../../../Malawi_DHS/SL_2013_DHS/SLIR61DT/SLIR61FL.DTA"
data_path[26] <- "../../../Malawi_DHS/SN_2010-11_DHS/SNIR61DT/SNIR61FL.DTA"
# data_path[27] <- "../../../Malawi_DHS/ST_2008-09_DHS/STIR50DT/STIR50FL.DT" 
#  data_path[28] <- "../../../Malawi_DHS/SZ_2006-07_DHS/SZIR51DT/szIR51fl.dta"
data_path[29] <- "../../../Malawi_DHS/TD_2014-15_DHS/TDIR71DT/TDIR71FL.DTA"
data_path[1] <- "../../../Malawi_DHS/TG_2013-14_DHS/TGIR61DT/TGIR61FL.DTA"
# data_path[31] <- "../../../Malawi_DHS/TZ_2015-16_DHS/TZIR7HDT/TZIR7HFL.DTA" #missing variables
data_path[15] <- "../../../Malawi_DHS/UG_2011_DHS/UGIR60DT/UGIR60FL.DTA"
data_path[27] <- "../../../Malawi_DHS/ZM_2013-14_DHS/ZMIR61DT/ZMIR61FL.DTA"
data_path[28] <- "../../../Malawi_DHS/ZW_2015_DHS/ZWIR71DT/ZWIR71FL.DTA"


data_function <-  "DHS_rec7_females_Angola_data"


for (i in seq(1,length(data_path))){
  #print(data_path[i])
  data_name <- file_path_sans_ext(basename(as.character(data_path[i])))
  output <- paste("../Output/Females/Output_", data_name, sep="_") # !handle warnings when the directory exists
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

output_plot <- paste(output_plots,  data_name, sep = "") #change the name of the plot
#############
model <- plot_bn_strength(to_analyze)
DAG_matrix <- model$DAG_matrix
DAG_graph <- model$DAG_graph
strength_bn <- model$strength_bn

saveRDS(DAG_matrix, paste(output_models, "DAG_matrix.rds", sep="/"))
saveRDS(DAG_graph, paste(output_models, "DAG_graph.rds", sep="/"))
saveRDS(strength_bn, paste(output_models, "strength_bn.rds", sep="/"))

#png(paste(output_plots, 'DAG.png', sep="/"))
######graphviz.plot(DAG_graph, shape="rectangle")
#plot_amat(DAG_matrix)
#dev.off()
}