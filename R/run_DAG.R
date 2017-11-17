data_name <- "Malawi_male_2010_all"
data_path <- "//home//b//zbaran/Documents/Genf/Malawi-SNF/Malawi_DHS//MWMR61DT//MWMR61FL.DTA"

output <- paste("../Output/Output", data_name, sep="_") # handle warnings when the directory exists
output_plots  <- paste(output, "Plots", sep = "/")
output_models  <- paste(output, "Models", sep = "/")

dir.create(output)
dir.create(output_plots)
dir.create(output_models)

clean_data <- DHS_rec5_males_Malawi_data(data_path)
to_analyze <- clean_data$to_analyze
description <- clean_data$description

saveRDS(to_analyze, paste(output, "to_analyze.rds", sep="/"))
saveRDS(description, paste(output, "description.rds", sep="/"))

#output_plot <- paste(output_plots,  data_name, sep = "") #change the name of the plot
model <- plot_bn_strength(to_analyze)
DAG_matrix <- model$DAG_matrix
DAG_graph <- model$DAG_graph
strength_bn <- model$strength_bn

saveRDS(DAG_matrix, paste(output_models, "DAG_matrix.rds", sep="/"))
saveRDS(DAG_graph, paste(output_models, "DAG_graph.rds", sep="/"))
saveRDS(strength_bn, paste(output_models, "strength_bn.rds", sep="/"))

png(paste(output_plots, 'DAG.jpg', sep="/"))
graphviz.plot(DAG_graph, shape="rectangle")
#plot_amat(DAG_graph)
dev.off()
