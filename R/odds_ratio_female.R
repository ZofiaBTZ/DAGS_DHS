
library(ggplot2)
library(scales)
library(reshape2)
#install.packages("RColorBrewer")
library(RColorBrewer)
library(xlsx)

MW_desc <- read.csv("../Output/Females/Output__MWIR7HFL/description.csv")
MW_analyze <- read.csv("../Output/Females/Output__MWIR7HFL/to_analyze.csv")
n <- nrow(MW_desc)


q75 <- function(x){
  return(quantile(x, 0.75))
}

q25 <- function(x){
  return(quantile(x, 0.25))
}


countries <- list.files(path = "../Output/Females/",
                        pattern = "to_analyze.csv", 
                        recursive = TRUE,
                        full.names = TRUE)
options(digits=2)
my_analyze <- read.csv("../Output/Females/Output__MWIR7HFL//to_analyze.csv")
my_n <- nrow(MW_desc)
ORs <- array(1, dim=c(my_n, my_n, length(countries)))
CORs<- array(1, dim=c(my_n, my_n, length(countries)))
ors_text <- array(1, dim=c(my_n, my_n))
ors_median <- array(1, dim=c(my_n, my_n))
cors_text <- array(1, dim=c(my_n, my_n))
dimnames(ORs)[[1]] <- colnames(MW_analyze[2:(n+1)])
dimnames(ORs)[[2]] <- colnames(MW_analyze[2:(n+1)])
dimnames(ORs)[[3]] <- substr(countries,28,29)

for (k in 1:length(countries)){
  my_analyze <- read.csv(countries[k])
 # View(my_analyze)
  for (i in 2:(n)){
    for (j in (i+1):(n+1)){
     tab_or <- table(my_analyze[,i], my_analyze[,j])
     #p <- prop.test(tab_or)
    # print(tab_or)
     t <- 1000
     CORs[i-1,j-1, k] <- cor(my_analyze[,i], my_analyze[,j])
     ORs[i-1,j-1, k] <-((tab_or[2,2]/t)*(tab_or[1,1]/t))/((tab_or[1,2]/t)*(tab_or[2,1]/t))
    # print(ORs[i-1,j-1, k])
    }
  }
  View(ORs[,,k])

  
  
  #write.xlsx(ORs[,,k], file="../Output/Females/OR_all_countriesF.xlsx", sheetName=substr(countries[k],27,29), append=TRUE)
  


  #write.csv(ors_text, "../Output/Females/CORs.csv")
}
saveRDS(ORs, file="../Output/Females/OR_all_countriesF.RDS")
#View(ORs)
#heatmap(ORs)
OR_median <- apply(ORs,c(1,2),median)
dimnames(OR_median)[[1]] <- colnames(MW_analyze[2:(n+1)])
dimnames(OR_median)[[2]] <- colnames(MW_analyze[2:(n+1)])
#ord=hclust(1-as.dist(OR_median))$order
#co <- melt(OR_median[ord,ord])
#co <- melt(OR_median)
#ggplot(co, aes(Var1, Var2)) + # x and y axes => Var1 and Var2
#  geom_tile(aes(fill = value)) + # background colours are mapped according to the value column
#  geom_text(aes(fill = co$value, label = round(co$value, 2))) + # write the values
#  scale_fill_gradient2(low = muted("midnightblue"), 
#                       mid = "white", 
#                       high = muted("darkred"), 
#                       midpoint = 1) + # determine the colour
#  theme(panel.grid.major.x=element_blank(), #no gridlines
#        panel.grid.minor.x=element_blank(), 
#        panel.grid.major.y=element_blank(), 
#        panel.grid.minor.y=element_blank(),
#        panel.background=element_rect(fill="white"), # background=white
#        axis.text.x = element_text(angle=90, hjust = 1,vjust=1,size = 6,face = "bold"),
#        plot.title = element_text(size=6,face="bold"),
#        axis.text.y = element_text(size = 6,face = "bold")) + 
#  ggtitle("Median odds ratio") + 
#  theme(legend.title=element_text(face="bold", size=14)) + 
#  scale_x_discrete(name="") +
#  scale_y_discrete(name="") +
#  labs(fill="Odds ratio")




################# All countries
#View(ORs)
#heatmap(ORs)
min_5 <- function(x){
  return(min(x,5))
}

ORs_ord <- array(0, dim=c(my_n, length(countries), my_n))


for (i in 1:n){
  for (j in 1:n){
    if (i>=j){
      ORs[i,j,]<-1
    }
    ORs[i,j,] <- sort(ORs[i,j,])
    for (k in 1:length(countries)){
      #if (k==16){
      #  ORs_ord[i,j,k]<-mean(ORs[i,j,])
      #}
      ORs_ord[i,k,j] <- ORs[i,j,k]
     
    }
  }
}

#dimnames(OR_median)[[1]] <- colnames(MW_analyze[2:(n+1)])
#dimnames(OR_median)[[2]] <- colnames(MW_analyze[2:(n+
hmcol = colorRampPalette(brewer.pal(9, "RdBu"))(100)
lab_12 <- colnames(MW_analyze[2:(n+1)])
lab_12[8] <- "Married"
lab_12[2] <- "Rural"
lab_12[3] <- "Household head female"
lab_12[1] <- "Age 24 or younger"
#
ORs_2D <- (matrix(ORs_ord, 12,12*28))
ORs_5 <- apply(ORs_2D, c(1,2), min_5)
rownames(ORs_5) <- lab_12
c_v <- rep("", 12*28)
c_v[28*(1:12)-14] <- lab_12
colnames(ORs_5) <- as.character(paste(c(1:(12*28)), "__ ", sep=""))
#colnames(ORs_5)[28*(1:12)-14] <- colnames(MW_analyze[2:(n+1)]) 
rownames(ORs_5) <- lab_12
#ord=hclust(1-as.dist(OR_median))$order
#co <- melt(OR_median[ord,ord])
co <- melt(t(ORs_5))
keeps<-c(28*(1:12)-14)
labels <- rep("",12*28)

labels[keeps] <- lab_12
ggplot(co, aes(Var1, Var2)) + # x and y axes => Var1 and Var2
  geom_tile(aes(fill = value)) + # background colours are mapped according to the value column
  #geom_text(aes(fill = co$value, label = round(co$value, 2))) + # write the values
  scale_fill_gradient2(low = muted("midnightblue"), 
                       mid = "white", 
                       high = muted("darkred"), 
                       midpoint = 1) + # determine the colour
  theme(panel.grid.major.x=element_blank(), #no gridlines
        panel.grid.minor.x=element_blank(), 
        panel.grid.major.y=element_blank(), 
        panel.grid.minor.y=element_blank(),
        panel.background=element_rect(fill="white"), # background=white
        axis.text.x = element_text(angle=90, hjust = 1,vjust=1,size = 10, color = "black"),
        plot.title = element_text(size=10,color = "black"),
        axis.ticks = element_blank(),
        axis.text.y = element_text(size = 10,color = "black")) + 
  ggtitle("Odds ratios, females, for African countries") + 
  theme(legend.title=element_text(size=14, color = "black")) + 
  #scale_x_discrete(name="") +
  scale_x_discrete(
    #breaks=c(28*(1:12)-14),
    name = "",
    labels =labels)+
  scale_y_discrete(name="") +
  labs(fill="Odds ratio")


#bk = unique(c(seq(0.1,1, length=100),seq(1,5,length=100)))
#hmcols1<- colorRampPalette(c("midnightblue","white"))(100)
#hmcols2<- colorRampPalette(c("white", "firebrickred"))(100)

#heatmap.2(ORs_5, dendrogram = "none", Rowv = FALSE, Colv=FALSE, tracecol=NA, labCol = c_v,
#          col=hmcols, breaks = bk)