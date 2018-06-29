# prepare data for bnlearn causal networks.
# 2015 male 
#source('./pcalg_DHS_utils.R')

############# variables 2010
#data_path <- "//home//b//zbaran/Documents/Genf/Malawi-SNF/Malawi_DHS//MWMR61DT//MWMR61FL.DTA"
DHS_rec7_males_Angola_data <- function(data_path){
  ind_MWMR61 <-  read.dta(data_path, convert.factors=FALSE)

  #ind_MWAR61 <-  read.dta("//home//b//zbaran/Documents/Genf/Malawi-SNF/Malawi_DHS//MWAR61DT//MWAR61FL.DTA")
  #names(ind_MWAR61)[1:7] <- c('mv001', 'mv002', 'mv003', 'hiv01', 'hiv02','M_hiv03','hiv05') 
  # to be able to merge HIV DHS with males DHS 
  #ind_HIV_MWMR61 <- merge(ind_MWAR61, ind_MWMR61, by=c('mv001', 'mv002', 'mv003'), all.x = FALSE, all.y=FALSE ) # leaves out the HIV females results

  indx <- sapply(ind_MWMR61, is.factor)
  ind_MWMR61[indx] <- lapply(ind_MWMR61[indx], function(x) {
    levels(x) <- make.unique(levels(x))
    x })

  media <- c("almost every day", "at least once a week",2,3)  # at least once per week
  ind_MWMR61[which(ind_MWMR61[,'mv158'] %in% media | ind_MWMR61[,'mv157'] %in% media | ind_MWMR61[,'mv159'] %in% media),'mv158'] <- "at least once a week" 
  # use radio, TV, newspaper at least once per week

  #ind_MWMR61[which(ind_MWMR61[,'mv850a']==1 | ind_MWMR61[,'mv850b']==1) ,'mv850a'] <-1
  ind_MWMR61[which(ind_MWMR61[,'mv763a']=="yes" | ind_MWMR61[,'mv763b']=="yes" | ind_MWMR61[,'mv763c']=="yes"),'mv763a'] <-"yes" # any STI, combined
  #ind_MWMR61[which(!(ind_MWMR61[,'mv744a']=='no' & ind_MWMR61[,'mv744b']=='no' & ind_MWMR61[,'mv744c']=='no' 
  #                 & ind_MWMR61[,'mv744d']=='no' & ind_MWMR61[,'mv744e']=='no')),'mv744a'] <- 'yes'
  # wife beating never justified 

  ind_MWMR61[which(!(ind_MWMR61[,'mv754jp'] =='no' & ind_MWMR61[,'mv754wp']=='no' 
                   & ind_MWMR61[,'mv756'] == 'yes'))
             ,"754jp"] <- 'don\'t know' # false believes. 
  #Mosquito bites, sharing food, healthy person with AIDS
  ind_MWMR61[which(is.na(ind_MWMR61[,'mv754jp'])),'mv754jp']<- 'don\'t know'

  ind_MWMR61[which((ind_MWMR61[,'mv778']=='no' | # not ready to care for AIDS 
                    ind_MWMR61[,'mv779']=='no' | # no teachers with AIDS
                    ind_MWMR61[,'mv777']=='no' #& # vegetables from AIDS vendor, 
                    )) , 'mv778'] <- 'no' # stigma 

  ind_MWMR61[which(ind_MWMR61[,'mv384a']=="yes" | ind_MWMR61[,'mv384b']=="yes" |
                   ind_MWMR61[,'mv384c']=="yes"), 'mv384a'] <- "yes" 
  # heard FP radio, TV, newspaper

  #ind_MWMR61[which(ind_MWMR61[,'mv467b']==1 | ind_MWMR61[,'mv467c']==1 |
  #                   ind_MWMR61[,'mv467d']==1 | ind_MWMR61[,'mv467e']==1 |
  #                   ind_MWMR61[,'mv467f']==1), 'mv467b'] <-1 
  #HC access difficulties 

  ind_MWMR61[which(!(ind_MWMR61[,'mv743a'] %in% c("1","2") & ind_MWMR61[,'mv743b'] %in% c("1","2") &
                   ind_MWMR61[,'mv743d'] %in% c("1","2"))), 'mv743a'] <-"3" 
  # final say of major purchases, visits to family etc

  #ind_MWMR61[which(is.na(ind_MWMR61[,'mv633a'])),'mv633a']<- 1 # NA ~ formerly married. TODO check
  ind_MWMR61[which(is.na(ind_MWMR61[,'mv769'])),'mv769']<-0 # 'no' could get a condom
  ind_MWMR61[which(is.na(ind_MWMR61[,'mv503'])),'mv503']<- 1 # Number of unions. actully, 0. But split no unions or one vs. more than once
  ind_MWMR61[which(is.na(ind_MWMR61[,'mv778'])),'mv778']<- 0 # 'no'
  ind_MWMR61[which(is.na(ind_MWMR61[,'mv780'])),'mv780']<- 0 # 'no' # never heard of AIDS --> we assume children should not be taught about condoms
  ind_MWMR61[which(is.na(ind_MWMR61[,'mv302'])),'mv302']<- 'never used' # doesn't know the method -> never used

  label_dataset <- 'DHS 2010, all male sample'
  weights_dhs <- ind_MWMR61$mv005

  # Labels and levels for variables
  DHS_indicators <-list(
    #  list(var= "M_hiv03", level = 1, label = "HIV status"),
    list(var = "mv013",  level = c("15-19","20-24",1,2), label = "Age"),
    list(var = "mv025", level = c("rural", "Rural", 2), label = "Urban/ \n rural"),
   # list(var = "mv130", level = 6, label = "Religion"),
    list(var = "mv151", level = c("female", "Female", 2), label = "Househead"),
    list(var = "mv155", level = c("able to read whole sentence",2), label = "Literacy"),
    list(var = "mv158", level = c("almost every day", "at least once a week",2,3), label = "Media access"),
    #list(var = "mv302", level = 2, label = "Ever \n modern \n contraception"),
   # list(var = "mv384a", level = 2, label = "Family \n planning \n on media"),
    list(var = "mv531", level = c(8,9,10,11,12,13,14,15), label = "First \n sex < 16 "), # change to 15
   # list(var = "mv633a", level = 2, label = "Exist reason \n for no sex"),
    list(var = "mv731", level = c(2, "currently working", "Currently working"), label = "Currently \n  working"), 
    #list(var = "mv483", level = 3, label = "Circumcision"), 
    list(var = "mv502", 
         level = c(1,2,"currently in union/living with a woman", 
                   "formerly in union/living with a woman"), label = "Marital status"), 
    list(var = "mv754jp", level = c(1,"yes", "don't know", "Don't know", 8), label = "False \n beliefs AIDS"),
  #  list(var = "mv762az", level = 2, label = "Source of \n condoms \n not known"),
  #  list(var = "mv769", level = 2, label = "Could \n get condom"),
  list(var = "mv744d", level = c(1,"yes", "don't know", "Don't know", 8), label = "Wife beating \n justified "), # combined a-e
     list(var = "mv822", level = c(1,"yes"), label = "Justified \n asking husband \n to use condom"),
  # list(var = "mv778", level = 2, label = "AIDS \n acceptance"), 
   # list(var = "mv780", level = 2, label = "Children \n taught condoms"),
    list(var = "mv781", level = c(1,"yes"), label = "Ever \n tested HIV"))

  df_DHS <- do.call(rbind, DHS_indicators)
  View(df_DHS)
  labels_DHS <- df_DHS[,'label']
  var_DHS <- df_DHS[,'var']
  print(var_DHS[which(!(var_DHS %in% names(ind_MWMR61)))])
  # to check which variables from the script are missing in the DHS 

  df <- ind_MWMR61
  var_list <- df_DHS
  index <- sample(c(1:nrow(df)), replace=TRUE, size = 100000, prob = df$mv005/1000000)
  weighted_df <- df[index,]
  
  a <- prepare_df_DAG(df = weighted_df, var_list = df_DHS, DHS_indicators = DHS_indicators, w_var = 'mv005')

 # a <- prepare_df_DAG(df = df, var_list = df_DHS, DHS_indicators = DHS_indicators, w_var = 'mv005')

  to_analyze <- a$to_analyze
  weights_dhs <- a$weights
  strata_dhs <- a$strata
  colnames(to_analyze) <- labels_DHS
  description <- a$description

  for (f in seq(1:length(to_analyze))){
    to_analyze[,f] <- as.numeric(to_analyze[,f]) 
  }

  View(to_analyze)
  return(list(to_analyze = to_analyze, description = description))
}

#DHS_rec5_males_Malawi_data(data_path)
