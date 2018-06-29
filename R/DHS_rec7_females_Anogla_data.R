# prepare data for bnlearn causal networks.
# 2015 male 
#source('./pcalg_DHS_utils.R')

############# variables 2010
DHS_rec7_females_Angola_data <- function(data_path){
  ind_MWIR61 <-  read.dta(data_path)
  print(data_path)
  print(length(ind_MWIR61$v025))
  #ind_MWAR61 <-  read.dta("//home//b//zbaran/Documents/Genf/Malawi-SNF/Malawi_DHS//MWAR61DT//MWAR61FL.DTA")
  #names(ind_MWAR61)[1:7] <- c('v001', 'v002', 'v003', 'hiv01', 'hiv02','M_hiv03','hiv05') 
  # to be able to merge HIV DHS with males DHS 
  #ind_HIV_MWIR61 <- merge(ind_MWAR61, ind_MWIR61, by=c('v001', 'v002', 'v003'), all.x = FALSE, all.y=FALSE ) # leaves out the HIV females results

  indx <- sapply(ind_MWIR61, is.factor)
  ind_MWIR61[indx] <- lapply(ind_MWIR61[indx], function(x) {
    levels(x) <- make.unique(levels(x))
    x })

  media <- c("almost every day", "at least once a week",2,3)  # at least once per week
  ind_MWIR61[which(ind_MWIR61[,'v158'] %in% media | ind_MWIR61[,'v157'] %in% media | ind_MWIR61[,'v159'] %in% media),'v158'] <- "at least once a week" 
  # use radio, TV, newspaper at least once per week

  #ind_MWIR61[which(ind_MWIR61[,'v850a']==1 | ind_MWIR61[,'v850b']==1) ,'v850a'] <-1
  ind_MWIR61[which(ind_MWIR61[,'v763a']=="yes" | ind_MWIR61[,'v763b']=="yes" | ind_MWIR61[,'v763c']=="yes"),'v763a'] <-"yes" # any STI, combined
  ind_MWIR61[which(!(ind_MWIR61[,'v744a']%in% c(0,'no') & ind_MWIR61[,'v744b']%in% c(0,'no') 
                      & ind_MWIR61[,'v744c']%in% c(0,'no') & ind_MWIR61[,'v744d']%in% c(0,'no')
                     & ind_MWIR61[,'v744e']%in% c(0,'no'))),'v744a'] <- 'yes'
  # wife beating never justified 

  ind_MWIR61[which(!(ind_MWIR61[,'v754jp'] =='no' & ind_MWIR61[,'v754wp']=='no' 
                   & ind_MWIR61[,'v756'] == 'yes'))
             ,"754jp"] <- 'don\'t know' # false believes. 
  #Mosquito bites, sharing food, healthy person with AIDS
  ind_MWIR61[which(is.na(ind_MWIR61[,'v754jp'])),'v754jp']<- 'don\'t know'

  ind_MWIR61[which((ind_MWIR61[,'v778']=='no' | # not ready to care for AIDS 
                    ind_MWIR61[,'v779']=='no' | # no teachers with AIDS
                    ind_MWIR61[,'v777']=='no' #& # vegetables from AIDS vendor, 
                    )) , 'v778'] <- 'no' # stigma 

  ind_MWIR61[which(ind_MWIR61[,'v384a']=="yes" | ind_MWIR61[,'v384b']=="yes" |
                   ind_MWIR61[,'v384c']=="yes"), 'v384a'] <- "yes" 
  # heard FP radio, TV, newspaper

  #ind_MWIR61[which(ind_MWIR61[,'v467b']==1 | ind_MWIR61[,'v467c']==1 |
  #                   ind_MWIR61[,'v467d']==1 | ind_MWIR61[,'v467e']==1 |
  #                   ind_MWIR61[,'v467f']==1), 'v467b'] <-1 
  #HC access difficulties 

  ind_MWIR61[which(!(ind_MWIR61[,'v743a'] %in% c("1","2") & ind_MWIR61[,'v743b'] %in% c("1","2") &
                   ind_MWIR61[,'v743d'] %in% c("1","2"))), 'v743a'] <-"3" 
  # final say of major purchases, visits to family etc

  #ind_MWIR61[which(is.na(ind_MWIR61[,'v633a'])),'v633a']<- 1 # NA ~ formerly married. TODO check
  ind_MWIR61[which(is.na(ind_MWIR61[,'v769'])),'v769']<-0 # 'no' could get a condom
  ind_MWIR61[which(is.na(ind_MWIR61[,'v503'])),'v503']<- 1 # Number of unions. actully, 0. But split no unions or one vs. more than once
  ind_MWIR61[which(is.na(ind_MWIR61[,'v778'])),'v778']<- 0 # 'no'
  ind_MWIR61[which(is.na(ind_MWIR61[,'v780'])),'v780']<- 0 # 'no' # never heard of AIDS --> we assume children should not be taught about condoms
  ind_MWIR61[which(is.na(ind_MWIR61[,'v302'])),'v302']<- 'never used' # doesn't know the method -> never used

  label_dataset <- 'DHS 2010, all male sample'
  weights_dhs <- ind_MWIR61$v005

  # Labels and levels for variables
  DHS_indicators <-list(
    #  list(var= "M_hiv03", level = 1, label = "HIV status"),
    list(var = "v013",  level = c("15-19","20-24",1,2), label = "Age"),
    list(var = "v025", level = c("rural", "Rural", 2), label = "Urban/ \n rural"),
   # list(var = "v130", level = 6, label = "Religion"),
    list(var = "v151", level = c("female", "Female", 2), label = "Househead"),
    list(var = "v155", level = c("able to read whole sentence",2), label = "Literacy"),
    list(var = "v158", level = c("almost every day", "at least once a week",2,3), label = "Media access"),
    #list(var = "v302", level = 2, label = "Ever \n modern \n contraception"),
   # list(var = "v384a", level = 2, label = "Family \n planning \n on media"),
    list(var = "v531", level = c(8,9,10,11,12,13,14,15), label = "First \n sex < 16 "), # change to 15
   # list(var = "v633a", level = 2, label = "Exist reason \n for no sex"),
    list(var = "v731", level = c(2, "currently working", "Currently working"), label = "Currently \n  working"), 
    list(var = "v744d", level = c(1,"yes", "don't know", "Don't know", 8), label = "Wife beating \n justified "), # combined a-e
    #list(var = "v483", level = 3, label = "Circumcision"), 
    list(var = "v502", 
         level = c(1,2,"currently in union/living with a man", 
                   "formerly in union/living with a man"), label = "Marital status"), 
    list(var = "v754jp", level = c(1,"yes", "don't know", "Don't know", 8), label = "False \n beliefs AIDS"),
  #  list(var = "v762az", level = 2, label = "Source of \n condoms \n not known"),
  #  list(var = "v769", level = 2, label = "Could \n get condom"),
     list(var = "v822", level = c(1,"yes"), label = "Justified \n asking husband \n to use condom"),
  # list(var = "v778", level = 2, label = "AIDS \n acceptance"), 
   # list(var = "v780", level = 2, label = "Children \n taught condoms"),
    list(var = "v781", level = c(1,"yes"), label = "Ever \n tested HIV"))

  df_DHS <- do.call(rbind, DHS_indicators)
  #View(df_DHS)
  labels_DHS <- df_DHS[,'label']
  var_DHS <- df_DHS[,'var']
  #print(var_DHS[which(!(var_DHS %in% names(ind_MWIR61)))])
  # to check which variables from the script are missing in the DHS 

  df <- ind_MWIR61
  var_list <- df_DHS
  #View(weights_dhs)
  #print(nrow(df))
  #print(length(weights_dhs))
  index <- sample(c(1:nrow(df)), replace=TRUE, size = 100000, prob = df$v005/1000000)
  weighted_df <- df[index,]
  
  a <- prepare_df_DAG(df = weighted_df, var_list = df_DHS, DHS_indicators = DHS_indicators, w_var = 'v005')

  to_analyze <- a$to_analyze
  weights_dhs <- a$weights
  strata_dhs <- a$strata
  colnames(to_analyze) <- labels_DHS
  description <- a$description

  for (f in seq(1:length(to_analyze))){
    to_analyze[,f] <- as.numeric(to_analyze[,f]) 
  }

  #View(to_analyze)
  return(list(to_analyze = to_analyze, description = description))
}

#DHS_rec5_males_Malawi_data(data_path)
