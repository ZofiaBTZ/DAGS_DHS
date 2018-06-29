# prepare data for bnlearn causal networks.
# 2010 male 
source('./pcalg_DHS_utils.R')

############# variables 2010
#data_path <- "//home//b//zbaran/Documents/Genf/Malawi-SNF/Malawi_DHS//MWMR61DT//MWMR61FL.DTA"
DHS_prepare_data <- function(data_path, HIV_path = "", var_csv){
  var_info <- read.csv(var_csv)
  DHS_data <- read.dta(data_path)
  #if (HIV_path!=""){
  #  HIV_data <- read.dta(HIV_path)
  #  names(HIV_data)[1:7] <- c('v001', 'v002', 'v003', 'hiv01', 'hiv02','hiv03','hiv05')
  #  DHS_data_HIV <- merge(HIV_data, DHS_data, by=c('v001', 'v002', 'v003'), all.x = FALSE, all.y=FALSE ) 
  #  DHS_data <- DHS_data_HIV
  #}
  var_info[,1] <- tolower(var_info[,1])
  vv <- unique(tolower(var_info[,1]))
  vars <- intersect(vv, colnames(DHS_data))
  vars <- c(vars, "v005", "hiv03")
  var_info[,1] <- tolower(var_info[,1])
  DHS_sub <- DHS_data[,colnames(DHS_data) %in% vars]
  #for (i in seq(1:length(vars))){
    #levels(DHS_data[,vars[i]] )[levels(DHS_data[,vars[i]] )=="beta"] <- "two"  
   # levels_var <-var_info[var_info[,1]==vars[i],] 
    #for (f in seq(1:nrow(levels_var))){
     # lev = levels_var[f,3]
      #print(lev)
      #lev_name = levels_var[f,4]
      #print(lev_name)
      #if (!(lev == c(" na"))){
      #print("na")
      #if (is.null(lev)){
      #  print("lev is null")
      #}
      #if (is.null(lev_name)){
      #  print("lev_name is null")
      #}
      #if (length(DHS_sub[DHS_sub[,vars[i]]==str(lev), vars[i]])>0) {
      #  print("str_lev")
      #  print(str(lev))
      #  print(str(lev_name))
        #print(DHS_sub[DHS_sub[,vars[i]]==str(lev), vars[i]] )
        #print(length(DHS_sub[DHS_sub[,vars[i]]==str(lev), vars[i]] ))
      #  print("end str lev")
        #DHS_sub[DHS_sub[,vars[i]]==str(lev), vars[i]]  <- str(lev_name)
      #}
      #if (length(DHS_sub[DHS_sub[,vars[i]]==lev, vars[i]] >0)){
      #  print("lev")
      #  print(lev)
      #  print(str(lev))
      #  print(str(lev_name))
        #print(DHS_sub[DHS_sub[,vars[i]]==lev, vars[i]] )
        #print(length(DHS_sub[DHS_sub[,vars[i]]==lev, vars[i]] ))
      #  print("end lev")
        #DHS_sub[DHS_sub[,vars[i]]==lev, vars[i]]   <- str(lev_name)
      #} 
      #  print("change name of level")
      #}
      #else {
      #  print("sth else??")
      #  DHS_sub[DHS_sub[,vars[i]]==str(lev), vars[i]] <- str(lev_name)
      #}
   # }
  #} 
  View(DHS_sub)

  #ind_MWAR61 <-  read.dta("//home//b//zbaran/Documents/Genf/Malawi-SNF/Malawi_DHS//MWAR61DT//MWAR61FL.DTA")

  #names(ind_MWAR61)[1:7] <- c('v001', 'v002', 'v003', 'hiv01', 'hiv02','M_hiv03','hiv05') 
  # to be able to merge HIV DHS with males DHS 

  #ind_HIV_MWMR61 <- merge(ind_MWAR61, DHS_sub, by=c('v001', 'v002', 'v003'), all.x = FALSE, all.y=FALSE ) # leaves out the HIV females results

  indx <- sapply(DHS_sub, is.factor)
  DHS_sub[indx] <- lapply(DHS_sub[indx], function(x) {
    levels(x) <- make.unique(levels(x))
    x })
  print("media")
  media <- c( 2,3)  # at least once per week
  table(DHS_sub$v158)
  #DHS_sub[which(DHS_sub[,'v158'] %in% media | DHS_sub[,'v157'] %in% media | DHS_sub[,'v159'] %in% media),'v158'] <- 2 
  # use radio, TV, newspaper at least once per week
  print("wb")
  #DHS_sub[which(DHS_sub[,'v850a']==1 | DHS_sub[,'v850b']==1) ,'v850a'] <-1
  #DHS_sub[which(DHS_sub[,'v763a']=="yes" | DHS_sub[,'v763b']=="yes" | DHS_sub[,'v763c']=="yes"),'v763a'] <-"yes" # any STI, combined
  DHS_sub[which(!(DHS_sub[,'v744b']==0 & DHS_sub[,'v744b']==0 & DHS_sub[,'v744c']==0 
                   & DHS_sub[,'v744d']==0 & DHS_sub[,'v744e']==0)),'v744b'] <- 1 
  # wife beating never justified 
  print("false")
  DHS_sub[which(!(DHS_sub[,'v754jp'] ==0 & DHS_sub[,'v754wp']==0 
                   & DHS_sub[,'v756'] == 1))
             ,"754jp"] <- 1 # false believes. 
  #Mosquito bites, sharing food, healthy person with AIDS
  DHS_sub[which(is.na(DHS_sub[,'v754jp'])),'v754jp']<- 1

  #DHS_sub[which((DHS_sub[,'v778']=='no' | # not ready to care for AIDS 
  #                  DHS_sub[,'v779']=='no' | # no teachers with AIDS
  #                  DHS_sub[,'v777']=='no' #& # vegetables from AIDS vendor, 
  #                  )) , 'v778'] <- 'no' # stigma 
 print("fp")
  DHS_sub[which(DHS_sub[,'v384a']=="yes" | DHS_sub[,'v384b']=="yes" |
                   DHS_sub[,'v384c']=="yes"), 'v384a'] <- "yes" 
  # heard FP radio, TV, newspaper

  #DHS_sub[which(DHS_sub[,'v467b']==1 | DHS_sub[,'v467c']==1 |
  #                   DHS_sub[,'v467d']==1 | DHS_sub[,'v467e']==1 |
  #                   DHS_sub[,'v467f']==1), 'v467b'] <-1 
  #HC access difficulties 

 # DHS_sub[which(!(DHS_sub[,'v743a'] %in% c("1","2") & DHS_sub[,'v743b'] %in% c("1","2"))), 
#          'v743a'] <-"3"
                #  & DHS_sub[,'v743d'] %in% c("1","2"))
                 
  # final say of major purchases, visits to family etc

  #DHS_sub[which(is.na(DHS_sub[,'v633a'])),'v633a']<- 1 # NA ~ formerly married. TODO check
  #DHS_sub[which(is.na(DHS_sub[,'v769'])),'v769']<-0 # 'no' could get a condom
  DHS_sub[which(is.na(DHS_sub[,'v503'])),'v503']<- 1 # Number of unions. actully, 0. But split no unions or one vs. more than once
  #DHS_sub[which(is.na(DHS_sub[,'v778'])),'v778']<- 0 # 'no'
  #DHS_sub[which(is.na(DHS_sub[,'v780'])),'v780']<- 0 # 'no' # never heard of AIDS --> we assume children should not be taught about condoms
  #DHS_sub[which(is.na(DHS_sub[,'v302'])),'v302']<- 'never used' # doesn't know the method -> never used

  label_dataset <- 'DHS 2010, all male sample'
  weights_dhs <- DHS_sub$v005
  print("start choose indicators")
  # Labels and levels for variables
  DHS_indicators <-list(
    #  list(var= "M_hiv03", level = 1, label = "HIV status"),
    list(var = "v013",  level = c(1,2), label = "Age"),
    list(var = "v025", level = 1, label = "Urban/ \n rural"),
    list(var = "v130", level = 6, label = "Religion"),
    list(var = "v151", level = 2, label = "Househead"),
    list(var = "v155", level = 3, label = "Literacy"),
    list(var = "v158", level = 3, label = "Media access"),
   # list(var = "v302", level = 2, label = "Ever \n modern \n contraception"),
    list(var = "v384a", level = 2, label = "Family \n planning \n on media"),
    list(var = "v531", level = c(2,3,4,5,6,7,8,9), label = "First \n sex < 16 "), # change to 15
  #  list(var = "v633a", level = 2, label = "Exist reason \n for no sex"),
    list(var = "v731", level = c(1,2), label = "Currently \n  working"), 
    list(var = "v744b", level = 2, label = "Wife beating \n justified "), # combined a-e
    #list(var = "v483", level = 3, label = "Circumcision"), 
    list(var = "v502", level = 3, label = "Marital status"), 
    list(var = "v754jp", level = c(2,3,4), label = "False \n beliefs AIDS"),
   # list(var = "v762az", level = 2, label = "Source of \n condoms \n not known"),
  #  list(var = "v769", level = 2, label = "Could \n get condom"),
   # list(var = "v778", level = 2, label = "AIDS \n acceptance"), 
    #list(var = "v780", level = 2, label = "Children \n taught condoms"),
    list(var = "v781", level = 2, label = "Ever \n tested HIV"))
 # list(var = "hiv03", level = 1, label = "HIV status"))
  print("chosen indicators")
  df_DHS <- do.call(rbind, DHS_indicators)
  labels_DHS <- df_DHS[,'label']
  var_DHS <- df_DHS[,'var']
  print(var_DHS[which(!(var_DHS %in% names(DHS_sub)))])
  # to check which variables from the script are missing in the DHS 

  df <- DHS_sub
  var_list <- df_DHS
  print("prepare DHS for DAG")
  a <- prepare_df_DAG(df = df, var_list = df_DHS, DHS_indicators = DHS_indicators, w_var = 'v005')

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

#data_path <- "//home//b//zbaran/Documents/Genf/Malawi-SNF/Malawi_DHS//MWMR61DT//MWIR61FL.DTA"
var_csv <- "//home//b//zbaran/Documents/Genf/Malawi-SNF/process_DHS/clean_DHS/output/MWIR7HFL_levels.csv"
data_path_2016 <- "//home//b//zbaran/Documents/Genf/Malawi-SNF/Malawi_DHS//MW_2015-16_DHS_03092018_45_117973/MWIR7HDT/MWIR7HFL.DTA"
HIV_path_2016 <- "//home//b//zbaran/Documents/Genf/Malawi-SNF/Malawi_DHS//MW_2015-16_DHS_03092018_45_117973/MWAR7ADT/MWAR7AFL.DTA"
data_path_2010 <- "//home//b//zbaran/Documents/Genf/Malawi-SNF/Malawi_DHS//MWIR61DT/MWIR61FL.DTA"
HIV_path_2010 <-  "//home//b//zbaran/Documents/Genf/Malawi-SNF/Malawi_DHS//MWAR61DT/MWAR61FL.DTA"
data_path_2000 <- "//home//b//zbaran/Documents/Genf/Malawi-SNF/Malawi_DHS//MWIR41DT/MWIR41FL.DTA"
#HIV_path_2000 <- "//home//b//zbaran/Documents/Genf/Malawi-SNF/Malawi_DHS//MWAR61DT/MWAR61FL.DTA"

#MHH <- "//home//b//zbaran/Documents/Genf/Malawi-SNF/Malawi_DHS//MW_2015-16_DHS_03092018_45_117973/MWAR7ADT/MWAR7AFL.DTA

to_analyze_MW_females_2010 <- DHS_prepare_data(data_path_2010, HIV_path_2010, var_csv)
#to_analyze_MW_females_2010 <- DHS_prepare_data(data_path_2010, HIV_path_2010, var_csv)
#to_analyze_MW_females_2000 <- DHS_prepare_data(data_path_2000, HIV_path_2000, var_csv)

View(to_analyze_MW_females_2010$description)
#View(to_analyze_MW_females_2010$description)
#View(to_analyze_MW_females_2000$description)

#saveRDS(to_analyze_MW_females_2000, "to_analyze_MW_females_2000.rds")
#saveRDS(to_analyze_MW_females_2010, "to_analyze_MW_females_2010.rds")
saveRDS(to_analyze_MW_females_2010, "to_analyze_MW_females_2010.rds")

write.csv(to_analyze_MW_females_2010$description, "to_analyze_MW_females_2010.csv")
#write.csv(DAG_data_2010$description, "MW_2010_f.csv")
#write.csv(DAG_data_2000$description, "MW_2000_f.csv")
