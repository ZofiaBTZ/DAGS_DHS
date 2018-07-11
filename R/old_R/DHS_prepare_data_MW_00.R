# prepare data for bnlearn causal networks.
# 2010 male 
source('./pcalg_DHS_utils.R')

############# variables 2010
#data_path <- "//home//b//zbaran/Documents/Genf/Malawi-SNF/Malawi_DHS//MWMR61DT//MWMR61FL.DTA"
DHS_prepare_data <- function(data_path, var_csv){
  var_info <- read.csv(var_csv)
  DHS_data <- read.dta(data_path)
  var_info[,1] <- tolower(var_info[,1])
  vv <- unique(tolower(var_info[,1]))
  vars <- intersect(vv, colnames(DHS_data))
  vars <- c(vars, "mv005")
  var_info[,1] <- tolower(var_info[,1])
  DHS_sub <- DHS_data[,colnames(DHS_data) %in% vars]
  for (i in seq(1:length(vars))){
    #levels(DHS_data[,vars[i]] )[levels(DHS_data[,vars[i]] )=="beta"] <- "two"  
    levels_var <-var_info[var_info[,1]==vars[i],] 
    for (f in seq(1:nrow(levels_var))){
      lev = levels_var[f,3]
      print(lev)
      lev_name = levels_var[f,4]
      print(lev_name)
      #if (!(lev == c(" na"))){
      #print("na")
      if (is.null(lev)){
        print("lev is null")
      }
      if (is.null(lev_name)){
        print("lev_name is null")
      }
      #if (length(DHS_sub[DHS_sub[,vars[i]]==str(lev), vars[i]])>0) {
      #  print("str_lev")
      #  print(str(lev))
       # print(str(lev_name))
        #print(DHS_sub[DHS_sub[,vars[i]]==str(lev), vars[i]] )
        #print(length(DHS_sub[DHS_sub[,vars[i]]==str(lev), vars[i]] ))
      #  print("end str lev")
        #DHS_sub[DHS_sub[,vars[i]]==str(lev), vars[i]]  <- str(lev_name)
     # }
     # if (length(DHS_sub[DHS_sub[,vars[i]]==lev, vars[i]] >0)){
    #    print("lev")
    #    print(lev)
    #    print(str(lev))
    #    print(str(lev_name))
        #print(DHS_sub[DHS_sub[,vars[i]]==lev, vars[i]] )
        #print(length(DHS_sub[DHS_sub[,vars[i]]==lev, vars[i]] ))
     #   print("end lev")
        #DHS_sub[DHS_sub[,vars[i]]==lev, vars[i]]   <- str(lev_name)
    #  } 
      #  print("change name of level")
      #}
      #else {
      #  print("sth else??")
      #  DHS_sub[DHS_sub[,vars[i]]==str(lev), vars[i]] <- str(lev_name)
      #}
    }
  } 
  View(DHS_sub)

  #ind_MWAR61 <-  read.dta("//home//b//zbaran/Documents/Genf/Malawi-SNF/Malawi_DHS//MWAR61DT//MWAR61FL.DTA")

  #names(ind_MWAR61)[1:7] <- c('mv001', 'mv002', 'mv003', 'hiv01', 'hiv02','M_hiv03','hiv05') 
  # to be able to merge HIV DHS with males DHS 

  #ind_HIV_MWMR61 <- merge(ind_MWAR61, DHS_sub, by=c('mv001', 'mv002', 'mv003'), all.x = FALSE, all.y=FALSE ) # leaves out the HIV females results

  indx <- sapply(DHS_sub, is.factor)
  DHS_sub[indx] <- lapply(DHS_sub[indx], function(x) {
    levels(x) <- make.unique(levels(x))
    x })

  #media <- c( 2,3)  # at least once per week
  #DHS_sub[which(DHS_sub[,'mv158'] %in% media | DHS_sub[,'mv157'] %in% media | DHS_sub[,'mv159'] %in% media),'mv158'] <- 2 
  # use radio, TV, newspaper at least once per week

  #DHS_sub[which(DHS_sub[,'mv850a']==1 | DHS_sub[,'mv850b']==1) ,'mv850a'] <-1
  DHS_sub[which(DHS_sub[,'mv763a']=="yes" | DHS_sub[,'mv763b']=="yes" | DHS_sub[,'mv763c']=="yes"),'mv763a'] <-"yes" # any STI, combined
  DHS_sub[which(!(DHS_sub[,'mv744b']=="no" & DHS_sub[,'mv744b']=="no" & DHS_sub[,'mv744c']=="no" 
                   & DHS_sub[,'mv744d']=="no" & DHS_sub[,'mv744e']=="no")),'mv744b'] <- "yes" 
  # wife beating never justified 

  DHS_sub[which(!(DHS_sub[,'mv754jp'] =='no' & DHS_sub[,'mv754wp']=='no'
                   & DHS_sub[,'mv756'] == 'yes'))
             ,"754jp"] <- 'don\'t know' # false believes. 
  #Mosquito bites, sharing food, healthy person with AIDS
  DHS_sub[which(is.na(DHS_sub[,'mv754jp'])),'mv754jp']<- 'don\'t know'

  #DHS_sub[which((DHS_sub[,'mv778']=='no' | # not ready to care for AIDS 
  #                  DHS_sub[,'mv779']=='no' | # no teachers with AIDS
  #                  DHS_sub[,'mv777']=='no' #& # vegetables from AIDS vendor, 
  #                  )) , 'mv778'] <- 'no' # stigma 

  DHS_sub[which(DHS_sub[,'mv384a']=="yes" | DHS_sub[,'mv384b']=="yes" |
                   DHS_sub[,'mv384c']=="yes"), 'mv384a'] <- "yes" 
  # heard FP radio, TV, newspaper

  #DHS_sub[which(DHS_sub[,'mv467b']==1 | DHS_sub[,'mv467c']==1 |
  #                   DHS_sub[,'mv467d']==1 | DHS_sub[,'mv467e']==1 |
  #                   DHS_sub[,'mv467f']==1), 'mv467b'] <-1 
  #HC access difficulties 

 # DHS_sub[which(!(DHS_sub[,'mv743a'] %in% c("1","2") & DHS_sub[,'mv743b'] %in% c("1","2"))), 
#          'mv743a'] <-"3"
                #  & DHS_sub[,'mv743d'] %in% c("1","2"))
                 
  # final say of major purchases, visits to family etc

  #DHS_sub[which(is.na(DHS_sub[,'mv633a'])),'mv633a']<- 1 # NA ~ formerly married. TODO check
  #DHS_sub[which(is.na(DHS_sub[,'mv769'])),'mv769']<-0 # 'no' could get a condom
  DHS_sub[which(is.na(DHS_sub[,'mv503'])),'mv503']<- 1 # Number of unions. actully, 0. But split no unions or one vs. more than once
  #DHS_sub[which(is.na(DHS_sub[,'mv778'])),'mv778']<- 0 # 'no'
  #DHS_sub[which(is.na(DHS_sub[,'mv780'])),'mv780']<- 0 # 'no' # never heard of AIDS --> we assume children should not be taught about condoms
  #DHS_sub[which(is.na(DHS_sub[,'mv302'])),'mv302']<- 'never used' # doesn't know the method -> never used

  label_dataset <- 'DHS 2010, all male sample'
  weights_dhs <- DHS_sub$mv005
  print("start choose indicators")
  # Labels and levels for variables
  DHS_indicators <-list(
    #  list(var= "M_hiv03", level = 1, label = "HIV status"),
    list(var = "mv013",  level = c(1,2), label = "Age"),
    list(var = "mv025", level = 1, label = "Urban/ \n rural"),
    list(var = "mv130", level = 4, label = "Religion"),
    list(var = "mv151", level = 2, label = "Househead"),
    list(var = "mv155", level = 2, label = "Literacy"),
    list(var = "mv158", level = c(1,2), label = "Media access"),
  #  list(var = "mv302", level = 2, label = "Ever \n modern \n contraception"),
    list(var = "mv384a", level = 2, label = "Family \n planning \n on media"),
    list(var = "mv531", level = c(2,3,4,5,6,7,8,9), label = "First \n sex < 16 "), # change to 15
  #  list(var = "mv633a", level = 2, label = "Exist reason \n for no sex"),
    list(var = "mv731", level = c(1), label = "Currently \n  working"), 
    list(var = "mv744b", level = 2, label = "Wife beating \n justified "), # combined a-e
    #list(var = "mv483", level = 3, label = "Circumcision"), 
    list(var = "mv502", level = 3, label = "Marital status"), 
    list(var = "mv754jp", level = c(1,3), label = "False \n beliefs AIDS"),
   # list(var = "mv762az", level = 2, label = "Source of \n condoms \n not known"),
  #  list(var = "mv769", level = 2, label = "Could \n get condom"),
   # list(var = "mv778", level = 2, label = "AIDS \n acceptance"), 
    #list(var = "mv780", level = 2, label = "Children \n taught condoms"),
    list(var = "mv781", level = 2, label = "Ever \n tested HIV"))
  print("chosen indicators")
  df_DHS <- do.call(rbind, DHS_indicators)
  labels_DHS <- df_DHS[,'label']
  var_DHS <- df_DHS[,'var']
  print(var_DHS[which(!(var_DHS %in% names(DHS_sub)))])
  # to check which variables from the script are missing in the DHS 

  df <- DHS_sub
  var_list <- df_DHS
  print("prepare DHS for DAG")
  a <- prepare_df_DAG(df = df, var_list = df_DHS, DHS_indicators = DHS_indicators, w_var = 'mv005')

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
var_csv <- "//home//b//zbaran/Documents/Genf/Malawi-SNF/process_DHS/clean_DHS/output/MWMR7HFL_levels.csv"
#data_path_2016 <- "//home//b//zbaran/Documents/Genf/Malawi-SNF/Malawi_DHS//MW_2015-16_DHS_03092018_45_117973/MWMR7HDT/MWMR7HFL.DTA"
#data_path_2010 <- "//home//b//zbaran/Documents/Genf/Malawi-SNF/Malawi_DHS/MWMR61DT/MWMR61FL.DTA"
data_path_2000 <- "//home//b//zbaran/Documents/Genf/Malawi-SNF/Malawi_DHS/MWMR41DT/MWMR41FL.DTA"
#MHH <- "//home//b//zbaran/Documents/Genf/Malawi-SNF/Malawi_DHS//MW_2015-16_DHS_03092018_45_117973/MWAR7ADT/MWAR7AFL.DTA"
#to_analyze_MW_males_2016 <- DHS_prepare_data(data_path_2016, var_csv)
#to_analyze_MW_males_2010 <- DHS_prepare_data(data_path_2010, var_csv)
to_analyze_MW_males_2000 <- DHS_prepare_data(data_path_2000, var_csv)

#View(to_analyze_MW_males_2016$description)
#View(to_analyze_MW_males_2010$description)
View(to_analyze_MW_males_2000$description)

saveRDS(to_analyze_MW_males_2000, "to_analyze_MW_males_2000.rds")
#saveRDS(to_analyze_MW_males_2010, "to_analyze_MW_males_2010.rds")
#saveRDS(to_analyze_MW_males_2016, "to_analyze_MW_males_2016.rds")

#write.csv(DAG_data_2016$description, "MW_2016_m.csv")
#write.csv(DAG_data_2010$description, "MW_2010_m.csv")
write.csv(DAG_data_2000$description, "MW_2000_m.csv")

