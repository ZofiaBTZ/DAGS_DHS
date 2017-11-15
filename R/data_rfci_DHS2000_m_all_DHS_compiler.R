# prepare data for pcalg causal networks.
# 2010 female never married
source('./pcalg_DHS_utils.R')

############# variables 1992
#ind_MWMR61 <-  read.dta("//home//b//zbaran/Documents/Genf/Malawi-SNF/Malawi_DHS//MWMR61DT//MWMR61FL.DTA")
#ind_MWAR61 <-  read.dta("//home//b//zbaran/Documents/Genf/Malawi-SNF/Malawi_DHS//MWAR41D//MWAR61FL.DTA")
ind_MWMR61 <-  read.dta("//home//b//zbaran/Documents/Genf/Malawi-SNF/Malawi_DHS//MWMR41DT//MWMR41FL.DTA")


#names(ind_MWAR61)[1:7] <- c('mv001', 'mv002', 'mv003', 'hiv01', 'hiv02','M_hiv03','hiv05')
#names(ind_MWMR61)[3:5] <- c('mv001', 'mv002', 'husband_linenumber')
#names(ind_MWMR61)[which(names(ind_MWMR61)=="v034")] <- "husband_linenumber"

#ind_MWMR61 <- merge(ind_MWMR61, ind_MWMR61, by=c('mv001', 'mv002', 'husband_linenumber'), all.x = TRUE)
#ind_MWMR61 <- merge(ind_MWAR61, ind_MWMR61, by=c('mv001', 'mv002', 'mv003'), all.x = FALSE, all.y=FALSE ) # leaves out the HIV males results
#names(ind_MWAR61)[1:7] <- c('mv001', 'mv002', 'husband_linenumber', 'hiv01', 'hiv02','M_hiv03','hiv05')
#ind_MWMR61 <- merge(ind_MWAR61, ind_MWMR61, by=c('mv001', 'mv002', 'husband_linenumber'), all.y = TRUE) # leaves out the HIV females results
# no merging with husbands for never married


indx <- sapply(ind_MWMR61, is.factor)
ind_MWMR61[indx] <- lapply(ind_MWMR61[indx], function(x) {
  levels(x) <- make.unique(levels(x))
  x })

#ind_MWMR61 <- unfactor(ind_MWMR61)
#ind_MWMR61[which(ind_MWMR61[,'mv123']==1 | ind_MWMR61[,'mv124']==1 | ind_MWMR61[,'mv125']==1),'mv123'] <-1

media <- c( 2,3)

ind_MWMR61[which(ind_MWMR61[,'mv158'] %in% media | ind_MWMR61[,'mv157'] %in% media | ind_MWMR61[,'mv159'] %in% media),'mv158'] <- 2 
# use radio, TV, newspaper 
#ind_MWMR61[which(ind_MWMR61[,'mv850a']==1 | ind_MWMR61[,'mv850b']==1) ,'mv850a'] <-1

ind_MWMR61[which(ind_MWMR61[,'mv763a']=="yes" | ind_MWMR61[,'mv763b']=="yes" | ind_MWMR61[,'mv763c']=="yes"),'mv763a'] <-"yes" # any STI, combined

ind_MWMR61[which(!(ind_MWMR61[,'mv744a']=="no" & ind_MWMR61[,'mv744b']=="no" & ind_MWMR61[,'mv744c']=="no" 
                 & ind_MWMR61[,'mv744d']=="no" & ind_MWMR61[,'mv744e']=="no")),'mv744a'] <- "yes" 
                  # wife beating never justified 
View(table(ind_MWMR61[,'mv744a']))
#ind_MWMR61[which(ind_MWMR61[,'mv633a']=='yes' || ind_MWMR61[,'mv633b']=='yes' || ind_MWMR61[,'mv633b']=='yes' 
#                 || ind_MWMR61[,'mv633c']=='yes'),'mv633a'] <-'yes'
#2010 - reason for not having sex - only STD considered

#ind_MWMR61[which(ind_MWMR61[,'mv754bp']==0 | ind_MWMR61[,'mv754cp']==0 
#                 | ind_MWMR61[,'mv754dp']==0),"754bp"] <- 0 # possible to decrease the risk by no sex,
                                                                  #less partners, condoms

ind_MWMR61[which(!(ind_MWMR61[,'mv754jp'] =='no' & ind_MWMR61[,'mv754wp']=='no' 
                 & ind_MWMR61[,'mv756'] == 'yes'))
           ,"754jp"] <- 'don\'t know' # false believes. 
#Mosquito bites, sharing food, healthy person with AIDS
ind_MWMR61[which(is.na(ind_MWMR61[,'mv754jp'])),'mv754jp']<- 'don\'t know'

#ind_MWMR61[which(!(ind_MWMR61[,'mv774a']==1& ind_MWMR61[,'mv774b']==1& ind_MWMR61[,'mv774c']==1)), 
#           'mv774a'] <-0 # MTC by delivery, pregnancy, breastfeeding v744 instead 


ind_MWMR61[which((ind_MWMR61[,'mv778']=='no' | # not ready to care for AIDS 
                   ind_MWMR61[,'mv779']=='no' | # no teachers with AIDS
                   ind_MWMR61[,'mv777']=='no' #& # vegetables from AIDS vendor, 
                  # ind_MWMR61[,'mv825']==1)
                  )) , 'mv778'] <- 'no' # no teachers with AIDS
                   
                   
                  # ind_MWMR61[,'mv780']=='no' || # children not taught about condoms
                  # ind_MWMR61[,'mv759b']=='no' || # not aaceptable to talk about HIV on TV
                  # ind_MWMR61[,'s817bx']=='no' ||#condoms not safe
                  # ind_MWMR61[,'s817by']=='no'), #no HIV test before married 
                  # 'mv779'] <- 'no'  
           # AIDS stigma: ids condoms, AIDS TV, 
#condoms safe, HIV test before marriage 


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
 
ind_MWMR61[which(is.na(ind_MWMR61[,'mv633a'])),'mv633a']<- "not in union" #1 # NA ~ formerly married. TODO check
ind_MWMR61[which(is.na(ind_MWMR61[,'mv769'])),'mv769']<- 'no'
#ind_MWMR61[which(is.na(ind_MWMR61[,'mv774'])),'mv774']<- 'no'
ind_MWMR61[which(is.na(ind_MWMR61[,'mv503'])),'mv503']<- 1 # actully, 0. But split no unions or one vs. more than once
ind_MWMR61[which(is.na(ind_MWMR61[,'mv778'])),'mv778']<- 'no'
ind_MWMR61[which(is.na(ind_MWMR61[,'mv780'])),'mv780']<- 'no' # never heard of AIDS --> we assume children should not be taught about condoms
ind_MWMR61[which(is.na(ind_MWMR61[,'mv302'])),'mv302']<- 'never used' # doesn't know the method -> never used

#ind_MWMR61[which(ind_MWMR61[,'d106']==1 | ind_MWMR61[,'d107']==1),'d106'] <- 1 #severe or less severe violence

f_25plus <- ind_MWMR61[(ind_MWMR61[,'mv013'] %in% c("25-29", "30-34", "35-39", "40-44", "45-49")),]
f_25minus <- ind_MWMR61[(ind_MWMR61[,'mv013'] %in% c("15-19", "20-25")),]

#f_married <- ind_MWMR61[(ind_MWMR61[,'mv502'] == 'currently married'),]

label_dataset <- 'DHS 2000, all female sample'
weights_dhs <- ind_MWMR61$mv005

DHS_indicators <-list(
#  list(var= "M_hiv03", level = 1, label = "HIV status"),
  list(var = "mv013",  level = c(1,2), label = "Age"),
  list(var = "mv025", level = 1, label = "Urban/ \n rural"),
  list(var = "mv130", level = 4, label = "Religion"),
  list(var = "mv151", level = 2, label = "Househead"),
  list(var = "mv155", level = 2, label = "Literacy"),
  list(var = "mv158", level = c(1,2), label = "Media access"),
  list(var = "mv302", level = 2, label = "Ever \n modern \n contraception"),
  list(var = "mv384a", level = 2, label = "Family \n planning \n on media"),
  list(var = "mv531", level = c(2,3,4,5,6,7,8,9), label = "First \n sex < 16 "), # change to 15
  list(var = "mv633a", level = 2, label = "Exist reason \n for no sex"),
 list(var = "mv731", level = 3, label = "Currently \n  working"), 
  list(var = "mv744a", level = 1, label = "Wife beating \n justified "), # combined a-e
 #list(var = "mv483", level = 3, label = "Circumcision"), 
   list(var = "mv502", level = 3, label = "Marital status"), 
  list(var = "mv754jp", level = 2, label = "False \n beliefs AIDS"),
  list(var = "mv762az", level = 2, label = "Source of \n condoms \n not known"),
  list(var = "mv769", level = 2, label = "Could \n get condom"),
  list(var = "mv778", level = 2, label = "AIDS \n acceptance"), 
  list(var = "mv780", level = 2, label = "Children \n taught condoms"),
  list(var = "mv781", level = 2, label = "Ever \n tested HIV"))



df_DHS <- do.call(rbind, DHS_indicators)
labels_DHS <- df_DHS[,'label']
var_DHS <- df_DHS[,'var']
print(var_DHS[which(!(var_DHS %in% names(ind_MWMR61)))])
#abandom PLHIV


df <- ind_MWMR61#[1:2000,]#f_married#f_25plus ## more data
var_list <- df_DHS
#DHS_indicators <- DHS_indicators
#prepare_df_DAG <- function(df, var_list, DHS_indicators, w_var){ 
a <- prepare_df_DAG(df = df, var_list = df_DHS, DHS_indicators = DHS_indicators, w_var = 'mv005')

to_analyze <- a$to_analyze
weights_dhs <- a$weights
strata_dhs <- a$strata
colnames(to_analyze) <- labels_DHS

for (f in seq(1:length(to_analyze))){
  to_analyze[,f] <- as.numeric(to_analyze[,f]) 
}

View(to_analyze)
#print(colSums(to_analyze))
#plot_DAG(df = to_analyze, weights = weights_dhs, strata = strata_dhs, my_alpha = 0.0001, df_labels = as.vector(unlist(labels_DHS)), df_plot_name ='M_all_10')
#plot_DAG(df = to_analyze, weights = weights_dhs, strata = strata_dhs, my_alpha = 0.001, df_labels = as.vector(unlist(labels_DHS)), df_plot_name ='M_all_10')
#plot_DAG(df = to_analyze, weights = weights_dhs, strata = strata_dhs, my_alpha = 0.01, df_labels = as.vector(unlist(labels_DHS)), df_plot_name ='M_all_10')
#plot_DAG(df = to_analyze, weights = weights_dhs, strata = strata_dhs, my_alpha = 0.1, df_labels = as.vector(unlist(labels_DHS)), df_plot_name ='M_all_10')

