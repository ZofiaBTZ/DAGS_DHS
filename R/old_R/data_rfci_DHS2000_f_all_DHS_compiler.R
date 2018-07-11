# prepare data for pcalg causal networks.
# 2010 female never married
source('./pcalg_DHS_utils.R')

############# variables 1992
ind_MWIR61 <-  read.dta("//home//b//zbaran/Documents/Genf/Malawi-SNF/Malawi_DHS//MWIR41DT//MWIR41FL.DTA")
#ind_MWAR61 <-  read.dta("//home//b//zbaran/Documents/Genf/Malawi-SNF/Malawi_DHS//MWAR61DT//MWAR61FL.DTA")
#ind_MWMR61 <-  read.dta("//home//b//zbaran/Documents/Genf/Malawi-SNF/Malawi_DHS//MWMR61DT//MWMR61FL.DTA")


#names(ind_MWAR61)[1:7] <- c('v001', 'v002', 'v003', 'hiv01', 'hiv02','F_hiv03','hiv05')
#names(ind_MWMR61)[3:5] <- c('v001', 'v002', 'husband_linenumber')
#names(ind_MWIR61)[which(names(ind_MWIR61)=="v034")] <- "husband_linenumber"

#ind_MWIR61 <- merge(ind_MWIR61, ind_MWMR61, by=c('v001', 'v002', 'husband_linenumber'), all.x = TRUE)
#ind_MWIR61 <- merge(ind_MWAR61, ind_MWIR61, by=c('v001', 'v002', 'v003'), all.x = FALSE, all.y=FALSE ) # leaves out the HIV males results
#names(ind_MWAR61)[1:7] <- c('v001', 'v002', 'husband_linenumber', 'hiv01', 'hiv02','M_hiv03','hiv05')
#ind_MWIR61 <- merge(ind_MWAR61, ind_MWIR61, by=c('v001', 'v002', 'husband_linenumber'), all.y = TRUE) # leaves out the HIV females results
# no merging with husbands for never married


indx <- sapply(ind_MWIR61, is.factor)
ind_MWIR61[indx] <- lapply(ind_MWIR61[indx], function(x) {
  levels(x) <- make.unique(levels(x))
  x })

ind_MWIR61 <- unfactor(ind_MWIR61)
#ind_MWIR61[which(ind_MWIR61[,'v123']==1 | ind_MWIR61[,'v124']==1 | ind_MWIR61[,'v125']==1),'v123'] <-1

media <- c( 2,3)

ind_MWIR61[which(ind_MWIR61[,'v158'] %in% media | ind_MWIR61[,'v157'] %in% media | ind_MWIR61[,'v159'] %in% media),'v158'] <- 2 
# use radio, TV, newspaper 
#ind_MWIR61[which(ind_MWIR61[,'v850a']==1 | ind_MWIR61[,'v850b']==1) ,'v850a'] <-1

ind_MWIR61[which(ind_MWIR61[,'v763a']=="yes" | ind_MWIR61[,'v763b']=="yes" | ind_MWIR61[,'v763c']=="yes"),'v763a'] <-"yes" # any STI, combined

ind_MWIR61[which(!(ind_MWIR61[,'v744a']=="no" & ind_MWIR61[,'v744b']=="no" & ind_MWIR61[,'v744c']=="no" 
                 & ind_MWIR61[,'v744d']=="no" & ind_MWIR61[,'v744e']=="no")),'v744a'] <- "yes" 
                  # wife beating never justified 
View(table(ind_MWIR61[,'v744a']))
#ind_MWIR61[which(ind_MWIR61[,'v633a']=='yes' || ind_MWIR61[,'v633b']=='yes' || ind_MWIR61[,'v633b']=='yes' 
#                 || ind_MWIR61[,'v633c']=='yes'),'v633a'] <-'yes'
#2010 - reason for not having sex - only STD considered

#ind_MWIR61[which(ind_MWIR61[,'v754bp']==0 | ind_MWIR61[,'v754cp']==0 
#                 | ind_MWIR61[,'v754dp']==0),"754bp"] <- 0 # possible to decrease the risk by no sex,
                                                                  #less partners, condoms

ind_MWIR61[which(!(ind_MWIR61[,'v754jp'] =='no' & ind_MWIR61[,'v754wp']=='no' 
                 & ind_MWIR61[,'v756'] == 'yes'))
           ,"754jp"] <- 'don\'t know' # false believes. 
#Mosquito bites, sharing food, healthy person with AIDS
ind_MWIR61[which(is.na(ind_MWIR61[,'v754jp'])),'v754jp']<- 'don\'t know'

#ind_MWIR61[which(!(ind_MWIR61[,'v774a']==1& ind_MWIR61[,'v774b']==1& ind_MWIR61[,'v774c']==1)), 
#           'v774a'] <-0 # MTC by delivery, pregnancy, breastfeeding v744 instead 


ind_MWIR61[which((ind_MWIR61[,'v778']=='no' | # not ready to care for AIDS 
                   ind_MWIR61[,'v779']=='no' | # no teachers with AIDS
                   ind_MWIR61[,'v777']=='no' #& # vegetables from AIDS vendor, 
                  # ind_MWIR61[,'v825']==1)
                  )) , 'v778'] <- 'no' # no teachers with AIDS
                   
                   
                  # ind_MWIR61[,'v780']=='no' || # children not taught about condoms
                  # ind_MWIR61[,'v759b']=='no' || # not aaceptable to talk about HIV on TV
                  # ind_MWIR61[,'s817bx']=='no' ||#condoms not safe
                  # ind_MWIR61[,'s817by']=='no'), #no HIV test before married 
                  # 'v779'] <- 'no'  
           # AIDS stigma: ids condoms, AIDS TV, 
#condoms safe, HIV test before marriage 


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

ind_MWIR61[which(is.na(ind_MWIR61[,'v633a'])),'v633a']<- "not in union" #1 # NA ~ formerly married. TODO check
ind_MWIR61[which(is.na(ind_MWIR61[,'v769'])),'v769']<- 'no'
#ind_MWIR61[which(is.na(ind_MWIR61[,'v774'])),'v774']<- 'no'
ind_MWIR61[which(is.na(ind_MWIR61[,'v503'])),'v503']<- 1 # actully, 0. But split no unions or one vs. more than once
ind_MWIR61[which(is.na(ind_MWIR61[,'v778'])),'v778']<-  'no'
ind_MWIR61[which(is.na(ind_MWIR61[,'v780'])),'v780']<-  'no' # never heard of AIDS --> we assume children should not be taught about condoms
ind_MWIR61[which(is.na(ind_MWIR61[,'v302'])),'v302']<- 'never used' # doesn't know the method -> never used

#ind_MWIR61[which(ind_MWIR61[,'d106']==1 | ind_MWIR61[,'d107']==1),'d106'] <- 1 #severe or less severe violence

f_25plus <- ind_MWIR61[(ind_MWIR61[,'v013'] %in% c("25-29", "30-34", "35-39", "40-44", "45-49")),]
f_25minus <- ind_MWIR61[(ind_MWIR61[,'v013'] %in% c("15-19", "20-25")),]

#f_married <- ind_MWIR61[(ind_MWIR61[,'v502'] == 'currently married'),]

label_dataset <- 'DHS 2000, all female sample'
weights_dhs <- ind_MWIR61$v005

DHS_indicators <-list(
 # list(var= "F_hiv03", level = 1, label = "HIV status"),
  list(var = "v013",  level = c(1,2), label = "Age"),
  list(var = "v025", level = 1, label = "Urban/ \n rural"),
  list(var = "v130", level = 6, label = "Religion"),
  list(var = "v151", level = 2, label = "Househead"),
  list(var = "v155", level = 3, label = "Literacy"),
  list(var = "v158", level = 3, label = "Media access"),
  list(var = "v302", level = 2, label = "Ever  \n modern  \n contraception"),
  list(var = "v384a", level = 2, label = "Family \n planning \n on media"),
  list(var = "v531", level = c(2,3,4,5,6,7,8,9), label = "First \n sex < 16 "), # change to 15
  list(var = "v633a", level = 2, label = "Exist reason \n for no sex"),
 list(var = "v731", level = 3, label = "Currently \n  working"), 
  list(var = "v744a", level = 1, label = "Wife beating \n justified "), # combined a-e
  list(var = "v502", level = 3, label = "Marital status"), 
  list(var = "v754jp", level = 2, label = "False \n beliefs AIDS"),
  list(var = "v762az", level = 2, label = "Source of \n condoms \n not known"),
  list(var = "v769", level = 2, label = "Could \n get condom"),
  list(var = "v778", level = 2, label = "AIDS \n acceptance"), 
  list(var = "v780", level = 2, label = "Children \n taught condoms"),
  list(var = "v781", level = 2, label = "Ever \n tested HIV"))



df_DHS <- do.call(rbind, DHS_indicators)
labels_DHS <- df_DHS[,'label']
var_DHS <- df_DHS[,'var']
print(var_DHS[which(!(var_DHS %in% names(ind_MWIR61)))])
#abandom PLHIV


df <- ind_MWIR61#[1:6000,]#f_married#f_25plus ## more data
var_list <- df_DHS
#DHS_indicators <- DHS_indicators
#prepare_df_DAG <- function(df, var_list, DHS_indicators, w_var){ 
a <- prepare_df_DAG(df = df, var_list = df_DHS, DHS_indicators = DHS_indicators, w_var = 'v005')

to_analyze <- a$to_analyze
weights_dhs <- a$weights
strata_dhs <- a$strata
colnames(to_analyze) <- labels_DHS
for (f in seq(1:length(to_analyze))){
  to_analyze[,f] <- as.numeric(to_analyze[,f]) 
}
