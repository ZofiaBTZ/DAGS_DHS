mw_men <- read.dta("//home//b//zbaran//Documents/Genf/Malawi-SNF/Malawi_DHS/MWMR61DT/MWMR61FL.DTA")
all_rows <- mw_men[,colSums(is.na(mw_men[1:1000,])) == 0]
varrank::varrank(data.df = complete.cases(mw_men[,c("mv013", "mv025", "mv151", "mv155",
                                                    "mv158", "mv531",
                                                    "mv731", "mv744a", "mv502",
                                                    "mv754jp", "mv769", "mv781")]),
                 variable.important = "mv531",scheme = "mid",
                 discretization.method = "cencov",method = "peng",algorithm = "forward")

a <- mw_men[,c("mv013", "mv025", "mv151", "mv155",
          "mv158", "mv531",
          "mv731", "mv744a", "mv502",
          "mv754jp", "mv769", "mv781")]
colnames(a) <- c("age", "urban \n rural", "househead", "Literacy", "Media \n access", "First \n sex 16",
                 "currently \n working", "wife \n beating", "married", "false \n beliefs", "condom \n access",
                 "HIV \n tested")
full_cases <- na.omit(a)
#Boruta(full_cases, full_cases$mv781)

tmp <- varrank::varrank(data.df = full_cases, variable.important = "condom \n access", scheme = "mid",
                 discretization.method = "cencov",method = "peng",algorithm = "forward")

plot(tmp)