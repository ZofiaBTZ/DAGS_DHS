library(lsr)
cv.test = function(x,y) {
  CV = sqrt(chisq.test(x, y, correct=FALSE)$statistic /
              (length(x) * (min(length(unique(x)),length(unique(y))) - 1)))
  print.noquote("Cramer V / Phi:")
  return(as.numeric(CV))
}


data_path <- "//home//b//zbaran//Documents/Genf/Malawi-SNF/Malawi_DHS/TD_2014-15_DHS/TDMR71DT/TDMR71FL.DTA"
td_men <-  read.dta(data_path)
tb <-as.matrix(table(td_men$mv822, td_men$mv025)[1:2,1:2])
chisq.test(tb)
cv.test(td_men$mv822, td_men$mv013)
# [1] 0.5199662