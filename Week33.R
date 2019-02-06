
#######    SEM 2019 WEEK 3
#######    Mikko Patronen 29.1.2019
#######
#######    Exercise 3
#

wk3data <- read.fortran(file = "ELEMM1.DAT", c("22F1.0"))
colnames(wk3data) <- paste("ITEM", 1:22, sep="")
describe(wk3data)

library(lavaan)
library(sem)
detach(package:sem, unload = TRUE)
library(semPlot)

malli <- "
EE =~ ITEM1 + ITEM2 + ITEM3 + ITEM6 + ITEM8 + ITEM13 + ITEM14 + ITEM16 + ITEM20
DP =~ ITEM5 + ITEM10 + ITEM11 + ITEM15 + ITEM22
PA =~ ITEM4 + ITEM7 + ITEM9 + ITEM12 + ITEM17 + ITEM18 + ITEM19 + ITEM21
"

fit_MBI <- cfa(malli, data = wk3data)
summary(fit_MBI, fit.measures = T)

semPaths(fit_MBI, title = FALSE, curvePivot = TRUE)

