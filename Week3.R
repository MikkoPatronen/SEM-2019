
#######    SEM 2019 WEEK 3
#######    Mikko Patronen 5.2.2019
#######
#######    Exercise 3
#
getwd()
viikko3 <- read.fortran(file = "ELEMM1.DAT", c("22F1.0"))
colnames(viikko3) <- c("ITEM1", "ITEM2", "ITEM3", "ITEM4", "ITEM5",
                   "ITEM6", "ITEM7", "ITEM8", "ITEM9", "ITEM10",
                   "ITEM11", "ITEM12", "ITEM13", "ITEM14",
                   "ITEM15", "ITEM16", "ITEM17", "ITEM18",
                   "ITEM19", "ITEM20", "ITEM21", "ITEM22")

describe(viikko3)

library(dplyr)
library(corrplot)
library(ggplot2)
library(GGally)
library(lavaan)
library(sem)
library(semPlot)

cor_matrix<-cor(viikko3) %>% round(digits = 2)
corrplot(cor_matrix, method="circle", type="upper", cl.pos="b", tl.pos="d", tl.cex = 0.6)
ggpairs(viikko3)

library(psych)
library(likert)
vk3 <- viikko3

vk3$ITEM1 = factor(vk3$ITEM1,
                   levels = c("1", "2", "3", "4", "5", "6", "7"),
                   ordered = TRUE)
vk3$ITEM2 = factor(vk3$ITEM2,
                   levels = c("1", "2", "3", "4", "5", "6", "7"),
                   ordered = TRUE)
vk3$ITEM3 = factor(vk3$ITEM3,
                   levels = c("1", "2", "3", "4", "5", "6", "7"),
                   ordered = TRUE)
vk3$ITEM4 = factor(vk3$ITEM4,
                   levels = c("1", "2", "3", "4", "5", "6", "7"),
                   ordered = TRUE)
vk3$ITEM5 = factor(vk3$ITEM5,
                   levels = c("1", "2", "3", "4", "5", "6", "7"),
                   ordered = TRUE)
vk3$ITEM6 = factor(vk3$ITEM6,
                   levels = c("1", "2", "3", "4", "5", "6", "7"),
                   ordered = TRUE)
vk3$ITEM7 = factor(vk3$ITEM7,
                   levels = c("1", "2", "3", "4", "5", "6", "7"),
                   ordered = TRUE)
vk3$ITEM8 = factor(vk3$ITEM8,
                   levels = c("1", "2", "3", "4", "5", "6", "7"),
                   ordered = TRUE)
vk3$ITEM9 = factor(vk3$ITEM9,
                   levels = c("1", "2", "3", "4", "5", "6", "7"),
                   ordered = TRUE)
vk3$ITEM10 = factor(vk3$ITEM10,
                   levels = c("1", "2", "3", "4", "5", "6", "7"),
                   ordered = TRUE)
vk3$ITEM11 = factor(vk3$ITEM11,
                   levels = c("1", "2", "3", "4", "5", "6", "7"),
                   ordered = TRUE)
vk3$ITEM12 = factor(vk3$ITEM12,
                   levels = c("1", "2", "3", "4", "5", "6", "7"),
                   ordered = TRUE)
vk3$ITEM13 = factor(vk3$ITEM13,
                   levels = c("1", "2", "3", "4", "5", "6", "7"),
                   ordered = TRUE)
vk3$ITEM14 = factor(vk3$ITEM14,
                   levels = c("1", "2", "3", "4", "5", "6", "7"),
                   ordered = TRUE)
vk3$ITEM15 = factor(vk3$ITEM15,
                   levels = c("1", "2", "3", "4", "5", "6", "7"),
                   ordered = TRUE)
vk3$ITEM16 = factor(vk3$ITEM16,
                   levels = c("1", "2", "3", "4", "5", "6", "7"),
                   ordered = TRUE)
vk3$ITEM17 = factor(vk3$ITEM17,
                   levels = c("1", "2", "3", "4", "5", "6", "7"),
                   ordered = TRUE)
vk3$ITEM18 = factor(vk3$ITEM18,
                   levels = c("1", "2", "3", "4", "5", "6", "7"),
                   ordered = TRUE)
vk3$ITEM19 = factor(vk3$ITEM19,
                   levels = c("1", "2", "3", "4", "5", "6", "7"),
                   ordered = TRUE)
vk3$ITEM20 = factor(vk3$ITEM20,
                   levels = c("1", "2", "3", "4", "5", "6", "7"),
                   ordered = TRUE)
vk3$ITEM21 = factor(vk3$ITEM21,
                   levels = c("1", "2", "3", "4", "5", "6", "7"),
                   ordered = TRUE)
vk3$ITEM22 = factor(vk3$ITEM22,
                   levels = c("1", "2", "3", "4", "5", "6", "7"),
                   ordered = TRUE)

describe(vk3)


Result = likert(vk3)

plot(Result,
     type="bar")



model_MBI <- '
EE =~ ITEM1 + ITEM2 + ITEM3 + ITEM6 + ITEM8 + ITEM13 + ITEM14 + ITEM16 + ITEM20
DP =~ ITEM5 + ITEM10 + ITEM11 + ITEM15 + ITEM22
PA =~ ITEM4 + ITEM7 + ITEM9 + ITEM12 + ITEM17 + ITEM18 + ITEM19 + ITEM21
'

fit_ML <- cfa(model_MBI, data = viikko3)

summary(fit_ML, fit.measures = T)


semPaths(fit_ML, title = FALSE, curvePivot = TRUE)
semPaths(fit_ML, layout='tree2')

# MLM
fit_MLM <- cfa(model_MBI, data = viikko3, estimator="MLM")

summary(fit_MLM, fit.measures = T)

semPaths(fit_MLM, title = FALSE, curvePivot = TRUE)
semPaths(fit_MLM, layout='tree2')
