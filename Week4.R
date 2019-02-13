
#######    SEM 2019 WEEK 4
#######    Mikko Patronen 12.2.2019
#######
#######    Exercise 4
#

wk4 <- read.table(file = "allsecondary.DAT")

colnames(wk4) <- c("ROLEA1", "ROLEA2", "ROLEC1", "ROLEC2", "WORK1", "WORK2",
                   "CCLIM1", "CCLIM2", "CCLIM3", "CCLIM4", "DEC1", "DEC2", "SSUP1", "SSUP2",
                   "PSUP1", "PSUP2", "SELF1", "SELF2", "SELF3", "ELC1", "ELC2", "ELC3", "ELC4", "ELC5",
                   "EE1", "EE2", "EE3", "DP1", "DP2", "PA1", "PA2", "PA3")
library(psych)
describe(wk4)

library(lavaan)

# chapter 16 github

model <- '
# latent variables
F1  =~ ROLEA1 + ROLEA2 + DEC2
F2  =~ ROLEC1 + ROLEC2
F3  =~ WORK1 + WORK2
F4  =~ CCLIM1 + CCLIM2 + CCLIM3 + CCLIM4
F5  =~ DEC1 + DEC2
F6  =~ SSUP1 + SSUP2 + DEC2
F7  =~ PSUP1 + PSUP2
F8  =~ SELF1 + SELF2 + SELF3
F9  =~ ELC1 + ELC2 + ELC3 + ELC4 + ELC5
F10  =~ EE1 + EE2 + EE3
F11  =~ DP1 + DP2
F12  =~ PA1 + PA2 + PA3

# regressions
F8 ~ F5 + F6 + F7
F9 ~ F5
F10 ~ F2 + F3 + F4 
F11 ~ F2 + F10
F12 ~ F1 + F8 + F9 + F10 + F11
'

# OUTPUT: MODINDICES TECH1

fit <- cfa(model, data = wk4)
fit <- sem(model, data = wk4, estimator="MLM")
summary(fit, fit.measures = T)


library(semPlot)

semPaths(fit, title = FALSE, curvePivot = TRUE)
semPaths(fit, what = "path", whatLabels = "hide", style = "lisrel",
         layout='tree', residuals = TRUE, nCharNodes = 0,
         sizeMan = 5.5, sizeLat = 6, sizeLat2 = 5, esize = 1,
         label.cex = 0.55, label.scale = FALSE, width = 9, height = 4,
         rotation = 2)

semPaths(fit, what = "col", whatLabels = "hide", style = "lisrel",
         layout='circle2', residuals = TRUE, nCharNodes = 0,
         sizeMan = 6, sizeLat = 6, sizeLat2 = 5, esize = 1,
         label.cex = 0.55, label.scale = FALSE, width = 5, height = 5,
         rotation = 1, optimizeLatRes = TRUE)

semPaths(fit, what = "path", whatLabels = "hide", style = "lisrel",
         residuals = TRUE, curve = 3.2, rotation = 4, nCharNodes = 0,
         sizeLat = 7, sizeMan = 4, sizeMan2 = 2, esize = 1, label.cex = 1.5,
         mar = c(3, 8, 3, 15))

semPaths(fit, what = "path", whatLabels = "hide", style = "lisrel",
         layout = "tree2", residuals = TRUE, nCharNodes = 0, 
         rotation = 2, sizeMan = 6, sizeMan2= 3, sizeLat = 8, sizeLat2 = 6,
         esize = 1.3, label.cex = 0.7, label.scale = FALSE, width = 6, height = 5,
         exoCov = FALSE)

?semPaths

JSsem <- sem(JSmodel, sample.cov = houghtonFull.cov, sample.nobs = 263)

longLabels = c("JW1","JW2","JW3", "UF1","UF2","FOR", "DA1","DA2","DA3", "EBA","ST","MI", "Constructive \n Thought \n Strategies",
               "Dysfunctional \n Thought \n Processes",
               "Subjective \n Well-Being",
               "Job \n Satisfaction")

semPaths(fit, # filetype = "pdf", filename = "JSmodel-KV",
         what = "path", whatLabels = "hide", style = "lisrel", layout = "tree2", residuals = TRUE, nodeLabels = names, nCharNodes = 0,
         sizeMan = 4.5, sizeLat = 14, sizeLat2 = 10,
         esize = 2, label.cex = 0.7, label.scale = FALSE, width = 6, height = 5)


library(sem)

# MLM
fit_MLM <- cfa(model_MBI, data = viikko3, estimator="MLM")

summary(fit_MLM, fit.measures = T)

semPaths(fit_MLM, title = FALSE, curvePivot = TRUE)
semPaths(fit_MLM, layout='tree2')



colnames(wk4) <- paste("ITEM", 1:22, sep="")
getwd()


library(dplyr)
library(corrplot)
library(ggplot2)
library(GGally)

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

X <- rnorm(100)
Y <- rnorm(100)
Z <- rnorm(1)*X + rnorm(1)*Y + rnorm(1)*X*Y
DF <- data.frame(X,Y,Z)



semPaths(fit, what = "col", whatLabels = "hide", style = "lisrel",
         layout='circle2', residuals = TRUE, nCharNodes = 0,
         sizeMan = 6, sizeLat = 6, sizeLat2 = 5, esize = 1,
         label.cex = 0.55, label.scale = FALSE, width = 9, height = 9,
         rotation = 1, exoCov = FALSE)

semPaths(fit, what = "path", whatLabels = "hide", style = "lisrel",
         residuals = TRUE, curve = 3.2, rotation = 4, nCharNodes = 0,
         sizeLat = 7, sizeMan = 6, sizeMan2 = 4, esize = 1, label.cex = 1.2,
         mar = c(3, 8, 3, 15), exoCov = FALSE)

semPaths(fit, what = "path", whatLabels = "hide", style = "lisrel",
         layout = "tree2", residuals = TRUE, nCharNodes = 0, 
         rotation = 2, sizeMan = 6, sizeMan2= 3, sizeLat = 8, sizeLat2 = 6,
         esize = 1.3, label.cex = 0.7, label.scale = FALSE, width = 6, 
         height = 5, exoCov = FALSE)
