
#######    SEM 2019 WEEK 5
#######    Mikko Patronen 19.2.2019
#######
#######    Exercise 5
#
# T5.2 eteenpäin hyödyllinen: http://lavaan.ugent.be/tutorial/groups.html

# reading in the data sets:

elementary <- read.table("mbielm1.dat", header = FALSE)
secondary <- read.table("mbisec1.dat", header = FALSE)
names(elementary) <- names(secondary) <- paste("ITEM", 1:22, sep = "")

# Model for data "elementary" and also for data "secondary":
# F1 BY ITEM1 - ITEM3 ITEM6 ITEM8 ITEM13 ITEM14 ITEM16 ITEM20;
# F2 BY ITEM5 ITEM10 ITEM11 ITEM15 ITEM22;
# F3 BY ITEM4 ITEM7 ITEM9 ITEM12 ITEM17 - ITEM19 ITEM21;

model <- '
# F1
EE =~ ITEM1 + ITEM2 + ITEM3 + ITEM6 + ITEM8 + ITEM13 + ITEM14 + ITEM16 + ITEM20
# F2
DP =~ ITEM5 + ITEM10 + ITEM11 + ITEM15 + ITEM22
# F3
PA =~ ITEM4 + ITEM7 + ITEM9 + ITEM12 + ITEM17 + ITEM18 + ITEM19 + ITEM21
'

fit_elem <- cfa(model, data = elementary, estimator="MLM")
fit_sec <- cfa(model, data = secondary, estimator="MLM")

library(dplyr)
msr <- c("chisq.scaled", "df.scaled", "pvalue.scaled", 
         "chisq.scaling.factor", "cfi.robust", "tli.robust",
         "rmsea.robust")

M1 <- c(fitMeasures(fit)[msr]) %>% round(3)
print(M1) 



# (later, when having the initial baseline models ready for each group)

# Combine (merge) the data sets into one:

elmsec <- merge(data.frame(elementary, group = "elementary"),
                data.frame(secondary, group = "secondary"), all = TRUE, sort = FALSE)

# I used these four indices all the time:
fourfitindices = c("CFI", "TLI", "RMSEA", "SRMR")

# Configural model (common baseline model for both groups simultaneously):

INV1model <- '
# EE: EmotionalExhaustion (including ITEM12!)
# EP: Depersonalization
# PA: PersonalAccomplishment
EE =~ ITEM1 + ITEM2 + ITEM3 + ITEM6 + ITEM8 + ITEM13 + ITEM14 + ITEM16 + ITEM20 + ITEM12
DP =~ ITEM5 + ITEM10 + ITEM11 + ITEM15 + ITEM22
PA =~ ITEM4 + ITEM7 + ITEM9 + ITEM12 + ITEM17 + ITEM18 + ITEM19 + ITEM21

# Residual covariances (in both groups)
ITEM1 ~~ ITEM2
ITEM6 ~~ ITEM16
ITEM10 ~~ ITEM11

# Group-specific model parameters
ITEM4 ~~ c(NA,0)*ITEM7 # only for elm
EE =~ c(0,NA)*ITEM11 # only for sec
ITEM9 ~~ c(0,NA)*ITEM19 # only for sec
'

# Here I used the mimic option:

INV1fit <- cfa(INV1model, data = elmsec, estimator = "MLM", group = "group", mimic = "Mplus")

summary(INV1fit, standardized = TRUE)
fitMeasures(INV1fit, fourfitindices)