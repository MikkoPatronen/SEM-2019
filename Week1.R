
#######    SEM 2019 WEEK 1
#######    Mikko Patronen 22.1.2019
#######
#######    Exercise 1.2
#
# a)

df <- ex3.1
save(df, file="df.Rdata")

y1 <- df$V1
x1 <- df$V2
x3 <- df$V3

model <- lm(y1 ~ x1 + x3)
summary(model)

# b)
df2 <- ex4.1a
colnames(df2) <- c("y1", "y2", "y3", "y4", "y5", "y6", "y7", "y8", "y9", "y10", "y11", "y12")

save(df2, file="df2.Rdata")
