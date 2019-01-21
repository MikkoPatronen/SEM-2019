
#######    SEM 2019 WEEK 1
#######    Mikko Patronen 22.1.2019
#######
#######    Exercise 1.2
#
# a)

testi <- as.data.frame(ex3.1)
df <- ex3.1
save(df, file="df.Rdata")

y1 <- df$V1
x1 <- df$V2
x3 <- df$V3

model <- lm(y1 ~ x1 + x3)
summary(model)

# b)
df2 <- ex4.1a
save(df2, file="df2.Rdata")

y1 <- df2$V1
y2 <- df2$V2
y3 <- df2$V3
y4 <- df2$V4
y5 <- df2$V5
y6 <- df2$V6
y7 <- df2$V7
y8 <- df2$V8
y9 <- df2$V9
y10 <- df2$V10
y11 <- df2$V11
y12 <- df2$V12
