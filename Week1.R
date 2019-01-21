
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
save(df2, file="df2.Rdata")