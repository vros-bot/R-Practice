library(MASS)
#one sample t-test
t.test(chem, mu = 1, alternative = "l")


#two smple t-test
df <- split(cats, cats$Sex)
df
female <- df[[1]]
male <- df[[2]]
female
male
t.test(female$Bwt, male$Bwt, mu= 0, alternative = "two.sided", paired = FALSE, var.equal = FALSE)

shoes

#paired t-test
t.test(shoes$A, shoes$B, mu= 0, alternative="less", paired = TRUE, conf.level = 0.99)

#proportion test
df1 <- split(bacteria, bacteria$trt)
df1
Pgroup <- df1[[1]]
Pgroup
dgroup <- rbind(df1[[2]], df1[[3]])
dgroup
Pgroup
pprop <- table(Pgroup$y)
prop
dprop <- table(dgroup$y)
dprop
proptable <- rbind(pprop,dprop)
proptable
prop.test(proptable, alternative = "greater")

#F-test
var.test(male$Bwt, female$Bwt, alternative = "two.sided", conf.level = 0.99)
