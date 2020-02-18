#two sample t-test to see if both the species have equal body depths
df <- split(crabs, crabs$sp)
df
Blue <- df[[1]]
Orange <- df[[2]]
Blue
Orange
t.test(Blue$BD, Orange$BD, mu= 0, alternative = "two.sided", paired = FALSE, var.equal = FALSE)

#proportion to test to see the difference between proportion of female left handers and male left handers
P <-table(survey$Sex, survey$W.Hnd)
P
prop.test(P, alternative = "two.sided")


#F-test to test the variances of writing and non-writing hands
var.test(survey$Wr.Hnd, survey$NW.Hnd, alternative = "two.sided", conf.level = 0.95)
