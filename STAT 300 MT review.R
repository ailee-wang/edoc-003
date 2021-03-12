##   KW test---
a <- c(1,2,3)
b <- c(3,5,9)
c <- c(7,6,9,8)
data <- c(a,b,c)
#把每一组data用C()组合到一起
factor <- c(rep(1,3), rep(2,3), rep(3,4))
#建立对应的factor
kruskal.test(data~ factor) 
#H0: all groups have same distribution

#手算版本

#H is the test statistic 但是我没找到能直接算的code，如果直接问H的话其实可以直接用上面的code算，找个最相近的答案就可以了，千层面说可以有容错范围，
#或者带入公式反推一下（Hc=H*1.0005956）

pchiq(H,df,lower.tail=FALSE)

## permutation test

x = c(rep(0,17), rep(1,2), 2)
y =c(rep(1,3),3)


library(exactRankTests)
perm.test(x, y, paired=FALSE,
          alternative="less", mu=0, exact=TRUE, conf.int=FALSE)

perm.test(x, y,paired = F,alternative = "less")


#goodness of fit x^2 test 
ei <- c()
oi <- c()
x <- 
test_stat <- sum((oi - ei)^2/ei)
p_val <- pchisq(test_stat, df= x, lower.tail = FALSE)
 
#contingency table(independece)
CHFdata <- matrix(c(), nrow=2, byrow=TRUE)#matrix 的顺序是从第一行第一个开始到最后一行
chisq.test(CHFdata, correct =FALSE)

#Density curve
oc <- c(8, 22, 25, 32, 34, 31, 31, 4)
n<-sum(oc)
m <- 
s <- 
cutpoints <- seq(5.959, 11.449, length.out = 9)#number of cutoff create bins (# cutpoints = # bins + 1)
#expected counts (187 is the sample size of observation)
ec <- n*(pnorm(cutpoints[2:9], mean=m, sd=s) - pnorm(cutpoints[1:8],mean=8.768, sd=1.253))# the mean and sd can be edit
ec[1] <- n*pnorm(cutpoints[2], mean=m, sd=s)
ec[8] <- n*pnorm(cutpoints[8], mean=m8, sd=s,lower.tail = FALSE)
ec
chis <- sum((oc - ec)^2/ec)
chis
dfs <- 
pchisq(chis,df = dfs, lower.tail=FALSE)



## One Way anova
# Or, if .csv file, use this(请提前自行建立,已发群)
my_data <- read.delim(file.choose())
my_data

library(dplyr)
group_by(my_data, Type) %>%
  summarise(
    count = n(),
    mean = mean(my_data$data, na.rm = TRUE),
    sd = sd(my_data$data, na.rm = TRUE)
  )

# 输出anova 表格
res.aov <- aov(data ~ Type, data = my_data)
# Summary of the analysis
summary(res.aov)


##Wilcoxon rank sum test
female_bill_length <- subset(penguins, sex == 'female')$bill_length_mm
male_bill_length <- subset(penguins, sex == 'male')$bill_length_mm

wilcox.test(female_bill_length, male_bill_length)
wilcox.test(female_bill_length, male_bill_length, alternative = "greater")
wilcox.test(female_bill_length, male_bill_length, alternative = "less")
#总结：wilcox.test(A,B,alternative=?)
