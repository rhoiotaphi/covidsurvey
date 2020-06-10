# Analysis | Rahul
# Last Modified : 5/25/2020


rm(list=ls())
library(readr)
library(stringr)
library(tm)
library(tidyverse)
library(reshape2)
library(jtools)
library(broom.mixed)
library(lattice)
data <- read_csv("C:/Users/hemra/Downloads/data_clean.csv")

#Coding different Groups in the Data
data$control <- ifelse(data$group==9,1,0)
data$pid <- as.numeric(data$pid)
data$pid_scale <- ifelse(data$pid<4,data$pid, ifelse(data$pid==9,NA,data$pid-1))
data$republican <- ifelse(data$pid>5 & data$pid<9,1,0)
data$group <- as.numeric(data$group)
data$expertise <- ifelse(data$group<5,1,ifelse(data$group==9,NA,0))
data$fact <- ifelse(data$group<3 | data$group==5 | data$group==6,1,ifelse(data$group==9,NA,0))
data$gain <- ifelse(data$group==2 | data$group==4| data$group==6 | data$group==8,1,ifelse(data$group==9,NA,0))



#Assigning Ethnicity
data$ethnicity <- as.numeric(data$ethnicity)
data$ethnicity <- ifelse(data$ethnicity>7,7,data$ethnicity)

#Filtering low-attension out of the data
data <- data[which(data$lowattention==0),]

#Power Anlysis for T-Test
power.t.test(n=4722,sig.level=0.05,power=0.9)
#Delta of 0.067 which means we can detect a substatially small effect size

#T-test comparing controls with others
t.test(data$treatment1  ~  data$control)
t.test(data$treatment2  ~  data$control)
t.test(data$treatment3  ~  data$control)
t.test(data$treatment4  ~  data$control)

#Summary plots for all treatments for republications and non-republicans
data2 <- data[which(data$republican==1),]
data2 <- data2 %>% group_by(group) %>% summarise(t1=mean(treatment1,na.rm=T),t2=mean(treatment2,na.rm=T), t3=mean(treatment3,na.rm=T), t4=mean(treatment4,na.rm=T))
data3 <- melt(data = data2, id.vars = "group")
ggplot(data=data3,aes(x = group, y=value)) + geom_bar(stat = "identity", fill = "maroon") + facet_wrap(~variable) + scale_x_discrete(limits=c(1:9))

data2 <- data[which(data$republican==0),]
data2 <- data2 %>% group_by(group) %>% summarise(t1=mean(treatment1,na.rm=T),t2=mean(treatment2,na.rm=T), t3=mean(treatment3,na.rm=T), t4=mean(treatment4,na.rm=T))
data3 <- melt(data = data2, id.vars = "group")
ggplot(data=data3,aes(x = group, y=value)) + geom_bar(stat = "identity", fill = "navy blue") + facet_wrap(~variable) + scale_x_discrete(limits=c(1:9))

ggplot(data=data2,aes(x = group, y=t1)) + geom_bar(stat = "identity")
ggplot(data=data2,aes(x = group, y=t2)) + geom_bar(stat = "identity")
ggplot(data=data2,aes(x = group, y=t3)) + geom_bar(stat = "identity")
ggplot(data=data2,aes(x = group, y=t4)) + geom_bar(stat = "identity")

#ANOVA for Republicans
data2 <- data[which(data$republican==1),]
summary(aov(data2$treatment1~data2$group))
summary(aov(data2$treatment2~data2$group))
summary(aov(data2$treatment3~data2$group))
summary(aov(data2$treatment4~data2$group))

#T-test for difference between republicans and non-republicans
t.test(data$treatment1 ~ data$republican)
t.test(data$treatment2 ~ data$republican)
t.test(data$treatment3 ~ data$republican)
t.test(data$treatment4 ~ data$republican)


#T-tests for Expertise, Fact and Gain-based statements for all treatments
t.test(data$treatment1 ~ data$expertise)
t.test(data$treatment2 ~ data$expertise)
t.test(data$treatment3 ~ data$expertise)
t.test(data$treatment4 ~ data$expertise)

t.test(data$treatment1 ~ data$fact)
t.test(data$treatment2 ~ data$fact)
t.test(data$treatment3 ~ data$fact)
t.test(data$treatment4 ~ data$fact)

t.test(data$treatment1 ~ data$gain)
t.test(data$treatment2 ~ data$gain)
t.test(data$treatment3 ~ data$gain)
t.test(data$treatment4 ~ data$gain)

t.test(data$treatment1 ~ data$fox)
t.test(data$treatment2 ~ data$fox)
t.test(data$treatment3 ~ data$fox)
t.test(data$treatment4 ~ data$fox)

#T-test for Expertise given fact
t.test(data[which(data$fact==1),]$treatment1 ~ data[which(data$fact==1),]$expertise)
t.test(data[which(data$fact==1),]$treatment2 ~ data[which(data$fact==1),]$expertise)
t.test(data[which(data$fact==1),]$treatment3 ~ data[which(data$fact==1),]$expertise)
t.test(data[which(data$fact==1),]$treatment4 ~ data[which(data$fact==1),]$expertise)

#T-test given Independance
data$independance <- ifelse(data$pid==5,1,0)
data4 <- data[which(data$independance>0),]

data$independance_levels <- ifelse(data$pid==1|data$pid==8, 3,ifelse(data$pid==2|data$pid==7,2,ifelse(data$pid==3|data$pid==6,1,0)))

t.test(data4$treatment1 ~ data4$control)
t.test(data4$treatment2 ~ data4$control)
t.test(data4$treatment3 ~ data4$control)
t.test(data4$treatment4 ~ data4$control)

#Regressions for Treatment1
model1a <- lm(treatment1 ~ expertise, data)
summary(model1a)
model1b <- lm(treatment1 ~ expertise + gain, data)
summary(model1b)
model1c <- lm(treatment1 ~ expertise + gain + fact, data)
summary(model1c)
model1d <- lm(treatment1 ~ expertise + gain + fact + pid_scale, data)
summary(model1d)
model1e <- lm(treatment1 ~ expertise + gain + fact + pid_scale + expertise*pid_scale, data)
summary(model1e)
model1f <- lm(treatment1 ~ expertise + gain + fact + pid_scale + birth_year, data)
summary(model1f)
model1g <- lm(treatment1 ~ expertise + gain + fact + pid_scale + birth_year + fox, data)
summary(model1g)

#Regressions for Treatment2
model2a <- lm(treatment2 ~ expertise, data)
summary(model2a)
model2b <- lm(treatment2 ~ expertise + gain, data)
summary(model2b)
model2c <- lm(treatment2 ~ expertise + gain + fact, data)
summary(model2c)
model2d <- lm(treatment2 ~ expertise + gain + fact + pid_scale, data)
summary(model2d)
model2e <- lm(treatment2 ~ expertise + gain + fact + pid_scale + expertise*pid_scale, data)
summary(model2e)
model2f <- lm(treatment2 ~ expertise + gain + fact + pid_scale + birth_year, data)
summary(model2f)
model2g <- lm(treatment2 ~ expertise + gain + fact + pid_scale + birth_year+ fox, data)
summary(model2g)

#Regressions for Treatment3
model3a <- lm(treatment3 ~ expertise, data)
summary(model3a)
model3b <- lm(treatment3 ~ expertise + gain, data)
summary(model3b)
model3c <- lm(treatment3 ~ expertise + gain + fact, data)
summary(model3c)
model3d <- lm(treatment3 ~ expertise + gain + fact + pid_scale, data)
summary(model3d)
model3e <- lm(treatment3 ~ expertise + gain + fact + pid_scale + expertise*pid_scale, data)
summary(model3e)
model3f <- lm(treatment3 ~ expertise + gain + fact + pid_scale + birth_year, data)
summary(model3f)
model3g <- lm(treatment3 ~ expertise + gain + fact + pid_scale + birth_year +fox, data)
summary(model3g)

#Regressions for Treatment4
model4a <- lm(treatment4 ~ expertise, data)
summary(model4a)
model4b <- lm(treatment4 ~ expertise + gain, data)
summary(model4b)
model4c <- lm(treatment4 ~ expertise + gain + fact, data)
summary(model4c)
model4d <- lm(treatment4 ~ expertise + gain + fact + pid_scale, data)
summary(model4d)
model4e <- lm(treatment4 ~ expertise + gain + fact + pid_scale + expertise*pid_scale, data)
summary(model4e)
model4f <- lm(treatment4 ~ expertise + gain + fact + pid_scale + birth_year, data)
summary(model4f)
model4g <- lm(treatment4 ~ expertise + gain + fact + pid_scale + birth_year + fox, data)
summary(model4g)

#Regression using Trust Variables

model <- lm(data$trust_president ~ data$republican)
summary(model)
model <- lm(data$trust_doctors ~ data$republican)
summary(model)

#Plotting the models
par(mfrow=c(2,2))
plot_summs(model1g, omit.coefs = c("(Intercept)"),plot.distributions = F)
plot_summs(model2g, omit.coefs = c("(Intercept)"),plot.distributions = F)
plot_summs(model3g, omit.coefs = c("(Intercept)"),plot.distributions = F)
plot_summs(model4g, omit.coefs = c("(Intercept)"),plot.distributions = F)

