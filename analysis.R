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
data$expertise <- ifelse(data$group<5,1,ifelse(data$group==9,0,0))
data$fact <- ifelse(data$group<3 | data$group==5 | data$group==6,1,ifelse(data$group==9,0,0))
data$gain <- ifelse(data$group==2 | data$group==4| data$group==6 | data$group==8,1,ifelse(data$group==9,0,0))
data$treatment <- ifelse(data$group==9,0,1)
data$birth_year <- as.numeric(data$birth_year)
data$ideology <- as.numeric(data$ideology)

#Assigning Ethnicity
data$ethnicity <- as.numeric(data$ethnicity)
data$ethnicity <- ifelse(data$ethnicity>7,7,data$ethnicity)


#Filtering low-attension out of the data
data <- data[which(data$lowattention==0),]
me
means <- aggregate(value ~  group, data3, mean)

#Summary Plot for Treatment Averages
data2 <- data %>% select(group,treatment1, treatment2,treatment3,treatment4)
data3 <- melt(data = data2, id.vars = "group", na.rm= T)
data3$group <- as.factor(data3$group)
means <- aggregate(value ~  group + variable, data3, mean)
sds <- aggregate(value ~  group + variable, data3, sd)
ggplot(data=data3,aes(x = group, y=value, fill=group)) + geom_boxplot(alpha=0.3) + theme(legend.position="none") +
scale_fill_brewer(palette="BuPu")+ facet_wrap(~variable)  +scale_y_discrete(limits=c(1:7)) +
  stat_summary(fun=mean, colour="black", geom="point",shape=18, size=0.1) +  
geom_text(data = means, aes(label = round(value,2), y = value + 0.5), size=2, colour = "dark red")

#Summary Plot for Ideology Scale Averages
data2 <- data %>% select(ideology,treatment1, treatment2,treatment3,treatment4)
data3 <- melt(data = data2, id.vars = "ideology", na.rm= T)
data3$ideology <- as.factor(data3$ideology)
means <- aggregate(value ~  ideology + variable, data3, mean)
sds <- aggregate(value ~  ideology + variable, data3, sd)
ggplot(data=data3,aes(x = ideology, y=value, fill=ideology)) + geom_boxplot(alpha=0.3) + theme(legend.position="none") +
  scale_fill_brewer(palette="PuRd")+ facet_wrap(~variable)  +scale_y_discrete(limits=c(1:7)) +
  stat_summary(fun=mean, colour="black", geom="point",shape=18, size=0.1) +  
  geom_text(data = means, aes(label = round(value,2), y = value + 0.5), size=2, colour = "dark red")

#Summary Plot for Party Scale Averages
data2 <- data %>% select(pid_scale,treatment1, treatment2,treatment3,treatment4)
data3 <- melt(data = data2, id.vars = "pid_scale", na.rm= T)
data3$pid_scale <- as.factor(data3$pid_scale)
means <- aggregate(value ~  pid_scale + variable, data3, mean)
sds <- aggregate(value ~  pid_scale + variable, data3, sd)
ggplot(data=data3,aes(x = pid_scale, y=value, fill=pid_scale)) + geom_boxplot(alpha=0.3) + theme(legend.position="none") +
  scale_fill_brewer(palette="PuRd")+ facet_wrap(~variable)  +scale_y_discrete(limits=c(1:7)) +
  stat_summary(fun=mean, colour="black", geom="point",shape=18, size=0.1) +  
  geom_text(data = means, aes(label = round(value,2), y = value + 0.5), size=2, colour = "dark red")

#Summary Plot for Media
data2 <- data %>% select(fox,abc,fb,insta,twitter,nbc,cnn,nyt,treatment1, treatment2,treatment3,treatment4)
data3 <- melt(data = data2, id.vars = c("treatment1", "treatment2","treatment3","treatment4"), na.rm= T)
data3<-filter(data3,value==1)
data3<-data3[,-6]
data3 <- melt(data = data3, id.vars = "variable", na.rm= T)
colnames(data3) <- c("media","treatment","value")
means <- aggregate(value ~  media + treatment, data3, mean)
ggplot(data=data3,aes(x = media, y=value, fill=media)) + geom_boxplot(alpha=0.3) + theme(legend.position="none") +
  scale_fill_brewer(palette="RdYlBu")+ facet_wrap(~treatment)  +scale_y_discrete(limits=c(1:7)) +
  stat_summary(fun=mean, colour="black", geom="point",shape=18, size=0.1) +  
  geom_text(data = means, aes(label = round(value,2), y = value + 0.5), size=2, colour = "dark red")

#Power Anlysis for T-Test
power.t.test(n=502,sig.level=0.05,power=0.9)
#Delta of 0.067 which means we can detect a small effect size

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
model1g <- lm(treatment1 ~ treatment + expertise + gain + fact + pid_scale + ideology + birth_year + fox + nbc, data)
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
model2g <- lm(treatment2 ~ treatment + expertise + gain + fact + pid_scale + ideology + birth_year + fox + nbc, data)
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
model3g <- lm(treatment3 ~ treatment + expertise + gain + fact + pid_scale + ideology + birth_year + fox + nbc, data)
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
model4g <- lm(treatment4 ~ treatment + expertise + gain + fact + pid_scale + ideology + birth_year + fox + nbc, data)
summary(model4g)

#Regression using Trust Variables

model <- lm(data$trust_president ~ data$republican)
summary(model)
model <- lm(data$trust_doctors ~ data$republican)
summary(model)

#Plotting the models

plot_summs(model4g,model3g,model2g,model1g, omit.coefs = c("(Intercept)"),plot.distributions = F, scale=T,model.names = c("Treatment 1","Treatment 2","Treatment 3","Treatment 4"), legend.title = "Treatment") + theme(legend.position="bottom")


#Distribution of Manipulation Checks
table(data$mcscore,data$group)
summary(aov(data$mcscore~data$group))

data5<- data[which(is.na(data$pid_scale)),]
model5a <- lm(treatment4 ~ control, data5)
summary(model5a)
model5b <- lm(treatment1 ~ expertise + gain+fact, data5)
summary(model5b)
model5c <- lm(treatment4 ~ expertise + gain + fact, data5)
summary(model5c)
model5d <- lm(treatment4 ~ expertise + gain + fact +ideology, data5)
summary(model5d)
model5e <- lm(treatment4 ~ expertise + gain + fact + pid_scale + expertise*pid_scale, data5)
summary(model5e)
model5f <- lm(treatment4 ~ expertise + gain + fact + pid_scale + birth_year, data5)
summary(model5f)
model5g <- lm(treatment4 ~ expertise + gain + fact + pid_scale + birth_year + fox, data5)
summary(model5g)
