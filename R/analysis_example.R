library(readr)
library(dplyr)

insurance <- read_csv("./BIDS/data/insurance.csv")
df <- as_tibble(insurance)

View(df)
summary(df)

### convert to factor (categories)

df$sex <- factor(df$sex)
df$smoker <- factor(df$smoker)
df$region <- factor(df$region)

summary(df)

### create graphs
par(mfrow=c(3,2))
#pdf("insurance_graphs.pdf")
for (i in names(df)[1:6]) {
  plot(df$charges ~ df[[i]],
       main=paste("Charges vs",i), ylab="Charges",xlab=i)
}
par(mfrow=c(1,1))
#dev.off()

## Age

plot(df$charges ~ df$age,col=df$sex)
plot(df$charges ~ df$age,col=df$smoker)
plot(df$charges ~ df$age,col=df$region)

## Age-Smoker
plot(df$charges ~ df$age,col=df$smoker)
abline(h=15000,col="darkgreen")
abline(h=32000,col="darkgreen")

## BMI
plot(df$charges ~ df$bmi,col=df$sex)
plot(df$charges ~ df$bmi,col=df$smoker)
plot(df$charges ~ df$bmi,col=df$region)

## BMI-Smokers
plot(df$charges ~ df$bmi,col=df$smoker)
abline(h=15000,col="darkgreen")
abline(h=32000,col="darkgreen")
abline(v=30,col="darkgreen")

### Analysis with agregations

df %>% 
  group_by(smoker) %>%
  summarise(age_min=min(age,na.rm=TRUE),
            age_25pct=quantile(age,probs = 0.25,na.rm=TRUE),
            age_mean=mean(age,na.rm=TRUE),
            age_median=median(age,na.rm=TRUE),
            age_75pct=quantile(age,probs = 0.75,na.rm=TRUE),
            age_max=max(age,na.rm=TRUE),
            bmi_min=min(bmi,na.rm=TRUE),
            bmi_25pct=quantile(bmi,probs = 0.25,na.rm=TRUE),
            bmi_mean=mean(bmi,na.rm=TRUE),
            bmi_median=median(bmi,na.rm=TRUE),
            bmi_75pct=quantile(bmi,probs = 0.75,na.rm=TRUE),
            bmi_max=max(bmi,na.rm=TRUE),
            charge_min=min(charges,na.rm=TRUE),
            charge_25pct=quantile(charges,probs = 0.25,na.rm=TRUE),
            charge_mean=mean(charges,na.rm=TRUE),
            charge_median=median(charges,na.rm=TRUE),
            charge_75pct=quantile(charges,probs = 0.75,na.rm=TRUE),
            charge_max=max(charges,na.rm=TRUE)
            ) %>%
  t()


df %>% 
  mutate(obese=factor(ifelse(bmi >= 30,1,0),
                      levels=c(0,1),
                      labels=c("Normal","Obese"))) %>%
  group_by(smoker, obese) %>%
  summarise(age_min=min(age,na.rm=TRUE),
            age_25pct=quantile(age,probs = 0.25,na.rm=TRUE),
            age_mean=mean(age,na.rm=TRUE),
            age_median=median(age,na.rm=TRUE),
            age_75pct=quantile(age,probs = 0.75,na.rm=TRUE),
            age_max=max(age,na.rm=TRUE),
            charge_min=min(charges,na.rm=TRUE),
            charge_25pct=quantile(charges,probs = 0.25,na.rm=TRUE),
            charge_mean=mean(charges,na.rm=TRUE),
            charge_median=median(charges,na.rm=TRUE),
            charge_75pct=quantile(charges,probs = 0.75,na.rm=TRUE),
            charge_max=max(charges,na.rm=TRUE)
            
  ) %>%
  t()

age_charge_nonsmokers <- df %>% 
  filter(smoker=="no") %>%
  group_by(age) %>%
  summarise(charge_min=min(charges,na.rm=TRUE),
            charge_25pct=quantile(charges,probs = 0.25,na.rm=TRUE),
            charge_mean=mean(charges,na.rm=TRUE),
            charge_median=median(charges,na.rm=TRUE),
            charge_75pct=quantile(charges,probs = 0.75,na.rm=TRUE),
            charge_max=max(charges,na.rm=TRUE)
            
  ) 

### Age by smoke and obese
df$smoke_obese <- ifelse(df$smoker=="yes" & df$bmi >= 30,3,
                  ifelse(df$smoker=="yes" & df$bmi < 30,2,1))
plot(df$charges ~ df$age,col=df$smoke_obese)


### check for the second group:

## by sex
df %>%
  filter(smoke_obese == 2) %>%
  mutate(obese=factor(ifelse(bmi >= 30,1,0),
                      levels=c(0,1),
                      labels=c("Normal","Obese"))) %>%
  group_by(sex) %>%
  summarize(age_min=min(age,na.rm=TRUE),
            age_25pct=quantile(age,probs = 0.25,na.rm=TRUE),
            age_mean=mean(age,na.rm=TRUE),
            age_median=median(age,na.rm=TRUE),
            age_75pct=quantile(age,probs = 0.75,na.rm=TRUE),
            age_max=max(age,na.rm=TRUE),
            bmi_min=min(bmi,na.rm=TRUE),
            bmi_25pct=quantile(bmi,probs = 0.25,na.rm=TRUE),
            bmi_mean=mean(bmi,na.rm=TRUE),
            bmi_median=median(bmi,na.rm=TRUE),
            bmi_75pct=quantile(bmi,probs = 0.75,na.rm=TRUE),
            bmi_max=max(bmi,na.rm=TRUE)) %>%
  t()

## by children
df %>%
  filter(smoke_obese == 2) %>%
  mutate(obese=factor(ifelse(bmi >= 30,1,0),
                      levels=c(0,1),
                      labels=c("Normal","Obese"))) %>%
  group_by(children) %>%
  summarize(age_min=min(age,na.rm=TRUE),
            age_25pct=quantile(age,probs = 0.25,na.rm=TRUE),
            age_mean=mean(age,na.rm=TRUE),
            age_median=median(age,na.rm=TRUE),
            age_75pct=quantile(age,probs = 0.75,na.rm=TRUE),
            age_max=max(age,na.rm=TRUE),
            bmi_min=min(bmi,na.rm=TRUE),
            bmi_25pct=quantile(bmi,probs = 0.25,na.rm=TRUE),
            bmi_mean=mean(bmi,na.rm=TRUE),
            bmi_median=median(bmi,na.rm=TRUE),
            bmi_75pct=quantile(bmi,probs = 0.75,na.rm=TRUE),
            bmi_max=max(bmi,na.rm=TRUE),
            cnt=n()) %>%
  t()

  

## BMI - Smokers and obese
plot(df$charges ~ df$bmi,col=df$smoke_obese)
abline(h=15000,col="darkgreen")
abline(h=32000,col="darkgreen")
abline(v=30,col="darkgreen")

## Age - Smokers and obese
plot(df$charges ~ df$age,col=df$smoke_obese)
abline(h=15000,col="darkgreen")
abline(h=32000,col="darkgreen")

## the middle group is less homogeneous. This does not depend on
## the smoking, age or bmi. There must be other factors influencing 
## the value of charges



col <- ifelse(df$charges < 15000, 1, 
       ifelse(df$charges >= 15000 & df$bmi < 30, 2,
       ifelse(df$charges < 32000,3, 4 )))

plot(df$charges ~ df$bmi,col=col)
plot(df$charges ~ df$age,col=col)

