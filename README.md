# sds315_hw7

```{r setup, include=FALSE, message=FALSE}
knitr::opts_chunk$set(echo = TRUE)

library(ggplot2)
library(dplyr)
library(tidyverse)
library(knitr)
library(lubridate)
library(mosaic)
library(MatchIt)
 ```
1A
``` {r, message=FALSE, echo=FALSE}

#load data set
armfold <- read_csv("armfold.csv")

#find how many females
num_female_count <- armfold |>
  summarise(num_female=sum(Sex=="Female")) |> #111 females 
  pull(num_female)

#find how many men
num_male_count <- armfold |>
  summarise(num_male=sum(Sex=="Male")) |> #106 males 
  pull(num_male)

#find proportion of men and women that fold left arm on top 
prop_male_left <- mean(armfold$LonR_fold[armfold$Sex == "Male"] == 1)
prop_female_left <- mean(armfold$LonR_fold[armfold$Sex=="Female"]==1)
```

1B
```{r, message=FALSE, echo=FALSE}

#calculate difference in proportions
left_diff_prop= (prop_male_left- prop_female_left )
```
1C 
``` {r, message=FALSE, echo=FALSE}

#bootstrap
left_bootstrap <- do(10000)*diffprop(LonR_fold ~ Sex, data=resample(armfold))
#confidence interval 
confint(left_bootstrap, level=0.95)

{r, message=FALSE, echo=FALSE}

#assign values for formula 
p1= prop_male_left
p2= prop_female_left
n1= num_male_count
n2= num_female_count

#se formula for diff in props 
se_calculated= (((p1*(1-p1))/n1) + 
((p2*(1-p2))/n2))**0.5
```

2A
``` {r, message=FALSE, echo=FALSE}

#load data set 
turnout <- read_csv("turnout.csv")

#calculate proportion of those recieving a govt call who voted in 1998 
voted_call_1998 <- mean(turnout$voted1998[turnout$GOTV_call==1]==1) # 0.6477733 

#calculate sample proprtion of those not recieving a call who voted in 1998 
voted_nocall_1998 <- mean(turnout$voted1998[turnout$GOTV_call==0]==1) #0.4442449
```
```{r, message=FALSE, echo=FALSE}

#95% confidence interval for difference in proportions 

vote_1998_bootstrap= do(10000)*diffprop(voted1998 ~ GOTV_call, data=resample(turnout))
#confidence interval 
confint(vote_1998_bootstrap, level=0.95)
```
2B
```{r, message=FALSE, echo=FALSE, fig.width=3, fig.height=2}

#bootstrap for voting in 1996 vs gotv call 
vote1996_bootstrap= do(10000)*diffprop(GOTV_call ~ voted1996, data=resample(turnout))
#ci 
confint(vote1996_bootstrap, level= 0.95)

#plot
ggplot(data=vote1996_bootstrap)+
  geom_histogram(aes(x=diffprop), fill='pink', bins=50)
```
```{r, message=FALSE, echo=FALSE, fig.width=3, fig.height=2}

#bootstrap for voting in 1996 vs voting in 1998 
vote1996_bootstrap2 = do(10000)*diffprop(voted1998 ~ voted1996, data=resample(turnout))
#ci 
confint(vote1996_bootstrap2, level=0.95)

#plot 
ggplot(data=vote1996_bootstrap2)+
  geom_histogram(aes(x=diffprop), fill= 'pink3', bins=50)
```
```{r, message=FALSE, echo=FALSE, fig.width=3, fig.height=2}

#bootstrap for age and gotv 
age_bootstrap = do(10000)*diffmean(AGE ~ GOTV_call, data=resample(turnout))
#ci 
confint(age_bootstrap, level=0.95)

#plot
ggplot(data=age_bootstrap)+
  geom_histogram(aes(x=diffmean), fill='skyblue', bins=50)
```
```{r, message=FALSE, echo=FALSE, fig.width=3, fig.height=2}

#bootstrap for age and voting in 1996 
age_bootstrap2= do(10000)*diffmean(AGE ~ voted1998, data=resample(turnout))
#ci
confint(age_bootstrap2, level=0.95)

#plot
ggplot(data=age_bootstrap2)+
  geom_histogram(aes(x=diffmean), fill='skyblue4', bins=50)
```
```{r, message=FALSE, echo=FALSE, fig.width=3, fig.height=2}

#bootstrap for majorpty and gotv call
majorpty_bootstrap= do(10000)*diffprop(GOTV_call ~ MAJORPTY, data=resample(turnout))
#ci 
confint(majorpty_bootstrap, level= 0.95)

#plot
ggplot(data=majorpty_bootstrap)+
  geom_histogram(aes(x=diffprop), fill='purple', bins=50)
```
```{r,message=FALSE, echo=FALSE, fig.width=3, fig.height=2}

#bootstrap for majorpty and vote1998 
majorpty_bootstrap2= do(10000)*diffprop(voted1998~ MAJORPTY, data=resample(turnout))
#ci 
confint(majorpty_bootstrap2, level=0.95)

#plot
ggplot(data=majorpty_bootstrap)+
  geom_histogram(aes(x=diffprop), fill="purple4", bins=50)
```
2C
```{r, echo=FALSE, message=FALSE, fig.width=3, fig.height=2}

#match for gotv call and 1996 
turnout_match = matchit(GOTV_call ~ voted1996 + AGE + MAJORPTY, data=turnout, ratio=5)

#check covariate balance 
summary(turnout_match)

#extract only matched pairs 
turnout_matched = match.data(turnout_match)

#bootstrapped confidence interval 
boot_match = do(10000)*diffmean(voted1998 ~ GOTV_call, data=resample(turnout_matched))

#plot
ggplot(data=boot_match)+
  geom_histogram(aes(x=diffmean))

#confidence interval
confint(boot_match, level=0.95)


#find proportions from matched data set 
matched_voted_call_1998 <- mean(turnout_matched$voted1998[turnout_matched$GOTV_call==1]==1) #call

matched_voted_nocall_1998 <-mean(turnout_matched$voted1998[turnout_matched$GOTV_call==0]==1) #no call 
  ```



