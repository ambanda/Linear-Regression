
```
The Impact of Legalized Abortion on Crime

Could abortion influence the rate of crime in later years? 
Women having a right to chose when to have children might be beneficial to society after all. 
The study was conducted to evaluate the impacts that abortion could cause on crime rate in subsequent years. 
The study postulated that legalizing abortion had a positive impact on crime rate reduction. 
The study analysed abortion rates from 1990-2000 and the crime rate from 2010-2020 in different states that legalized abortion within those years and how has it changed the crime rates 20 years later

The variables of the focus were; 
crime rates as our dependant variable, 
abortion rates, police per capita and unemployment as independent variables.

Control variable;
We set unemployment and police per capita as our control variable

```


library(cspp)
library(tidyverse)
library(dplyr)
library(Hmisc)
library(ggplot2)
library(broom)
library(ggpubr)

data1 <-  get_cspp_data(vars = c("poptotal","popmale", "popfemale", 
                                 "pctpopfemale", "murderrate", 
                                 "gsppcap", "robrate", "vcrimerate", 
                                 "raperate", "propcrimerate", 
                                 "ig_polf", "abortionrate", "unemployment"),
                        years = seq(1990, 2020))
print(summary(data1))
print(colnames(data1))
print(data1)






df_arbortion= data1 %>%
  mutate(crimerate=murderrate+robrate+vcrimerate+raperate+propcrimerate) %>%
  filter(year == seq(1990, 2000)) %>%
  select(state,poptotal,year,popmale,popfemale,crimerate,abortionrate,unemployment)

print(colnames(df_arbortion))

#I manually used excel to attach police per 1,000 which we use as our pol_per_1000 column. Because the data was in a state and per county in a state, i averaged county pol_per_1000 in a county. 

df_arbortion1=read.csv(file = "C:\\Users\\AMBANDA\\Documents\\AMBANDA\\UVO\\R\\Final Project\\Arbortion.csv",stringsAsFactors=FALSE)

print(colnames(df_arbortion1))

df_arbortion1=df_arbortion1 %>%
  select(year,crimerate,abortionrate,unemployment,pol_per_1000)


print(tail(df_arbortion1))



# imputing median value with NA 

all_column_median <- apply(df_arbortion1, 2, median, na.rm=TRUE)

# imputing median value with NA 
for(i in colnames(df_arbortion1))
  df_arbortion1[,i][is.na(df_arbortion1[,i])] <- all_column_median[i]

print(head(df_arbortion1))

write.csv(df_arbortion1, "C:\\Users\\AMBANDA\\Documents\\AMBANDA\\UVO\\R\\New folder\\df_arbortion1.csv", row.names = FALSE)

print(summary(df_arbortion1))


ggplot(df_arbortion1, aes(x=year, y=pol_per_1000)) + 
  geom_line() + 
  labs(tittle="Yearly Crime rate")+
  stat_smooth()



ggplot(df_arbortion1, aes(x=year, y=unemployment)) + 
  geom_line() + 
  labs(tittle="Yearly Crime rate")+
  stat_smooth()

#Checking for Normality

hist(df_arbortion1$abortionrate)
hist(df_arbortion1$unemployment)
hist(df_arbortion1$pol_per_1000)

#Linearity

plot(abortionrate ~ crimerate, data = df_arbortion1)

plot(unemployment ~ crimerate, data = df_arbortion1)

plot(pol_per_1000 ~ crimerate, data = df_arbortion1)

#Independency of observation

cor(df_arbortion1$crimerate, df_arbortion1$abortionrate)

cor(df_arbortion1$crimerate, df_arbortion1$unemployment)

cor(df_arbortion1$crimerate, df_arbortion1$pol_per_1000)

#Multi-colinearity

cor(df_arbortion1) 


#Regression 
crimerate.abortionrate.lm<- lm(crimerate ~ abortionrate, data = df_arbortion1)

summary(crimerate.abortionrate.lm)

#Regression with control variable(Unemployment)

crimerate.abortionrate.lm<- lm(crimerate ~ abortionrate+unemployment, data = df_arbortion1)

summary(crimerate.abortionrate.lm)

#Regression with control variable(pol_per_1000)
crimerate.abortionrate.lm<- lm(crimerate ~ abortionrate+pol_per_1000, data = df_arbortion1)

summary(crimerate.abortionrate.lm)

#regression with control variables(unemployment and pol_per_1000)

crimerate.abortionrate.lm <- lm(crimerate ~ abortionrate+unemployment+pol_per_1000, data = df_arbortion1)

summary(crimerate.abortionrate.lm)


```
We fit regression line first without any control variables,

Call:
  lm(formula = crimerate ~ abortionrate, data = df_arbortion1)

Residuals:
  Min      1Q  Median      3Q     Max 
-5956.8 -4249.0 -3318.6   311.2 18237.8 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)   
(Intercept)  10068.00    2969.80   3.390  0.00133 **
  abortionrate    28.47     108.59   0.262  0.79418   
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 7006 on 53 degrees of freedom
Multiple R-squared:  0.001295,	Adjusted R-squared:  -0.01755 
F-statistic: 0.06875 on 1 and 53 DF,  p-value: 0.7942


This shows that without the control variables, crime rate increases with increase with abortion rate,
From the results it was seen that the model is positive. 
The p-value is greater than the significance level thus implying that the reject our null hypothesis


lm(formula = crimerate ~ abortionrate + pol_per_1000, data = df_arbortion1)

Residuals:
  Min     1Q Median     3Q    Max 
-7344  -5312  -1379   2388  17067 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)  15841.792   3781.489   4.189 0.000109 ***
  abortionrate    -1.302    105.111  -0.012 0.990161    
pol_per_1000 -1277.803    549.187  -2.327 0.023911 *  
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 6731 on 52 degrees of freedom
Multiple R-squared:  0.09546,	Adjusted R-squared:  0.06067 
F-statistic: 2.744 on 2 and 52 DF,  p-value: 0.07363


With control variable pol_per_capita we see changes in coefficients, This shows that there is negative relationship btn crime rate and abortion rate,
which implies that indeed there is reduction in crime rate when documented abortion rate was increasing and also when police per capita was increased.
The p-value is less than our significance level implying that we acept our null hypothesis which is that crime rate reduces with increase in arbortion rate and police per capitalize()
```
#Homoscedasticity
par(mfrow=c(2,2))
plot(crimerate.abortionrate.lm)
par(mfrow=c(1,1))

#Visualize the results with a graph

#Plot the data points on a graph


crimerate.graph<-ggplot(df_arbortion, aes(x=abortionrate, y=crimerate))+
  geom_point()
crimerate.graph

#Add the linear regression line to the plotted data


crimerate.graph <- crimerate.graph + geom_smooth(method="lm", col="black")

crimerate.graph

#Add the equation for the regression line.


crimerate.graph <- crimerate.graph +
  stat_regline_equation(label.x = 8, label.y = 12)

crimerate.graph

#Make the graph ready for publication
crimerate.graph +
  theme_bw() +
  labs(title = "Reported crimerate as a function of unemployment",
       x = "Income (x$10,000)",
       y = "Happiness score (0 to 10)")


```

Conclusions

The null hypothesis was to test whether the abortion that was done between 1990 -2000 for each state can affect crime rate for that state from 2010- 2020
From the results it was seen that the model is positive. The p-value is greater than the significance level thus implying that the reject our null hypothesis
This was probably due to the fact that the study had very limited data and could not properly predict the desired outcome.
The model showed that for every 28 abortion rates causes a unit increase in crime rate without any control variable. This was the opposite of the null hypothesis which was to test if an abortion done will reduce the crime rate 20 years after. 

When control variable unemployment was introduced, it increased the abortion rate from unit 28 to unit 57 per unit increase in crime rate
It can be deduced from the model that when unemployment increases by 920 it increases unit crime rate
This might be true since it is a global phenomenon that unemployment causes crime rate to increase

When another control variable police per capita was added, the changes were seen in our model.
The relationship of crime rate and abortion rate become what the null hypothesis intuited. There was a reduction of crime rate for every 1.3 abortion rate done

This might be due to the fact that the more police per capita cause reduction in crime in general. It can be seen that for every increase in police per capita there is a unit reduction of crime rate


Discuss how this research can be improved for future studies 

The missing values especially for abortion rate was a limiting factor for the research. We can improve the research by using well curated data with few null values.

The research could use more features to test multiple colinearity and observation interdependency.




