# linear_models_Chicago_houses
### The Data ###
The data was obtain from 
### Scatter Plot: Selling Price vs. Tax Bill ###
To create a scatter plot of Selling Price vs. the Tax Bill for Chicago Houses, I used the following code: 
```
#import packages
library(ggplot2)

#set path to working directory
setwd("~/Desktop/university-of-the-cumberlands/R/hw5")

#create data set
houseData <- read.table("Houses.txt", header = TRUE)

#set variables
y <- houseData$price
x1 <- houseData$taxes
x2 <- houseData$new

#create linear model
m <- lm(y ~ x1, data = houseData)

#create scatter plot with model
scatterPlot <- ggplot(houseData, aes(x1, y)) +
  geom_point() +
  geom_abline(aes(intercept = coef(m)[1], slope = coef(m)[2]),
              colour = "red")+
  xlab('tax bill (in dollars)')+
  ylab('selling price (in dollars)') +
  ggtitle("Scatter Plot of Selling Price vs. Tax Bill")
print(scatterPlot)
```
Output
<img width="494" alt="Screenshot 2023-10-06 at 12 48 12 PM" src="https://github.com/samaguiar/linear_models_Chicago_houses/assets/89755252/1915354c-d267-433b-afd1-1aae75349524">


The scatter plot shows a positive relationship between x1 and y, meaning that as the tax bill increases, the selling price of the house also increases. Because the data does not follow the model exactly, more investigation is needed, especially since the scatter plot has a funnel look. This could indicate heteroscedasticity (Hayes, 2020). So, I calculated a summary of the linear model by using the following code since  x1, the tax bill, is a continuous numerical variable: 
```
#print the summary of the linear model to determine fit
print(summary(m))

Output
Call:
lm(formula = y ~ x1, data = houseData)

Residuals:
    Min      1Q  Median      3Q     Max 
-239.92  -50.26    1.01   35.71  404.20 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) 35.504052  15.206753   2.335   0.0216 *  
x1           0.103486   0.006698  15.450   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 82.36 on 98 degrees of freedom
Multiple R-squared:  0.7089,	Adjusted R-squared:  0.706 
F-statistic: 238.7 on 1 and 98 DF,  p-value: < 2.2e-16
```

The adjusted R-squared values show a positive, moderate relationship between the two values as the values are greater than 0.6. With the p-value of the model being less than 0.005, this suggests the model is a good fit. However, according to Zumel, Mount, Howard, and Thomas, an indication that a model is a good fit has a residual median near zero and the 1st and 3rd quartiles are symmetric around the median (Zumel et al., 2020). Looking at the residuals from the summary output, it does not fit that criteria, indicating there is heteroscedasticity. I confirmed this by using the following code to create a plot of the residual: 
```
#calculate residuals
residuals <- residuals(m)

# Create a residual plot against fitted values
plot(m$fitted.values, residuals, xlab = 'Fitted Values', ylab = 'Residuals',
     main = 'Residuals vs. Fitted Values')
# Add a horizontal line at y = 0
abline(h = 0, col = 'red')  
```
Output
<img width="461" alt="Screenshot 2023-10-06 at 12 48 55 PM" src="https://github.com/samaguiar/linear_models_Chicago_houses/assets/89755252/4ba9c398-6a92-422c-97b9-06b60876feed">

The residual plot also shows a funnel-like shape. As residuals should be randomly distributed around the y=0 line, this plot indicates heteroscedasticity (Hayes, 2020).

### Identity Link Function: Normal GLM ###
To fit the data set to the normal GLM, I used the following code: 
```
#identity link function - normal GLM
normal_glm <- glm(y ~ x1 + x2, data = houseData, family = gaussian())
print(summary(normal_glm))

Call:
glm(formula = y ~ x1 + x2, family = gaussian(link = "identity"), 
    data = houseData)

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) 41.965143  14.695055   2.856  0.00525 ** 
x1           0.095132   0.006933  13.722  < 2e-16 ***
x2          86.200077  27.244808   3.164  0.00208 ** 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for gaussian family taken to be 6212.741)

    Null deviance: 2284086  on 99  degrees of freedom
Residual deviance:  602636  on 97  degrees of freedom
AIC: 1162.2

Number of Fisher Scoring iterations: 2
```
According to the summary, the x2 coefficient has an approximation of 86.2, with a standard error of 27.2. This means that new houses have a selling price of $86,200 higher than old houses, holding the tax bill constant. 
Identity Link Function: Gamma GLM
Similarly, to determine the Gamma GLM, I used the following code: 
```
#identity link function - gamma GLM
gamma_glm <- glm(y ~ x1 + x2, data = houseData, family = Gamma())
print(summary(gamma_glm))

Output
Call:
glm(formula = y ~ x1 + x2, family = Gamma(), data = houseData)

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)  6.802e-03  2.617e-04  25.987  < 2e-16 ***
x1          -8.955e-07  6.205e-08 -14.430  < 2e-16 ***
x2          -9.681e-04  2.585e-04  -3.745 0.000306 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for Gamma family taken to be 0.1133188)

Null deviance: 31.94  on 99  degrees of freedom
Residual deviance: 12.09  on 97  degrees of freedom
AIC: 1140.8

Number of Fisher Scoring iterations: 5
```
In this model, the x2 coefficient is negative, there is a slight decreasing relationship, but as the value is -9.681e-04, it is very close to zero. This is interpreted as there is no significance in house prices if the house is new or old, when the tax bill is held constant. 

### Model Preference ###
Using the AIC values from each model, I can determine the model preference. The Normal GLM had an AIC value of 1162.2 and the Gamma GLM had an AIC value of 1140.8. According to Staton, the lower AIC shows a better model fit (Staton, 2021). However, the AIC values do not different in an extreme way. 

To further analyze the model preference, I used the following code to create residual plots: 
```
#binned plot for normal
binnedNormal <- binnedplot(predict(normal_glm , type = "response"),
           residuals(normal_glm , type = "response"))
print(binnedNormal)

#binned plot for Gamma
binnedGamma <- binnedplot(predict(gamma_glm , type = "response"),
                           residuals(gamma_glm , type = "response"))
print(binnedNormal)

Output
[1]
<img width="526" alt="Screenshot 2023-10-06 at 12 47 19 PM" src="https://github.com/samaguiar/linear_models_Chicago_houses/assets/89755252/07a07139-baf7-4981-bd4d-4cb6785b500c">
[2]
<img width="494" alt="Screenshot 2023-10-06 at 12 48 03 PM" src="https://github.com/samaguiar/linear_models_Chicago_houses/assets/89755252/3025c446-a9e6-493f-8e2c-1230a198e2d6">
```

Next, I performed a likelihood ratio deviance test using the following code, as the residual plots did not give me enough information to determine model preference: 
```
print(anova(normal_glm, gamma_glm, test = "LRT"))

Analysis of Deviance Table

Model 1: y ~ x1 + x2
Model 2: y ~ x1 + x2
  Resid. Df Resid. Dev Df Deviance Pr(>Chi)
1        97     602636                     
2        97         12  0   602624 
```
Looking at the residual deviance of each model, the Normal GLM has a residual deviance of 602636, while the Gamma GLM has a value of 12. Since the Gamma GLM has a significantly lower residual deviance, this is the better model for the data. 

### References ###
Hayes, A. (2022, April 20). Heteroscedasticity definition: Simple meaning and types explained. Investopedia. https://www.investopedia.com/terms/h/heteroskedasticity.asp#:~:text=In%20statistics%2C%20heteroskedasticity%20(or%20heteroscedasticity,periods%2C%20are%20non%2Dconstant. 
Kenton, W. (2022, December 31). Homoskedastic: What it means in regression modeling, with example. Investopedia. https://www.investopedia.com/terms/h/homoskedastic.asp 
Staton, B. (2021). Introduction to R for natural resource scientists. https://bstaton1.github.io/au-r-workshop/ 
Zumel, N., Mount, J., Howard, J., & Thomas, R. (2020). Practical data science with R. Manning Publications Co. 
