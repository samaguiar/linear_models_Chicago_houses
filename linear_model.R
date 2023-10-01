#import packages
library(ggplot2)
library(arm)

#set path to working directory
setwd("~/Desktop/university-of-the-cumberlands/R/hw5")

#create data set
houseData <- read.table("Houses.txt", header = TRUE)

#set variables
y <- houseData$price
x1 <- houseData$taxes
x2 <- houseData$new

summary(houseData)
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

#print the summary of the linear model to determine fit
print(summary(m))

#calculate residuals
residuals <- residuals(m)

# Create a residual plot against fitted values
plot(m$fitted.values, residuals, xlab = 'Fitted Values', ylab = 'Residuals',
     main = 'Residuals vs. Fitted Values')
# Add a horizontal line at y = 0
abline(h = 0, col = 'red')  

#identity link function - normal GLM
normal_glm <- glm(y ~ x1 + x2, data = houseData, family = gaussian())
print(summary(normal_glm))

#identity link function - gamma GLM
gamma_glm <- glm(y ~ x1 + x2, data = houseData, family = Gamma())
print(summary(gamma_glm))

# Make predictions from both models and add them to the original dataset
houseData$Normal_Predicted <- predict(normal_glm, newdata = houseData, type = "response")
houseData$Gamma_Predicted <- predict(gamma_glm, newdata = houseData, type = "response")

# Create a scatter plot
scatter_plot <- ggplot(houseData, aes(x = y, y = Normal_Predicted)) +
  geom_point(aes(color = "Normal Model"), alpha = 0.5) +
  geom_point(aes(x = y, y = Gamma_Predicted, color = "Gamma Model"), alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "black", linetype = "dashed") + 
  xlab("Actual Selling Price") +
  ylab("Predicted Selling Price") +
  scale_color_manual(values = c("Normal Model" = "blue", "Gamma Model" = "red")) +
  theme_minimal()  

# Display the scatter plot
print(scatter_plot)

#binned plot for M
binnedM <- binnedplot(predict(m, type = "response"),
                           residuals(m, type = "response"))
print(binnedNormal)

#binned plot for normal
binnedNormal <- binnedplot(predict(normal_glm , type = "response"),
           residuals(normal_glm , type = "response"))
print(binnedNormal)

#binned plot for Gamma
binnedGamma <- binnedplot(predict(gamma_glm , type = "response"),
                           residuals(gamma_glm , type = "response"))
print(binnedNormal)

print(anova(normal_glm, gamma_glm, test = "LRT"))



