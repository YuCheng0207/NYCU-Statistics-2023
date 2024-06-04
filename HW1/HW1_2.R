# Preprocess
Milk <- c(27.6, 26.7, 25.7, 23.9, 23.0, 22.9, 23.3)
Soda <- c(35.1, 35.7, 46.2, 47.4, 47.9, 49.7, 49.3)
Milk_or_Soda <- data.frame(Milk, Soda)
Soda -> x
Milk -> y
# a.
plot(Soda, Milk, xlab = "Soda", ylab = "Milk", pch=19)
boxplot(Soda, xlab = "Soda")
boxplot(Milk, xlab = "Milk")
# b.
cor(Soda, Milk, method = "pearson") # Pearson's correlation
## [1] -0.9262881
cor(Soda, Milk, method = "kendall") # Kendall's tau
## [1] -0.9047619
cor(Soda, Milk, method = "spearman") # Spearman's rho
## [1] -0.9642857
# c.
model <- lm(Milk ~ Soda)
summary(model)
# Call:
#   lm(formula = Milk ~ Soda)
# 
# Residuals:
#   1         2         3         4         5         6         7 
# 0.228245 -0.502526  1.458967 -0.002577 -0.761553 -0.353869 -0.066687 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 37.27160    2.30151  16.194 1.64e-05 ***
#   Soda        -0.28205    0.05131  -5.497  0.00272 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.7928 on 5 degrees of freedom
# Multiple R-squared:  0.858,	Adjusted R-squared:  0.8296 
# F-statistic: 30.21 on 1 and 5 DF,  p-value: 0.002722
# d.
residuals(model)
# 1            2            3            4            5            6            7 
# 0.228245389 -0.502526419  1.458966933 -0.002576684 -0.761553191 -0.353868617 -0.066687411
par(mfrow = c(2,2))
plot(model)