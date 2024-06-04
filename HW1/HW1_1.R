# a. 
Percent_returning <- c(74, 66, 81, 52, 73, 62, 52, 45, 62, 46, 60, 46, 38)
New_adults <- c(5, 6, 8, 11, 12, 15, 16, 17, 18, 18, 19, 20, 20)
Bird_colonise <- data.frame(Percent_returning, New_adults)
plot(Percent_returning, New_adults, xlab = "Percent returning", ylab = "New adults", pch=19)
boxplot(Percent_returning, xlab = "Percent returning")
boxplot(New_adults, xlab = "New adults")
# b.
Percent_returning -> x
New_adults -> y
x_mean <- mean(x) # X-Bar
## [1] 58.23077
y_mean <- mean(y) # Y-Bar
## [1] 14.23077
S_sub_x_square <- sum((x-mean(x))^2)/(length(x)-1)
## [1] 169.859
S_sub_y_square <- sum((y-mean(y))^2)/(length(y)-1)
## [1] 28.02564
# Pearson's correlation(r)
r <- sum((x-mean(x))*(y-mean(y)))/(sqrt(sum((x-mean(x))^2))*sqrt(sum((y-mean(y))^2)))
## [1] -0.7484673
# c.
cor(Percent_returning, New_adults, method = "pearson") # Pearson's correlation
## [1] -0.7484673
cor(Percent_returning, New_adults, method = "kendall") # Kendall's tau
## [1] -0.5960396
cor(Percent_returning, New_adults, method = "spearman") # Spearman's rho
## [1] -0.7538043
# d.
beta_estimate <- r*sqrt(S_sub_y_square)/sqrt(S_sub_x_square)
## [1] -0.3040229
alpha_estimate <- y_mean-(beta_estimate*x_mean)
## [1] 31.93426
sigma_estimate_squre <- sum((y-alpha_estimate-(beta_estimate*x))^2)/(length(x)-2)
## [1] 13.44609
# e.
R_squre <- 1 - sum((y-alpha_estimate-(beta_estimate*x))^2)/sum((y-y_mean)^2)
## [1] 0.5602033
r_squre <- r^2
## [1] 0.5602033
R_squre == r^2
## [1] FALSE ??
# f.
e_sub_i <- y - (alpha_estimate + beta_estimate*x)
y_sub_i_estimate <- alpha_estimate + beta_estimate*x
plot(y_sub_i_estimate, e_sub_i, xlab = "y_sub_i_estimate", ylab = "e_sub_i", pch = 19)
# fit regression mode
model <- lm(New_adults ~ Percent_returning)
summary(model)
# Call:
#   lm(formula = New_adults ~ Percent_returning)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -5.8687 -1.2532  0.0508  2.0508  5.3071 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       31.93426    4.83762   6.601 3.86e-05 ***
#   Percent_returning -0.30402    0.08122  -3.743  0.00325 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 3.667 on 11 degrees of freedom
# Multiple R-squared:  0.5602,	Adjusted R-squared:  0.5202 
# F-statistic: 14.01 on 1 and 11 DF,  p-value: 0.003248
residuals(model)
# 1           2           3           4           5           6           7 
# -4.43656125 -5.86874481  0.69159937 -5.12506604  2.25941580  1.91516341 -0.12506604 
# 8           9          10          11          12          13 
# -1.25322666  4.91516341  0.05079629  5.30711752  2.05079629 -0.38138727
par(mfrow = c(2,2))
plot(model)
