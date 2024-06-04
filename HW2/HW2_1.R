pizza_data = read.table("/Users/yucheng/Documents/統計與應用方法/HW2/pizza2.txt",
                        header = TRUE,
                        row.names = NULL,
                        dec = " ")
head(pizza_data)
pizza_df = data.frame(pizza_data)
attach(pizza_df)
pizza_df$rating = as.double(pizza_df$rating)
pizza_df$cost = as.double(pizza_df$cost)
# I.

# a.
# coal mean and std
mean_coal = mean(pizza_df[pizza_df[,"heat"] == "Coal", "rating"])
std_coal = sd(pizza_df[pizza_df[,"heat"] == "Coal", "rating"])
# gas mean and std
mean_gas = mean(pizza_df[pizza_df[,"heat"] == "Gas", "rating"])
std_gas = sd(pizza_df[pizza_df[,"heat"] == "Gas", "rating"])
# wood mean and std
mean_wood = mean(pizza_df[pizza_df[,"heat"] == "Wood", "rating"])
std_wood = sd(pizza_df[pizza_df[,"heat"] == "Wood", "rating"])

# b.
anova_model = aov(rating ~ heat, data = pizza_df)
summary(anova_model)
#              Df Sum Sq Mean Sq F value   Pr(>F)    
# heat          2     58  29.022   9.875 8.18e-05 ***
# Residuals   197    579   2.939                     
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# c.
lm_fit = lm(rating ~ heat, data = pizza_df)
summary(lm_fit)
# Call:
#   lm(formula = rating ~ heat, data = pizza_df)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -3.506 -1.715  0.379  1.562  2.039 
# 
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   4.6888     0.4158  11.277  < 2e-16 ***
# heatGas      -1.7278     0.4376  -3.948 0.000109 ***
# heatWood     -0.8124     0.5389  -1.507 0.133289    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 1.714 on 197 degrees of freedom
# Multiple R-squared:  0.09112,	Adjusted R-squared:  0.08189 
# F-statistic: 9.875 on 2 and 197 DF,  p-value: 8.184e-05

# II.

# e.
lm_heat = lm(rating ~ heat + area + cost, data = pizza_df)
summary(lm_heat)
# Call:
# lm(formula = rating ~ heat + area + cost, data = pizza_df)
# 
# Residuals:
#      Min       1Q   Median       3Q      Max 
# -1.98864 -0.52516  0.00599  0.51428  1.92332 
# 
# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      0.72260    0.34461   2.097  0.03731 *  
# heatGas         -1.59555    0.20526  -7.773 4.52e-13 ***
# heatWood        -0.45753    0.26056  -1.756  0.08069 .  
# areaEVillage     4.17970    0.24628  16.971  < 2e-16 ***
# areaLES          2.37294    0.26106   9.089  < 2e-16 ***
# areaLittleItaly  0.78700    0.25268   3.115  0.00212 ** 
# areaSoHo         3.65362    0.24498  14.914  < 2e-16 ***
# cost             0.43865    0.06613   6.633 3.26e-10 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.7957 on 192 degrees of freedom
# Multiple R-squared:  0.8092,	Adjusted R-squared:  0.8022 
# F-statistic: 116.3 on 7 and 192 DF,  p-value: < 2.2e-16

# f.
lm_heat_re = lm(rating ~ heat_re + area + cost, data = pizza_df)
summary(lm_heat_re)
# Call:
# lm(formula = rating ~ heat_re + area + cost, data = pizza_df)
# 
# Residuals:
#      Min       1Q   Median       3Q      Max 
# -1.97759 -0.51011 -0.02969  0.52497  2.15583 
# 
# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      0.96212    0.31668   3.038  0.00271 ** 
# heat_re         -0.87601    0.09242  -9.479  < 2e-16 ***
# areaEVillage     4.10646    0.24378  16.845  < 2e-16 ***
# areaLES          2.26091    0.25405   8.900 4.08e-16 ***
# areaLittleItaly  0.69163    0.24774   2.792  0.00577 ** 
# areaSoHo         3.54383    0.23768  14.910  < 2e-16 ***
# cost             0.44911    0.06618   6.786 1.38e-10 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.7997 on 193 degrees of freedom
# Multiple R-squared:  0.8062,	Adjusted R-squared:  0.8002 
# F-statistic: 133.8 on 6 and 193 DF,  p-value: < 2.2e-16

# predict
pred_lm_heat = predict(lm_heat, data.frame(heat = "Coal",
                                      area = "LittleItaly",
                                      cost = 2.50), interval = "prediction")
pred_lm_heat_re = predict(lm_heat_re, data.frame(heat_re = 0,
                                                 area = "LittleItaly",
                                                 cost = 2.50), interval = "prediction")
pred_lm_heat
#        fit       lwr      upr
# 1 2.606232 0.9747882 4.237676
pred_lm_heat_re
#        fit     lwr      upr
# 1 2.776521 1.14876 4.404281

# III.
# t-based confidence interval
area_summary <- pizza_df %>% 
  group_by(area) %>% 
  summarize(mean_rating = mean(rating), 
            se_rating = sd(rating)/sqrt(n()), 
            lower = mean_rating - qt(0.975, n()-1)*se_rating, 
            upper = mean_rating + qt(0.975, n()-1)*se_rating)

ggplot(area_summary, aes(x=area, y=mean_rating)) +
  geom_point(size=3) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=0.7) +
  coord_cartesian(ylim=c(0, 5)) +
  labs(x="Area", y="Mean Rating") +
  theme_bw()

