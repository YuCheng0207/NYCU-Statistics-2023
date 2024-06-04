mood_data = read.csv("/Users/yucheng/Documents/統計與應用方法/HW2/mood.csv",
                     header = TRUE)
head(mood_data)
mood_df = data.frame(mood_data)
attach(mood_df)

# a.
par(mfrow=c(1,2))
boxplot(MoodScore ~ MusicType, data = mood_df, main = "Mood Scores by Music Type")
boxplot(MoodScore ~ Gender, data = mood_df, main = "Mood Scores by Gender")

# b.
model_music = lm(MoodScore ~ MusicType, data = mood_df)
model_null = lm(MoodScore ~ 1, data = mood_df)
summary(model_music)
# Call:
# lm(formula = MoodScore ~ MusicType, data = mood_df)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -1.6666 -0.4780 -0.1157  0.3584  2.0050 
# 
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     7.4903     0.1748  42.848  < 2e-16 ***
# MusicTypeJazz  -0.6948     0.2648  -2.624 0.011131 *  
# MusicTypePop   -0.9642     0.2501  -3.854 0.000297 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.8199 on 57 degrees of freedom
# Multiple R-squared:  0.2167,	Adjusted R-squared:  0.1892 
# F-statistic: 7.883 on 2 and 57 DF,  p-value: 0.0009496

summary(model_null)
# Call:
# lm(formula = MoodScore ~ 1, data = mood_df)
# 
# Residuals:
#      Min       1Q   Median       3Q      Max 
# -1.88933 -0.57837  0.01026  0.68716  1.94760 
# 
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   6.9560     0.1176   59.17   <2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.9106 on 59 degrees of freedom

anova(model_null, model_music)
# Analysis of Variance Table
# 
# Model 1: MoodScore ~ 1
# Model 2: MoodScore ~ MusicType
#   Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
# 1     59 48.920                                  
# 2     57 38.321  2    10.599 7.8828 0.0009496 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# c.
model_full = lm(MoodScore ~ MusicType + Gender, data = mood_df)
model_reduced = lm(MoodScore ~ MusicType, data = mood_df)
summary(model_full)
# Call:
# lm(formula = MoodScore ~ MusicType + Gender, data = mood_df)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.6510 -0.4725 -0.1094  0.3666  2.0205 
# 
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    7.50775    0.21735  34.542  < 2e-16 ***
# MusicTypeJazz -0.69834    0.26833  -2.603  0.01182 *  
# MusicTypePop  -0.96757    0.25353  -3.816  0.00034 ***
# GenderMale    -0.02956    0.21505  -0.137  0.89115    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.8271 on 56 degrees of freedom
# Multiple R-squared:  0.2169,	Adjusted R-squared:  0.175 
# F-statistic: 5.171 on 3 and 56 DF,  p-value: 0.003172
summary(model_reduced)
# Call:
# lm(formula = MoodScore ~ MusicType, data = mood_df)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -1.6666 -0.4780 -0.1157  0.3584  2.0050 
# 
# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     7.4903     0.1748  42.848  < 2e-16 ***
# MusicTypeJazz  -0.6948     0.2648  -2.624 0.011131 *  
# MusicTypePop   -0.9642     0.2501  -3.854 0.000297 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.8199 on 57 degrees of freedom
# Multiple R-squared:  0.2167,	Adjusted R-squared:  0.1892 
# F-statistic: 7.883 on 2 and 57 DF,  p-value: 0.0009496

anova(model_reduced, model_full)
# Analysis of Variance Table
# 
# Model 1: MoodScore ~ MusicType
# Model 2: MoodScore ~ MusicType + Gender
#   Res.Df    RSS Df Sum of Sq      F Pr(>F)
# 1     57 38.321                           
# 2     56 38.308  1  0.012927 0.0189 0.8912

# d.
model_interaction = lm(MoodScore ~ MusicType * Gender, data = mood_df)
summary(model_interaction)
# Call:
# lm(formula = MoodScore ~ MusicType * Gender, data = mood_df)
# 
# Residuals:
#      Min       1Q   Median       3Q      Max 
# -1.67484 -0.49535 -0.07491  0.39122  1.97658 
# 
# Coefficients:
#                           Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                7.5778     0.2803  27.033  < 2e-16 ***
# MusicTypeJazz             -0.7896     0.3964  -1.992  0.05146 .  
# MusicTypePop              -1.0775     0.3780  -2.851  0.00616 ** 
# GenderMale                -0.1481     0.3647  -0.406  0.68629    
# MusicTypeJazz:GenderMale   0.1636     0.5477   0.299  0.76630    
# MusicTypePop:GenderMale    0.2024     0.5177   0.391  0.69736    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.8409 on 54 degrees of freedom
# Multiple R-squared:  0.2194,	Adjusted R-squared:  0.1471 
# F-statistic: 3.035 on 5 and 54 DF,  p-value: 0.01739

dummy_musicType = model.matrix( ~ MusicType - 1, data = mood_df)
dummy_gender = model.matrix( ~ Gender - 1, data = mood_df)
t.test(dummy_musicType, dummy_gender, paired = TRUE)

factor_musicType = factor(MusicType)
factor_gender = factor(Gender)
num_musicType = as.numeric(factor_musicType)
num_gender = as.numeric(factor_gender)




