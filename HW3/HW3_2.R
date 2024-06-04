fern_data = read.csv('fern.csv', header = TRUE)
head(fern_data)
#   wave_light Block_age Response_area
# 1      420nm     young        1017.0
# 2      460nm     young         929.0
# 3      600nm     young         939.8
# 4      720nm     young        1081.5
# 5      420nm       old         854.7
# 6      460nm       old         689.9
fern_df = data.frame(fern_data)
attach(fern_df)

# a.b.
model = aov(Response_area ~ factor(Block_age) + factor(wave_light), data = fern_df)
summary(model)
#                    Df Sum Sq Mean Sq F value Pr(>F)  
# factor(Block_age)   1  76793   76793  22.697 0.0176 *
# factor(wave_light)  3  21954    7318   2.163 0.2713  
# Residuals           3  10150    3383                 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

