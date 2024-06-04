wastewater_data = read.csv('wastewater.csv', header = TRUE)
head(wastewater_data)
#     AF   FS  FCC
# 1 34.6 38.8 26.7
# 2 35.1 39.0 26.7
# 3 35.3 40.1 27.0
# 4 35.8 40.9 27.1
# 5 36.1 41.0 27.5
# 6 36.5 43.2 28.1
wastewater_df = data.frame(wastewater_data)
attach(wastewater_df)

data = data.frame(
  Method = rep(c("AF", "FS", "FCC"), each = 10),
  Organic_Carbon = c(AF, FS, FCC)
)

# a.
model = aov(Organic_Carbon ~ Method, data = data)
summary(model)
#             Df Sum Sq Mean Sq F value   Pr(>F)    
# Method       2 1251.5   625.8   60.63 1.03e-10 ***
# Residuals   27  278.7    10.3                     
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# b.
library(ggplot2)
ggplot(data, aes(x = Method, y = Organic_Carbon)) +
  geom_boxplot() +
  labs(x = "Treatment Method", y = "Organic Carbon")