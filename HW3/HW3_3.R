Cotinine_data = read.csv('Cotinine.csv', header = TRUE)
head(Cotinine_data)
#   Gender  Race cotinine
# 1   Male White      210
# 2   Male White      300
# 3   Male White      150
# 4   Male White      325
# 5   Male White      100
# 6   Male Black      245
Cotinine_df = data.frame(Cotinine_data)
attach(Cotinine_df)

# a.
# calculate 4 cell means
cell_means = aggregate(cotinine, by = list(Gender, Race), mean)
names(cell_means) = c("Gender", "Race", "cotinine")

# plot cell means
library(ggplot2)

ggplot(cell_means, aes(x = factor(Gender), y = cotinine, color = factor(Race))) +
  geom_point(size = 3, shape = 21, fill = "white") +
  geom_line(aes(group = Race), linewidth = 1) +
  scale_color_manual(values = c("red", "blue")) +
  labs(x = "Gender", y = "cotinine(milligrams per milliliter)") +
  theme_classic()

# b.
model = aov(cotinine ~ Gender * Race, data = Cotinine_df)
summary(model)
#             Df Sum Sq Mean Sq F value Pr(>F)
# Gender       1   1248    1248   0.204  0.657
# Race         1  13005   13005   2.129  0.164
# Gender:Race  1   2554    2554   0.418  0.527
# Residuals   16  97731    6108     

# c.
model_full = aov(cotinine ~ Gender + Race, data = Cotinine_df)
summary(model_full)
