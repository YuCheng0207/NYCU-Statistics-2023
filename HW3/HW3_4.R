mood_data = read.csv('mood.csv', header = TRUE)
head(mood_data)
#   Participant MusicType Gender MoodScore
# 1           1       Pop Female  6.196393
# 2           2       Pop Female  6.415426
# 3           3       Pop Female  7.847855
# 4           4      Jazz Female  6.818518
# 5           5       Pop   Male  7.925675
# 6           6      Jazz   Male  7.888698
mood_df = data.frame(mood_data)
attach(mood_df)

# a.
# calculate 6 cell means
cell_means = aggregate(MoodScore, by = list(MusicType, Gender), mean)
names(cell_means) = c("MusicType", "Gender", "MoodScore")

# plot cell means
library(ggplot2)

ggplot(cell_means, aes(x = factor(MusicType), y = MoodScore, color = factor(Gender))) +
  geom_point(size = 3, shape = 21, fill = "white") +
  geom_line(aes(group = Gender), linewidth = 1) +
  scale_color_manual(values = c("red", "blue")) +
  labs(x = "MusicType", y = "MoodScore") +
  theme_classic()

# b.
model = aov(MoodScore ~ MusicType * Gender, data = mood_df)
summary(model)
#                  Df Sum Sq Mean Sq F value  Pr(>F)   
# MusicType         2  10.60   5.300   7.494 0.00134 **
# Gender            1   0.01   0.013   0.018 0.89295   
# MusicType:Gender  2   0.12   0.060   0.085 0.91868   
# Residuals        54  38.19   0.707                   
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# c.
model_full = aov(MoodScore ~ MusicType + Gender, data = mood_df)
summary(model_full)
#             Df Sum Sq Mean Sq F value  Pr(>F)   
# MusicType    2  10.60   5.300   7.747 0.00107 **
# Gender       1   0.01   0.013   0.019 0.89115   
# Residuals   56  38.31   0.684                   
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
model_reduced = aov(MoodScore ~ MusicType, data = mood_df)
summary(model_reduced)
#             Df Sum Sq Mean Sq F value  Pr(>F)    
# MusicType    2  10.60   5.300   7.883 0.00095 ***
# Residuals   57  38.32   0.672                    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1