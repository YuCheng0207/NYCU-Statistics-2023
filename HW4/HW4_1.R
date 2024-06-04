library(dplyr)
library(ggplot2)
data = data.frame(
  Female = c(68, 91, 5, 61),
  Male = c(56, 40, 6, 59),
  row.names = c("Accounting", "Administration", "Economics", "Finance")
)
data
#                Female Male
# Accounting         68   56
# Administration     91   40
# Economics           5    6
# Finance            61   59

attach(data)
# a.
data_test = chisq.test(data)
print(data_test)

# b.
data_expected = chisq.test(data)$expected
print(data_expected)

# c.
data_plot = data.frame(
  Gender = c(rep("Female", 4), rep("Male", 4)),
  major = c(rep(c("Accounting", "Administration", "Economics", "Finance"), 2)),
  value = c(68, 91, 5, 61, 56, 40, 6, 59)
)
data_plot = data_plot %>%
  group_by(Gender) %>%
  mutate(prop = value/sum(value))

ggplot(data = data_plot, aes(x = major, y = prop, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Study of the career plans",
       x = "Major", y = "Proportion of Respondents",
       fill = "Gender") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# reshape
data_table = reshape(data_plot, idvar = "major", timevar = "Gender", direction = "wide")
data_table = data_table[, c("Female", "Male")]

# d.
result = data_test$observed - data_test$expected
result

# e.
respondents = sum(rowSums(data))
total = 722
percent_nonrespondents = ( 1 - respondents / total ) * 100
paste("Percent of nonrespondents: ", percent_nonrespondents, "%")
