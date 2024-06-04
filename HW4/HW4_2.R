Business = data.frame(
  Admit = c(480, 180),
  Deny = c(120, 20),
  row.names = c("Male", "Female")
)
#        Admit Deny
# Male     480  120
# Female   180   20
Law = data.frame(
  Admit = c(10, 100),
  Deny = c(90, 200),
  row.names = c("Male", "Female")
)
#        Admit Deny
# Male      10  100
# Female    90  200

# a.
data = data.frame(
  Admit = Business$Admit + Law$Admit,
  Deny = Business$Deny + Law$Deny,
  row.names = c("Male", "Female")
)

# b.
rowsums = apply(data, 1, sum)
closums = apply(data, 2, sum)
total = sum(data)

male_admit = data[1, 1] / rowsums[1]
female_admit = data[2, 1] / rowsums[2]
cat("Male/Female admit rate: ", male_admit*100, "% /", female_admit*100, "%")

# c.
Business_rowsums = apply(Business, 1, sum)
Business_closums = apply(Business, 2, sum)
Business_total = sum(Business)

Law_rowsums = apply(Law, 1, sum)
Law_closums = apply(Law, 2, sum)
Law_total = sum(Law)

Business_male_admit = Business[1, 1] / Business_rowsums[1]
Business_female_admit = Business[2, 1] / Business_rowsums[2]

Law_male_admit = Law[1, 1] / Law_rowsums[1]
Law_female_admit = Law[2, 1] / Law_rowsums[2]

cat("Business Male/Female admit rate: ", Business_male_admit*100, "% /", Business_female_admit*100, "%")
cat("Law Male/Female admit rate: ", Law_male_admit*100, "% /", Law_female_admit*100, "%")

