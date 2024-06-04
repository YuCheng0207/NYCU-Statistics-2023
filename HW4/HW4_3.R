data = data.frame(
  Probability = c(0.32, 0.41, 0.2, 0.07),
  Count = c(22, 38, 20, 11),
  row.names = c("A", "B", "C", "D/F")
)
#     Probability Count
# A          0.32    22
# B          0.41    38
# C          0.20    20
# D/F        0.07    11

attach(data)

# a.
total_student = apply(data, 2, sum)[2]
professor_probability = Count / total_student * 100
cat("Professor's percents A:", professor_probability[1],
    "% B:", professor_probability[2], "% C:", professor_probability[3],
    "% D/F:", professor_probability[4], "%")

barplot(rbind(Probability, professor_probability/100), beside = TRUE,
        names.arg = c("A", "B", "C", "D/F"), 
        ylim = c(0,0.5), col = c("blue", "orange"),
        main = "Comparison of TA and Professor's Grade Distributions",
        xlab = "Grades", ylab = "Percentage")
legend("topright", c("TA", "Professor"), fill = c("blue", "orange"))


# b.
expected_count = Probability * total_student
expected_count

# c.
chisq.test(Count, p=Probability)
