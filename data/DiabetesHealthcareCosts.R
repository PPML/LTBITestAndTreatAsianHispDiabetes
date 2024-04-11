diabetesHealthCosts <- read.csv("DiabetesHealthCareCosts.csv")

colnames(diabetesHealthCosts) <- c("Age", "Mean", "Lower bound", "Upper bound")

range(diabetesHealthCosts$Mean)

range(diabetesHealthCosts$`Lower bound`)

range(diabetesHealthCosts$`Upper bound`)
