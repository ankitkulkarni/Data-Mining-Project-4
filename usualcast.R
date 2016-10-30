library(arules)
cat("importing data\n")
ususalSuspects <- read.csv("usualsuspects.csv", header = TRUE, na.strings=c(" ", ""))
ususalSuspects = na.omit(ususalSuspects)
cat("\n")
cat("mining all rules where Support = 0.0007 and Confidence = 1\n")
rules <- apriori(ususalSuspects, parameter = list(support = 0.0005, confidence = 1))
cat("\n")
cat("sorting by lift\n")
rules_sorted = sort(rules, by="lift")
inspect(rules_sorted)
subset.matrix <- is.subset(rules_sorted, rules_sorted)
subset.matrix[lower.tri(subset.matrix, diag = TRUE)] <- NA
red_rules <- colSums(subset.matrix, na.rm = TRUE) >= 1
pruned_rules <- rules_sorted[!red_rules]
inspect(pruned_rules)
