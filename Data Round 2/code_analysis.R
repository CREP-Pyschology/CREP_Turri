library(rio)
master <- import("~/GitHub/CREP_Turri 004/Data Round 2/USA_159.csv",
                 stringsAsFactors = F)

#descriptive statistics
#gender
table(master$DD02, master$isCorrect) / nrow(master) * 100

#ethnicity
table(master$DD05, master$isCorrect) / nrow(master) * 100

#remake this data as useful
#vd02_01 slider for believes/knows
#vd04_01 slider for reasonable/unreasonable
#vd06_01 slider for inability goodluck/badluck
smaller_data <- master[ , c("VD02_01", "VD04_01", "VD06_01",
                            "VE02_01", "VE04_01", "VE06_01",
                            "VG02_01", "VG04_01", "VG06_01",
                            "isCorrect", "Name_order", "Condition_order",
                            "CASE")]
smaller_data <- subset(smaller_data,
                       isCorrect == 1)

darrel <- smaller_data[ , c("VD02_01", "VD04_01", "VD06_01", "CASE")]
darrel$order <- unlist(lapply(smaller_data$Name_order, function(x){ gregexpr("D", x)[[1]][1]}))
darrel$condition <- substr(smaller_data$Condition_order, darrel$order, darrel$order)

emma <- smaller_data[ , c("VE02_01", "VE04_01", "VE06_01", "CASE")]
emma$order <- unlist(lapply(smaller_data$Name_order, function(x){ gregexpr("E", x)[[1]][1]}))
emma$condition <- substr(smaller_data$Condition_order, emma$order, emma$order)

gerald <- smaller_data[ , c("VG02_01", "VG04_01", "VG06_01", "CASE")]
gerald$order <- unlist(lapply(smaller_data$Name_order, function(x){ gregexpr("G", x)[[1]][1]}))
gerald$condition <- substr(smaller_data$Condition_order, gerald$order, gerald$order)

colnames(darrel) <- colnames(emma) <- colnames(gerald) <- c(
  "question2", "question4", "question6", "CASE", "order", "condition"
)

final_data <- rbind(darrel, emma, gerald)

#knows and believes
library(ez)
ezANOVA(data = final_data,
        dv = question2,
        wid = CASE,
        within = condition, 
        type = 3,
        detailed = T)

tapply(final_data$question2, final_data$condition, mean)
tapply(final_data$question2, final_data$condition, sd)

pairwise.t.test(final_data$question2, 
                final_data$condition,
                paired = T,
                p.adjust.method = "bonferroni")

#reasonable unreasonable
ezANOVA(data = final_data,
        dv = question4,
        wid = CASE,
        within = condition, 
        type = 3,
        detailed = T)

tapply(final_data$question4, final_data$condition, mean)
tapply(final_data$question4, final_data$condition, sd)

# pairwise.t.test(final_data$question4, 
#                 final_data$condition,
#                 paired = T,
#                 p.adjust.method = "bonferroni")

#inability goodluck/badluck
ezANOVA(data = final_data,
        dv = question6,
        wid = CASE,
        within = condition, 
        type = 3,
        detailed = T)

tapply(final_data$question6, final_data$condition, mean)
tapply(final_data$question6, final_data$condition, sd)

pairwise.t.test(final_data$question6, 
                final_data$condition,
                paired = T,
                p.adjust.method = "bonferroni")
