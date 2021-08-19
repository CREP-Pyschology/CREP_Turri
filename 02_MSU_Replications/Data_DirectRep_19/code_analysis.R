master <- read.csv("~/OneDrive - Harrisburg University/Research/2 projects/PSA-projects/CREP_Turri 004/Data Direct Rep/CREP_Turri.csv")

tapply(master$age, #dv
       list(master$para_condition, master$scale_condition), #iv
       mean, na.rm = T)
tapply(master$age, #dv
       list(master$para_condition, master$scale_condition), #iv
       sd, na.rm = T)
tapply(master$age, #dv
       list(master$para_condition, master$scale_condition), #iv
       length)

table(master$race_ethnic, master$para_condition, master$scale_condition)/
  nrow(master) * 100 

table(master$gender, master$para_condition, master$scale_condition)/
  nrow(master) * 100 

table(master$squirrel_prairie, master$para_condition)
#gettier 20/25
#ignorance 19/24
#knowledge 23/24  

master$knows_believes = factor(master$knows_believes, #column
                               levels = c("Believes", "Knows"))
master$reason_unreason = factor(master$reason_unreason, 
                                levels = c("Reasonable",
                                           "Unreasonable"))

table(master$knows_believes, master$para_condition)
#gettier 11/19
#ignorance 3/11
#knowledge 6/9

table(master$reason_unreason, master$para_condition)
#gettier 18/19
#ignorance 10/11
#knowledge 9/9

chisq.test(master$knows_believes, 
           master$para_condition)
sqrt(3.7232/sum(table(master$knows_believes)))

chisq.test(master$reason_unreason, 
           master$para_condition)
sqrt(0.84223/sum(table(master$reason_unreason)))

sqrt(4.49/135)

model = lm(knows_believesVAS ~ para_condition, 
           data = master)
summary.aov(model)
summary(model)$r.squared

tapply(master$knows_believesVAS, 
       master$para_condition, 
       mean, na.rm = T)
tapply(master$knows_believesVAS, 
       master$para_condition,
       sd, na.rm = T)

model = lm(reason_unreasonVAS ~ para_condition, 
           data = master)
summary.aov(model)
summary(model)$r.squared

tapply(master$reason_unreasonVAS, 
       master$para_condition, 
       mean, na.rm = T)
tapply(master$reason_unreasonVAS, 
       master$para_condition,
       sd, na.rm = T)
