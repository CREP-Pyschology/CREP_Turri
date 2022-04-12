library(ggplot2)
library(RColorBrewer)

threat <- rep(c("No Threat (Knowledge)", "Threat (Gettier)", "No Detection (Ignorance)"), each = 4)
choice <- rep(c("Knows", "Believes", "Reasonable", "Unreasonable"), 3)
dv <- rep(c("Knowledge", "Knowledge", "Reason", "Reason"), 3)
number <- c(81, 19, 98, 2, 
            67, 33, 94, 6,
            16, 84, 87, 13)
DF <- data.frame(threat, choice, number, dv)
DF$choice <- factor(DF$choice, 
                    levels = c("Believes", "Knows", "Unreasonable", "Reasonable"))

ggplot(DF, aes(threat, number, fill = choice)) + 
  facet_wrap(~ dv) + 
  geom_bar(stat = "identity", 
           color = "black") + 
  theme_classic() + 
  xlab("Assigned Condition") + 
  ylab("Response Rates (%)") + 
  scale_fill_brewer(palette = "Greys", name = "Choice", 
                    direction = -1) + 
  theme(text = element_text(size = 15)) + 
  coord_flip()
