library(mice)
library(stargazer)
library(tidyverse)
wages <- read_csv("wages.csv")
wages1 <- wages %>% drop_na(hgc, tenure)
View(wages1)

wages2 <- wages1 %>% drop_na(logwage)
linear1 <- lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married, data=wages2)
stargazer(linear1)

mean_wage <- wages1
mean_wage$logwage[is.na(mean_wage$logwage)] <- mean(mean_wage$logwage, na.rm = TRUE) 
linear2 <- lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married, data=mean_wage)
stargazer(linear2)

wages1$logwage_pred <- wages1$logwage
test <- lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married, data=wages1, na.action=na.exclude)
wages1$preds <- NA
wages1$preds [!is.na(wages1$hgc) & !is.na(wages1$tenure)] <- predict(test, wages1, na.action=na.exclude)
wages1$logwage_pred[is.na(wages1$logwage)] <- wages1%preds[is.na(wages1$logwage)]

head(wages1)
wages1.imp = mice(wages1, seed = 12345, m = 20)
summary(wages1.imp)
fit = with(wages1.imp, lm(logwage ~ hgc + college + tenure + age + married))
round(summary(pool(fit)),2)

stargazer(linear1, linear2, test)
