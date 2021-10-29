library(tidyverse)
male <- read_csv(file.choose())
male %>% ggplot(aes(x = Mass, y = Tcell)) + geom_point() + geom_smooth(method = 'lm', level = .99)
?geom_smooth
fit <- lm(Tcell~Mass, data = male)
summary(fit)
preds <- predict(fit, interval = 'prediction', level = 0.99)
male2 <- cbind(male, preds)
male2 %>% ggplot(aes(x = Mass, y = Tcell)) + geom_point() + 
  geom_line(aes(y = lwr), color = "red", linetype = "dashed") + 
  geom_line(aes(y = upr), color = "red", linetype = "dashed") + 
  geom_smooth(method = 'lm', level = 0.99) + theme_bw() + ggtitle("99% Confidence and Prediction Intervals of Regression Line")
crickets <- read_csv(file.choose())
crickets_fit <- lm(temp~chirps, data = crickets)
summary(crickets_fit)
predict(crickets_fit, interval = 'prediction', level = 0.95)
confint(crickets_fit)
anova(crickets_fit)
df <- data.frame(temp = NA, chirps = 1188)
cricket_pred <- predict(crickets_fit, newdata = df, interval = 'prediction')
cricket_preds <- predict(crickets_fit, interval = 'prediction', level = 0.95)
crickets2 <- cbind(crickets, cricket_preds)
crickets2 %>% ggplot(aes(x = chirps, y = temp)) + geom_point() +
  geom_line(aes(y = lwr), color = "red", linetype = 'dashed') +
  geom_line(aes(y = upr), color = "red", linetype = 'dashed') +
  geom_smooth(method = 'lm', level = 0.95) + theme_bw() + ggtitle("99% Confidence and Prediction Intervals")
head(crickets)
