library(tidyverse)
male <- read_csv(file.choose())
male %>% ggplot(aes(x = Mass, y = Tcell)) + geom_point() + geom_smooth(method = 'lm', level = .99)
?geom_smooth
fit <- lm(Tcell~Mass, data = male)
confint(fit, level = 0.99)
summary(fit)
preds <- predict(fit, interval = 'prediction', level = 0.99)
predict(fit, newdata = data.frame(Mass = 4), interval = 'confidence', level = 0.99)
predict(fit, newdata = data.frame(Mass = 4.5, Tcell = NA), interval = 'prediction', level = 0.99)
male2 <- cbind(male, preds)
male2 %>% ggplot(aes(x = Mass, y = Tcell)) + geom_point() + 
  geom_line(aes(y = lwr), color = "red", linetype = "dashed") + 
  geom_line(aes(y = upr), color = "red", linetype = "dashed") + 
  geom_smooth(method = 'lm', level = 0.99) + 
  stat_regline_equation() + theme_bw() + ggtitle("99% Confidence and Prediction Intervals of Regression Line")
library(ggpubr)
male3 <- cbind(male, cal)
male %>%  ggplot(aes(x = Mass, y = Tcell)) + geom_point() +
  geom_line(aes(x = -0.785), color = "red", linetype = "dashed") +
  geom_line(aes(x = 13.7345), color = "red", linetype = "dashed") +
  geom_smooth(method = 'lm', level = 0.99) + theme_bw() + ggtitle("Regression with Calibration Interval for Individual T-Cell Response = 0.3")



calibrate

display <- male %>% mutate(sresids = rstudent(fit))
ggplot(display, aes(sresids)) + 
  geom_histogram(aes(y = ..density..), bins = 7, color = "gray30", fill = "gray70") +
  stat_function(fun = dnorm, args = list(mean = mean(display$sresids, sd = sd(display$sresids)))) + 
  theme_bw() + ggtitle("Histogram of Residuals with Normal Distribution Overlay") + labs(x = "Residuals", y = "Density")
dim(male)
qt(1-(.01/2), 19)
max(male$Mass)
cal <- calibrate(fit, y0 = 0.3, interval = 'Wald', mean.response = TRUE, level = 0.99, limit = FALSE)
data.frame(cal)
preds
broom <- augment(fit)
broom %>% ggplot(aes(x = .fitted, y = .resid)) + geom_point() + 
  geom_line(y = 0) + theme_bw() + ggtitle("Residual Plot")

ggplot(broom, aes(x = .resid, y = .fitted)) + geom_histogram(stat = 'identity') + stat_function(fun = dnorm, args = list(mean = mean(norm$PF), sd = sd(norm$PF)))

ggplot(norm, aes(x = PF)) + geom_histogram()

norm <- data.frame(PF = 10*rnorm(1000))



#Q3
black <- read_csv(file.choose())







#Breakout room question

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
predict(crickets_fit, newdata = data.frame(chirps = 1000), interval = 'confidence')
crickets2 %>% ggplot(aes(x = chirps, y = temp)) + geom_point() +
  geom_line(aes(y = lwr), color = "red", linetype = 'dashed') +
  geom_line(aes(y = upr), color = "red", linetype = 'dashed') +
  geom_smooth(method = 'lm', level = 0.95) + theme_bw() + ggtitle("99% Confidence and Prediction Intervals")
head(crickets)
library(investr)
calibrate(crickets_fit, y0 = 80, interval = 'Wald', mean.response = TRUE)
