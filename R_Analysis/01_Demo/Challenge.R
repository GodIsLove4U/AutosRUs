mecha_data <- read.csv('MechaCar_mpg.csv')
mecha_data2 <- read.csv("MechaCar_mpg.csv",stringsAsFactors = F,check.names = F)
mecha_lm <- lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD,data=mecha_data2)
summary(mecha_lm)
library(evaluate)
library()

mpgdata$AWD <- factor(mpgdata$AWD)

ggplot(mpgdata, aes(x = AWD, y = mpg)) +
  geom_boxplot(color = "black", fill = "steelblue") +
  geom_jitter(color = "red", width = .1, height = 0)


res.ttest <- t.test(mpg ~ AWD, data = mpgdata)

ggplot(mpgdata, aes(x = vehicle.length, y = mpg)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red")

res.lm <- lm(mpg ~ ., data = mpgdata)
summary(res.lm)

mpgdata$pred <- predict(res.lm)

# windows(20, 12)
ggplot(mpgdata, aes(x = pred, y = mpg)) +
  geom_point(color = "blue")


res.lm2 <- lm(mpg ~ (. - pred)^2, data = mpgdata)
summary(res.lm2)

mpgdata$pred2 <- predict(res.lm2)

# windows(20, 12)
ggplot(mpgdata, aes(x = pred2, y = mpg)) +
  geom_point(color = "blue")
