# Econometrics---Class-work

ggplot(data, aes(x = cigs, y = bwght)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "yellow") +
  labs(title = "Scatter Plot", x = "Cigarettes per Day", y = "Birth Weight") +
  theme_minimal()

model <- lm(bwght~cigs, data=data)
summary_model = summary(model)
se_cigs <- summary_model$coefficients["cigs", "Std. Error"]

model2 <- lm(bwght ~ cigs + faminc, data = data)
summary_model2 <- summary(model2)
se_cigs2 <- summary_model2$coefficients["cigs", "Std. Error"]
