# Econometrics---Class-work
#ps2
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



#ps3
load("~/sleep75.RData")

###

model <- lm(sleep ~ male + totwrk + educ + age, data = your_data)
summary(model)

#Computed means of variables
mean_totwrk <- mean(data$totwrk, na.rm = TRUE)
mean_educ <- mean(data$educ, na.rm = TRUE)
mean_age <- mean(data$age, na.rm = TRUE)

# Create a data frame for prediction
pred_data <- data.frame(
  male = c(0, 1), # 0 for female, 1 for male
  totwrk = mean_totwrk,
  educ = mean_educ,
  age = mean_age
)

# Predict sleep for men and women
predicted_sleep <- predict(model, newdata = pred_data)

#Add the newly-calculated-predictions to the data frame
pred_data$predicted_sleep <- predicted_sleep

#Men
predicted_sleep_men <- pred_data$predicted_sleep[pred_data$male == 1]
print(predicted_sleep_men)

#Women
predicted_sleep_women <- pred_data$predicted_sleep[pred_data$male == 0]
print(predicted_sleep_women)

###############################

model1 <- lm(sleep ~ male * totwrk + educ + age, data = data)
summary(model1)
