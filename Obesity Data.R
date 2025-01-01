# DATASET: https://archive.ics.uci.edu/dataset/544/estimation+of+obesity+levels+based+on+eating+habits+and+physical+condition

obesity = ObesityDataSet_raw_and_data_sinthetic[ , c(2,4,7,8,11,13)]

hist(obesity$Weight, breaks = 10,
     labels = T,
     main = "Weight Histogram",
     xlab = "weight (kilograms)",
     ylab = "count",
     col = "#9F79EE")

boxplot(obesity$Weight,
        horizontal = TRUE,
        main = "Weight Boxplot",
        xlab = "weight (kilograms)",
        ylab = "count",
        col = "orchid3")

# Explantory Data Analysis
summary(obesity$Age)
sd(obesity$Age)
summary(obesity$FCVC)
sd(obesity$FCVC)
summary(obesity$NCP)
sd(obesity$NCP)
summary(obesity$CH2O)
sd(obesity$CH2O)
summary(obesity$FAF)
sd(obesity$FAF)

fivenum(obesity$Weight)

# Numerical variable shape
hist(obesity$Age,
     density = 50,
     col = "#CD5555",
     main = "Histogram of Age",
     xlab = "Age",
     ylab = "Frequency")
hist(obesity$FCVC,
     density = 50,
     col = "#71B966",
     main = "Histogram of Vegetables",
     xlab = "FCVC(vegetables in meals)",
     ylab = "Frequency")
hist(obesity$NCP,
     density = 50,
     col = "dodgerblue3",
     main = "Histogram of Main Meals",
     xlab = "NCP(amount of daily main meals)",
     ylab = "Frequency")
hist(obesity$CH2O,
     density = 50,
     col = "#8B2252",
     main = "Histogram of Water",
     xlab = "CH20(daily water intake(L))",
     ylab = "Frequency")
hist(obesity$FAF,
     density = 50,
     col = "#EE7942",
     main = "Histogram of Physical Activity",
     xlab = "FAF(weekly physical activity)",
     ylab = "Frequency")

y = lm(Weight ~ Age + FCVC + NCP + CH2O + FAF, data = obesity)
y1 = lm(obesity$Weight ~ obesity$Age + obesity$FCVC + obesity$NCP + obesity$CH2O
        + obesity$FAF)
rm(fit)
vif(y1)
summary(y)
confint(y)
cor(obesity$Weight,obesity$Age)

# marginal effect
cor(obesity$Weight,obesity$Age)*(sd(obesity$Weight)/sd(obesity$Age))
cor(obesity$Weight,obesity$FCVC)*(sd(obesity$Weight)/sd(obesity$FCVC))
cor(obesity$Weight,obesity$NCP)*(sd(obesity$Weight)/sd(obesity$NCP))
cor(obesity$Weight,obesity$CH2O)*(sd(obesity$Weight)/sd(obesity$CH2O))
cor(obesity$Weight,obesity$FAF)*(sd(obesity$Weight)/sd(obesity$FAF))

# cor(obesity$Weight,obesity$Age)*(sd(obesity$Age)/sd(obesity$Weight))
# cor(obesity$Weight,obesity$FCVC)*(sd(obesity$FCVC)/sd(obesity$Weight))
# cor(obesity$Weight,obesity$NCP)*(sd(obesity$NCP)/sd(obesity$Weight))
# cor(obesity$Weight,obesity$CH2O)*(sd(obesity$CH2O)/sd(obesity$Weight))
# cor(obesity$Weight,obesity$FAF)*(sd(obesity$FAF)/sd(obesity$Weight))

library(corrplot)
corrplot(cor(obesity),
         method = "number",
         type = "lower")

summary(aov(obesity$Age))

cor(obesity$Weight,obesity$FCVC)*(sd(obesity$Weight)/sd(obesity$FCVC))
10.60176/ 0.99529
9.61189/0.99529
pt(9.657,2110)

pt(9.657, 2109, lower.tail = F)

y_FCVC = lm(obesity$Weight ~ obesity$FCVC, data = obesity)
summary(aov(y_FCVC))
library(car)
vif(y)

summary(y_FCVC)
pf(103.3, 1, 2109, lower.tail = F)
10.602/1.043
2*pt(10.1649,2110)

FCVC_group = cut(obesity$FCVC,
                 breaks = 3,
                 labels = c("Little", "Medium", "Large"))

# Scatter Plot
ggplot(obesity, aes(y = Weight, x = Age)) + 
  geom_point(color="#CD5555")  +
  geom_smooth(method = "lm", se = TRUE, level = 0.95, color = "gray40")
ggplot(obesity, aes(y = Weight, x = FCVC_group)) + 
  geom_boxplot(color="#71B966")  +
  geom_smooth(method = "lm", se = TRUE, level = 0.95, color = "gray40")
ggplot(obesity, aes(y = Weight, x = NCP_group)) + 
  geom_boxplot(color="dodgerblue3")  +
  geom_smooth(method = "lm", se = TRUE, level = 0.95, color = "gray40")
ggplot(obesity, aes(y = Weight, x = CH2O)) + 
  geom_point(color="#8B2252")  +
  geom_smooth(method = "lm", se = TRUE, level = 0.95, color = "gray40")
ggplot(obesity, aes(y = Weight, x = FAF_group)) + 
  geom_boxplot(color="#EE7942")  +
  geom_smooth(method = "lm", se = TRUE, level = 0.95, color = "gray40")

obesity = ObesityDataSet_raw_and_data_sinthetic[ , c(2,4,7,8,11,13)]

hist(obesity$Weight, breaks = 10,
     labels = T,
     main = "Weight Histogram",
     xlab = "weight (kilograms)",
     ylab = "count",
     col = "#9F79EE")

boxplot(obesity$Weight,
        horizontal = TRUE,
        main = "Weight Boxplot",
        xlab = "weight (kilograms)",
        ylab = "count",
        col = "orchid3")

fivenum(obesity$Age)
summary(obesity$FCVC)
summary(obesity$NCP)
summary(obesity$CH2O)
summary(obesity$FAF)
fivenum(obesity$FAF)

fivenum(obesity$Weight)

y = lm(obesity$Weight ~ obesity$Age + obesity$FCVC + obesity$NCP + obesity$CH2O
       + obesity$FAF, data = obesity)
summary(y)
confint(y)
cor(obesity$Weight,obesity$Age)
cor(obesity$Weight,obesity$Age)*(sd(obesity$Weight)/sd(obesity$Age))
cor(obesity$Weight,obesity$FCVC)*(sd(obesity$Weight)/sd(obesity$FCVC))
cor(obesity$Weight,obesity$NCP)*(sd(obesity$Weight)/sd(obesity$NCP))
cor(obesity$Weight,obesity$CH2O)*(sd(obesity$Weight)/sd(obesity$CH2O))
cor(obesity$Weight,obesity$FAF)*(sd(obesity$Weight)/sd(obesity$FAF))

# cor(obesity$Weight,obesity$Age)*(sd(obesity$Age)/sd(obesity$Weight))
# cor(obesity$Weight,obesity$FCVC)*(sd(obesity$FCVC)/sd(obesity$Weight))
# cor(obesity$Weight,obesity$NCP)*(sd(obesity$NCP)/sd(obesity$Weight))
# cor(obesity$Weight,obesity$CH2O)*(sd(obesity$CH2O)/sd(obesity$Weight))
# cor(obesity$Weight,obesity$FAF)*(sd(obesity$FAF)/sd(obesity$Weight))

# corrplot
corrplot(cor(obesity),
         method = "number",
         type = "lower")

summary(aov(obesity$Age))

cor(obesity$Weight,obesity$FCVC)*(sd(obesity$Weight)/sd(obesity$FCVC))
10.60176/ 0.99529
9.61189/0.99529
pt(9.657,2110)

pt(9.657, 2109, lower.tail = F)

y_FCVC = lm(obesity$Weight ~ obesity$FCVC, data = obesity)
summary(aov(y_FCVC))
library(car)
vif(y)

summary(y_FCVC)
pf(103.3, 1, 2109, lower.tail = F)
10.602/1.043
2*pt(10.1649,2110)

FCVC_group = cut(obesity$FCVC,
                 breaks = 3,
                 labels = c("Little", "Medium", "Large"))

# Scatter Plot
ggplot(obesity, aes(x = Weight, y = Age)) + 
  geom_point(color="black")  +
  geom_smooth(method = "lm", se = TRUE, level = 0.95)

hist(obesity$Weight, breaks = 10,
     labels = T,
     main = "Weight Histogram",
     xlab = "weight (kilograms)",
     ylab = "count",
     col = "#9F79EE")

boxplot(obesity$Weight,
        horizontal = TRUE,
        main = "Weight Boxplot",
        xlab = "weight (kilograms)",
        ylab = "count",
        col = "orchid3")

fivenum(obesity$Age)
summary(obesity$FCVC)
summary(obesity$NCP)
summary(obesity$CH2O)
summary(obesity$FAF)
fivenum(obesity$FAF)

fivenum(obesity$Weight)

y = lm(obesity$Weight ~ obesity$Age + obesity$FCVC + obesity$NCP + obesity$CH2O
       + obesity$FAF, data = obesity)
summary(y)
confint(y)

cor(obesity$Weight,obesity$Age)
cor(obesity$Weight,obesity$Age)*(sd(obesity$Weight)/sd(obesity$Age))
cor(obesity$Weight,obesity$FCVC)*(sd(obesity$Weight)/sd(obesity$FCVC))
cor(obesity$Weight,obesity$NCP)*(sd(obesity$Weight)/sd(obesity$NCP))
cor(obesity$Weight,obesity$CH2O)*(sd(obesity$Weight)/sd(obesity$CH2O))
cor(obesity$Weight,obesity$FAF)*(sd(obesity$Weight)/sd(obesity$FAF))

# cor(obesity$Weight,obesity$Age)*(sd(obesity$Age)/sd(obesity$Weight))
# cor(obesity$Weight,obesity$FCVC)*(sd(obesity$FCVC)/sd(obesity$Weight))
# cor(obesity$Weight,obesity$NCP)*(sd(obesity$NCP)/sd(obesity$Weight))
# cor(obesity$Weight,obesity$CH2O)*(sd(obesity$CH2O)/sd(obesity$Weight))
# cor(obesity$Weight,obesity$FAF)*(sd(obesity$FAF)/sd(obesity$Weight))

corrplot(cor(obesity),
         method = "number",
         type = "lower")

summary(aov(obesity$Age))

cor(obesity$Weight,obesity$FCVC)*(sd(obesity$Weight)/sd(obesity$FCVC))
10.60176/ 0.99529
9.61189/0.99529
pt(9.657,2110)

pt(9.657, 2109, lower.tail = F)

y_FCVC = lm(obesity$Weight ~ obesity$FCVC, data = obesity)
summary(aov(y_FCVC))
library(car)
vif(y)

summary(y_FCVC)
pf(103.3, 1, 2109, lower.tail = F)
10.602/1.043
2*pt(10.1649,2110)

FCVC_group = cut(obesity$FCVC,
                 breaks = 3,
                 labels = c("Little", "Medium", "Large"))
summary(obesity$FCVC)

FAF_group = cut(obesity$FAF,
                 breaks = 4,
                 labels = c("None", "Little", "Average", "Lots"))

# Scatter Plot
ggplot(obesity, aes(x = Weight, y = Age)) + 
  geom_point(color="black")  +
  geom_smooth(method = "lm", se = TRUE, level = 0.95)

ggplot(obesity, aes(x = Weight, y = FCVC)) + 
  geom_point(color="black")  +
  geom_smooth(method = "lm", se = TRUE, level = 0.95)

ggplot(obesity, aes(x = Weight, y = NCP)) + 
  geom_point(color="black")  +
  geom_smooth(method = "lm", se = TRUE, level = 0.95)

ggplot(obesity, aes(x = Weight, y = CH2O)) + 
  geom_point(color="black")  +
  geom_smooth(method = "lm", se = TRUE, level = 0.95)

ggplot(obesity, aes(x = Weight, y = FAF_group)) + 
  geom_boxplot(notch = FALSE)  +
 labs(y = "Execerise")

ggplot(obesity, aes(x = Weight, y = FCVC_group)) + 
  geom_boxplot(notch = FALSE) +
  labs(y = "Vegetables in Meals")

y_age = lm(Weight ~ Age, data = obesity)
y_FCVC = lm(Weight ~ FCVC, data = obesity)
y_NCP = lm(Weight ~ NCP, data = obesity)
y_CH2O = lm(Weight ~ CH2O, data = obesity)
y_FAF = lm(Weight ~ FAF, data = obesity)

age_data = data.frame(Age = 100)
FCVC_data = data.frame(FCVC = 100)
NCP_data = data.frame(NCP = 100)
CH2O_data = data.frame(CH2O = 100)
FAF_data = data.frame(FAF = 100)
predict(y, interval = "confidence")
predict(y_age, age_data, interval = "confidence")
predict(y_FCVC, FCVC_data, interval = "confidence")
predict(y_NCP, NCP_data, interval = "confidence")
predict(y_CH2O, CH2O_data, interval = "confidence")
predict(y_FAF, FAF_data, interval = "confidence")

# comparison between y and yhat
yhat = fit2$fitted 