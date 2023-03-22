library(ggplot2)
library(readr)
library(xtable)
world_happiness <- read_delim("Data/world_happiness-2.csv", 
                                delim = ";", escape_double = FALSE, col_types = cols(gdp = col_skip(), 
                                                                                     facebook = col_skip(), pop = col_skip()), 
                                locale = locale(decimal_mark = ","), 
                                trim_ws = TRUE)
head(world_happiness)
ggplot(world_happiness, aes(x = gdp1000, y = happiness)) + geom_point() +
  labs(title = "Happiness vs GDP") + xlab("GDP")
world_happ<- world_happiness[world_happiness$coffee > 0, ]

#### Residual analysis of lin-lin and lin-log ####
# Linear model: estimates and confidence interval
lin.model <- lm(happiness ~ gdp1000, data = world_happ)
(lin.coeff <- lin.model$coefficients)
lin.ci <- confint(lin.model)
lintable <- cbind(lin.coeff,lin.ci)
print(xtable(lintable, type = "latex"), file = "lincoeffs.txt")

# Calculate fit and confidence interval
happ.pred <- 
  cbind(world_happ, 
        fit = predict(lin.model),
        conf = predict(lin.model, interval = "confidence"))

happ.pred$conf.fit <- NULL
head(happ.pred)

#Plot with fit and confidence intervals
ggplot(data = happ.pred, aes(x = gdp1000, y = happiness)) + 
geom_point(size = 1) +
geom_line(aes(y = fit), color = "blue", size = 1) +
geom_ribbon(aes(ymin = conf.lwr, ymax = conf.upr), alpha = 0.2) +
xlab("GDP per capita (US dollars)") +
ylab("Happiness score") +
labs(title = "How happiness varies with GDP per capita") +
labs(caption = "data, fitted line and confidence interval") + 
theme(text = element_text(size = 12))

# Lin - Log model
log.model <- lm(happiness ~ log(gdp1000), data = world_happ)
(log.coeff <- log.model$coefficients)
log.ci <- confint(log.model)
logtable <- cbind(log.coeff,log.ci)
print(xtable(logtable, type = "latex"), file = "logcoeffs.txt")

# Calculate fit and confidence interval
happ.pred.log <- 
  cbind(world_happ, 
        fit = predict(log.model),
        conf = predict(log.model, interval = "confidence"))

happ.pred.log$conf.fit <- NULL
head(happ.pred.log)

#Plot with fit and confidence intervals
ggplot(data = happ.pred.log, aes(x = log(gdp1000), y = happiness)) + 
  geom_point(size = 1) +
  geom_line(aes(y = fit), color = "blue", size = 1) +
  geom_ribbon(aes(ymin = conf.lwr, ymax = conf.upr), alpha = 0.2) +
  xlab("log GDP per capita (US dollars)") +
  ylab("Happiness score") +
  labs(title = "How happiness varies with log GDP per capita") +
  labs(caption = "data, fitted line and confidence interval") + 
  theme(text = element_text(size = 12))

# Residual analysis for linear model
#Calculate residuals
happ.pred$e <- lin.model$residuals
head(happ.pred$e)

#Plot residuals
(max.e <- max(abs(happ.pred$e)))
(lin.elims <- c(-max.e, max.e))

ggplot(data = happ.pred, aes(x = fit, y = e)) +
  geom_point(size = 1) +
  geom_hline(yintercept = 0) +
  expand_limits(y = lin.elims) +
  xlab("Predicted happiness") +
  ylab("Residual") +
  labs(tag = "A") +
  labs(title = "Residuals vs predicted values Y-hat") +
  theme(text = element_text(size = 12)) + 
  geom_smooth()

ggplot(data = happ.pred, aes(x = gdp1000, y = e)) +
  geom_point(size = 1) +
  geom_hline(yintercept = 0) +
  expand_limits(y = lin.elims) +
  xlab("GDP per capita (1000 US Dollars)") +
  ylab("Residual") +
  labs(tag = "B") +
  labs(title = "Residuals vs GDP per capita in 1000 USD") +
  theme(text = element_text(size = 12)) + 
  geom_smooth()

#Q-Q-Plot
ggplot(data = happ.pred, aes(sample = e)) +
  geom_qq(size = 3) +
  geom_qq_line() + 
  labs(tag = "C") + 
  xlab("Theoretical quantile") +
  ylab("Sample quantile") +
  labs(title = "Normal Q-Q-plot of the residuals") +
  theme(text = element_text(size = 12))

#Histogram
ggplot(data = happ.pred, aes(x = e)) +
  geom_histogram(bins = 20) +
  xlab("Residuals") +
  labs(title = "Histogram of residuals") +
  theme(text = element_text(size = 12))

# Residual analysis for log model
#Calculate residuals
happ.pred.log$e <- log.model$residuals
head(happ.pred.log$e)

#Plot residuals
(max.e <- max(abs(happ.pred.log$e)))
(log.elims <- c(-max.e, max.e))

ggplot(data = happ.pred.log, aes(x = fit, y = e)) +
  geom_point(size = 1) +
  geom_hline(yintercept = 0) +
  expand_limits(y = log.elims) +
  xlab("Predicted happiness") +
  ylab("Residual") +
  labs(tag = "A") +
  labs(title = "Residuals vs predicted values Y-hat") +
  theme(text = element_text(size = 12)) + 
  geom_smooth()

ggplot(data = happ.pred.log, aes(x = log(gdp1000), y = e)) +
  geom_point(size = 1) +
  geom_hline(yintercept = 0) +
  expand_limits(y = log.elims) +
  xlab("log GDP per capita (1000 US Dollars)") +
  ylab("Residual") +
  labs(tag = "B") +
  labs(title = "Residuals vs log GDP per capita in 1000 USD") +
  theme(text = element_text(size = 12)) + 
  geom_smooth()

#Q-Q-Plot
ggplot(data = happ.pred.log, aes(sample = e)) +
  geom_qq(size = 3) +
  geom_qq_line() + 
  labs(tag = "C") +
  xlab("Theoretical quantile") +
  ylab("Sample quantile") +
  labs(title = "Normal Q-Q-plot of the residuals") +
  theme(text = element_text(size = 12))

#Histogram
ggplot(data = happ.pred.log, aes(x = e)) +
  geom_histogram(bins = 20) +
  xlab("Residuals") +
  labs(title = "Histogram of residuals") +
  theme(text = element_text(size = 12))



#### Freedom, Freedom sq, Coffee ####
freedom.model <- lm(happiness ~ freedom, data = world_happ)
freedom.sq.model <- lm(happiness ~ freedom + I(freedom^2), data = world_happ)
coffee.model <- lm(happiness ~ coffee, data = world_happ)
coffee.log.model <- lm(happiness ~ log(coffee), data = world_happ)

free.pred <- cbind(world_happ,
                   fit = predict(freedom.model),
                   conf = predict(freedom.model, interval = "confidence"))
free.pred$conf.fit <- NULL

free.pred <- cbind(free.pred,
                   fit.sq = predict(freedom.sq.model),
                   conf.sq = predict(freedom.sq.model, interval = "confidence"))
free.pred$conf.sq.fit <- NULL

# Plot the two freedom models with CI and moving average
ggplot(free.pred, aes(freedom, happiness)) + geom_smooth(se = FALSE, color = "blue", linetype = "dashed") + 
  geom_line(aes(y = fit), size = 0.5, color = "red") +
  geom_line(aes(y = fit.sq), size = 0.5, color = "green") +
  geom_ribbon(aes(ymin = conf.lwr, ymax = conf.upr), alpha = 0.2) + 
  geom_ribbon(aes(ymin = conf.sq.lwr, ymax = conf.sq.upr), alpha = 0.2) + 
  labs(title = "Freedom vs Happiness", caption = "blue = moving average, red = linear, green = quadratic")

coffee.pred <- cbind(world_happ,
                   fit = predict(coffee.model),
                   conf = predict(coffee.model, interval = "confidence"))
coffee.pred$conf.fit <- NULL

coffee.pred <- cbind(coffee.pred,
                   fit.log = predict(coffee.log.model),
                   conf.log = predict(coffee.log.model, interval = "confidence"))
free.pred$conf.log.fit <- NULL

# Plot the two freedom models with CI and moving average
ggplot(coffee.pred, aes(coffee, happiness)) + geom_smooth(se = FALSE, color = "blue", linetype = "dashed") + 
  geom_line(aes(y = fit), size = 0.5, color = "red") +
  geom_line(aes(y = fit.log), size = 0.5, color = "green") +
  geom_ribbon(aes(ymin = conf.lwr, ymax = conf.upr), alpha = 0.2) + 
  geom_ribbon(aes(ymin = conf.log.lwr, ymax = conf.log.upr), alpha = 0.2) + 
  labs(title = "Coffee vs Happiness", caption = "blue = moving average, red = linear, green = logarithmic")


#### Plotting all variables ####
ggplot(world_happ, aes(x = alcohol, y = happiness)) + geom_point()
ggplot(world_happ, aes(x = age_exp, y = happiness)) + geom_point()
ggplot(world_happ, aes(x = precipitation, y = happiness)) + geom_point()
ggplot(world_happ, aes(x = unemployment, y = happiness)) + geom_point()
ggplot(world_happ, aes(x = coffee, y = happiness)) + geom_point() +
  labs(title = "Happiness vs coffee") #make log
ggplot(world_happ, aes(x = pop_density, y = happiness)) + geom_point() 
ggplot(world_happ, aes(x = health_spend, y = happiness)) + geom_point()
ggplot(world_happ, aes(x = bmi_women, y = happiness)) + geom_point()
ggplot(world_happ, aes(x = bmi_men, y = happiness)) + geom_point()
ggplot(world_happ, aes(x = facebook_share, y = happiness)) + geom_point()
ggplot(world_happ, aes(x = corruption, y = happiness)) + geom_point()
ggplot(world_happ, aes(x = freedom, y = happiness)) + 
  geom_point() + labs(title = "Happiness vs freedom")


contx <- world_happ[, c("gdp1000", "alcohol", "age_exp", "precipitation", "unemployment",
                  "coffee", "pop_density", "health_spend", "bmi_women", "bmi_men",
                  "facebook_share", "corruption", "freedom")]
(corr <- cor(contx))
print(xtable(corr, type = "latex"), file = "corr.txt")

contx2 <- world_happ[, c("coffee", "corruption", "gdp1000", "age_exp")]
(corr2 <- cor(contx2))
print(xtable(corr2, type = "latex"), file = "corr.txt")

#### Full model ####
full.model <- lm(happiness ~ log(gdp1000) + corruption + freedom + I(freedom^2) + 
                   log(coffee) + alcohol + age_exp + precipitation + 
                   unemployment + pop_density + health_spend + bmi_women + 
                   bmi_men + facebook_share, data = world_happ)
full.coeffs <- full.model$coefficients
(full.ci <- confint(full.model))
fulltable <- cbind(full.coeffs, full.ci)
print(xtable(fulltable, type = "latex"), file = "fulltable.txt")
#Plot with fitted line and confidence intervals
full.pred <- cbind(world_happ,
                     fit = predict(full.model),
                     conf = predict(full.model, interval = "confidence"))
full.pred$conf.fit <- NULL

ggplot(full.pred, aes(fit, happiness)) + geom_point(size = 0.5) + 
  geom_line(aes(y = fit), size = 0.5, color = "red") +
  geom_ribbon(aes(ymin = conf.lwr, ymax = conf.upr), alpha = 0.2) + 
  labs(title = "Fit vs Happiness", caption = "red = fitted line")


  

#### Null model #### 
null.model <- lm(happiness ~ 1, data = world_happ)
null.coeffs <- null.model$coefficients

#### Statistical tests ####

# Test if the full model is better than the null model - Global F-test
(sum.full <- summary(full.model))
sum.full$coefficients

# Test if the full model is better than the one using only GDP - Partial F-test
gdp.model <- lm(happiness ~ log(gdp1000), data = world_happ)
(happ.anova <- anova(gdp.model, full.model))

# Test if the full model is better than the one using only freedom - Partial F-test
free.model <- lm(happiness ~ freedom, data = world_happ)
(happ.anova <- anova(free.model, full.model))

# Test if the full model is better than the one using only freedom^2 - Partial F-test
free.sq.model <- lm(happiness ~ I(freedom^2), data = world_happ)
(happ.anova <- anova(free.sq.model, full.model))

# Test if the full model is better than the one using freedom + freedom^2 - Partial F-test
free.2.model <- lm(happiness ~ freedom + I(freedom^2), data = world_happ)
(happ.anova <- anova(free.2.model, full.model))

# Test if the reduced model (without the two freedom terms)
# is better than the full model - Partial F-test
reduced.model <- lm(happiness ~ log(gdp1000) + corruption + 
                      log(coffee) + alcohol + age_exp + precipitation + 
                      unemployment + pop_density + health_spend + bmi_women + 
                      bmi_men + facebook_share, data = world_happ)
(happ.anova <- anova(reduced.model, full.model))

# Test, for each variable, if it is significant - T-test
t.test <- summary(full.model)
print(xtable(t.test, type = "latex"), file = "t.test.txt")


#### R squared, AIC, BIC #### 
model.stepAIC <- step(gdp.model, 
                      scope = list(lower = null.model, upper = full.model),
                      direction = "both")
model.stepBIC <- step(gdp.model,
                      scope = list(lower = null.model, upper = full.model),
                      direction = "both", k = log(nrow(world_happ))
)

AIC.coeff <- cbind(model.stepAIC$coefficients, confint(model.stepAIC))
BIC.coeff <- cbind(model.stepBIC$coefficients, confint(model.stepBIC))
print(xtable(AIC.coeff, type = "latex"), file = "aic.coeff.txt")
print(xtable(BIC.coeff, type = "latex"), file = "bic.coeff.txt")

# Constructing the final model
# GDP model: 
sum.gdp <- summary(gdp.model)
# Full model: 
sum.full <- summary(full.model)
# Null model: 
sum.null <- summary(null.model)
# Freedom model: 
sum.free <- summary(free.sq.model)
# Step AIC model: model.stepAIC
sum.aic <- summary(model.stepAIC)
# Step BIC model: model.stepBIC
sum.bic <- summary(model.stepBIC)


(collect.R2s <- data.frame(
  nr = seq(1, 6),
  model = c("0.gdp", "1.full",  
            "2.null","3.free", "4.step AIC", "5.step BIC"),
  R2 = c(sum.gdp$r.squared,
         sum.full$r.squared,
         sum.null$r.squared,
         sum.free$r.squared,
         sum.aic$r.squared,
         sum.bic$r.squared),
  R2.adj = c(sum.gdp$adj.r.squared,
             sum.full$adj.r.squared,
             sum.null$adj.r.squared,
             sum.free$adj.r.squared,
             sum.aic$adj.r.squared,
             sum.bic$adj.r.squared),
  AIC = c(AIC(gdp.model),
          AIC(full.model),
          AIC(null.model),
          AIC(free.sq.model),
          AIC(model.stepAIC),
          AIC(model.stepBIC)),
  BIC = c(BIC(gdp.model),
          BIC(full.model), 
          BIC(null.model), 
          BIC(free.sq.model), 
          BIC(model.stepAIC),
          BIC(model.stepBIC))))

print(xtable(collect.R2s, type = "latex"), file = "R2.txt")


# Step AIC has the highest R2.adj and lowesr AIC and BIC => final model 
model.stepAIC$coefficients
happiness.final <- cbind(world_happ,
                          fit = predict(model.stepAIC))

finalmodel <- model.stepAIC
finalmodel$coefficients
confint(finalmodel)


#### AIC-model ####
# Calculate fit and confidence interval
happ.pred.aic <- 
  cbind(world_happ, 
        fit = predict(model.stepAIC),
        conf = predict(model.stepAIC, interval = "confidence"))

happ.pred.aic$conf.fit <- NULL
head(happ.pred.aic)

#Plot with fit and confidence intervals
ggplot(data = happ.pred.aic, aes(x = fit, y = happiness)) + 
  geom_point(size = 1) +
  geom_line(aes(y = fit), color = "blue", size = 1) +
  geom_ribbon(aes(ymin = conf.lwr, ymax = conf.upr), alpha = 0.2) +
  xlab("Y hat") +
  ylab("Happiness score") +
  labs(title = "Happiness vs Yhat with confidence interval") +
  labs(caption = "data, fitted line and confidence interval") + 
  theme(text = element_text(size = 12))

#### BIC model ####

happ.pred.aic <- 
  cbind(happ.pred.aic, 
        fit.bic = predict(model.stepBIC),
        conf.bic = predict(model.stepBIC, interval = "confidence"))

happ.pred.aic$conf.bic.fit <- NULL
head(happ.pred.aic)

ggplot(data = happ.pred.aic, aes(x = fit, y = happiness)) + 
  geom_point(size = 1) +
  geom_line(aes(y = fit.bic), color = "red", size = 0.5) +
  geom_ribbon(aes(ymin = conf.bic.lwr, ymax = conf.bic.upr), alpha = 0.2) +
  xlab("Y hat") +
  ylab("Happiness score") +
  labs(title = "Happiness vs Yhat with confidence interval") +
  labs(caption = "data, fitted line and confidence interval") + 
  theme(text = element_text(size = 12))

#Plot with fit and confidence intervals
ggplot(data = happ.pred.aic, aes(x = fit, y = happiness)) + 
  geom_point(size = 1) +
  geom_line(aes(y = fit), color = "blue", size = 0.5) +
  geom_line(aes(y = fit.bic), color = "red", size = 0.5) +
  geom_ribbon(aes(ymin = conf.bic.lwr, ymax = conf.bic.upr), alpha = 0.2) +
  geom_ribbon(aes(ymin = conf.lwr, ymax = conf.upr), alpha = 0.2) +
  xlab("Y hat") +
  ylab("Happiness score") +
  labs(title = "Happiness vs Yhat with confidence interval") +
  labs(caption = "blue = AIC, red = BIC") + 
  theme(text = element_text(size = 12))


#### Prediction ####
country1 <- data.frame(freedom = 1, age_exp = 84.8, corruption = 89, 
                       alcohol = 6.52, precipitation = 842, unemployment = 0.61,
                       coffee = 1.845, pop_density = 38.40, health_spend = 38.40,
                       bmi_women = 21.7, bmi_men = 24.7, gdp1000 = 117.25,
                       facebook_share = 0.510322)
utopia <- data.frame(freedom = 1, age_exp = 100, 
                       unemployment = 0,
                       pop_density = 50,
                       gdp1000 = 150)

(happ.utopia <- cbind(predict(model.stepAIC, utopia), 
                       predict(model.stepAIC, utopia, interval = "confidence")))

print(xtable(happ.utopia, type = "latex"), file = "utopia.txt")

dystopia <- data.frame(freedom = 7, age_exp = 50, 
                     unemployment = 50,
                     pop_density = 1500,
                     gdp1000 = 0.50)

(happ.dystopia <- cbind(predict(model.stepAIC, dystopia), 
                      predict(model.stepAIC, dystopia, interval = "confidence")))

print(xtable(happ.dystopia, type = "latex"), file = "dystopia.txt")


#### Leverage, Cooks D, studentized residuals, DFbetas ####
happ.aic <- cbind(
  world_happ,
  fit = predict(model.stepAIC),
  r = rstudent(model.stepAIC),
  v = influence(model.stepAIC)$hat
)

# plot fit vs leverage to identify potentially influential points
ggplot(happ.aic, aes(x = fit, y = v)) +
  geom_jitter(width = 1) +
  geom_hline(yintercept = 1/nrow(world_happ)) +
  geom_hline(yintercept = 2*length(model.stepAIC$coefficients)/nrow(world_happ), 
             color = "red", linetype = "dashed") +
  labs(title = "Happiness: leverage vs y-hat") +
  xlab("y-hat") + 
  labs(caption = "y = 1/124 (black) and 10/124 (red)") +
  theme(text = element_text(size = 18))

v.strange <- which(happ.aic$v > 10/124)
lev <- happ.aic[v.strange, ]

(high.leverage <- data.frame(country = lev$Country, 
                       leverage = lev$v,
                        gdp = lev$gdp1000,
                        age_exp = lev$age_exp,
                        freedom = lev$freedom,
                        unemployment = lev$unemployment,
                        pop_density = lev$pop_density
  ))

print(xtable(high.leverage, type = "latex"), file = "high.leverage.txt")


# plotting fit vs happiness highlighting high leverage points
ggplot(happ.aic, aes(fit, happiness)) + 
  geom_point() +
  geom_point(data = happ.aic[v.strange, ], 
             color = "red", size = 3, shape = 24) +
  labs(title = "Fit vs happiness",
       caption = "") +
  theme(text = element_text(size = 14))

# plotting age vs studentized residuals 
r.large <- which(abs(happ.aic$r) >= 2)
res <- happ.aic[r.large,]

ggplot(happ.aic, aes(x = fit, y = r)) +
  geom_point(size = 1) +
  geom_hline(yintercept = c(-2, 0, 2)) +
  geom_hline(yintercept = c(-3, 3), linetype = 2) +
  geom_smooth(size = 0.5) +
  geom_point(data = happ.aic[v.strange, ], 
             color = "red", size = 4, shape = 24) +
  geom_point(data = happ.aic[r.large, ], 
             color = "springgreen4", size = 4, shape = 24) +
  labs(title = "Happiness: residuals vs fitted values") +
  xlab("Fitted values (happiness)") +
  ylab("Studentized residuals") +
  theme(text = element_text(size = 12))

ggplot(happ.aic, aes(x = fit, y = sqrt(abs(r)))) +
  geom_point(size = 1) +
  geom_hline(yintercept = c(sqrt(qnorm(0.75)), sqrt(2))) +
  geom_hline(yintercept = sqrt(3), linetype = 2) +
  geom_smooth(size = 0.5) +
  geom_point(data = happ.aic[v.strange, ], 
             color = "red", size = 4, shape = 24) +
  geom_point(data = happ.aic[r.large, ], 
             color = "springgreen4", size = 4, shape = 24) +
  labs(title = "Happiness: constant variance?") +
  xlab("fitted values (happiness)") +
  ylab("sqrt(|r*|)") +
  theme(text = element_text(size = 12))

(high.residuals <- data.frame(country = res$Country, 
                             residuals = res$r,
                             gdp = res$gdp1000,
                             age_exp = res$age_exp,
                             freedom = res$freedom,
                             unemployment = res$unemployment,
                             pop_density = res$pop_density
))

print(xtable(high.residuals, type = "latex"), file = "high.residuals.txt")

# Cook's distance
happ.aic$D <- cooks.distance(model.stepAIC)
head(happ.aic)

d.large <- which(happ.aic$D > 0.05)
cook <- happ.aic[d.large,]

(high.cooksd <- data.frame(country = cook$Country, 
                              cooks.d = cook$r,
                              gdp = cook$gdp1000,
                              age_exp = cook$age_exp,
                              freedom = cook$freedom,
                              unemployment = cook$unemployment,
                              pop_density = cook$pop_density
))

print(xtable(high.cooksd, type = "latex"), file = "high.cooksd.txt")

(f1 <- length(model.stepAIC$coefficients))
(f2 <- model.stepAIC$df.residual)
(cook.limit <- qf(0.5, f1, f2))
ggplot(happ.aic, aes(fit, D)) + 
  geom_point(size = 1) +
  geom_point(data = happ.aic[v.strange, ], 
             color = "red", size = 4, shape = 24) +
  geom_point(data = happ.aic[r.large, ],
             color = "springgreen4", size = 4, shape = 24) + 
  geom_point(data = happ.aic[d.large, ],
             color = "purple", size = 4, shape = 24) + 
  geom_hline(yintercept = cook.limit, color = "red") +
  geom_hline(yintercept = 4/nrow(happ.aic), linetype = 2, color = "red") +
  xlab("Fitted values") +
  ylab("D_i") +
  labs(title = "Happiness: Cook's Distance") +
  labs(caption = "4/n (dashed), F_0.5, p+1, n-(p+1) (solid)") +
  theme(text = element_text(size = 12))

# Calculate dfbetas
model.stepAIC$coefficients
happ.aic$df0 <- dfbetas(model.stepAIC)[, "(Intercept)"]
happ.aic$df1 <- dfbetas(model.stepAIC)[, "log(gdp1000)"]
happ.aic$df2 <- dfbetas(model.stepAIC)[, "unemployment"]
happ.aic$df3 <- dfbetas(model.stepAIC)[, "freedom"]
happ.aic$df4 <- dfbetas(model.stepAIC)[, "pop_density"]
happ.aic$df5 <- dfbetas(model.stepAIC)[, "age_exp"]
head(happ.aic)

# Plotting dfbetas against the responding x-variable
ggplot(happ.aic, aes(fit, df0)) + 
  geom_point(size = 1) +
  geom_point(data = happ.aic[v.strange, ], 
             color = "red", size = 4, shape = 24) +
  geom_point(data = happ.aic[r.large, ],
             color = "springgreen4", size = 4, shape = 24) + 
  geom_point(data = happ.aic[d.large, ],
             color = "purple", size = 4, shape = 24) + 
  geom_hline(yintercept = sqrt(cook.limit)*c(-1, 1), color = "red") +
  geom_hline(yintercept = 2/sqrt(314)*c(-1, 1), color = "red", linetype = "dashed") +
  xlab("Fitted values") +
  ylab("Dfbetas") +
  labs(title = "Happiness: Dfbetas0") +
  labs(caption = "y = sqrt(F_0.5) and 2/sqrt(n)") +
  theme(text = element_text(size = 12))

ggplot(happ.aic, aes(log(gdp1000), df1)) + 
  geom_point(size = 1) +
  geom_point(data = happ.aic[v.strange, ], 
             color = "red", size = 4, shape = 24) +
  geom_point(data = happ.aic[r.large, ],
             color = "springgreen4", size = 4, shape = 24) + 
  geom_point(data = happ.aic[d.large, ],
             color = "purple", size = 4, shape = 24) + 
  geom_hline(yintercept = sqrt(cook.limit)*c(-1, 1), color = "red") +
  geom_hline(yintercept = 2/sqrt(314)*c(-1, 1), color = "red", linetype = "dashed") +
  xlab("log GDP") +
  ylab("Dfbetas") +
  labs(title = "Happiness: log GDP") +
  labs(caption = "y = 2/sqrt(n) (dashed)") +
  theme(text = element_text(size = 12))

ggplot(happ.aic, aes(unemployment, df2)) + 
  geom_point(size = 1) +
  geom_point(data = happ.aic[v.strange, ], 
             color = "red", size = 4, shape = 24) +
  geom_point(data = happ.aic[r.large, ],
             color = "springgreen4", size = 4, shape = 24) + 
  geom_point(data = happ.aic[d.large, ],
             color = "purple", size = 4, shape = 24) + 
  geom_hline(yintercept = sqrt(cook.limit)*c(-1, 1), color = "red") +
  geom_hline(yintercept = 2/sqrt(314)*c(-1, 1), color = "red", linetype = "dashed") +
  xlab("unemployment") +
  ylab("Dfbetas") +
  labs(title = "Happiness: unemployment") +
  labs(caption = "y = 2/sqrt(n) (dashed)") +
  theme(text = element_text(size = 12))

ggplot(happ.aic, aes(freedom, df3)) + 
  geom_point(size = 1) +
  geom_point(data = happ.aic[v.strange, ], 
             color = "red", size = 4, shape = 24) +
  geom_point(data = happ.aic[r.large, ],
             color = "springgreen4", size = 4, shape = 24) + 
  geom_point(data = happ.aic[d.large, ],
             color = "purple", size = 4, shape = 24) + 
  geom_hline(yintercept = sqrt(cook.limit)*c(-1, 1), color = "red") +
  geom_hline(yintercept = 2/sqrt(314)*c(-1, 1), color = "red", linetype = "dashed") +
  xlab("freedom") +
  ylab("Dfbetas") +
  labs(title = "Happiness: freedom") +
  labs(caption = "y = 2/sqrt(n) (dashed)") +
  theme(text = element_text(size = 12))

ggplot(happ.aic, aes(pop_density, df4)) + 
  geom_point(size = 1) +
  geom_point(data = happ.aic[v.strange, ], 
             color = "red", size = 4, shape = 24) +
  geom_point(data = happ.aic[r.large, ],
             color = "springgreen4", size = 4, shape = 24) + 
  geom_point(data = happ.aic[d.large, ],
             color = "purple", size = 4, shape = 24) + 
  geom_hline(yintercept = sqrt(cook.limit)*c(-1, 1), color = "red") +
  geom_hline(yintercept = 2/sqrt(314)*c(-1, 1), color = "red", linetype = "dashed") +
  xlab("population density") +
  ylab("Dfbetas") +
  labs(title = "Happiness: population density") +
  labs(caption = "y = 2/sqrt(n) (dashed)") +
  theme(text = element_text(size = 12))

ggplot(happ.aic, aes(age_exp, df5)) + 
  geom_point(size = 1) +
  geom_point(data = happ.aic[v.strange, ], 
             color = "red", size = 4, shape = 24) +
  geom_point(data = happ.aic[r.large, ],
             color = "springgreen4", size = 4, shape = 24) + 
  geom_point(data = happ.aic[d.large, ],
             color = "purple", size = 4, shape = 24) + 
  geom_hline(yintercept = sqrt(cook.limit)*c(-1, 1), color = "red") +
  geom_hline(yintercept = 2/sqrt(314)*c(-1, 1), color = "red", linetype = "dashed") +
  xlab("age expectancy") +
  ylab("Dfbetas") +
  labs(title = "Happiness: age expectancy") +
  labs(caption = "y = 2/sqrt(n) (dashed)") +
  theme(text = element_text(size = 12))
