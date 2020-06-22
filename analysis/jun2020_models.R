library(modelsummary)
library(bife)
library(broom)
library(finalfit)
library(lmtest)
library(plm)

cyears <- read.csv("may2020data.csv")

# main models ----

p1 <- glm(new_mil ~ theta_diff_lag + theta_lag + gdpgr_lag + durable + recent_reb + (year>1989) + ethfrac, data=cyears, family=binomial("logit"))
summary(p1)
coeftest(p1, vcov. = vcovHC(p1, type="HC1", cluster="gwno_loc", adjust=T))

p2 <- glm(new_mil ~ theta_diff_lag + gdpgr_lag + log(gdppc_lag) + durable + recent_reb + factor(gwno_loc) -1, data=cyears, family=binomial("probit"))

msummary(list(p1,p2), 'markdown', coef_omit = 'gwno_loc', stars = T, title = 'Probit Models')

p3 <-  bife(new_mil ~ theta_diff_lag + theta_lag + gdpgr_lag + durable + recent_reb | gwno_loc, data=cyears, model="logit")
p3bc <- bias_corr(p3, L=1L)
summary(p3bc)
summary(get_APEs(p3bc))

p3b <- clogit(new_mil ~ theta_diff_lag + theta_lag + gdpgr_lag + durable + recent_reb + strata(gwno_loc), data=cyears)
summary(p3b)


# pred probs ----

pred_data <- with(cyears, data.frame(
  theta_diff_lag = seq(min(theta_diff_lag, na.rm = T), max(theta_diff_lag, na.rm = T), 100),
  theta_lag = mean(theta_lag, na.rm = T),
  gdpgr_lag = mean(gdpgr_lag, na.rm = T),
  durable = mean(durable, na.rm = T),
  recent_reb = 0,
  gwno_loc=651))


library(ggeffects)

ggpredict(p1, vcov.fun = "vcovCR", vcov.type = "CR0", vcov.args = list(cluster = cyears$gwno_loc))



# Old ----

pred_data <- data.frame(theta_diff_lag=seq(min(cyears$theta_diff_lag,na.rm=T), max(cyears$theta_diff_lag,na.rm=T), length.out = 100),  theta_lag=, gdpgr_lag=rep(mean(cyears$gdpgr_lag, na.rm=T), 100), durable=rep(mean(cyears$durable, na.rm=T), 100), recent_reb=rep(0, 100))

p1 %>% 
  boot_predict(pred_data, R = 10000, condense = F) %>% 
  ggplot(aes(x=theta_diff_lag, y=estimate, ymin = estimate_conf.low, ymax = estimate_conf.high)) +
  geom_line() +
  geom_ribbon(alpha=0.1)

pred_data$pred = predict(p1, pred_data, type='response', se.fit = T)$fit
pred_data$se = predict(p1, pred_data, type='response', se.fit = T)$se.fit
pred_data$upper <- pred_data$pred + 1.96*pred_data$se
pred_data$lower <- pred_data$pred - 1.96*pred_data$se

ggplot(pred_data, aes(x=theta_lag, y=pred)) + geom_line() + geom_line(aes(x=theta_lag, y=upper), color="red") + geom_line(aes(x=theta_lag, y=lower), color="red")


# binary ivs ----

p4 <- glm(new_mil ~ downgrade_lag + gdpgr_lag + durable + recent_reb, data=cyears, family=binomial("logit"))

p5 <- glm(new_mil ~ theta_shock3 + gdpgr_lag + durable + recent_reb, data=cyears, family=binomial("logit"))

p6 <- glm(new_mil ~ theta_shock5 + gdpgr_lag + durable + recent_reb, data=cyears, family=binomial("logit"))

p7 <- glm(new_mil ~ nonviol_repress_lag3 + gdpgr_lag + durable + recent_reb, data=cyears, family=binomial("logit"))

p8 <- glm(new_mil ~ nonviol_repress_lag3 + gdpgr_lag + durable + recent_reb, data=cyears, family=binomial("logit"))

msummary(list(p4,p5,p6,p7), 'markdown', coef_omit = 'gwno_loc', stars = T, title = 'Probit Models')

p3 <-  bife(new_mil ~ theta_diff_lag + gdpgr_lag + gdppc_lag + durable + recent_reb | gwno_loc, data=cyears, model="logit")
p3bc <- bias_corr(p3, L=1L)
summary(p3bc)
summary(get_APEs(p3bc))


pred5 <- with(cyears, data.frame(theta_shock3=0:1,
                                 gdpgr_lag=mean(gdpgr_lag, na.rm=T),
                                 durable=mean(durable, na.rm=T),
                                 recent_reb=0))

p5 %>% 
  augment(newdata = pred5, type.predict="response") %>% 
  mutate(upper = .fitted + 1.96 * .se.fit,
         lower = .fitted - 1.96 * .se.fit)  %>% 
  ggplot(aes(factor(theta_shock3), .fitted)) + 
  geom_point(size = 1.5) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = .2)


p9 <- glm(thyne_attempt ~ theta_shock5 + gdpgr_lag + durable + recent_reb, data=cyears, family=binomial("logit"))
summary(p9)

pred9 <- with(cyears, data.frame(theta_shock5=0:1,
                                 gdpgr_lag=mean(gdpgr_lag, na.rm=T),
                                 durable=mean(durable, na.rm=T),
                                 recent_reb=0))

p9 %>% 
  augment(newdata = pred9, type.predict = "response") %>% 
  mutate(upper = .fitted + 1.96 * .se.fit,
         lower = .fitted - 1.96 * .se.fit) %>% 
  ggplot(aes(factor(theta_shock5), .fitted)) +
  geom_point(size = 1.5) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = .2)
  
library(finalfit)

pred5 <- with(cyears, data.frame(theta_shock3=0:1,
                                 gdpgr_lag=mean(gdpgr_lag, na.rm=T),
                                 durable=mean(durable, na.rm=T),
                                 recent_reb=0))

p5 %>% 
  augment(newdata = pred5, type.predict="response") %>% 
  mutate(upper = .fitted + 1.96 * .se.fit,
         lower = .fitted - 1.96 * .se.fit)  %>% 
  ggplot(aes(factor(theta_shock3), .fitted)) + 
  geom_point(size = 1.5) + 
  geom_errorbar(aes(ymin = lower, ymax = upper), width = .2)

p5 %>% 
  boot_predict(pred5, R = 1000, condense = F) %>% 
  ggplot(aes(factor(theta_shock3), estimate)) +
  geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = estimate_conf.low, ymax = estimate_conf.high), width = .2)
