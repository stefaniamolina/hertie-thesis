#create random sample 
lbsample <- sample_n(lbdata, 1000000) #random sample of 1000000
save(lbsample, file = "lbsample.Rda")

#create age groups 
AgeGroup <- cut(lbsample$IDADEMAE, breaks = c(0, 14, 19, 24, 29, 34, 39, Inf),
                labels = c('10-14', '15-19', '20-24', '25-29', '30-34', '35-39', '>39'))
lbsample <- lbsample %>% mutate(AgeGroup = AgeGroup)

#create year groups 
lbsample$YearGroup[(lbsample$DTNASC == "2000")|(lbsample$DTNASC == "2001")|(lbsample$DTNASC == "2002")|(lbsample$DTNASC == "2003")|(lbsample$DTNASC == "2004")|(lbsample$DTNASC == "2005")|(lbsample$DTNASC == "2006")] <- "2000-2006"
lbsample$YearGroup[(lbsample$DTNASC == "2007")|(lbsample$DTNASC == "2008")|(lbsample$DTNASC == "2009")|(lbsample$DTNASC == "2010")|(lbsample$DTNASC == "2011")|(lbsample$DTNASC == "2012")|(lbsample$DTNASC == "2013")] <- "2007-2013"
lbsample$YearGroup[(lbsample$DTNASC == "2014")|(lbsample$DTNASC == "2015")|(lbsample$DTNASC == "2016")|(lbsample$DTNASC == "2017")|(lbsample$DTNASC == "2018")|(lbsample$DTNASC == "2019")] <- "2014-2019"
lbsample$YearGroup <- as.factor(lbsample$YearGroup) #transform YearGroup in factor 

#prop tables

#year percentage by age group 
t1 <- table(lbsample$YearGroup, lbsample$AgeGroup)
propeyear <- prop.table(t1, 2)

#order of birth percentage by age group 
t2 <- table(lbsample$order_dummy, lbsample$AgeGroup)
proporder <- prop.table(t2, 2)

#level of education percentage by age group 
t3 <- table(lbsample$educ, lbsample$AgeGroup)
propeducation <- prop.table(t3, 2)

#ethnicity percentage by age group
t4 <- table(lbsample$RACACOR, lbsample$AgeGroup)
propethnicity <- prop.table(t4, 2)

#region percentage by age group
t5 <- table(lbsample$region, lbsample$AgeGroup)
propregion <- prop.table(t5, 2)

#marital status percentage by age group
t6 <- table(lbsample$ESTCIVMAE, lbsample$AgeGroup)
propmarital <- prop.table(t6, 2)


#logistic regression

#model1
logit1 <- glm(age_dummy ~ YearGroup + order_dummy + educ, family=binomial, data=lbsample)
summary(logit1)

OR1 <- exp(coef(logit1))

#model2
lbsample$Region <- relevel(lbsample$Region, ref = "Southeast") #change reference
logit2 <- glm(age_dummy ~ Region + YearGroup + order_dummy, family=binomial, data=lbsample)
summary(logit2)

OR2 <- exp(coef(logit2))

#model3
logit3 <- glm(age_dummy ~ YearGroup + order_dummy + Region + educ, family=binomial, data=lbsample)
summary(logit3)

OR3 <- exp(coef(logit3))

#model4
lbsample$RACACOR <- relevel(lbsample$RACACOR, ref = "White") #change reference
logit4 <- glm(age_dummy ~ YearGroup + order_dummy + RACACOR, family=binomial, data=lbsample)
summary(logit4)

OR4 <- exp(coef(logit4))

#model5 
logit5 <- glm(age_dummy ~ YearGroup + order_dummy + RACACOR + educ, family=binomial, data=lbsample)
summary(logit5)

OR5 <- exp(coef(logit5))

#model 6
logit6 <- glm(age_dummy ~ YearGroup + order_dummy + RACACOR + Region + educ, family=binomial, data=lbsample)
summary(logit6)

OR6 <- exp(coef(logit6))

#interaction year and region (education in the model)

logit7 <- glm(age_dummy ~ Region:DTNASC + educ + order_dummy + RACACOR, data=lbsample, family=binomial())
summary(logit6)

predict1 <- ggeffects::ggpredict(logit7, 
                                 terms = c("DTNASC", "Region"))

plot1 <- ggplot(predict1, aes(x = x, y = predicted, group=group)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = .15) +
  ylab("Probabilty") +
  xlab("Interaction: Year and Region") +
  geom_line(aes(colour = group), size = 1.5) +
  ylim(0,0.22) + 
  scale_colour_manual(values = c("#B33F62", "#264653","#F3C677","#2A9D8F","#F9564F", "grey53"),
                      name = 'Region',
                      guide = guide_legend(title.position = "top")) +
  scale_fill_manual(values = c("#B33F62", "#264653","#F3C677","#2A9D8F","#F9564F", "grey53"),
                    name = 'Region',
                    guide = guide_legend(title.position = "top")) +
  theme_bw() +
  theme(text = element_text(size=18)) + 
  theme(legend.position="right", 
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.title.align=0.5) +
  theme(panel.grid.major.x = element_blank()) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

# interaction year and region (no education in the model)
logit8 <- glm(age_dummy ~ Region:DTNASC + order_dummy + RACACOR, data=lbsample, family=binomial())
summary(logit8)

predict2 <- ggeffects::ggpredict(logit8, 
                                 terms = c("DTNASC", "Region"))

plot2 <- ggplot(predict2, aes(x = x, y = predicted, group=group)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = .15) +
  ylab("Probabilty") +
  xlab("Interaction: Year and Region") +
  geom_line(aes(colour = group), size = 1.5) +
  ylim(0,0.22) + 
  scale_colour_manual(values = c("#B33F62", "#264653","#F3C677","#2A9D8F","#F9564F", "grey53"),
                      name = 'Region',
                      guide = guide_legend(title.position = "top")) +
  scale_fill_manual(values = c("#B33F62", "#264653","#F3C677","#2A9D8F","#F9564F", "grey53"),
                    name = 'Region',
                    guide = guide_legend(title.position = "top")) +
  theme_bw() +
  theme(text = element_text(size=18)) + 
  theme(legend.position = "none") +
  theme(panel.grid.major.x = element_blank()) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())
