#graph regions 

regions <- split(lbsample, lbsample$Region)

regn <- table(regions[["North"]]$DTNASC, regions[["North"]]$AgeGroup2) %>% 
  prop.table(margin = 1) %>% 
  as.data.frame() %>% 
  mutate(group = 'North')

regne <- table(regions[["Northeast"]]$DTNASC, regions[["Northeast"]]$AgeGroup2) %>% 
  prop.table(margin = 1) %>% 
  as.data.frame() %>% 
  mutate(group = 'Northeast')

regcw <- table(regions[["Central-West"]]$DTNASC, regions[["Central-West"]]$AgeGroup2) %>% 
  prop.table(margin = 1) %>% 
  as.data.frame() %>% 
  mutate(group = 'Central-West')

regs <- table(regions[["South"]]$DTNASC, regions[["South"]]$AgeGroup2) %>% 
  prop.table(margin = 1) %>% 
  as.data.frame() %>% 
  mutate(group = 'South')

regse <- table(regions[["Southeast"]]$DTNASC, regions[["Southeast"]]$AgeGroup2) %>% 
  prop.table(margin = 1) %>% 
  as.data.frame() %>% 
  mutate(group = 'Southeast')

br <- table(lbsample$DTNASC, lbsample$AgeGroup2) %>% 
  prop.table(margin = 1) %>% 
  as.data.frame() %>% 
  mutate(group = 'Brazil')

all <- rbind(regn, regne, regcw, regs, regse, br)

#reorder level groups 
all$group <- factor(all$group, levels = c("North", "Northeast", "Central-West", "South", "Southeast", "Brazil"))

#plot age group 0-19
fig1 <- all %>% 
  filter(Var2 == '0-19') %>% 
  group_by(Var1) %>%
  ggplot(aes(
    x = Var1,
    y = (Freq) * 100, group = group, color = group, type = group)) +
  labs(x = "Year", 
       y = "Percentage of births") +
  geom_line(size = 1.5) +
  scale_colour_manual(values = c("#B33F62", "#264653","#F3C677","#2A9D8F","#F9564F", "grey53"),
                      name = 'Region',
                      guide = guide_legend(title.position = "top")) + 
  geom_point(size = 2.5) +
  theme_bw() +
  theme(text = element_text(size=18)) + 
  theme(legend.title.align = 0.5,
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16)) +
  theme(panel.grid.major.x = element_blank()) +
  theme(axis.text.x = element_text(size = 14))


#graph ethnicity

eth <- split(lbsample, lbsample$RACACOR)
levels(lbsample$RACACOR)

white <- table(eth[["White"]]$DTNASC, eth[["White"]]$AgeGroup2) %>% 
  prop.table(margin = 1) %>% 
  as.data.frame() %>% 
  mutate(group = 'White')

black <- table(eth[["Black"]]$DTNASC, eth[["Black"]]$AgeGroup2) %>% 
  prop.table(margin = 1) %>% 
  as.data.frame() %>% 
  mutate(group = 'Black')

asian <- table(eth[["Asian"]]$DTNASC, eth[["Asian"]]$AgeGroup2) %>% 
  prop.table(margin = 1) %>% 
  as.data.frame() %>% 
  mutate(group = 'Asian')

parda <- table(eth[["Parda"]]$DTNASC, eth[["Parda"]]$AgeGroup2) %>% 
  prop.table(margin = 1) %>% 
  as.data.frame() %>% 
  mutate(group = 'Parda')

indigenous <- table(eth[["Indigenous"]]$DTNASC, eth[["Indigenous"]]$AgeGroup2) %>% 
  prop.table(margin = 1) %>% 
  as.data.frame() %>% 
  mutate(group = 'Indigenous')

all_eth <- rbind(white, black, asian, parda, indigenous)

#reorder level groups 
all_eth$group <- factor(all_eth$group, levels = c("White", "Black", "Asian", "Parda", "Indigenous"))

#plot age group 0-19
fig2 <- all_eth %>% 
  filter(Var2 == '0-19') %>% 
  group_by(Var1) %>%
  ggplot(aes(
    x = Var1,
    y = (Freq) * 100, group = group, color = group, type = group)) +
  labs(x = "Year", 
       y = "Percentage of births") +
  geom_line(size = 1.5) +
  scale_colour_manual(values = c("#B33F62", "#264653","#F3C677","#2A9D8F","#F9564F"),
                      name = 'Ethnicity',
                      guide = guide_legend(title.position = "top")) + 
  geom_point(size = 2.5) +
  theme_bw() +
  theme(text = element_text(size=18)) + 
  theme(legend.title.align = 0.5,
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16)) +
  theme(panel.grid.major.x = element_blank()) +
  theme(axis.text.x = element_text(size = 14))

#graph education

educ <- split(lbsample, lbsample$educ)
levels(lbsample$educ)

years1 <- table(educ[["0-7 years"]]$DTNASC, educ[["0-7 years"]]$AgeGroup2) %>% 
  prop.table(margin = 1) %>% 
  as.data.frame() %>% 
  mutate(group = '0-7 years')

years2 <- table(educ[["8-11 years"]]$DTNASC, educ[["8-11 years"]]$AgeGroup2) %>% 
  prop.table(margin = 1) %>% 
  as.data.frame() %>% 
  mutate(group = '8-11 years')

years3 <- table(educ[["12 years or more"]]$DTNASC, educ[["12 years or more"]]$AgeGroup2) %>% 
  prop.table(margin = 1) %>% 
  as.data.frame() %>% 
  mutate(group = '12 years or more')

all_educ <- rbind(years1, years2, years3)

#reorder levels of educ
all_educ$group <- factor(all_educ$group, levels = c("0-7 years", "8-11 years", "12 years or more"))

#plot age group 0-19
fig3 <- all_educ %>% 
  filter(Var2 == '0-19') %>% 
  group_by(Var1) %>%
  ggplot(aes(
    x = Var1,
    y = (Freq) * 100, group = group, color = group, type = group)) +
  labs(x = "Year", 
       y = "Percentage of births") +
  geom_line(size = 1.5) +
  scale_colour_manual(values = c("#B33F62", "#F9564F", "#F3C677"),
                      name = 'Completed years of education',
                      guide = guide_legend(title.position = "top")) + 
  geom_point(size = 2.5) +
  theme_bw() +
  theme(text = element_text(size=18)) + 
  theme(legend.title.align = 0.5,
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16)) +
  theme(panel.grid.major.x = element_blank()) +
  theme(axis.text.x = element_text(size = 14))



