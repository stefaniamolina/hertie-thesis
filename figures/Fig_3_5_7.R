# plot educ X ethnicity                      
tethnicity <- table(lbsample$educ, lbsample$RACACOR) %>%
  prop.table(margin = 2) %>%
  as.data.frame()

fig5 <- tethnicity %>% ggplot(aes(x=Var2, y=(Freq)*100, fill = Var1)) %>%
  + geom_bar(stat="identity", width = 0.8, position = "dodge") %>%
  + expand_limits(y=0) %>%
  + scale_x_discrete("Ethnicity") %>%
  + scale_y_continuous("Percentage of mothers") %>%
  + theme_bw() %>%
  + theme(text = element_text(size=18)) %>%
  + theme(legend.title.align = 0.5,
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 14)) %>%
  + theme(panel.grid.major.x = element_blank()) %>%
  + scale_fill_manual(values = c("#B33F62", "#F9564F", "#F3C677"),
                      name = 'Completed years of education',
                      guide = guide_legend(direction = "vertical",
                                           title.position = "top"))

#plot educ X region
tregion <- table(lbsample$educ, lbsample$Region) %>%
  prop.table(margin = 2) %>%
  as.data.frame()

fig3 <- tregion %>% ggplot(aes(x=Var2, y=(Freq)*100, fill = Var1)) %>%
  + geom_bar(stat="identity", width = 0.8, position = "dodge") %>%
  + expand_limits(y=0) %>%
  + scale_x_discrete("Region") %>%
  + scale_y_continuous("Percentage of mothers") %>%
  + theme_bw() %>%
  + theme(text = element_text(size=18)) %>%
  + theme(legend.title.align = 0.5,
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 14)) %>%
  + theme(panel.grid.major.x = element_blank()) %>%
  + scale_fill_manual(values = c("#B33F62", "#F9564F", "#F3C677"),
                      name = 'Completed years of education',
                      guide = guide_legend(direction = "verical",
                                           title.position = "top"))

#plot ethnicty X region
tregion2 <- table(lbsample$RACACOR, lbsample$Region) %>%
  prop.table(margin = 2) %>%
  as.data.frame()


fig7 <- tregion2 %>% ggplot(aes(x=Var2, y=(Freq)*100, fill = Var1)) %>%
  + geom_bar(stat="identity", width = 0.8, position = "stack") %>%
  + expand_limits(y=0) %>%
  + scale_x_discrete("Region") %>%
  + scale_y_continuous("Percentage of mothers") %>%
  + theme_bw() %>%
  + theme(text = element_text(size=18)) %>%
  + theme(legend.title.align = 0.5,
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 14)) %>%
  + theme(panel.grid.major.x = element_blank()) %>%
  + scale_fill_manual(values = c("#B33F62", "#264653","#F3C677","#2A9D8F","#F9564F"),
                      name = 'Ethnicity',
                      guide = guide_legend(direction = "verical",
                                           title.position = "top"))
