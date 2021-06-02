s2 <- lbsample %>%
  filter(RACACOR == 'White')

t2 <- table(s2$educ, s2$DTNASC) %>%
  prop.table(margin = 2) %>%
  as.data.frame()%>%
  mutate(group = 'White')

s3 <- lbsample %>%
  filter(RACACOR == 'Black')

t3 <- table(s3$educ, s3$DTNASC) %>%
  prop.table(margin = 2) %>%
  as.data.frame() %>%
  mutate(group = 'Black')

s4 <- lbsample %>%
  filter(RACACOR == 'Asian')

t4 <- table(s4$educ, s4$DTNASC) %>%
  prop.table(margin = 2) %>%
  as.data.frame() %>%
  mutate(group = 'Asian')

s5 <- lbsample %>%
  filter(RACACOR == 'Parda')

t5 <- table(s5$educ, s5$DTNASC) %>%
  prop.table(margin = 2) %>%
  as.data.frame() %>%
  mutate(group = 'Parda')

s6 <- lbsample %>%
  filter(RACACOR == 'Indigenous')

t6 <- table(s6$educ, s6$DTNASC) %>%
  prop.table(margin = 2) %>%
  as.data.frame() %>%
  mutate(group = 'Indigenous')


all <- rbind(t2, t3, t4, t5, t6)

#reorder levels of group
all$group <- as.factor(all$group)
levels(all$group)

all$group <- factor(all$group, levels = c("White", "Black", "Asian", "Parda", 
                                          "Indigenous"))

fig3 <- all %>% ggplot(aes(x=Var2, y=(Freq)*100, fill = Var1)) %>%
  + geom_bar(stat="identity", width = 0.8, position = "stack") %>%
  + expand_limits(y=0) %>%
  + scale_x_discrete("Years") %>%
  + scale_y_continuous("Percentage of mothers") %>%
  # + labs(title = "Completed years of education among mothers in Brazil, 2000-2019", 
  # subtitle = "Share of mothers by age group",
  # caption = "Source: DATASUS") %>% 
  + theme_bw() %>%
  + theme(text = element_text(size=18)) %>%
  + theme(axis.text.x = element_text(size = 12)) %>%
  # + theme(plot.title = element_text(color = "black", size = 14, face = "bold"),
  #   plot.subtitle = element_text(color = "black", size = 10),
  #   plot.caption = element_text(color = "black", face = "italic")) %>%
  + theme(legend.position = "bottom",
          legend.justification = "center") %>%
  + guides(colour = guide_legend(title.position = "top")) %>%
  + theme(panel.grid.major.x = element_blank()) %>%
  + theme(axis.title.x = element_blank(),
          axis.text.x = element_text(angle=90)) %>%
  + theme(strip.background = element_rect(color="white", fill="white", size=1.5, linetype="solid")) %>%
  + scale_fill_manual(values = c("#B33F62", "#F9564F", "#F3C677"),
                      name = 'Completed years of education',
                      guide = guide_legend(direction = "horizontal",
                                           title.position = "top")) %>%
  + theme(legend.title.align = 0.5,
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 14)) %>%
  + facet_wrap(~group, scales = "free")
