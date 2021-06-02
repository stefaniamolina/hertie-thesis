#graph births by order of birth 
fig11 <- lbsample %>%
  ggplot(aes(x=IDADEMAE, fill = order_dummy), show.legend=FALSE) + 
  geom_vline(xintercept= 25,
             color= c("red"),
             linetype="dashed", size=0.8) + 
  geom_density(adjust = 3, alpha = 0.3) +
  scale_x_continuous("Age of mother", breaks=c(10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60)) +
  scale_y_continuous("Density") +
  theme_bw() + 
  theme(panel.grid.major.x = element_blank()) + 
  theme(text = element_text(size=18)) + 
  theme(legend.title.align = 0.5,
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 14)) + 
  scale_fill_manual(values = c("#264653", "#2A9D8F"), name = "Order of birth")

#calculate mean age
mean(lbsample$IDADEMAE)


