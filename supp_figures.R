require(ggplot2)
require(wesanderson)

# read in stored variable importance scores
vimp <- read.csv(paste0(outdir,"/csv/VarImp Master.csv"), header=T)

# obtain relative importance
rel_vimp <- vimp%>%
  dplyr::group_by(Source, With_SI)%>%
  dplyr::mutate(Rel_Importance = (Importance / max(Importance))*100
            )%>%
  dplyr::ungroup()

## plots
rel_vimp <- merge(rel_vimp, variable_labels, by="Variable", all.x=T)
plot_theme_labs <-   theme_bw() + 
  theme(text = element_text(family = "Arial",size = 12),
        axis.text.x = element_text(angle = 320)) + 
  labs(x = "Feature", y = "Relative Importance")
bar_color <- "black"

vimp_plot1 <- ggplot(data=filter(rel_vimp, Rel_Importance > 0 & (Source == "EMA" & With_SI == "with SI")), aes(x=reorder(Label, Rel_Importance), y=Rel_Importance)) + 
  geom_bar(stat='identity',color=bar_color) +
  scale_y_continuous(expand = c(0, 0), limits = c(0,NA)) +
  plot_theme_labs
figure_s2 <- vimp_plot1 + coord_flip()

vimp_plot2 <- ggplot(data=filter(rel_vimp, Rel_Importance > 0 & (Source == "EMA and Passive" & With_SI == "with SI")), aes(x=reorder(Label, Rel_Importance), y=Rel_Importance)) + 
  geom_bar(stat='identity',color=bar_color) +
  scale_y_continuous(expand = c(0, 0), limits = c(0,NA)) +
  plot_theme_labs
figure_s3 <- vimp_plot2 + coord_flip()

vimp_plot3 <- ggplot(data=filter(rel_vimp, Rel_Importance > 0 & (Source == "EMA" & With_SI == "without SI")), aes(x=reorder(Label, Rel_Importance), y=Rel_Importance)) + 
  geom_bar(stat='identity',color=bar_color) +
  scale_y_continuous(expand = c(0, 0), limits = c(0,NA)) +
  plot_theme_labs
figure_s4 <- vimp_plot3 + coord_flip()

timeseries_plot = analytic_dat %>%
  ggplot(aes(x = Day, y = SI_any_nextday, color = ID))+
  geom_point()+
  facet_wrap(~ID)+
  theme_bw() + 
  scale_y_continuous(breaks = seq(0,1,1),
                     limits = c(0,1)) +
  theme(legend.position = "none",
        strip.text = element_blank(),
        plot.title = element_text(size = 13, face = "bold"))+
  labs(x = "Day in Study", y = "Presence of Next-Day Suicidal Ideation (1=Yes, 0=No)")
figure_s1 <- timeseries_plot + scale_color_manual(values = wes_palette("Darjeeling2", 102, type = "continuous"))
