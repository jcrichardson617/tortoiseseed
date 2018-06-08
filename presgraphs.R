groupedplants_proportion_means

allplants_rate_means

allplants_proportion_means
allplants_proportion_means$mean + allplants_proportion_means$se
allplants_proportion_means[7,]$se <- 0.1071429

pd <- position_dodge(0.5)

#all species proportion####
ggplot(allplants_proportion_means, aes(species, mean, color = factor(scat),
                                 shape = factor(passage))) +
  geom_point(size = 2, position = pd) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), position = pd, width = 0.3) +
  ylab("Mean proportion germinated ± 1 SE") +
  scale_y_continuous(expand = c(0,0), breaks=seq(0,1,0.5)) +
  expand_limits(y = c(0,1)) +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(allplants_proportion_means$species))) +
  scale_color_manual(values = c("dark gray", "black")) +
  theme(axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "transparent",color = NA),
        axis.title.x = element_text(color = "black"),
        axis.title.y = element_blank(),
        axis.text = element_text(color = "black"),
        axis.ticks = element_line(color = "black"),
        plot.title = element_text(color = "black"),
        legend.position="none")
ggsave("allprop.png", width = 10, height = 5)

#all species time####
ggplot(allplants_rate_means, aes(species, mean, color = factor(scat),
                                       shape = factor(passage))) +
  geom_point(size = 2, position = pd) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), position = pd, width = 0.3) +
  ylab("Mean time to germination (days) ± 1 SE") +
  scale_y_continuous(expand = c(0,0), breaks=seq(0,50,10)) +
  expand_limits(y = c(0,50)) +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(allplants_proportion_means$species))) +
  scale_color_manual(values = c("dark gray", "black")) +
  theme(axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "transparent",color = NA),
        axis.title.x = element_text(color = "black"),
        axis.title.y = element_blank(),
        axis.text = element_text(color = "black"),
        axis.ticks = element_line(color = "black"),
        plot.title = element_text(color = "black"),
        legend.position="none")
ggsave("alltime.png", width = 10, height = 5)

#grouped proportions####
ggplot(groupedplants_proportion_means, aes(species, mean, color = factor(scat),
                                       shape = factor(passage))) +
  geom_point(size = 2, position = pd) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), position = pd, width = 0.3) +
  ylab("Mean proportion germinated ± 1 SE") +
  scale_y_continuous(expand = c(0,0), breaks=seq(0,1,0.5)) +
  expand_limits(y = c(0,1)) +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(groupedplants_proportion_means$species))) +
  scale_color_manual(values = c("dark gray", "black")) +
  theme(axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "transparent",color = NA),
        axis.title.x = element_text(color = "black"),
        axis.title.y = element_blank(),
        axis.text = element_text(color = "black"),
        axis.ticks = element_line(color = "black"),
        plot.title = element_text(color = "black"),
        legend.position="none")
ggsave("groupedprop.png", width = 8, height = 2)

#grouped species time####
ggplot(groupedplants_rate_means, aes(species, mean, color = factor(scat),
                                 shape = factor(passage))) +
  geom_point(size = 2, position = pd) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), position = pd, width = 0.3) +
  ylab("Mean time to germination (days) ± 1 SE") +
  scale_y_continuous(expand = c(0,0), breaks=seq(10,20,5)) +
  expand_limits(y = c(10,20)) +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(groupedplants_rate_means$species))) +
  scale_color_manual(values = c("dark gray", "black")) +
  theme(axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "transparent",color = NA),
        axis.title.x = element_text(color = "black"),
        axis.title.y = element_blank(),
        axis.text = element_text(color = "black"),
        axis.ticks = element_line(color = "black"),
        plot.title = element_text(color = "black"),
        legend.position="none")
ggsave("groupedtime.png", width = 8, height = 2)







#opuntia figures####
test <- perc[(perc$species == "opuntia"),]
  levels(test$passage) <- c("Gut-passed","Control")
    
  ggplot(test, aes(passage, prop, fill = factor(scat))) +
              geom_boxplot(position = position_dodge(0.9), color = "black", outlier.color = "black") +
              ylab("Proportion germinated") +
    xlab("") +
              scale_fill_grey(start = 0.9, end = 0.5) +
              scale_y_continuous(expand = c(0,0), breaks=seq(0,1,0.2)) +
    stat_summary(fun.y=median, geom="line", position=position_dodge(0.9), 
                 aes(group=scat)) +
              expand_limits(y = c(0,1)) +
              theme(axis.line = element_line(color = "black"),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_blank(),
                    panel.background = element_blank(),
                    legend.position="none",
                    axis.text.x=element_text(angle=0, hjust=0.5),
                    plot.background = element_rect(fill = "transparent",color = NA),
                    axis.title.x = element_text(color = "black"),
                    axis.title.y = element_text(color = "black"),
                    axis.text = element_text(color = "black"),
                    axis.ticks = element_line(color = "black"),
                    plot.title = element_text(color = "black"))
  ggsave("opuntiaperc.png", height = 5.57, width = 7.57, bg = "transparent")

              
  ggplot(test, aes(passage, days, fill = factor(scat))) +
              geom_boxplot(position = position_dodge(0.9), color = "black", outlier.color = "black") +
              ylab("Time to germination (days)") +
    xlab("") +          
    scale_fill_grey(start = 0.9, end = 0.5) +
              scale_y_continuous(expand = c(0,0), breaks=seq(6,24,6)) +
              expand_limits(y = c(6,24)) +
    stat_summary(fun.y=median, geom="line", position=position_dodge(0.9), 
                 aes(group=scat)) +
              theme(axis.line = element_line(color = "black"),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_blank(),
                    panel.background = element_blank(),
                    legend.position="none",
                    axis.text.x=element_text(angle=0, hjust=0.5),
                    plot.background = element_rect(fill = "transparent",color = NA),
                    axis.title.x = element_text(color = "black"),
                    axis.title.y = element_text(color = "black"),
                    axis.text = element_text(color = "black"),
                    axis.ticks = element_line(color = "black"),
                    plot.title = element_text(color = "black"))
  ggsave("opuntiatime.png", height = 5.57, width = 7.57,bg = "transparent")
  
#physalis figures####
  test <- perc[(perc$species == "physalis"),]
  
  levels(test$passage) <- c("Gut-passed","Control")
  
  ggplot(test, aes(passage, prop, fill = factor(scat))) +
    geom_boxplot(position = position_dodge(0.9), color = "black", outlier.color = "black") +
    ylab("Proportion germinated") +
    xlab("") +
    scale_fill_grey(start = 0.9, end = 0.5) +
    scale_y_continuous(expand = c(0,0), breaks=seq(0,1,0.2)) +
    stat_summary(fun.y=median, geom="line", position=position_dodge(0.9), 
                 aes(group=scat)) +
    expand_limits(y = c(0,1)) +
    theme(axis.line = element_line(color = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.position="none",
          axis.text.x=element_text(angle=0, hjust=0.5),
          plot.background = element_rect(fill = "transparent",color = NA),
          axis.title.x = element_text(color = "black"),
          axis.title.y = element_text(color = "black"),
          axis.text = element_text(color = "black"),
          axis.ticks = element_line(color = "black"),
          plot.title = element_text(color = "black"))
  ggsave("physalisperc.png", height = 5.57, width = 7.57, bg = "transparent")
  
  ggplot(test, aes(passage, days, fill = factor(scat))) +
    geom_boxplot(position = position_dodge(0.9), color = "black", outlier.color = "black") +
    ylab("Time to germination (days)") +
    xlab("") +
    scale_fill_grey(start = 0.9, end = 0.5) +
    scale_y_continuous(expand = c(0,0), breaks=seq(5,21,5)) +
    stat_summary(fun.y=median, geom="line", position=position_dodge(0.9), 
                 aes(group=scat)) +
    expand_limits(y = c(5,21)) +
    theme(axis.line = element_line(color = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          legend.position="none",
          axis.text.x=element_text(angle=0, hjust=0.5),
          plot.background = element_rect(fill = "transparent",color = NA),
          axis.title.x = element_text(color = "black"),
          axis.title.y = element_text(color = "black"),
          axis.text = element_text(color = "black"),
          axis.ticks = element_line(color = "black"),
          plot.title = element_text(color = "black"))
  ggsave("physalistime.png", height = 5.57, width = 7.57,bg = "transparent")
  
  
 
  