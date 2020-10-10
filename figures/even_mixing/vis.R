library(ggplot2)
library(ggnet)
library(gridExtra)
library(tidyverse)
library(extrafont)
library(latex2exp)
library(extrafont)
setwd("~/MetapopulationDynamics/even_mixing/")
loadfonts()
string_to_tex = function(str) { return(TeX(str)) }
ggthemr('fresh', spacing=2, layout = 'scientific')

evenmixing = read.csv('output.csv') %>%
  group_by(treatment) %>%
  summarize(pcc=mean(PCC), mean_abd=mean(MeanAbundance)) %>%
  full_join(read.csv('metadata.csv'), by='treatment') %>%
  ungroup() %>%
  group_by(number_of_populations, alpha) %>%
  mutate(alpha_facet = paste("$", "\\alpha = ", alpha, "$", sep="")) %>%
  filter(pcc == max(pcc)) %>%
  ggplot(aes(number_of_populations, migration_probability,  shape=factor(alpha), color=factor(alpha))) + 
    geom_point(size=3) +
    stat_function(fun = function(x){ 1- (1/x)}) +
    facet_wrap(. ~ alpha_facet,labeller=as_labeller(string_to_tex, default=label_parsed) ) +
    labs(x=TeX("$N_p$"), y=TeX("$m*$"), color=TeX("$\\alpha$"), shape=TeX("$\\alpha$")) +
    theme(aspect.ratio=1.0, 
        text=element_text(family="LM Roman 10", size=20),
        axis.text.y = element_text(size=10),
        axis.text.x = element_text(size=10),
        legend.text = element_text(size=14),
        axis.title.y= element_text(angle=0, vjust=0.5 ),
        panel.border = element_rect(fill=NA,color="#222222", size=1),
        strip.text.y = element_text(angle=0),
        strip.text.x = element_text(size=16)
    ) +
  scale_y_continuous(expand = c(0.03,0.03), limits = c(0.4,1),  breaks=c(0.4,0.6,0.8, 1)) +
    scale_color_manual(values=c("#394d56", "#19b4ca", "#40967b",  "#fb5a3d")) +
  scale_shape_manual(values=c(1,2,5,0))
  


leftpanel = read.csv('output.csv') %>%
  group_by(treatment) %>%
  summarize(pcc=mean(PCC), mean_abd=mean(MeanAbundance)) %>%
  full_join(read.csv('metadata.csv'), by='treatment') %>%
  filter(number_of_populations==8, alpha==3) %>%
  ggplot(aes(migration_probability, pcc, color=factor(model))) + 
  geom_point(shape=1) +
  geom_line() +
  labs(y=TeX("$PCC$"), x=TeX("$m$")) +
  theme(aspect.ratio=1.0, 
        legend.position = 'none',
        text=element_text(family="LM Roman 10", size=20),
        axis.text = element_text(size=10),
        legend.text = element_text(size=14),
        axis.title=element_text(size=14),
        axis.title.y= element_text(angle=0, vjust=0.5 ),
        panel.border = element_rect(fill=NA,color="#222222", size=1),
        strip.text.y = element_text(angle=0),
        strip.text.x = element_text(size=16),
      
  ) +
  
    scale_color_manual(values=c("#19b4ca", "#40967b",  "#fb5a3d")) +
    geom_vline(aes(xintercept=0.75), size=1.5, color='#394d56',linetype='dashed')

lay <- rbind(c(NA,NA,NA,2,2,2,2),
             c(1,1,NA,2,2,2,2),
             c(1,1,NA,2,2,2,2),
             c(NA,NA,NA,2,2,2,2))
evenmixingfig = grid.arrange(leftpanel,evenmixing, layout_matrix = lay)
ggsave("even_mixing.png",plot=evenmixingfig, device=png(), width=14, height=10)




# number of populations facet wrap
nps = seq(2,25)p
np_levels = paste("$N_p = ", nps, "$", sep='')
num_pops_facet = read.csv('output.csv') %>%
  group_by(treatment) %>%
  summarize(pcc=mean(PCC), mean_abd=mean(MeanAbundance)) %>%
  full_join(read.csv('metadata.csv'),by='treatment') %>%
  mutate(np_facet = paste("$", "N_p = ", number_of_populations, "$", sep="")) %>%
  mutate(np_facet = factor(np_facet, levels=np_levels)) %>%
  ggplot(aes(migration_probability, pcc, color=factor(alpha), group=factor(alpha))) +
  geom_line(size=1.3, alpha=0.9) +
  labs(y=TeX("$PCC$"), x=TeX("$m$"), color=TeX("$\\alpha$")) +
  scale_x_continuous(expand = c(0.03,0.03), breaks=c(0, 0.25, 0.5, 0.75, 1), limits=c(0,1)) +
  scale_y_continuous(expand = c(0.03,0.03), limits = c(0,1),  breaks=c(0,0.25,0.5,0.75,1)) +
  theme(aspect.ratio=1.0, 
        text=element_text(family="LM Roman 10", size=20),
        axis.text.y = element_text(size=7),
        axis.text.x = element_text(size=7),
        legend.text = element_text(size=14),
        axis.title.y= element_text(angle=0, vjust=0.5 ),
        panel.border = element_rect(fill=NA,color="#222222", size=1),
        strip.text.y = element_text(angle=0),
        strip.text.x = element_text(size=14)
  ) +
  scale_shape_manual(values=c(1,2,5,0)) +
  scale_color_manual(values=c("#394d56", "#19b4ca", "#40967b",  "#fb5a3d")) + 
  facet_wrap(. ~ (np_facet),
             nrow=4,
             labeller=as_labeller(string_to_tex, default=label_parsed)) 
ggsave("num_pops_facet.png",plot=num_pops_facet, device=png(), width=12, height=10)
