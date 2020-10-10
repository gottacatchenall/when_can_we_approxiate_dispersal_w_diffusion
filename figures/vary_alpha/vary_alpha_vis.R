library(ggplot2)
library(gridExtra)
library(tidyverse)
library(latex2exp)
library(extrafont)
setwd("~/MetapopulationDynamics/vary_alpha/")
loadfonts()
string_to_tex = function(str) { return(TeX(str)) }



plt =read.csv('output.csv') %>%
  group_by(treatment) %>%
  summarize(pcc=mean(PCC)) %>%
  full_join(read.csv('metadata.csv'),by='treatment') %>%
  select(-treatment) %>%
  group_by(lambda, migration_probability) %>%
  spread(model, pcc)  %>%
  mutate(alpha_facet = paste("$", "\\alpha = ", alpha, "$", sep="")) %>%
  mutate(pcc_diff=abs(RickerModelWStochasticDispersal-RickerModelWDiffusionDispersal)) %>%
  ggplot(aes(lambda, migration_probability, fill=pcc_diff)) +
  geom_tile( color="#222222",size=0.15) +
  labs(y=TeX("$m$"), x=TeX("$\\lambda$"), fill=TeX("$PCC_{diff}$")) +
  scale_x_continuous(breaks=seq(2,14,by=1), expand=c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(0,1), breaks=c(0,0.25,0.5,0.75,1)) +
  theme(text=element_text(family="LM Roman 10", size=20)) +
  facet_wrap(. ~ (alpha_facet), ncol=2, labeller = as_labeller(string_to_tex, default=label_parsed))+
  theme(aspect.ratio=1.0, 
        text=element_text(family="LM Roman 10", size=20),
        axis.text = element_text(size=12),
        legend.text = element_text(size=11),
        axis.title.y = element_text(angle=0, vjust=0.5),
        panel.border = element_rect(fill=NA,color="#222222", size=1)
  ) +
  scale_fill_stepsn(
    colors=c("#353945","#394d56", "#19b4ca", "#40967b", "#d8c256", "#f4ad45", "#fb5a3d"),  
    breaks=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7,0.8), limits=c(0,0.75)) 

plt

ggsave("vary_alpha.png", plot=plt, device=png(), width=10, height=10)


