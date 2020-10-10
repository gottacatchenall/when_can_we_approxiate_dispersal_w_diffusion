library(tidyverse)
library(ggthemr)
library(extrafont)
library(latex2exp)
loadfonts()

setwd("~/MetapopulationDynamics/abundance_pcc_chi/")

ggthemr('fresh', layout='scientific', spacing=3)
string_to_tex = function(str) { return(TeX(str)) }

plt = read.csv('output.csv') %>%
  group_by(treatment) %>%
  summarize(pcc=mean(PCC), mean_abd=mean(MeanAbundance)) %>%
  full_join(read.csv('metadata.csv'),by='treatment') %>%
  group_by(lambda, number_of_populations, migration_probability, predation_strength,alpha)  %>%
  pivot_wider(id_cols =  c("lambda", "number_of_populations", "predation_strength", "migration_probability", "alpha"), names_from = model, values_from = c(pcc, mean_abd)) %>%
  mutate(pcc_diff=abs(pcc_RickerModelWStochasticDispersal-pcc_RickerModelWDiffusionDispersal)) %>%
  mutate(mean_mean_abd=mean(c(mean_abd_RickerModelWDiffusionDispersal, mean_abd_RickerModelWStochasticDispersal))) %>%
  mutate(chi_facet=paste("$","\\chi =",predation_strength,"$", sep='')) %>%
  mutate(npfacet=paste("$","N_p =",number_of_populations,"$", sep='')) %>%
  mutate(m_facet=paste("$m=",migration_probability,"$", sep='')) %>%
  ggplot(aes(mean_mean_abd, pcc_diff, color=lambda, group=factor(migration_probability))) +
  geom_point(size=2, shape=1) + geom_point(size=2, alpha=0.7) +
  geom_line(size=1, alpha=0.5) +
  labs(y=TeX("$PCC_{diff}$"), x=TeX("$\\bar{N}$"), fill=TeX("$PCC_{diff}$"), color=TeX("$\\lambda$")) +
  scale_x_continuous(breaks=c(25, 50, 75, 100), limits=c(25,100)) +
  scale_fill_stepsn(colors=c("#353945","#394d56", "#19b4ca", "#40967b", "#79a770", "#f4ad45", "#fb5a3d"),  breaks=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7)) +
  scale_y_continuous(breaks=c(0,0.25,0.5), limits=c(0,0.6)) +
  theme(aspect.ratio=1.0, 
        text=element_text(family="LM Roman 10", size=20),
        axis.text = element_text(size=12),
        axis.title = element_text(size=16),
        legend.text = element_text(size=14),
        axis.title.y= element_text(angle=0, vjust=0.5),
        strip.text.y = element_text(angle=0),
        panel.border = element_rect(fill=NA,color="#222222", size=1)
  ) +
  facet_grid(vars(chi_facet), vars(npfacet),labeller = as_labeller(string_to_tex, default=label_parsed)) +
  scale_color_gradientn(
    colors=c("#353945","#394d56", "#19b4ca", "#40967b", "#d8c256", "#f4ad45", "#fb5a3d"),  
    breaks=c(2,5,8,11,14)) 

plt

ggsave("abundance_chi.png",  device=png(), plot=plt,  width=10, height=10,)

