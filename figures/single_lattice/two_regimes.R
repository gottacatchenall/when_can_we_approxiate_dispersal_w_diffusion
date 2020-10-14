library(tidyverse)
library(ggthemr)
library(extrafont)
library(latex2exp)
loadfonts()

lambda_vals = seq(1,50,by=0.5)
m_vals = seq(0,1, by=0.01)
lattice_size = length(m_vals)*length(lambda_vals)
df = data.frame(matrix(ncol=3, nrow=lattice_size)) 
colnames(df) = c("m", "lambda", "val")


ggthemr('fresh', layout='scientific', spacing=5)

setwd("~/papers/when_can_we_approxiate_dispersal_w_diffusion/figures/single_lattice")
lattice_plot = read.csv('output.csv') %>%
    group_by(treatment) %>%
    summarize(pcc=mean(PCC)) %>%
    full_join(read.csv('metadata.csv'),by='treatment') %>%
    select(-treatment) %>%
    group_by(lambda, migration_probability) %>%
    spread(model, pcc)  %>%
    mutate(pcc_diff=abs(`RickerModelWStochasticDispersal()`-`RickerModelWDiffusionDispersal()`)) %>%
    ggplot(aes(lambda, migration_probability, fill=pcc_diff)) +
    geom_tile( color="#222222",size=0.15) +
    labs(y=TeX("$m$"), x=TeX("$\\lambda$"), fill=TeX("$PCC_{diff}$")) +
    scale_x_continuous(breaks=seq(2,14,by=1), expand=c(0,0)) +
    scale_y_continuous(expand = c(0,0), limits = c(0,1), breaks=c(0,0.25,0.5,0.75,1)) +
    theme(text=element_text(family="LM Roman 10", size=20)) +
    theme(aspect.ratio=1.0, 
          panel.border = element_rect(fill=NA,color="#222222", size=1),
          axis.title.y = element_text(angle=0, vjust=0.5),
          
    ) + 
    scale_fill_stepsn(
        colors=c("#353945","#394d56", "#19b4ca", "#40967b", "#d8c256", "#f4ad45", "#fb5a3d"),  
        breaks=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7), limits=c(0,0.75)) 

lattice_plot 


# pcc diff example panel
setwd("~/papers/when_can_we_approxiate_dispersal_w_diffusion/figures/migr_gradient/")
pcc_diff_panel = read.csv('output.csv') %>%
    group_by(treatment) %>%
    full_join(read.csv('metadata.csv'),by='treatment') %>% 
    filter(number_of_populations==10) %>%
    filter(alpha == 0)  %>%
    filter(lambda == 5)  %>%
    mutate(lambda_label = paste("$" , "\\lambda = ", lambda,  "$",sep='')) %>%
    mutate(lambda_label = factor(lambda_label, levels = c("$\\lambda = 2$","$\\lambda = 5$", "$\\lambda = 8$", "$\\lambda = 11$","$\\lambda = 14$"))) %>%
    mutate(model_label = ifelse(model == "RickerModelWDiffusionDispersal", "Diffusion" , "Stochastic Dispersal")) %>%
    mutate(mid=mean(PCC, 0.5)) %>%
    ggplot(aes(migration_probability, mid,color=factor(model_label))) +
    labs(color='') +
    geom_point(aes(migration_probability, mid),size=1.2, shape=1,alpha=1) + 
    geom_line(size=0.6) +
    labs(x=TeX("$m$"), y=TeX("$PCC$"), color="") + 
    theme(aspect.ratio=1.0, 
          legend.position = 'top',
          text=element_text(family="LM Roman 10", size=20),
          axis.text = element_text(size=12),
          axis.title.y = element_text(angle=0, vjust=0.5),
          strip.text.y = element_text(angle=0),
          panel.border = element_rect(fill=NA,color="#222222", size=1)
    ) +
    scale_fill_manual(values= c("#394d56", "#40967b")) 
pcc_diff_panel


lay <- rbind(c(NA,NA,NA,NA,2,2,2,2),
             c(1,1,1,NA,2,2,2,2),
             c(1,1,1,NA,2,2,2,2),
             c(1,1,1,NA,2,2,2,2))
lattice_combined = grid.arrange(pcc_diff_panel,lattice_plot, layout_matrix = lay)
lattice_combined


setwd("~/papers/when_can_we_approxiate_dispersal_w_diffusion/figures/single_lattice")
ggsave('lattice_combined_unannotated.png', plot=lattice_combined, device=png(), width=18, height=10)
