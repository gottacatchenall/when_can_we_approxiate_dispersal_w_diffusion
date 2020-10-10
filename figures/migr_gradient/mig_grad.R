library(ggplot2)
library(gridExtra)
library(tidyverse)
library(latex2exp)
library(ggthemr) # devtools::install_github("")
library(extrafont)
setwd("~/MetapopulationDynamics/migr_gradient/")
loadfonts()
string_to_tex = function(str) { return(TeX(str)) }

ggthemr('fresh', spacing=2, layout='scientific')

## PCC_diff_example
setwd("~/MetapopulationDynamics/migr_gradient/")
migr_grad_over_lambda = read.csv('output.csv') %>%
  group_by(treatment) %>%
  full_join(read.csv('metadata.csv'),by='treatment') %>% 
  filter(alpha == 0 & lambda %in% c(5,8,11))  %>%
  mutate(lambda_label = paste("$" , "\\lambda = ", lambda,  "$",sep='')) %>%
  mutate(lambda_label = factor(lambda_label, levels = c("$\\lambda = 2$","$\\lambda = 5$", "$\\lambda = 8$", "$\\lambda = 11$","$\\lambda = 14$"))) %>%
  mutate(model_label = ifelse(model == "RickerModelWDiffusionDispersal", "Diffusion" , "Stochastic Dispersal")) %>%
  mutate(upper975=quantile(PCC, 0.975)) %>%
  mutate(upper75=quantile(PCC, 0.75)) %>%
  mutate(mid=mean(PCC, 0.5)) %>%
  mutate(lower25=quantile(PCC, 0.25)) %>%
  mutate(lower025=quantile(PCC,0.025)) %>%
  ggplot(aes(migration_probability, PCC,fill=factor(model_label))) +
    labs(fill='') +
    geom_line(aes(migration_probability, mid),size=1.2, alpha=1) + 
#  geom_point(shape=1, alpha=0.1) + 
    geom_ribbon(aes(ymin=lower025, ymax=upper975),color=NA,alpha=0.25)+
    geom_ribbon(aes(ymin=lower25, ymax=upper75), color=NA, alpha=0.5)+
    labs(x=TeX("$m$"), color="") +  theme(aspect.ratio=1.0, 
                  text=element_text(family="LM Roman 10", size=20),
                  axis.text = element_text(size=12),
                  axis.title.y = element_text(angle=0, vjust=0.5),
                  strip.text.y = element_text(angle=0),
                  panel.border = element_rect(fill=NA,color="#222222", size=1)
  ) + facet_wrap(. ~ (lambda_label), labeller = as_labeller(string_to_tex, default=label_parsed))+
  scale_fill_manual(values= c("#394d56", "#40967b")) 

#colors=c("#353945","#394d56", "#19b4ca", "#40967b", "#d8c256", "#f4ad45", "#fb5a3d"),  
ggsave("migr_grad_over_lambda.png",plot=migr_grad_over_lambda, device=png(), width=12, height=8)




## alpha
setwd("~/MetapopulationDynamics/migr_gradient/")
migr_grad_over_alpha =  read.csv('output.csv') %>%
  group_by(treatment) %>%
  full_join(read.csv('metadata.csv'),by='treatment') %>% 
  filter(lambda %in% c(8))  %>%
  mutate(alpha_facet = paste("$", "\\alpha = ", alpha, "$", sep="")) %>%
  mutate(lambda_label = paste("$" , "\\lambda = ", lambda,  "$",sep='')) %>%
  mutate(model_label = ifelse(model == "RickerModelWDiffusionDispersal", "Diffusion" , "Stochastic Dispersal")) %>%
  mutate(upper975=quantile(PCC, 0.975)) %>%
  mutate(upper75=quantile(PCC, 0.75)) %>%
  mutate(mid=mean(PCC, 0.5)) %>%
  mutate(lower25=quantile(PCC, 0.25)) %>%
  mutate(lower025=quantile(PCC,0.025)) %>%
  ggplot(aes(migration_probability, PCC, fill=factor(model_label))) +
      geom_line(size=1) + 
      geom_ribbon(aes(ymin=lower025, ymax=upper975),alpha=0.25)+
      geom_ribbon(aes(ymin=lower25, ymax=upper75), alpha=0.5)+ 
      labs(x=TeX("$m$"), color="") + 
      labs(x=TeX("m"), y="PCC", fill="") +
      theme(aspect.ratio=1.0, 
                                        text=element_text(family="LM Roman 10", size=20),
                                        axis.text = element_text(size=12),
                                        axis.title.y = element_text(angle=0, vjust=0.5),
                                        strip.text.y = element_text(angle=0),
                                        panel.border = element_rect(fill=NA,color="#222222", size=1)
    ) + 
    facet_wrap(. ~ (alpha_facet), labeller = as_labeller(string_to_tex, default=label_parsed)) +
    scale_fill_manual(values= c("#394d56", "#40967b")) 
ggsave("migr_grad_over_alpha.png",plot=migr_grad_over_alpha, device=png(), width=8, height=8)
#colors=c("#353945","#394d56", "#19b4ca", "#40967b", "#d8c256", "#f4ad45", "#fb5a3d"),  


## lambda and alpha grid with confidence intervals
setwd("~/MetapopulationDynamics/migr_gradient/")
lambda_and_alpha_grid = read.csv('output.csv') %>%
  group_by(treatment) %>%
  full_join(read.csv('metadata.csv'),by='treatment') %>% 
  mutate(lambda_facet = factor(paste("$\\lambda = ", lambda,  "$",sep=""), levels=c("$\\lambda = 2$","$\\lambda = 5$","$\\lambda = 8$","$\\lambda = 11$","$\\lambda = 14$"))) %>%
  mutate(alpha_facet = paste("$", "\\alpha = ", alpha, "$", sep="")) %>%
  mutate(model_label = ifelse(model == "RickerModelWDiffusionDispersal", "Diffusion" , "Stochastic Dispersal")) %>%
  mutate(upper975=quantile(PCC, 0.975)) %>%
  mutate(upper75=quantile(PCC, 0.75)) %>%
  mutate(mid=mean(PCC, 0.5)) %>%
  mutate(lower25=quantile(PCC, 0.25)) %>%
  mutate(lower025=quantile(PCC,0.025)) %>%
  ggplot(aes(migration_probability, mid, fill=factor(model_label))) + 
    geom_line(size=0.8) +
    geom_ribbon(aes(ymin=lower025, ymax=upper975),alpha=0.25)+
    geom_ribbon(aes(ymin=lower25, ymax=upper75), alpha=0.5)+ 
    facet_grid(vars(alpha_facet),vars(lambda_facet), labeller = as_labeller(string_to_tex, default=label_parsed)) +
    labs(x=TeX("m"), y="PCC", fill="") +
    theme(aspect.ratio=1.0, 
        text=element_text(family="LM Roman 10", size=20),
        axis.text = element_text(size=8),
        axis.title.y = element_text(angle=0, vjust=0.5),
        strip.text.y = element_text(angle=0),
        panel.border = element_rect(fill=NA,color="#222222", size=1)) +
  scale_fill_manual(values= c("#394d56", "#40967b")) 

ggsave("lambda_and_alpha.png",plot=lambda_and_alpha_grid, device=png(), width=12, height=8)
