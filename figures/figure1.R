library(ggplot2)
library(gridExtra)
library(tidyverse)
library(extrafont)
library(latex2exp)
setwd("~/phase-transitions-in-metapopulation-synchrony/figs/figure1_metapopulations_as_a_network/output")
loadfonts()
## ============================================
## DISPERSAL KERNEL PLOTTING / FIG 1
## ============================================



exp_kern = function(alpha, d_ij){
    return(exp(-1*alpha*d_ij))
}

get_dispersal_potential = function(x_vals=runif(10), y_vals=runif(10), alpha=3, epsilon=0.01){

    n_pops = length(x_vals)

    dispersal_potential = matrix(nrow=n_pops, ncol=n_pops)
    for (pt_ct in seq(1, n_pops)){
        s = 0
        for (pt_ct2 in seq(1, n_pops)){
            
            if (pt_ct != pt_ct2){
                x1 = x_vals[pt_ct]
                y1 = y_vals[pt_ct]
                x2 = x_vals[pt_ct2]
                y2 = y_vals[pt_ct2]
    
                dist = sqrt((x2-x1)^2 + (y2-y1)^2)
                kernval = exp_kern(alpha, dist)
                if (kernval < epsilon){
                    dispersal_potential[pt_ct, pt_ct2] = 0
                }
                else{
                    dispersal_potential[pt_ct, pt_ct2] = kernval
                }
                s = s + dispersal_potential[pt_ct, pt_ct2]
            }
            else{
                dispersal_potential[pt_ct, pt_ct2] = 0 
            }
        }
        for (pt_ct2 in seq(1, n_pops)){
            if (s > 0){
                dispersal_potential[pt_ct, pt_ct2]  = dispersal_potential[pt_ct, pt_ct2]  / s
            }
            else{
                dispersal_potential[pt_ct,pt_ct2] = 0
            }
        }
    }
    return(dispersal_potential)
}

get_pop_points = function(x_vals, y_vals, alpha, epsilon, kernel=exp_kern, mig=0.1, kernel_name="Exp.", xaxis=F, yaxis=F){
    # make df with all pairwise and group by indiv pairwise val
    df = data.frame(matrix(ncol=5))

    line_ct = 1
    grp_ct = 0

    # make a dis potential first
    dispersal_potential = get_dispersal_potential(x_vals, y_vals, alpha, epsilon)
    for (pt_ct in seq(1, n_pops)){
        for (pt_ct2 in seq(1, n_pops)){
            x1 = x_vals[pt_ct]
            y1 = y_vals[pt_ct]
            x2 = x_vals[pt_ct2]
            y2 = y_vals[pt_ct2]
            #opac =1
            opac = 0
            width = 0
            if (dispersal_potential[pt_ct, pt_ct2]  > 0){
              opac = 0.05 + dispersal_potential[pt_ct, pt_ct2] * 5
              width = 0.4 + dispersal_potential[pt_ct, pt_ct2] * 2 + mig
            }
        
              df[line_ct,] = c(x1,y1, grp_ct, opac, width)
              line_ct = line_ct + 1
              df[line_ct,] = c(x2,y2, grp_ct, opac, width)
              line_ct = line_ct + 1
              grp_ct = grp_ct + 1
        }
    }
    colnames(df) = c("x", "y", "grp", "opac", "width")

    plt = ggplot(df, aes(x,y, group=grp, color=as.factor(x))) +
        geom_line(alpha=df$opac, size=df$width, color='#222222')+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), panel.border = element_rect(fill=NA, size=1, colour = "#222222"))+
        geom_point(shape=1, size=8, color='black') + 
        geom_point(alpha=0.8, size=7.5) 

    if (xaxis==F){
        plt = plt + scale_x_continuous(breaks=c(0,1))
    }

    if (yaxis == F){
        plt = plt + scale_y_continuous(breaks=c(0,1))
    }

    if (xaxis == T){
        plt = plt +
        scale_x_continuous(breaks=c(0,1)) +
        labs(x=TeX("$x$"))
    }
    if (yaxis == T){
        plt = plt + scale_y_continuous(breaks=c(0,1)) + labs(y=TeX("$y$"))
    }
    title_exp = (sprintf("$\\alpha = %d$", alpha))
    plt = plt + labs(title=TeX(title_exp))


    return(plt + coord_cartesian(xlim=c(0,1), ylim=c(0,1)))
}



"""
part 2 
synchrony plots
"""

get_diffusion_matrix = function(x=runif(10),y=runif(10), alpha=3, epsilon=0.01, migration_rate=0.2){
    n_pops = length(x)
    
    dispot = get_dispersal_potential(x,y,alpha, epsilon)
    d = matrix(nrow=n_pops,ncol=n_pops)
    for (p1 in seq(1,n_pops)){
        for (p2 in seq(1,n_pops)){
            if (p1 != p2){
               d[p1,p2] = migration_rate*dispot[p1,p2]
            }
            else{
                d[p1,p2] = (1.0-migration_rate)
            }
        }
    }

    return(d)
}

abundance_matrix_to_df = function(abd_matrix, log_freq=10){
    npops = nrow(abd_matrix)
    n_timesteps = ncol(abd_matrix)
    
    nlogpts = n_timesteps/log_freq
    
    df = data.frame(pop=rep(0,nlogpts), time=rep(0,nlogpts), abundance=rep(0,nlogpts))
    
    row = 1
    for (t in seq(1,n_timesteps)){
        if (t %% log_freq == 0){
            for (p in seq(1,npops)){
                df[row,] = c(p,t,abd_matrix[p,t])   
                row = row + 1
            }
        }
    }
    
    return(df)
}

diffusion_ricker = function(x,y,
                            alpha=3, 
                            lambda = 8,
                            chi = 0.03,
                            migration_rate=0.1, 
                            epsilon=0.01,
                            n_timesteps=100
                            ){
    
    n_pops = length(x)
    
    d = get_diffusion_matrix(x=x,y=y, alpha=alpha, migration_rate=migration_rate, epsilon=epsilon) 
    abund_matrix = matrix(nrow = n_pops, ncol = n_timesteps)
    abund_matrix[,1] = floor(50+20*runif(n_pops))
    for (t in seq(2,n_timesteps)){
        for (p in seq(1, n_pops)){
            rate =  abund_matrix[p,t-1] * lambda * exp(-1*chi*abund_matrix[p,t-1])
            abund_matrix[p,t] = max(c(rpois(lambda=rate, 1), 0))
        }

        for (p1 in seq(1,n_pops)){
            for (p2 in seq(1, n_pops)){
                abund_matrix[p1, t] = abund_matrix[p1, t]  + d[p2,p1] * abund_matrix[p2,t]
            }
        }
    }
    return(abund_matrix)
}

plot_ricker = function(x,y,m,alpha, epsilon=0.01){
    ab =  diffusion_ricker(x, y, migration_rate=m, alpha=alpha, epsilon=epsilon)
    abundance_matrix_to_df(ab) %>% 
        ggplot(aes(time, abundance, group=factor(pop),color=factor(pop))) +
        geom_line(size=1.2) + 
        theme(aspect.ratio = 1, legend.position = 'none')
}


#ggthemr('fresh', spacing=1)
n_pops = 12

set.seed(5)

thm =   theme(aspect.ratio=1, text=element_text("LM Roman 10"),
          axis.text.x = element_text(size=14),
          axis.text.y = element_text(size=14),
          axis.title  = element_text(size=16),
          title = element_text(size=18),
          legend.position='none')

alphas = c(0,5,10,15)
m = c(0.3, 0.3,0.3,0.3)
epsilon = 0.01

color_set = c(  "#284e6c",
                "#0a4162",
                "#045a8d",
                "#236ad2",
                "#3297d3",
                "#4dc8f4",
                "#2ab3b5",
                "#73dfa4",
                "#41b388",
                "#398166",
                "#165b41",
                "#0b3d2a"
                )
x_vals = runif(n_pops, 0, 1)
y_vals = runif(n_pops, 0, 1)

{plt1 = get_pop_points(x_vals, y_vals, alphas[1], epsilon,  mig=m[1],  kernel=exp_kern,yaxis=T) + thm  + scale_color_manual(values = color_set)
plt2 = get_pop_points(x_vals, y_vals, alphas[2], epsilon,  mig=m[2], kernel=exp_kern, yaxis=F, xaxis=F) + thm + scale_color_manual(values = color_set)
plt3 = get_pop_points(x_vals, y_vals, alphas[3], epsilon,  mig=m[3],  kernel=exp_kern, yaxis=F, xaxis=F) + thm + scale_color_manual(values = color_set)
plt4 = get_pop_points(x_vals, y_vals, alphas[4], epsilon, mig=m[4],  kernel=exp_kern, yaxis=F, xaxis=F) + thm + scale_color_manual(values = color_set)
plt5 = plot_ricker(x_vals, y_vals, m[1], alphas[1], epsilon) + thm  + scale_color_manual(values = color_set)
plt6 = plot_ricker(x_vals, y_vals, m[2], alphas[2], epsilon) + thm + scale_color_manual(values = color_set)
plt7 = plot_ricker(x_vals, y_vals, m[3], alphas[3], epsilon) + thm  + scale_color_manual(values = color_set)
plt8 = plot_ricker(x_vals, y_vals, m[4], alphas[4], epsilon) + thm  + scale_color_manual(values = color_set)
g2 = grid.arrange(plt1, plt2, plt3, plt4, plt5,plt6,plt7,plt8, nrow=2) }


plot(g2)

setwd("~/papers/estimating_dispersal_from_synchrony/vis/")
ggsave("out.png", g2, device=png(), width=18, height=9)
