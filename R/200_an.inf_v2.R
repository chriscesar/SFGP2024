# 200_an.inf_v2.R ####
### Multivariate analysis of 2024 intertidal invertebrate data

# Set up ####
## load packages ####
ld_pkgs <- c("tidyverse","ggplot2","vegan",
             "mvabund",
             "tictoc"
             )
vapply(ld_pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)
rm(ld_pkgs)

tictoc::tic.clearlog();tic("SET UP")
print("Setting up")
## set metadata ####
source("R/00_meta_setMeta.R")

## load data ####
source("R/100_imp.inf0.R")
toc(log=TRUE)

# Extract current year's data ####
dfw %>% 
  dplyr::filter(.,year == cur.yr) %>% 
  dplyr::filter(.,zone1 != "Wash") %>% 
  ## remove 'empty' columns
  dplyr::select(where(~ !is.numeric(.)| !all(.==0))) %>% 
  ## calculate means across replicates
  pivot_longer(.,cols = -c(year, transect, shore,,rep,zone1, mesh, core.area_m2),
                 names_to = "Taxon", values_to = "Abundance") %>%
  ## replace -9999 with 1
  mutate(Abundance = if_else(Abundance < 0, 1, Abundance)) %>% 
  dplyr::select(.,-rep) %>% 
  group_by(across(c(!Abundance)
  )) %>% 
  summarise(.,Abundance=mean(Abundance), .groups = "drop") %>% ungroup() %>% 
  pivot_wider(.,names_from = Taxon, values_from = Abundance) %>% 
  dplyr::select(.,-AFAUNAL) -> df.cur

## isolate taxon data ####
df.cur %>% dplyr::select(.,
                         -c(year,transect,
                            shore,zone1,mesh,
                            core.area_m2)) -> df.tx
toc(log=TRUE)

# MVABUND MODELS ####
tic("MVABUND MODELS")
df.cur$zone_model <- factor(df.cur$zone1, levels = c("Inside","Above",
                                                     "Inside2","Below"))
m1 <- mvabund::manyglm(mvabund::mvabund(df.tx)~df.cur$zone_model,
                       family = "negative.binomial")
m1.summary <- summary(m1, nBoot = 9999)
anova_m1 <- mvabund::anova.manyglm(m1,p.uni = "adjusted")

m2 <- mvabund::manyglm(mvabund::mvabund(df.tx)~df.cur$zone_model*df.cur$shore,family = "negative.binomial")
m2.summary <- summary(m2, nBoot = 9999)
anova_m2 <- mvabund::anova.manyglm(m2,p.uni = "adjusted")
toc(log=TRUE)

# PLOT ####
tic("Plot")
nums <- ncol(df.tx)

df.cur$zoneplot <- factor(df.cur$zone1, levels=c("Above","Inside","Inside2","Below"))
df.cur$shore <- factor(df.cur$shore, levels=c("Mid","Low"))

png(file = "output/figs/inf.current.yr.png",
    width=12*ppi, height=10*ppi, res=ppi)
df.cur %>% 
  pivot_longer(cols = -c(year,transect,shore,zone1,mesh,core.area_m2,zone_model,
                         zoneplot), 
               names_to = "Taxon", 
               values_to = "Mean_abundance") %>% 
  ggplot(aes(
    y = Taxon,
    x = log(Mean_abundance + 1),
    # x = Mean_abundance,
    group = zoneplot
  )) +
  geom_hline(yintercept = seq(0.5, (nums+0.5), by = 1), 
             color = "gray", linetype = "dashed") +  # Add grid lines between taxa
  geom_point(position = position_jitter(width = 0.01, height=0.4,seed = pi),
             alpha=0.5,size=2,
             aes(
               #shape = zoneplot,
               col = zoneplot,
               group = zoneplot),
             show.legend = FALSE) +
  facet_grid(shore~zoneplot)+
  scale_y_discrete(limits=rev)+
  scale_color_manual(values=cbPaletteTxt)+
  labs(
    title = "Infaunal abundances of intertidal assembalges",
    subtitle = "Monitored as part of the 2024 SGPBM",
    y="",
    x="log mean abundance (n+1)"
  )+
  theme(
    strip.text = element_text(face=2,size = 12),
    axis.title.y = element_blank(),
    axis.title.x = element_text(face=2)
  )
dev.off()
toc(log=TRUE)

unlist(tictoc::tic.log())

# DEVT: Visualisation of trends ####
df$shore <- factor(df$shore, levels =c("Mid","Low"))
df$zone1 <- factor(df$zone1,levels = c("Above","Inside","Inside2","Below","Wash"))
df %>% 
  ## remove presence-only
  filter(.,count>0) %>% 
  # remove Wash samples
  filter(., zone1 !="Wash") %>% #names()
  filter(., mesh=="1.0mm") %>% 
  ## drop cols, calculate means, widen, add zeroes, lengthen
  dplyr::select(.,year,transect, shore, zone1, mesh,taxonUSE, count) %>%
  group_by(across(!count)) %>% 
  summarise(count = mean(count)) %>% 
  pivot_wider(.,names_from = taxonUSE, values_from = count,values_fill = 0) %>% 
  pivot_longer(.,cols=-c(year,transect,shore,zone1,mesh),
               names_to = "Taxon", values_to = "Count"
              ) %>%
  ggplot(., aes(y = log(Count+1), x = as.factor(year),
                colour=zone1))+
  facet_grid(shore~zone1)+
  geom_smooth(method="loess", 
              #aes(group=taxonUSE),
              se=FALSE,alpha=0.8)+
  scale_x_discrete(limits=rev)+
  geom_point(aes(group=Taxon),alpha=0.5,
             position=position_jitter(width = .25,height=0.05),
             show.legend = FALSE)+
  coord_flip()+
  scale_colour_manual(values = cbPalette)+
  #coord_fixed(ylim(0,NA))+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(face=2),
        axis.text = element_text(face=2),
        strip.text = element_text(face=2),
        legend.position = "none")
  
  

# tidy up ####
rm(list=ls(pattern = "^df"));rm(list=ls(pattern = "^m"))
rm(list=ls(pattern = "^ano"));rm(list=ls(pattern = "^cb"))
rm(cur.yr,fol,gisfol,nums,perm,ppi,sum_zero,projfol,source_file)

detach("package:tidyverse", unload=TRUE)
detach("package:vegan", unload=TRUE)
detach("package:mvabund", unload=TRUE)
detach("package:tictoc", unload=TRUE)
