## 200_an.epi.R ####
## Analysis of epifaunal data from current year and combine with historic

## load data & format ####
source("R/100_imp.epi0.R")

dfw$zone1 <- factor(dfw$zone1,levels=c("Above","Inside","Inside2","Below","Wash"))

## load packages
ld_pkgs <- c("tidyverse","ggplot2", "vegan", "lmerTest","ggpp","ggtext",
             "mvabund", "patchwork","gllvm","tictoc")
vapply(ld_pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)
rm(ld_pkgs)

tic("Set metadata & set up")
### set metadata
source("R/00_meta_setMeta.R")

cbPalette2 <- c( ### colourblind-friendly chart colour palette
  # comments reflect Red/Green/Blue equivalents
  "#0072B2", #000, 114, 178
  "#003959",
  "#e79f00", #231, 159, 0
  "#745000",
  "#009E73", #000, 158, 115
  "#004F3A",
  "#9ad0f3", #154, 208, 243
  "#4D6879",
  "#000000", #0, 0, 0
  "#D55E00", #213, 94, 0
  "#CC79A7", #204, 121, 167
  "#DEAAC6", #222, 170, 198 - The Wash
  "#F0E442"  #240, 228, 66
)

cbPalette2 <- c( ### colourblind-friendly chart colour palette
  # comments reflect Red/Green/Blue equivalents
  "#0072B2", #000, 114, 178
  "#e79f00", #231, 159, 0
  "#009E73", #000, 158, 115
  "#9ad0f3", #154, 208, 243
  "#000000", #0, 0, 0
  "#D55E00", #213, 94, 0
  "#CC79A7", #204, 121, 167
  "#DEAAC6", #222, 170, 198 - The Wash
  "#F0E442"  #240, 228, 66
)

## calculate indices ####
## Remove AFAUNAL & Flag variables
dfw$AFAUNAL <- NULL
dfw$Flag <- NULL

## faunal density (N)
tmp <- dfw %>% dplyr::select(.,-c(year,transect,zone1,depth,mon))
tmp <- apply(tmp, 2, function(x) ifelse(x == "-9999", 0, as.numeric(x)))
# Replace negative values with 0
tmp <- pmax(tmp, 0)
# Calculate row sums, ignoring values less than zero
N <- rowSums(tmp)
rm(tmp)

## species richness (S)
tmp <- dfw %>% dplyr::select(.,-c(year,transect,zone1,depth,mon))
tmp <- (tmp <0 | tmp >0)
S <- rowSums(tmp)
rm(tmp)

### explore data for current year ####
# tax rich
# dfw %>% 
#   filter(., year == cur.yr) %>% 
#   dplyr::select(.,c(1:5,N:S)) %>% 
#   View(.)
toc(log=TRUE)

# RUN MODELS #####
tic("Model setup")
### amend structure to focus models on Inside
dfw$zone1mod <- factor(dfw$zone1, levels=c("Inside","Above","Inside2","Below"))
dfw <- dfw %>% relocate(., zone1mod, .after = zone1)
dfw$zone1 <- factor(dfw$zone1, levels=c("Above","Inside","Inside2","Below"))

## append N and S
dfw$N <- N;rm(N)
dfw$S <- S;rm(S)

#### taxon density #####
dfw %>% 
  filter(.,year == cur.yr) %>% #names()
  ## pivot and rewiden to remove 'empty' cols
  pivot_longer(., cols = -c(year,transect,zone1,zone1mod,depth,mon,N,S)) %>% 
  filter(.,value != 0) %>% 
  pivot_wider(.,names_from = name, values_from = value,
              values_fill = 0) -> dfcur

N_pl <- ggplot(dfcur,aes(x=zone1,y=N,fill=zone1))+
  geom_boxplot(outlier.colour = NA,show.legend = FALSE)+
  geom_jitter(height=0,width=0.35,alpha=0.3,show.legend = FALSE)+
  scale_fill_manual(values=cbPalette);N_pl

toc(log=TRUE)

tic("Run univariate models")
summary(mod1 <- lmer(N ~ zone1mod + (1|depth) + (1|mon),
                     data=dfcur))
anova(mod1)
d <- as.data.frame(ls_means(mod1, test.effs = "Group",pairwise = TRUE))
d[d$`Pr(>|t|)`<0.051,]
# sjPlot::plot_model(mod1,show.values=TRUE, show.p=TRUE)
# visreg::visreg(mod1)
rm(mod1,d,N_pl)

#### taxon richness ####
S_pl <- ggplot(dfcur,aes(x=zone1,y=S,fill=zone1))+
  geom_boxplot(outlier.colour = NA,show.legend = FALSE)+
  geom_jitter(height=0,width=0.35,alpha=0.3,show.legend = FALSE)+
  scale_fill_manual(values=cbPalette);S_pl

summary(mod1 <- lmer(S ~ zone1mod + (1|depth) + (1|mon),
                     data=dfcur))

anova(mod1)
d <- as.data.frame(ls_means(mod1, test.effs = "Group",pairwise = TRUE))
d[d$`Pr(>|t|)`<0.051,]
# sjPlot::plot_model(mod1,show.values=TRUE, show.p=TRUE)
# visreg::visreg(mod1)
rm(mod1,d,S_pl)

##########
### to do:####
## generate time series charts

### species ####
S <- ggplot(data = dfw, aes(y = S, x = year, fill = zone1))+
  geom_hline(yintercept = mean(dfw$S,na.rm = TRUE),colour="grey",linetype="dashed")+
  geom_hline(yintercept = min(dfw$S,na.rm = TRUE),colour="grey",linetype="dotted")+
  geom_hline(yintercept = max(dfw$S,na.rm = TRUE),colour="grey",linetype="dotted")+
  geom_point()+
  #geom_boxplot(aes(group=year))+
  # geom_jitter(alpha=0.6)+
  geom_smooth(method = "loess", colour = "red", span = .9)+
  # geom_smooth(method = "loess", span = .9, aes(group=mon, col=mon))+
  # geom_smooth(method = "gam", colour = "red", span = .9)+
  facet_grid(depth~zone1)+
  scale_colour_manual(name = "", values=cbPalette)+
  scale_fill_manual(name = "", values=cbPalette)+
  scale_x_continuous(breaks = seq(2011, 2024, by = 2))+
  xlab("Year") + ylab(bquote("Taxon richness"))+
  theme(legend.position="none",
        axis.title = element_text(face=2),
        strip.text.x = element_text(size = 12,face=2),
        strip.text.y = element_text(size = 12,face=2),
        axis.text.x = element_text(angle = 270,hjust=1,vjust=0.5))+
  coord_cartesian(ylim=c(0,NA));S

# png(file = "output/figs/epi.ts.S_loess_bx.png",
#     width=12*ppi, height=6*ppi, res=ppi)
png(file = "output/figs/epi.ts.S_loess_pt.png",
    width=12*ppi, height=6*ppi, res=ppi)
# png(file = "output/figs/epi.ts.S_gam.png",
#     width=12*ppi, height=6*ppi, res=ppi)
print(S);
dev.off();
rm(S)

### TaxDensity ####
N <- ggplot(data = dfw, aes(y = log(N+1), x = year, fill = zone1))+
  geom_hline(yintercept = mean(log(dfw$N+1),na.rm = TRUE),colour="grey",linetype="dashed")+
  geom_hline(yintercept = min(log(dfw$N+1),na.rm = TRUE),colour="grey",linetype="dotted")+
  geom_hline(yintercept = max(log(dfw$N+1),na.rm = TRUE),colour="grey",linetype="dotted")+
  geom_point()+
  # geom_boxplot(aes(group=year))+
  geom_smooth(method = "loess", colour = "red", span = .9)+
  # geom_smooth(method = "gam", colour = "red", span = .9)+
  facet_grid(depth~zone1)+
  scale_colour_manual(name = "", values=cbPalette)+
  scale_fill_manual(name = "", values=cbPalette)+
  scale_x_continuous(breaks = seq(2011, 2023, by = 2))+
  xlab("Year") + ylab(bquote(bold(Log[(n+1)]~Faunal~density)))+
    #"Log""Faunal density"))+
  theme(legend.position="none",
        axis.title = element_text(face=2),
        strip.text.x = element_text(size = 12,face=2),
        strip.text.y = element_text(size = 12,face=2),
        axis.text.x = element_text(angle = 270,hjust=1,vjust=0.5))+
  coord_cartesian(ylim=c(0,NA));N

# png(file = "output/figs/epi.ts.N_loess_bx.png",
#     width=12*ppi, height=6*ppi, res=ppi)
png(file = "output/figs/epi.ts.N_loess_pt.png",
    width=12*ppi, height=6*ppi, res=ppi)
# png(file = "output/figs/epi.ts.S_gam.png",
#     width=12*ppi, height=6*ppi, res=ppi)
print(N);
dev.off();
rm(N)
toc(log=TRUE)

# MULTIVARIATE ####
#=#=#=#=#=#=#=#=#==
### Ordination ####
#=#=#=#=#=#=#=#=#==
tic("Run & plot MDS")
met <- dfcur %>% 
  dplyr::select(.,year,transect,zone1,zone1mod,depth,mon,N,S)
dat <- dfcur %>% 
  dplyr::select(.,-c(year,transect,zone1,zone1mod,depth,mon,N,S)) %>% 
  mutate_all(~ifelse(. < 0, 1, .)) %>% 
  dplyr::select(.,(names(.)[colSums(.) >= 1])) #%>%

colnames(dat) <- make.cepnames(colnames(dat)) #shorten names for display

### RUN ORDINATION ####
set.seed(pi)
ord <- metaMDS(
  dat,
  distance = "bray",
  k = 2,try = 500,
  trymax = 500,
  maxit = 1500
)

plot(ord)


### MDS plot prep ####
abv <- c("T1")
ins <- c("T4","T8")
ins2 <- "T13"
bel <- c("T20")

data.scores <- as.data.frame(scores(ord)[1])
names(data.scores) <- c("NMDS1","NMDS2")
data.scores$transect <- met$transect
data.scores$depth <- met$depth
data.scores$mon <- met$mon
data.scores$zone1 <- ifelse(data.scores$transect %in% abv, "Above",
                            ifelse(data.scores$transect %in% ins, "Inside",
                                   ifelse(data.scores$transect %in% ins2, "Inside2",
                                          ifelse(data.scores$transect %in% bel, "Below","ERROR")
                                   )))

species.scores <- as.data.frame(scores(ord, "species"))
species.scores$species <- rownames(species.scores)
data.scores$zone1 <- factor(data.scores$zone1,
                            levels =
                              c("Above", "Inside", "Inside2",
                                "Below"))

data.scores$zoneMon <- paste0(data.scores$zone1,data.scores$mon)
rm(abv,bel,ins,ins2)

### MDS classic version ####
# #### version 1 ####
# # Need to tweak model to generate output for each month
# png(
#   # file = "output/figs/epi.MDS.23.Sep.png",
#   file = "output/figs/epi.MDS.24.png",
#   width = 12 * ppi,
#   height = 8 * ppi,
#   res = ppi
# )
# 
# ggplot() +
#   geom_hline(yintercept = 0, colour = "grey") +
#   geom_vline(xintercept = 0, colour = "grey") +
#   geom_text(data = species.scores,
#             aes(x = NMDS1, y = NMDS2,
#                 label = species),
#             alpha = 0.5) + # add the species labels
#   geom_point(data = data.scores[data.scores$mon=="Oct",],
#              aes(
#                x = NMDS1,
#                y = NMDS2,
#                shape = depth,
#                colour = zone1),
#              size = 8,
#              show.legend = TRUE
#   ) +# add the point markers
#   # scale_fill_manual(values = cbPalette) +
#   scale_color_manual("Zone", values = cbPalette) +
#   scale_shape_discrete(name = "Depth") +
#   coord_fixed()+
#   # geom_text_npc(aes(npcx = .99, npcy = .99, label=paste("September")))+
#   geom_text_npc(aes(npcx = .99, npcy = .99, label=paste("October\nStress = ",
#                                                         round(ord$stress, 3))))+
#   # facet_wrap(.~mon)
#   labs(
#     # title = paste0("Nonmetric Multidimensional Scaling plot of epifauna recorded in ",cur.yr),
#     #      # caption=expression(atop(Based~on~0.1~m^2~sediment~cores~sieved~over~a~1~mm~mesh,
#     #      #                         paste("Midshore sample of transect T8 excluded"))))+
#     caption="Based on epibenthic beam trawls.<br>'Presence-only' taxon scores replaced with values of '1'")+
#   theme(plot.caption = element_markdown(lineheight = 1.2))
# 
# dev.off()

#### version 2 ####
# Alternative version
png(
  file = "output/figs/epi.MDS.24.SepOct.png",
  width = 12 * ppi,
  height = 10 * ppi,
  res = ppi
)
data.scores$mon <- factor(data.scores$mon, levels=c("Sep","Oct"))
ggplot() +
  geom_hline(yintercept = 0, colour = "grey") +
  geom_vline(xintercept = 0, colour = "grey") +
  geom_text(data = species.scores,
            aes(x = NMDS1, y = NMDS2,
                label = vegan::make.cepnames(species)),
            alpha = 0.5) + # add the species labels
  geom_point(data = data.scores,
             aes(
               x = NMDS1,
               y = NMDS2,
               shape = depth,
               fill = zone1,
               colour=mon),
             size = 7,
             #show.legend = FALSE,
             stroke=1.5 #control border thickness of points
  ) +
  scale_colour_manual("Month", values = c("red","black")) +
  scale_shape_manual("Depth",values = rep(c(21,24,22),2))+
  scale_fill_manual("Zone", values = cbPalette) +
  coord_fixed()+
  # geom_text_npc(aes(npcx = .99, npcy = .99, label=paste("September")))+
  geom_text_npc(aes(npcx = .99, npcy = .99, label=paste("Stress = ",
                                                        round(ord$stress, 3))))+
  guides(colour = "none")+
  # facet_wrap(.~mon)
  labs(
    title = paste0("Nonmetric Multidimensional Scaling plot of epifauna recorded in ",cur.yr),
    caption="Based on epibenthic beam trawls.<br>'Presence-only' taxon scores replaced with values of '1'")+
  theme(plot.caption = element_markdown(lineheight = 1.2))+
  guides(fill = guide_legend(override.aes = list(shape = 22)),
         colour = guide_legend(override.aes = list(shape=22)))
dev.off()
toc(log=TRUE)

# ADONIS  (Permanova) ####
tic("Adonis (permanova)")
ano_epiinf <- adonis2(dat ~ met$zone1,
                      permutations = perm)
saveRDS(ano_epiinf,file="output/models/epiadonis2024.Rdat")
(ano_epiinf <- readRDS(file="output/models/epiadonis2024.Rdat"))
### is P <0.05?
ano_epiinf[["Pr(>F)"]][1] <0.05 ###Is P<0.05?

###output model summaries
###full model
write.csv(ano_epiinf,
          file="output/models/ano_epiinf.csv",
          row.names=TRUE)
rm(ano_epiinf)

# ADONIS  (Permanova) ####
ano_epiinf2 <- adonis2(dat ~ met$zone1*met$depth*met$mon,
                      permutations = perm)
saveRDS(ano_epiinf2, file="output/models/epiadonis2024_full.Rdat")
(ano_epiinf2 <- readRDS(file="output/models/epiadonis2024_full.Rdat"))
### is P <0.05?
ano_epiinf2[["Pr(>F)"]][1] <0.05 ###Is P<0.05?

###output model summaries
###full model
write.csv(ano_epiinf2,
          file="output/models/ano_epiinf.full.csv",
          row.names=TRUE)
toc(log=TRUE)

# MVABUND version ####
tictoc::tic("MVABUNDs")
cur_spp <- mvabund(dat)

# mean-variance plot
png(file = "output/figs/epiMeanVar.png",
    width=12*ppi, height=6*ppi, res=ppi)
mvpl <- mvabund::meanvar.plot(cur_spp, add.trendline=TRUE,
                              xlab="Mean",
                              ylab="Variance",
                              table=TRUE
)

# Step 1: Find the minimum and maximum values
min_value <- min(mvpl[,2])
max_value <- max(mvpl[,2])

min_order <- floor(log10(min_value))
max_order <- floor(log10(max_value))
orders_of_magnitude_covered <- max_order - min_order

ttl <- "Very strong mean-variance relationship in epifaunal abundance data"
sbtt <- paste0("Variance within the dataset covers ",orders_of_magnitude_covered," orders of magnitude*.\nMany multivariate analyses (e.g. ANOSIM, PERMANOVA) assume *no mean-variance relationship*\nThis makes interpretation of such analyses potentially erroneous. Model-based approaches offer an alternative, allowing the mean-variance relationship to be incorporated into the model predictions")

mtext(side=3, line = 3, at =-0.07, adj=0, cex = 1, ttl, font=2)
mtext(side=3, line = 0.75, at =-0.07, adj=0, cex = 0.7, sbtt)
dev.off()

rm(min_value,max_value,min_order,max_order,orders_of_magnitude_covered,ttl,sbtt)

## model 1 ####

mod1_pois <- manyglm(cur_spp ~ met$zone1, family="poisson")
plot(mod1_pois)
mod1_nb <- manyglm(cur_spp ~ met$zone1mod, family="negative_binomial")
plot(mod1_nb)

# m1.sum <- summary(mod1_nb)
# saveRDS(m1.sum, file="output/models/epi.2024.model1summary.Rdat")
(m1.sum <- readRDS("output/models/epi.2024.model1summary.Rdat"))

# anova_mod1 <- mvabund::anova.manyglm(mod1_nb,p.uni = "adjusted")
# saveRDS(anova_mod1,file="output/models/epi.2024.mvabund.mod1.Rdat")
(anova_mod1 <- readRDS("output/models/epi.2024.mvabund.mod1.Rdat"))


mod2 <- manyglm(cur_spp ~ met$zone1mod*met$depth, family="negative_binomial")
plot(mod2)

# m2.sum <- summary(mod2)
# saveRDS(m2.sum, file="output/models/epi.2024.model2summary.Rdat")
(m2.sum <- readRDS("output/models/epi.2024.model2summary.Rdat"))

# anova_mod2 <- mvabund::anova.manyglm(mod2,p.uni = "adjusted")
# saveRDS(anova_mod2,file="output/models/epi.2024.mvabund.mod2.Rdat")
(anova_mod2 <- readRDS("output/models/epi.2024.mvabund.mod2.Rdat"))

mod3 <- manyglm(cur_spp ~ met$zone1mod*met$depth*met$mon, family="negative_binomial")
plot(mod3)

# m3.sum <- summary(mod3)
# saveRDS(m3.sum, file="output/models/epi.2024.model3summary.Rdat")
m3.sum <- readRDS("output/models/epi.2024.model3summary.Rdat")

# anova_mod3 <- mvabund::anova.manyglm(mod3,p.uni = "adjusted")
# saveRDS(anova_mod3,file="output/models/epi.2024.mvabund_mod3.Rdat")
(anova_mod3 <- readRDS("output/models/epi.2024.mvabund_mod3.Rdat"))

toc(log=TRUE)

# PLOT ####
nums <- ncol(dat)
dfcur$mon <- factor(dfcur$mon, levels = c("Sep","Oct"))

png(file = "output/figs/epi.current.yr.png",
    width=8*ppi, height=14*ppi, res=ppi)
dfcur %>% 
  rename_with(~ vegan::make.cepnames(.), -c(year, transect, zone1,zone1mod,
                                            depth,mon, N, S)) %>% 
  pivot_longer(cols = -c(year,transect,zone1,zone1mod, depth, mon, N, S), 
               names_to = "Taxon", 
               values_to = "Abundance") %>%
  ## remove <0 values
  filter(., Abundance >0) %>% 
  # mutate(.,Taxon_lb = vegan::make.cepnames(Taxon)) %>% 
  mutate(Abundance = if_else(Abundance < 0, 1, Abundance)) %>% #View()
  filter(.,Abundance != 0) %>% #View()
  ggplot(aes(
    # y = Taxon_lb,
    y = Taxon,
    x = log(Abundance + 1),
    # x = Mean_abundance,
    group = zone1
  )) +
  geom_hline(yintercept = seq(0.5, (nums+0.5), by = 1), 
             color = "gray", linetype = "dashed") +  # Add grid lines between taxa
  geom_point(position = position_jitter(width = 0.01, height=0.4,seed = pi),
             alpha=0.5,size=2,
             aes(
               #shape = zoneplot,
               fill = zone1,
               colour=mon,
               group = zone1,
               shape=depth
               ),
             show.legend = FALSE) +
  facet_grid(.~zone1)+
  scale_y_discrete(limits=rev)+
  scale_shape_manual(values=c(21,22,23))+
  scale_colour_manual(values=c(1,2))+
  scale_fill_manual(values=cbPaletteTxt)+
  labs(
    title = "Abundances of taxa in epifaunal assembalges monitored as part of the\n2024 SGPBM",
    y="",
    x="log mean abundance (n+1)",
    caption = "Taxa recorded as presence-only excluded"
  )+
  theme(
    strip.text = element_text(face=2,size = 12),
    axis.title.y = element_blank(),
    axis.title.x = element_text(face=2),
    axis.text.y = element_text(size=9)
  )
dev.off()

# export current year's taxa for appendices ####
remove_zero_sum_numeric_cols <- function(data) {
  numeric_cols <- sapply(data, is.numeric)
  sum_zero <- sapply(data[, numeric_cols], sum) == 0
  data %>%
    select(-which(numeric_cols)[sum_zero])
}

# Apply the function to your dataframe
result <- remove_zero_sum_numeric_cols(dfcur)
write.csv(result, file = "output/dfw_epi.csv",row.names = FALSE)

# plot taxon trends ####
df0 %>% 
  dplyr::select(.,-c(taxonRecorded,Kingdom:Species,Comments,DataSource)) %>% 
  filter(.,taxonUse != "AFAUNAL") %>% #remove afaunal samples
  mutate(.,abund=as.numeric(abund)) %>% 
  group_by(year,transect,zone1,depth,mon,taxonUse) %>% 
  summarise(abund=sum(abund),.groups = "drop") %>% 
  # distinct() %>% group_by(year,transect,zone1,depth,mon,taxonUse) %>% count() %>% filter(n>1) %>% View(.)
  pivot_wider(names_from = taxonUse, values_from = abund,values_fill = 0) %>%
  pivot_longer(cols = Ammodytes:"Electra monostachys",names_to = "taxon", values_to = "abund") %>% 
  ## drop month and take mean by station
  dplyr::select(.,-c(mon)) %>% 
  group_by(year,transect,zone1,depth, taxon) %>% 
  summarise(abund=mean(abund),.groups="drop") %>% 
  filter(.,abund>-1) %>% 
  ggplot(data=.,aes(x=year, y=log(abund+1)))+
  geom_line(aes(group=taxon))+geom_point(aes(group=taxon))+
  facet_grid(depth~zone1, scales = "free_y")+
  geom_smooth(se=FALSE, alpha=0.8,aes(group=taxon))

# # GLLVM version ####
# ## tidy data ####
# ### take mean abundance across the 2 monthly samples and insert zero values
# dfw2 <- df0 %>% 
#   filter(.,Kingdom=="Animalia") %>% #keep only faunal taxa
#   filter(.,taxonUse != "AFAUNAL") %>% #remove afaunal samples
#   mutate(.,abund=as.numeric(abund)) %>% 
#   dplyr::select(.,-c(taxonRecorded,Kingdom:Species,Comments,DataSource)) %>% 
#   group_by(year,transect,zone1,depth,mon,taxonUse) %>% 
#   summarise(abund=sum(abund),.groups = "drop") %>% 
#   pivot_wider(names_from = taxonUse, values_from = abund,values_fill = 0) %>%
#   pivot_longer(cols = Ammodytes:"Electra monostachys",names_to = "taxon", values_to = "abund") %>% 
#   ## drop month and take mean by station
#   dplyr::select(.,-c(mon)) %>% 
#   group_by(year,transect,zone1,depth, taxon) %>% 
#   summarise(abund=mean(abund),.groups="drop") %>% 
#   filter(.,abund>-1) %>% 
#   # rewiden
#   pivot_wider(names_from = taxon, values_from = abund, values_fill = 0) %>% 
#   select(-where(sum_zero))#remove columns summing to zero
# 
# dfw_tx <- dfw2 %>% 
#   select(.,-c("year":"depth"))
# dfw_met <- dfw2 %>% 
#   select(.,c("year":"depth"))
# 
# ## reorder factors for model interpretation
# dfw_met$zone1 <- factor(dfw_met$zone1,levels=c(
#   "Inside", "Above","Inside2","Below"
# ))
# dfw2$zone1 <- factor(dfw2$zone1,levels=c(
#   "Inside", "Above","Inside2","Below"))
# 
# ### prep models ####
# ### fit models ####
# ### Unconstrained model ####
# #### Negative binomial ####
# ptm <- Sys.time()
# sDsn <- data.frame(depth = dfw_met$depth,
#                    transect=dfw_met$transect,
#                    zone1=dfw_met$zone1,
#                    year=dfw_met$year)
# m_lvm_0 <- gllvm(y = dfw_tx,
#                  X = sDsn,
#                  # formula = ~ zone1,
#                  family="negative.binomial",
#                  studyDesign = sDsn,
#                  row.eff = ~corAR1(1|year)#include year as a structured row effect
# )
# saveRDS(m_lvm_0, file="output/models/epi_gllvm_uncon_neg.bin_AR1.Rdat")
# Sys.time() - ptm;rm(ptm) #1.2 mins
# m_lvm_0 <- readRDS("output/models/epi_gllvm_uncon_neg.bin_AR1.Rdat")
# plot(m_lvm_0)
# summary(m_lvm_0)
# 
# ### Constrained model ####
# #### Negative binomial ####
# ptm <- Sys.time()
# sDsn <- data.frame(
#   # depth = dfw_met$depth,
#   transect=dfw_met$transect,
#   # zone1=dfw_met$zone1,
#   year=dfw_met$year)
# m_lvm_1 <- gllvm(y = dfw_tx,
#                  X = dfw2[,c(1:4)],
#                  formula = ~ zone1,
#                  family = "negative.binomial",
#                  studyDesign = sDsn,
#                  row.eff = ~corAR1(1|year)#include year as a structured row effect
# )
# saveRDS(m_lvm_1, file="output/models/epi_gllvm_con_neg.bin_AR1.Rdat")
# Sys.time() - ptm;rm(ptm) #49 sec
# m_lvm_1 <- readRDS("output/models/epi_gllvm_con_neg.bin_AR1.Rdat")
# plot(m_lvm_1)
# summary(m_lvm_1)
# anova(m_lvm_0,m_lvm_1)
# AIC(m_lvm_0,m_lvm_1)
# 
# coefplot(m_lvm_1)

# Tidy up ####
# unload packages
detach("package:mvabund", unload=TRUE)
detach("package:patchwork", unload=TRUE)
detach("package:ggtext", unload=TRUE)
detach("package:ggpp", unload=TRUE)
detach("package:lmerTest", unload=TRUE)
detach("package:vegan", unload=TRUE)
detach("package:ggplot2", unload=TRUE)
detach("package:tidyverse", unload=TRUE)

# remove data
rm(list = ls(pattern = "^mod"))
rm(list = ls(pattern = "^df"))
rm(list = ls(pattern = "^cbPal"))
rm(dat,fol, ppi, cur.yr,gisfol,perm,ord,res.binary,ano_epiinf2,cur_spp,
   data.scores,met,mvpl,species.scores,result,remove_zero_sum_numeric_cols)
