# 200_an_inf_div_v2.R ####
### calculate & analyse diversity indices ####
## using 2 way models

## load abundance & biomass time series data
source("R/100_imp.inf0.R")
### set metadata
source("R/00_meta_setMeta.R")
df_biom <- as_tibble(read.csv(paste0(fol,"inf.biom.ts.USE.csv"))) #load biomass TS
df_pre <- as_tibble(read.csv(paste0(fol,"inf_ts_div_pre2007.csv")))

### load packages
ld_pkgs <- c("tidyverse", "ggplot2", "vegan", "lmerTest", "patchwork","tictoc",
             "emmeans","sjPlot","sjmisc","sjlabelled","stargazer","ggridges")
vapply(ld_pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)
rm(ld_pkgs)

tictoc::tic.clearlog();tic("SET UP");print("SET UP")
### Calculate means by sample
dfw %>%
  # standardise values to individuals per m2
  mutate(
    across(
      .cols = 9:ncol(dfw), # Skip the first 8 columns
      .fns = ~ . / core.area_m2   # Multiply each column by df$core
    )
  ) %>% 
  # calculate means
  ## remove 'rep' columns
  dplyr::select(.,-rep,-AFAUNAL) %>%
  ## lengthen
  pivot_longer(.,cols = -c(1:7),names_to = "taxon", values_to = "abund") %>% 
  ## calculate mean
  group_by(across(c(!abund)
  )) %>%
  summarise(abund = mean(abund),.groups = "drop") %>% ungroup() %>% #View()
  ## rewiden
  pivot_wider(.,names_from = taxon,
              values_from = abund,
              values_fill = 0) -> dfwm2
toc(log=TRUE)

#############
# calculate indices ####
tic("calculate indices");print("calculate indices")

## taxon richness (S) ####
S <- vegan::specnumber(dfwm2 %>% dplyr::select(.,-c(year,transect,shore,
                                                    zone1,mesh,
                                                    core.area_m2
                                                    )))

## Total individuals (N) ### as density/m2 ####
##### NEED TO MODIFY COUNTS BY CORE AREA ####
tmp <- dfwm2###temp data
tmp[tmp<0] <- 0
Nm2 <- rowSums(tmp %>% dplyr::select(.,-c(year,transect,shore,
                                          zone1,mesh,
                                          core.area_m2)))#raw summed counts
rm(tmp)

## Shannon index (H) ####
tmp <- dfwm2;tmp[tmp<0] <- 0###temp data
H <- vegan::diversity(tmp %>% dplyr::select(.,-c(year,transect,shore,
                                                 zone1,mesh,
                                                 core.area_m2)),
                      index = "shannon")
rm(tmp)

## Simpson’s index (lamda) ####
tmp <- dfwm2;tmp[tmp<0] <- 0###temp data
Simp <- vegan::diversity(tmp %>% dplyr::select(.,-c(year,transect,shore,
                                                    zone1,mesh,
                                                    core.area_m2)),
                         index = "simpson")
rm(tmp)

## Pielou’s evenness (J) ####
# This is the Shannon index divided by the log of the species richness
J <- H/log(S)

## Margalef’s index (d) ####
# This is the number of taxa (S) minus 1 divided by the natural log of the total number of individuals (N)
d <- (S-1)/(Nm2)

## Hill’s N1 ####
# This is the exponent of the Shannon index (H)
N1 <- exp(H)

toc(log = TRUE)

tic("format biomass data")
# format biomass data & append to new df ####

## create diversity data set
df_div <- dfwm2 %>% dplyr::select(.,year,transect,shore,
                                  zone1,mesh,
                                  core.area_m2) %>% 
  mutate(code = paste(year,transect,shore,mesh,sep = "_")) %>% 
  relocate(code)

df_div$S <- S;rm(S)
df_div$Nm2 <- Nm2;rm(Nm2)
df_div$H <- H;rm(H)
df_div$Simp <- Simp;rm(Simp)
df_div$J <- J;rm(J)
df_div$d <- d;rm(d)
df_div$N1 <- N1;rm(N1)

## calculate mean index values
df_div %>% 
  pivot_longer(.,cols = S:N1,names_to = "index", values_to = "value") %>% 
  group_by(across(c(!value))) %>% 
  summarise(value = mean(value),.groups = "drop") %>% 
  pivot_wider(.,names_from = index, values_from = value)->df_div

## TO DO: calculate total biomass by sample
dfb <- df_biom %>% 
  dplyr::select(.,-biomass_raw_g, -units, -code, -Comment) %>%
  ungroup() %>% 
  filter(mesh=="1.0mm") %>% 
  # sum biomass by year/transect/mesh/shore/rep
  dplyr::select(.,-taxon) %>% ungroup() %>% 
  group_by(across(c(!biomass_g_per_m2))) %>% 
  summarise(biom=sum(biomass_g_per_m2),
            .groups = "drop") %>% 
  #calculate mean biomass across replicates
  dplyr::select(.,-rep) %>% 
  group_by(across(c(!biom))) %>% 
  summarise(biom=mean(biom),
            .groups = "drop") %>% 
  mutate(code = paste(year,transect,shore,mesh,sep = "_")) %>% 
  relocate(code)

df_div <- left_join(df_div,dfb %>% dplyr::select(code,biom),by="code")

### append pre-2006 data
df_pre <- df_pre %>% mutate(code = paste(year,transect,shore,mesh,sep = "_")) %>% 
  relocate(code) %>% 
  #rename N to Nm2
  rename(.,Nm2 = N)

dfdiv <- bind_rows(df_pre,df_div)

#write.csv(dfdiv,file="data/out/inf_div.csv",row.names = FALSE)

# assign factors
## transect
dfdiv$transect <- factor(dfdiv$transect,levels=c("T1N","T1", "T1S",
                                                 "T4",
                                                 "T5","T6",
                                                 "T7","T8",
                                                 "T9",
                                                 "T10",
                                                 "T11","T12","T13",
                                                 "T14",
                                                 "T15","T16","T17",
                                                 "T18","T19",
                                                 "T20","T21",
                                                 "T22","T23",
                                                 "T24","T25","T26",
                                                 "WA1","WA5","WA6"
))
## shore
dfdiv$shore <- factor(dfdiv$shore,levels = c("Mid","Low"))

###zone
ab <- c("T1N","T1", "T1S")
ins <- c("T4", "T5", "T6", "T7", "T8", "T9", "T10",
         "T11", "T12")
ins2 <- "T13"
bel <- c("T14",
         "T15","T16","T17",
         "T18","T19",
         "T20","T21",
         "T22","T23",
         "T24","T25","T26")
was <- c("WA1","WA5","WA6")

dfdiv$zone1 <- ifelse(dfdiv$transect %in% ab, "Above",
                      ifelse(dfdiv$transect %in% ins, "Inside",
                             ifelse(dfdiv$transect %in% ins2, "Inside2",
                                    ifelse(dfdiv$transect %in% bel, "Below",
                                           ifelse(dfdiv$transect %in% was, "Wash",NA
                                           )))))
dfdiv$zone1 <- factor(dfdiv$zone1,levels=c("Above","Inside",
                                           "Inside2","Below",
                                           "Wash"))
rm(ab,ins,ins2,bel,was)
# descriptive: current year & 1.0mm ####
df.cur <- dfdiv %>% 
  filter(., year == cur.yr) %>% 
  filter(., mesh == "1.0mm")

toc(log=TRUE)

### summarise across all zones ####
tic("Summarise data and produce statistical models")
mean(df.cur %>% filter(.,zone1!="Wash") %>% pull(S),na.rm = TRUE)
sd(df.cur %>% filter(.,zone1!="Wash") %>% pull(S),na.rm = TRUE)

### summarise means and SD by zone ####
# tmz <- droplevels(dfdiv[dfdiv$transect != "WA1" & dfdiv$mesh=="1.0mm",]) %>%
#   group_by(zone1) %>%
#   summarise(mn.S = mean(S,na.rm = TRUE), sd.S = sd(S, na.rm = TRUE),
#             mn.N = mean(N,na.rm = TRUE), sd.N = sd(N, na.rm = TRUE),
#             mn.simp = mean(simp,na.rm = TRUE), sd.simp = sd(simp, na.rm = TRUE),
#             mn.d = mean(d,na.rm = TRUE), sd.d = sd(d, na.rm = TRUE),
#             mn.H = mean(H,na.rm = TRUE), sd.H = sd(H, na.rm = TRUE),
#             mn.J = mean(J,na.rm = TRUE), sd.J = sd(J, na.rm = TRUE),
#             mn.N1 = mean(N1,na.rm = TRUE), sd.N1 = sd(N1, na.rm = TRUE),
#             mn.biom = mean(biom,na.rm = TRUE), sd.biom = sd(biom, na.rm = TRUE))

dfdivcur <- df_div %>% 
  filter(., mesh == "1.0mm") %>% 
  filter(., year == cur.yr) %>% 
  droplevels(.)

# models ####
# define "Inside" as ref level
dfdivcur$zone1 <- factor(dfdivcur$zone1,
                         levels=c("Inside","Above","Inside2","Below","Wash"))

## species richness ####
anova(modS_shore <- lm(S ~ shore,
                       data=dfdivcur %>% filter(.,zone1!="Wash")))
summary(modS_shore)

anova(modS_zone <- lm(S ~ zone1,
                       data=dfdivcur %>% filter(.,zone1!="Wash")))
summary(modS_shore)

# zone * shore
anova(modzS <- lm(S ~ zone1*shore,
                 data=dfdivcur %>% filter(.,zone1!="Wash")))
list(
  zone1 = emmeans(modzS, pairwise ~zone1), #main effect of zone1
  shore = emmeans(modzS, pairwise ~shore), #main effect of shore
  interaction = emmeans(modzS, pairwise ~ zone1 | shore)
)

summary(modzS);aov(modzS)

# sjPlot::plot_model(modzS,show.values=TRUE, show.p=TRUE)
# as mixed model
modS_lmer <- lmerTest::lmer(S ~ zone1 + (1|shore),
                            data=dfdivcur %>% filter(.,zone1!="Wash"))
sjPlot::plot_model(modS_lmer, show.values = TRUE, show.p = TRUE,
                   show.intercept = TRUE,
                   title = "Effect of nourishment zone on taxon richness (S)")
sjPlot::tab_model(modS_lmer)

## abundance ####
anova(modzNm2 <- lm(Nm2 ~ zone1*shore,
                 data=dfdivcur %>% filter(.,zone1!="Wash")))
list(
  zone1 = emmeans(modzNm2, pairwise ~zone1), #main effect of zone1
  shore = emmeans(modzNm2, pairwise ~shore), #main effect of shore
  interaction = emmeans(modzNm2, pairwise ~ zone1 | shore)
)
summary(modS_lmer)
# summary(modzNm2);aov(modzNm2)
# sjPlot::plot_model(modzNm2,show.values=TRUE, show.p=TRUE)

# as mixed model
modNm2_lmer <- lmerTest::lmer(Nm2 ~ zone1 + (1|shore),
                            data=dfdivcur %>% filter(.,zone1!="Wash"))
sjPlot::plot_model(modNm2_lmer, show.values = TRUE, show.p = TRUE,
                   show.intercept = TRUE,
                   title = "Effect of nourishment zone on faunal abundance (N)")
sjPlot::tab_model(modNm2_lmer)
summary(modNm2_lmer)

## biomass ####
mean(dfdivcur$biom); sd(dfdivcur$biom)
# dfdivcur[which.max(dfdivcur$N),c(2:7,(ncol(dfdivcur)-7):(ncol(dfdivcur)))]##max abund
# dfdivcur[which.min(dfdivcur$N),c(2:7,(ncol(dfdivcur)-7):(ncol(dfdivcur)))]##min abund]

anova(modzbiom <- lm(biom ~ zone1*shore,
                  data=dfdivcur %>% filter(.,zone1!="Wash")))
list(
  zone1 = emmeans(modzbiom, pairwise ~zone1), #main effect of zone1
  shore = emmeans(modzbiom, pairwise ~shore), #main effect of shore
  interaction = emmeans(modzbiom, pairwise ~ zone1 | shore)
)

summary(modzbiom);aov(modzbiom)
# sjPlot::plot_model(modzbiom,show.values=TRUE, show.p=TRUE)

# as mixed model
modzbiom_lmer <- lmerTest::lmer(biom ~ zone1 + (1|shore),
                              data=dfdivcur %>% filter(.,zone1!="Wash"))
sjPlot::plot_model(modzbiom_lmer, show.values = TRUE, show.p = TRUE,
                   show.intercept = TRUE,
                   title = "Effect of nourishment zone on faunal biomass")
sjPlot::tab_model(modzbiom_lmer)
summary(modzbiom_lmer)

## Margalef ####
anova(modzd <- lm(d ~ zone1*shore,
                  data=dfdivcur %>% filter(.,zone1 != "Wash") %>% 
                    filter(!is.na(d),
                           !is.infinite(d))))
list(
  zone1 = emmeans(modzd, pairwise ~zone1), #main effect of zone1
  shore = emmeans(modzd, pairwise ~shore), #main effect of shore
  interaction = emmeans(modzd, pairwise ~ zone1 | shore)
)

summary(modzd);aov(modzd)
# sjPlot::plot_model(modzd,show.values=TRUE, show.p=TRUE)

# as mixed model
# can't model when d = -Inf.  Replace -Inf with -10
dfdivcur$d_prox <- dfdivcur$d
dfdivcur$d_prox[is.infinite(dfdivcur$d_prox)] <- -10
modzd_lmer <- lmerTest::lmer(d_prox ~ zone1 + (1|shore),
                                data=dfdivcur %>% filter(.,zone1!="Wash"))
sjPlot::plot_model(modzd_lmer, show.values = TRUE, show.p = TRUE,
                   show.intercept = TRUE,
                   title = "Effect of nourishment zone on Margalef's index (d)")
sjPlot::tab_model(modzd_lmer)
summary(modzd_lmer)

## Shannon ####
anova(modzH <- lm(H ~ zone1*shore,
                  data=dfdivcur %>% filter(.,zone1 != "Wash")))
list(
  zone1 = emmeans(modzH, pairwise ~zone1), #main effect of zone1
  shore = emmeans(modzH, pairwise ~shore), #main effect of shore
  interaction = emmeans(modzH, pairwise ~ zone1 | shore)
)

summary(modzH);aov(modzH)
# sjPlot::plot_model(modzH,show.values=TRUE, show.p=TRUE)

# as mixed model
modzH_lmer <- lmerTest::lmer(H ~ zone1 + (1|shore),
                             data=dfdivcur %>% filter(.,zone1!="Wash"))
sjPlot::plot_model(modzH_lmer, show.values = TRUE, show.p = TRUE,
                   show.intercept = TRUE,
                   title = "Effect of nourishment zone on Shannon's H")
sjPlot::tab_model(modzH_lmer)
summary(modzH_lmer)


## Pielou ####
anova(modzJ <- lm(J ~ zone1*shore,
                  data=dfdivcur %>% filter(.,zone1 != "Wash")))
(anova_results <- anova(modzJ))  # Get ANOVA table
anova_summary <- as.data.frame(anova_results)
anova_summary$Variable <- rownames(anova_summary)  # Add names

# Select only relevant columns
anova_summary <- anova_summary[, c("Variable", "Pr(>F)")]
colnames(anova_summary) <- c("Term", "P_Value")

list(
  zone1 = emmeans(modzJ, pairwise ~zone1), #main effect of zone1
  shore = emmeans(modzJ, pairwise ~shore), #main effect of shore
  interaction = emmeans(modzJ, pairwise ~ zone1 | shore)
)

summary(modzJ);aov(modzJ)
# sjPlot::plot_model(modzJ,show.values=TRUE, show.p=TRUE)

# as mixed model
# replace NaN values with -10
dfdivcur$J_proxy <- dfdivcur$J
dfdivcur$J_proxy[is.nan(dfdivcur$J_proxy)] <- -10

modzJ_lmer <- lmerTest::lmer(J_proxy ~ zone1 + (1|shore),
                             data=dfdivcur %>% filter(.,zone1!="Wash"))
sjPlot::plot_model(modzJ_lmer, show.values = TRUE, show.p = TRUE,
                   show.intercept = TRUE,
                   title = "Effect of nourishment zone on Pielou's J")
sjPlot::tab_model(modzJ_lmer)
summary(modzJ_lmer)


## tabulate models ####
stargazer(modzS, modzNm2, modzbiom,modzd,modzJ,modzH, 
          type = "html", 
          out =  "output/models/inf_Univ_2024_model_summary2.html", 
          title = "Comparison of Models",
          column.labels = c("Taxon richness", "Abundance",
                            "Biomass","Margalef's d","Pielou's eveness",
                            "Shannon entropy"),
          align = TRUE,
          report=('vcsp'))
toc(log=TRUE)

# PLOTS ####
tic("Current year plots")
##reset order of factor levels
dfdivcur$zone1 <- factor(dfdivcur$zone1, levels = c(
  "Above","Inside","Inside2","Below","Wash"))
dfdivcur$transect <- factor(dfdivcur$transect,levels=c("T1N","T1", "T1S",
                                                       "T4",
                                                       "T5","T6",
                                                       "T7","T8",
                                                       "T9",
                                                       "T10",
                                                       "T11","T12","T13",
                                                       "T14",
                                                       "T15","T16","T17",
                                                       "T18","T19",
                                                       "T20","T21",
                                                       "T22","T23",
                                                       "T24","T25","T26",
                                                       "WA1","WA5","WA6"
))
dfdivcur$shore <- factor(dfdivcur$shore, levels = c("Mid","Low"))

## Current year ####
### Spp Rich ####
S <- ggplot(
  # data = dfdivcur,
  data = dfdivcur %>% filter(.,zone1 != "Wash"),
  aes(x = transect, y = S,
      fill = zone1, color = "black"))+
  geom_bar(stat = "identity", colour = "black")+
  facet_wrap(~ shore, ncol = 1)+
  theme(legend.position="none",strip.text.x = element_text(size = 12))+
  scale_x_discrete(breaks = NULL)+
  ylab("Taxon richness") + xlab("")+
  scale_fill_manual(values=cbPalette[c(1:4,7)])+
  labs(fill = "")+
  labs(subtitle = "Taxon richness")

### Faunal density ####
N <- ggplot(
  # data = dfdivcur,
  data = dfdivcur %>% filter(.,zone1 != "Wash"),
  aes(x = transect, y = Nm2,
      fill = zone1, color = "black"))+
  geom_bar(stat = "identity", colour = "black")+
  facet_wrap(~ shore, ncol = 1)+
  theme(legend.position="none",strip.text.x = element_text(size = 12))+
  scale_x_discrete(breaks=NULL)+
  ylab("Faunal density") + xlab("")+
  scale_fill_manual(values=cbPalette[c(1:4,7)])+
  labs(fill="")+
  labs(subtitle = "Taxon density")

### Logged faunal density ####
lN <- ggplot(
  # data = dfdivcur,
  data = dfdivcur %>% filter(.,zone1 != "Wash"),
  aes(x = transect, y = log(Nm2),
      fill = zone1, color = "black"))+
  geom_bar(stat = "identity", colour = "black")+
  facet_wrap(~ shore, ncol = 1)+
  theme(legend.position="none",strip.text.x = element_text(size = 12))+
  scale_x_discrete(breaks=NULL)+
  ylab("Log(faunal density)") + xlab("")+
  scale_fill_manual(values=cbPalette[c(1:4,7)])+
  labs(fill="")+
  labs(subtitle = "Taxon density (log)")

### Faunal Biomass ####
B <- ggplot(
  # data = dfdivcur,
  data = dfdivcur %>% filter(.,zone1 != "Wash"),
  aes(x = transect, y = biom,
      fill = zone1, color = "black"))+
  geom_bar(stat = "identity", colour = "black")+
  facet_wrap(~ shore, ncol = 1)+
  theme(legend.position="none",strip.text.x = element_text(size = 12))+
  scale_x_discrete(breaks=NULL)+
  ylab("Faunal biomass") + xlab("")+
  scale_fill_manual(values=cbPalette[c(1:4,7)])+
  labs(fill="")+
  labs(subtitle = "Faunal biomass")

### Logged faunal Biomass ####
lB <- ggplot(
  # data = dfdivcur,
  data = dfdivcur %>% filter(.,zone1 != "Wash"),
  aes(x = transect, y = log1p(biom),
      fill = zone1, color = "black"))+
  geom_bar(stat = "identity", colour = "black")+
  facet_wrap(~ shore, ncol = 1)+
  theme(legend.position="none",strip.text.x = element_text(size = 12))+
  scale_x_discrete(breaks=NULL)+
  ylab("Log faunal biomass") + xlab("")+
  scale_fill_manual(values=cbPalette[c(1:4,7)])+
  labs(fill="")+
  labs(subtitle = "Faunal biomass (log)")

### shannon ####
H <- ggplot(
  # data = dfdivcur,
  data = dfdivcur %>% filter(.,zone1 != "Wash"),
  aes(x = transect, y = H,
      fill = zone1, color = "black"))+
  geom_bar(stat = "identity", colour = "black")+
  facet_wrap(~ shore, ncol = 1)+
  theme(legend.position="none",strip.text.x = element_text(size = 12))+
  scale_x_discrete(breaks=NULL)+
  ylab("Shannon entropy") + xlab("")+
  scale_fill_manual(values=cbPalette[c(1:4,7)])+
  labs(fill="")+
  labs(subtitle = expression("Shannon entropy ("~italic("H'")~")"))

### Pielou ####
J <- ggplot(
  # data = dfdivcur,
  data = dfdivcur %>% filter(.,zone1 != "Wash"),
  aes(x = transect, y = J,
      fill = zone1, color = "black"))+
  geom_bar(stat = "identity", colour = "black")+
  facet_wrap(~ shore, ncol = 1)+
  theme(legend.position="none",strip.text.x = element_text(size = 12),
        axis.text.x  = element_text(angle=90, vjust=0.5, size = 12))+
  scale_y_continuous(breaks=c(0,0.5, 1))+
  ylab("Pielou's evenness") + xlab("")+
  scale_fill_manual(values=cbPalette[c(1:4,7)])+
  # labs(subtitle = "")
  labs(subtitle = expression("Pielou's evenness ("~italic("J'")~")"))

### Margalef ####
d <- ggplot(
  # data = dfdivcur,
  data = dfdivcur %>% filter(.,zone1 != "Wash"),
  aes(x = transect, y = d,
      fill = zone1, color = "black"))+
  geom_bar(stat = "identity", colour = "black")+
  facet_wrap(~ shore, ncol = 1)+
  theme(legend.title=element_blank(),legend.direction="horizontal",
        legend.position = "inside",
        legend.position.inside = c(.625, 1.4),
        strip.text.x = element_text(size = 12),
        axis.text.x  = element_text(angle=90,
                                    vjust=0.5,
                                    size = 12)
  )+
  #scale_y_continuous(breaks=c(0,0.5, 1))+
  ylab("Margalef's d") + xlab("")+
  scale_fill_manual(values=cbPalette[c(1:4,7)])+
  labs(fill="")+
  labs(subtitle = expression("Margalef's index ("~italic("d")~")"))
#guides(fill=guide_legend(nrow=2, byrow=TRUE))

### COMBINE plots into single chart
x <- (lN|lB)/
  (S|H)/
  (J|d)

png(file = paste0("output/figs/inf.",cur.yr,".div.png"),
    width = 12 * ppi, height = 10 * ppi, res = ppi)
x+plot_annotation(tag_levels = "A")
dev.off();
rm(J,d,lB,B,S,H,lN,N)
toc(log=TRUE)

tic("Time series plots")
## Time series ####
dfdiv$shore <- factor(dfdiv$shore, levels = c("Mid","Low"))
dfdiv$zone1 <- factor(dfdiv$zone1,levels = c("Above","Inside","Inside2","Below","Wash"))
dfts <- dfdiv %>% 
  filter(.,is.na(mesh) | mesh=="1.0mm") %>% 
  filter(.,zone1 != "Wash")

### LOG density ####
N <- ggplot(data = dfts, aes(y = log(Nm2+1), x = year, fill = zone1)) +
  geom_hline(yintercept = mean(log(dfts$N+1),na.rm = TRUE),colour="grey",linetype="dashed")+
  geom_hline(yintercept = min(log(dfts$N+1),na.rm = TRUE),colour="grey",linetype="dotted")+
  geom_hline(yintercept = max(log(dfts$N+1),na.rm = TRUE),colour="grey",linetype="dotted")+
  geom_point(alpha=0.5)+
  # geom_boxplot(aes(group=year))+
  geom_smooth(method = "loess", colour = "red", span = .9)+
  #geom_smooth(method = "gam", colour = "red", span = .9)+
  facet_grid(shore~zone1)+
  scale_colour_manual(name = "", values=cbPalette)+
  scale_fill_manual(name = "", values=cbPalette)+
  xlab("Year") + ylab(bquote("Log faunal density"))+
  scale_x_continuous(breaks = seq(1996, 2024, by = 4))+
  coord_cartesian(ylim=c(0,NA))+
  theme(legend.position="none",
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12),
        axis.text.x = element_text(angle = 270,hjust=1,vjust=0.5))

# png(file = "output/figs/inf.ts.logN.1996_loess_bx.png",
#     width=12*ppi, height=6*ppi, res=ppi)
png(file = "output/figs/inf.ts.logN.1996_loess_pt.png",
    width=12*ppi, height=6*ppi, res=ppi)
# png(file = "output/figs/inf.ts.logN.1996_loess.png",
#     width=12*ppi, height=6*ppi, res=ppi)
print(N);
dev.off();
rm(N)

### species ####
S <- ggplot(data = dfts, aes(y = S, x = year, fill = zone1))+
  geom_hline(yintercept = mean(dfts$S,na.rm = TRUE),colour="grey",linetype="dashed")+
  geom_hline(yintercept = min(dfts$S,na.rm = TRUE),colour="grey",linetype="dotted")+
  geom_hline(yintercept = max(dfts$S,na.rm = TRUE),colour="grey",linetype="dotted")+
  geom_point(alpha=0.5)+#geom_boxplot(aes(group=year))+
  geom_smooth(method = "loess", colour = "red", span = .9)+
  # geom_smooth(method = "gam", colour = "red", span = .9)+
  facet_grid(shore~zone1)+
  scale_colour_manual(name = "", values=cbPalette)+
  scale_fill_manual(name = "", values=cbPalette)+
  scale_x_continuous(breaks = seq(1996, 2024, by = 4))+
  xlab("Year") + ylab(bquote("Taxon richness"))+
  theme(legend.position="none",
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12),
        axis.text.x = element_text(angle = 270,hjust=1,vjust=0.5))+
  coord_cartesian(ylim=c(0,NA))

# png(file = "output/figs/inf.ts.S.1996_loess_bx.png",
#     width=12*ppi, height=6*ppi, res=ppi)
png(file = "output/figs/inf.ts.S.1996_loess_pt.png",
    width=12*ppi, height=6*ppi, res=ppi)
# png(file = "output/figs/inf.ts.S.1996_gam.png",
#     width=12*ppi, height=6*ppi, res=ppi)
print(S);
dev.off();
rm(S)

### shannon ####
H <- ggplot(data = dfts, aes(y = H, x = year, fill = zone1))+
  geom_hline(yintercept = mean(dfts$H,na.rm = TRUE),colour="grey",linetype="dashed")+
  geom_hline(yintercept = min(dfts$H,na.rm = TRUE),colour="grey",linetype="dotted")+
  geom_hline(yintercept = max(dfts$H,na.rm = TRUE),colour="grey",linetype="dotted")+
  geom_boxplot(aes(group=year))+
  geom_smooth(method = "loess", colour = "red", span = .9)+
  #geom_smooth(method = "gam", colour = "red", span = .9)+
  facet_grid(shore~zone1)+
  scale_colour_manual(name = "", values=cbPalette)+
  scale_fill_manual(name = "", values=cbPalette)+
  xlab("Year") + ylab(bquote("Shannon entropy"))+
  theme(legend.position="none",
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12),
        axis.text.x = element_text(angle = 270,hjust=1,vjust=0.5))+
  scale_x_continuous(breaks = seq(1996, 2024, by = 4))+
  coord_cartesian(ylim=c(0,NA))

png(file = "output/figs/inf.ts.H.1996_loess.png",
width=12*ppi, height=6*ppi, res=ppi)
# png(file = "output/figs/inf.ts.H.1996_gam.png",
#     width=12*ppi, height=6*ppi, res=ppi)
print(H);
dev.off();
rm(H)

### Pielou ####
J <- ggplot(data = dfts, aes(y = J, x = year, fill = zone1))+
  geom_hline(yintercept = mean(dfts$J,na.rm = TRUE),colour="grey",linetype="dashed")+
  geom_hline(yintercept = min(dfts$J,na.rm = TRUE),colour="grey",linetype="dotted")+
  geom_hline(yintercept = max(dfts$J,na.rm = TRUE),colour="grey",linetype="dotted")+
  geom_boxplot(aes(group=year))+
  geom_smooth(method = "gam", colour = "red", span = .9)+
  # geom_smooth(method = "loess", colour = "red", span = .9)+
  facet_grid(shore~zone1)+
  scale_colour_manual(name = "", values=cbPalette)+
  scale_fill_manual(name = "", values=cbPalette)+
  xlab("Year") + ylab(bquote("Pielou's evenness"))+
  theme(legend.position="none",
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12),
        axis.text.x = element_text(angle = 270,hjust=1,vjust=0.5))+
  scale_x_continuous(breaks = seq(1996, 2024, by = 4))+
  coord_cartesian(ylim=c(0,NA))

# png(file = "output/figs/inf.ts.J.1996_loess.png",
# width=12*ppi, height=6*ppi, res=ppi)
png(file = "output/figs/inf.ts.J.1996_gam.png",
    width=12*ppi, height=6*ppi, res=ppi)
print(J);
dev.off();
rm(J)

### Margalef ####
# d <- ggplot(data = dfts, aes(y = d, x = year, fill = zone1))+
# geom_hline(yintercept = mean(dfts$d,na.rm = TRUE),colour="grey",linetype="dashed")+
# geom_hline(yintercept = min(dfts$d,na.rm = TRUE),colour="grey",linetype="dotted")+
# geom_hline(yintercept = max(dfts$d,na.rm = TRUE),colour="grey",linetype="dotted")+
# geom_boxplot(aes(group=year))+
# # geom_smooth(method = "loess", colour = "red", span = .9)+
#   geom_smooth(method = "gam", colour = "red", span = .9)+
# facet_grid(shore~zone1)+
# scale_colour_manual(name = "", values=cbPalette)+
# scale_fill_manual(name = "", values=cbPalette)+
# xlab("Year") + ylab(bquote("Margalef's richness"))+
#   ylim(0,2)+
# theme(legend.position="none",
# strip.text.x = element_text(size = 12),
# strip.text.y = element_text(size = 12),
# axis.text.x = element_text(angle = 270,hjust=1,vjust=0.5))+
# scale_x_continuous(breaks = seq(2007, 2023, by = 2))+
# coord_cartesian(ylim=c(0,NA))
# 
# png(file = "output/figs/inf.ts.d.1996_gam.png",
# width=12*ppi, height=6*ppi, res=ppi)
# # png(file = "output/figs/inf.ts.d.1996_loess.png",
# #     width=12*ppi, height=6*ppi, res=ppi)
# print(d);
# dev.off();
# rm(d)

### log biomass_m2 ####
d <- ggplot(data = dfts, aes(y = log(biom+1), x = year, fill = zone1))+
  geom_hline(yintercept = mean(log(dfts$biom+1),na.rm = TRUE),colour="grey",linetype="dashed")+
  geom_hline(yintercept = min(log(dfts$biom+1),na.rm = TRUE),colour="grey",linetype="dotted")+
  geom_hline(yintercept = max(log(dfts$biom+1),na.rm = TRUE),colour="grey",linetype="dotted")+
  geom_boxplot(aes(group=year))+
  geom_smooth(method = "loess", colour = "red", span = .9)+
  # geom_smooth(method = "gam", colour = "red", span = .9)+
  facet_grid(shore~zone1)+
  scale_colour_manual(name = "", values=cbPalette)+
  scale_fill_manual(name = "", values=cbPalette)+
  xlab("Year") + ylab(bquote("log(Biomass)"))+
  scale_x_continuous(breaks = seq(1996, 2024, by = 4))+
  coord_cartesian(ylim=c(0,NA))+
  theme(legend.position="none",
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12),
        axis.text.x = element_text(angle = 270,hjust=1,vjust=0.5))

# png(file = "output/figs/inf.ts.logbiom.1996_gam.png",
# width=12*ppi, height=6*ppi, res=ppi)
png(file = "output/figs/inf.ts.logbiom.1996_loess.png",
    width=12*ppi, height=6*ppi, res=ppi)
print(d);
dev.off();
rm(d)

### export biomass for report appendices
## TO DO: calculate total biomass by sample
df_biom %>% 
  filter(year==cur.yr) %>% 
  filter(mesh=="1.0mm") %>% 
  dplyr::select(.,-biomass_raw_g, -units, -code, -Comment) %>%
  ungroup() %>% 
  ### widen, fill with zeroes, re-lengthen
  pivot_wider(names_from = taxon, values_from = biomass_g_per_m2,
              values_fill=list(biomass_g_per_m2 = 0)) %>% 
  pivot_longer(cols = AFAUNAL:"Pontocrates arenarius",
               names_to = "taxon",
               values_to = "biomass_g_per_m2") %>% 
  dplyr::select(.,-rep) %>%
  group_by(across(c(!biomass_g_per_m2))) %>% 
  summarise(mb=mean(biomass_g_per_m2),
            .groups = "drop")->df_biom_w


df_biom_w$yr.trn.sh.meth <- paste0(df_biom_w$year,".",df_biom_w$mesh,".",
                                   df_biom_w$transect,".",df_biom_w$shore)

df_biom_w <- df_biom_w %>%
  dplyr::select(.,-c(year,mesh,transect,shore)) %>% 
  pivot_wider(.,names_from = yr.trn.sh.meth,values_from = mb)

write.csv(df_biom_w,file="output/dfw_biomass.csv")

#development: better ways to display trends ####

ggplot(data = dfts, aes(x = log(Nm2+1), y = as.factor(year),
                        height = log(Nm2+1),
                        fill = zone1)) +
  facet_grid(shore ~ zone1)+
  geom_density_ridges(stat="identity",#scale=1.4,alpha=0.7,
                      aes(fill=zone1),
                      show.legend = FALSE)+
  scale_y_discrete(limits=rev)
  
  
  ggplot(data = .,
         aes(x = phi, y=as.factor(year), height = perc))+
  facet_grid(shore ~ zone1)+
  geom_density_ridges(stat="identity",#scale=1.4,alpha=0.7,
                      aes(fill=zone1),
                      show.legend = FALSE)+
  scale_y_discrete(limits=rev)+
  scale_fill_manual(values=cbPalette)+
  xlim(-5,9)+
  labs(x="Phi",y="Year",
       title = "Distribution of sediment grain sizes since 2011 within different beach nourishment zones on the Lincolnshire coast",
       subtitle="Sandy sediments characterise most of the monitoring zone. Coarser sediments (i.e., smaller phi values) dominate Inside the beach nourishment zone.\nThis difference is largely confined to the upper and mid shore sites which receive the majority of nourishment material",
       caption = "Background panel shading indicates broad sediment categories: Gravel (phi <0), Sand (phi 0-4, Silt (phi >4).")+
  theme(strip.text = element_text(size = 14, face="bold"))





+
  geom_hline(yintercept = mean(log(dfts$N+1),na.rm = TRUE),colour="grey",linetype="dashed")+
  geom_hline(yintercept = min(log(dfts$N+1),na.rm = TRUE),colour="grey",linetype="dotted")+
  geom_hline(yintercept = max(log(dfts$N+1),na.rm = TRUE),colour="grey",linetype="dotted")+
  geom_boxplot(aes(group=year))+
  geom_smooth(method = "loess", colour = "red", span = .9)+
  #geom_smooth(method = "gam", colour = "red", span = .9)+
  facet_grid(shore~zone1)+
  scale_colour_manual(name = "", values=cbPalette)+
  scale_fill_manual(name = "", values=cbPalette)+
  xlab("Year") + ylab(bquote("Log faunal density"))+
  scale_x_continuous(breaks = seq(1996, 2024, by = 4))+
  coord_cartesian(ylim=c(0,NA))+
  theme(legend.position="none",
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12),
        axis.text.x = element_text(angle = 270,hjust=1,vjust=0.5))


## Tidy up ####
rm(list = ls(pattern = "^df"))
rm(list=ls(pattern = "^anov"))
rm(list=ls(pattern = "^mod"))
rm(list=ls(pattern = "^cbP"))
rm(ppi, tmz, perm, cur.yr, x, projfol, source_file, sum_zero, fol, gisfol)
toc(log=TRUE)
unlist(tictoc::tic.log())

detach("package:tidyverse", unload=TRUE)
detach("package:lmerTest", unload=TRUE)
detach("package:vegan", unload=TRUE)
detach("package:patchwork", unload=TRUE)
