# 200_an.sed.phys.R ####
### import & analyse cone penetrometer and beach angle data

# Set up ####
### load packages ####
ld_pkgs <- c("tidyverse","ggthemes","lmerTest","effects","tictoc",
             "sjPlot","visreg")
vapply(ld_pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)
rm(ld_pkgs)

### load metadata ####
source("R/00_meta_setMeta.R")

## import & format data ####
df <- as_tibble(read.csv(paste0(fol,"sed.phys.ts.csv"),
                         stringsAsFactors = FALSE))

##order factors
df$transect <- factor(df$transect,
                      levels =
                        c("T1N","T1","T1S","T4", "T7", "T8",
                          "T11", "T12", "T13", "T15", "T17",
                          "T24","T25","T26",
                          "T20", "T21","T22", "T23","WA1","WA5","WA6"))
df$zone1 <- factor(df$zone1,
                   levels = c("Above", "Inside","Inside2","Below","Wash"))

df$shore <- factor(df$shore,
                   levels=c("Upper","Mid","Low","Surf", "ALL"))

cur_dat0 <- droplevels(subset(df, year == cur.yr))

cur_dat <- droplevels(cur_dat0 %>% filter(., shore != "ALL"))

##=============###
# Wave type   ####
##=============###
subset(cur_dat, type == "WaveClass")## pull out angle data

##=============###
# Beach Angle  ####
##=============###
ang_cu <- cur_dat %>% filter(., type == "angle") %>% ## pull out angle data
  mutate(., value = as.numeric(value))

png(file = paste0("output/figs/sed.",cur.yr,".sed.mor.angle.png"),
    width=12*ppi, height=6*ppi, res=ppi)
set.seed(pi); ggplot(data = ang_cu, aes(y = value, x = zone1,
                                        fill=zone1))+
  geom_boxplot(outlier.colour=NA)+
  geom_jitter(aes(shape = zone1),
              width = 0.3, height = 0.05,size = 3,stroke = 1,
              show.legend = FALSE)+
  scale_shape_manual(values = c(21:(length(unique(ang_cu$zone1))+21)))+
  labs(title = paste0("Beach slopes recorded in the ",cur.yr," monitoring programme"))+
  theme(plot.title = element_text(face = "bold"),
        legend.position = "bottom",#c(.5,1.02),
        legend.direction = "horizontal",
        legend.key.size = unit(1,"cm"),#change legend size!
        legend.title=element_blank(),
        legend.text=element_text(size = 12,
                                 face = "bold"),
        legend.box.margin=margin(-10,-10,-10,-10),
        legend.margin=margin(0,0,0,0),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        strip.text.x = element_text(size = 12),
        strip.text = element_text(face="bold")
  )+
  xlab("") + ylab("Beach angle (°)")+
  scale_fill_manual(values=cbPaletteFill[c(1:4,7)])+
  scale_x_discrete(breaks=NULL)+
  facet_wrap(~shore)+
  guides(fill=guide_legend(nrow=1))
dev.off()

png(file = paste0("output/figs/sed.",cur.yr,".sed.mor.angle_v2.png"),
    width=12*ppi, height=6*ppi, res=ppi)
set.seed(pi); df %>% 
  filter(!(zone1 %in% c("Above", "Inside", "Inside2", "Below") & year == 2024)) %>% 
  filter(., type == "angle") %>% 
  mutate(., value = as.numeric(value)) %>% 
  ggplot(data = ., aes(y = value, x = zone1,
                       fill=zone1))+
  geom_boxplot(outlier.colour=NA)+
  geom_jitter(aes(shape = zone1),
              width = 0.3, height = 0.05,size = 3,stroke = 1,
              show.legend = FALSE)+
  scale_shape_manual(values = c(21:(length(unique(ang_cu$zone1))+21)))+
  labs(title = paste0("Beach slopes recorded since 2008"),
       subtitle = "2024 data excluded from non-Wash stations")+
  theme(plot.title = element_text(face = "bold"),
        legend.position = "bottom",#c(.5,1.02),
        legend.direction = "horizontal",
        legend.key.size = unit(1,"cm"),#change legend size!
        legend.title=element_blank(),
        legend.text=element_text(size = 12,
                                 face = "bold"),
        legend.box.margin=margin(-10,-10,-10,-10),
        legend.margin=margin(0,0,0,0),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        strip.text.x = element_text(size = 12),
        strip.text = element_text(face="bold")
  )+
  xlab("") + ylab("Beach angle (°)")+
  scale_fill_manual(values=cbPaletteFill[c(1:4,7)])+
  scale_x_discrete(breaks=NULL)+
  facet_wrap(~shore)+
  guides(fill=guide_legend(nrow=1))
dev.off()

df %>% 
  filter(!(zone1 %in% c("Above", "Inside", "Inside2", "Below") & year == 2024)) %>% 
  filter(., type == "angle") %>% 
  mutate(., value = as.numeric(value)) %>% 
  summarise(mn = mean(value),
            sd = sd(value))

## Get summary ####
## mean by zone & shore: current year
(mean_23 <- ang_cu %>%
   group_by(zone1, shore) %>%
   summarise("mean.angle"=mean(value), sd(value))) %>% 
  arrange(.,by = mean.angle)

## mean by zone: current year
(mean_23_zone <- ang_cu %>%
    group_by(zone1) %>%
    summarise("sdangle"=mean(value), sd(value)))

## mean by zone & shore: all years
(mean_all <- subset(df, type == "angle") %>%
    group_by(zone1, shore) %>%
    summarise("mean.angle"=mean(as.numeric(value), na.rm=TRUE),
              "sd.angle"=sd(as.numeric(value),na.rm=TRUE)))

### mean by station: current year
(mn_st_cur <- subset(df, type == "angle") %>%
  filter(.,year == cur.yr) %>% 
  group_by(transect, shore) %>% 
  summarise("mean.angle"=mean(as.numeric(value),na.rm=TRUE),
            "sd.angle"=sd(as.numeric(value),na.rm=TRUE),
            .groups = "drop") %>% 
  arrange(.,by=mean.angle))
head(mn_st_cur);tail(mn_st_cur)

print(mean_23); print(mean_all)

df_tm <- df
df_tm$shore <- factor(df_tm$shore, levels=c("Upper","Mid","Low"))
png(file = "figs/sed.ts.mor.angle.png",
    width=12*ppi, height=6*ppi, res=ppi)
ggplot(data = df_tm[df_tm$type=="angle",],
       aes(y = as.numeric(value), x = year, fill = zone1))+
  geom_hline(yintercept = seq(0,12,by=2),colour="lightgrey",linetype=2)+
  geom_vline(xintercept = seq(2008,2024,by=1),colour="lightgrey",linetype=2)+
  geom_boxplot(aes(group=year),outlier.shape = NA)+
  geom_jitter(width = 0.1, height = 0,alpha=0.3)+
  geom_smooth(method = "loess", colour = "red", span = .9)+
  facet_grid(shore~zone1)+
  scale_colour_manual(name = "", values=cbPalette)+
  scale_fill_manual(name = "", values=cbPaletteFill[c(1:4,8)])+
  scale_x_continuous(breaks = seq(2008, 2024, by = 2))+
  xlab("Year") + ylab("Angle")+
  theme(plot.title = element_text(face = "bold"),
        legend.position="none",
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12),
        strip.text = element_text(face="bold"),
        axis.title = element_text(face="bold")
  )+
  labs(title = paste0("Beach slopes recorded since 2008 as part of the SFGPBM programme"))
dev.off()

## Statistical comparisons ####
tic("statistical comparison: Angle")
ang_cu_lnc <- droplevels(ang_cu %>% filter(., zone1 != "Wash"))

##re-level to compare all with Inside
ang_cu_lnc$zone1 <- factor(ang_cu_lnc$zone1,levels=c("Inside","Above","Inside2","Below"))

ang_cur_mod1 <- lmerTest::lmer(value ~ zone1 + (1|shore) , data = ang_cu_lnc,REML=FALSE)
ang_cur_mod2 <- lmerTest::lmer(value ~ zone1 + (transect|shore) , data = ang_cu_lnc,REML=FALSE)
anova(ang_cur_mod3 <- lmerTest::lmer(value ~ zone1 + (1|transect)+(1|shore) , data = ang_cu_lnc,REML=FALSE))
anova(ang_cur_mod3.1 <- lmerTest::lmer(value ~ zone1 + (shore|transect) , data = ang_cu_lnc,REML=TRUE))
anova(ang_cur_mod4 <- lmerTest::lmer(value ~ zone1*shore + (1|transect) , data = ang_cu_lnc,REML=FALSE))

# model checking: whis is best? Lowest AIC = 'better' fit
anova(ang_cur_mod1, ang_cur_mod2, ang_cur_mod3,ang_cur_mod3.1, ang_cur_mod4)###compare model fits
AIC(ang_cur_mod1,ang_cur_mod2,ang_cur_mod3,ang_cur_mod3.1,ang_cur_mod4) %>% 
  as.data.frame() %>%
  arrange(AIC) %>% print()
# ang_cur_mod1 has lowest AIC
plot(performance::compare_performance(ang_cur_mod1,
                                      ang_cur_mod2,
                                      ang_cur_mod3,
                                      ang_cur_mod3.1,
                                      ang_cur_mod4,
                                      rank = TRUE))
visreg::visreg(ang_cur_mod1)

# go with model ang_cur_mod1
anova(ang_cur_mod1)
summary(ang_cur_mod1)

lmerTest::ls_means(ang_cur_mod1, test.effs = "Group",pairwise = TRUE)
sjPlot::plot_model(ang_cur_mod1,show.values=TRUE, show.p=TRUE)
visreg::visreg(ang_cur_mod1)
performance::check_predictions(ang_cur_mod1)
toc(log = TRUE)

### WASH ONLY: compare transects ####
ang_cu %>% filter(., zone1=="Wash") ->ang_cu_wa
summary(m_ang_wa <- lm(value ~ shore, data=ang_cu_wa))


## time series ####
png(file = "output/figs/sed.ang.ts.png",
    width=12*ppi, height=6*ppi, res=ppi)
df %>% 
  filter(.,type=="angle") %>% 
  #filter(., zone1 != "Wash") %>%
  droplevels(.) %>% 
  mutate(value = as.numeric(value)) %>% 
  filter(., value >= 0) %>% 
  dplyr::select(.,c(year,shore,type,value,zone1)) %>% 
  ggplot(data=., aes(x=year, y=value, fill=zone1))+
  # ylim(0,12)+
  geom_vline(xintercept = seq(2008,cur.yr,by=1),linetype=2, colour="lightgrey")+
  geom_hline(yintercept = seq(0,12,by=2),linetype=2, colour="lightgrey")+
  geom_boxplot(outlier.shape = NA,
               aes(group=as.factor(year)), show.legend = FALSE)+
  geom_jitter(show.legend = FALSE, alpha=0.25)+
  facet_grid(shore~zone1)+
  scale_fill_manual(values=cbPaletteFill)+
  scale_y_continuous(breaks = seq(0,12,by = 4))+
  geom_smooth(span=0.9, show.legend = FALSE, col="red",
              # method = "gam"
              method = "loess"
  )+
  ylab("Beach angle (°)")+
  theme(axis.title.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12),
        strip.text = element_text(face="bold"))
dev.off()

###=============###
# Compaction ####
###=============###
com_cu <- cur_dat %>% filter(., type == "cone") %>% 
  mutate(value = as.numeric(value))

(com_cu_sum <- com_cu %>% 
    group_by(.,zone1) %>% 
    summarise(mean_cu=mean(value,na.rm=TRUE),sd.cu=sd(value,na.rm=TRUE)) %>% 
    ungroup())
com_cu_st <- com_cu %>% 
  filter(.,year==cur.yr) %>% 
  group_by(transect, shore) %>% 
  summarise("mean.cone"=mean(as.numeric(value),na.rm=TRUE),
            "sd.cone"=sd(as.numeric(value),na.rm=TRUE),
            .groups = "drop")
com_cu_st[which.min(com_cu_st$mean.cone),]
com_cu_st[which.max(com_cu_st$mean.cone),]

### boxplot ###
png(file = paste0("output/figs/sed.",cur.yr,".sed.mor.pen.png"),
    width=12*ppi, height=6*ppi, res=ppi)
set.seed(pi); com_cu %>% 
  #filter(., zone1 != "Wash") %>% 
  ggplot(data = ., aes(y = value, x = zone1,
                                        fill=zone1))+
  geom_boxplot(outlier.colour=NA)+
  geom_jitter(aes(shape = zone1),
              width = 0.3, height = 0.05,size = 3,stroke = 1,
              show.legend = FALSE)+
  theme(plot.title = element_text(face = "bold"),
        legend.position = "bottom",#c(.5,1.02),
        legend.direction = "horizontal",
        # legend.box = "horizontal",
        legend.key.size = unit(1,"cm"),#change legend size!
        legend.title=element_blank(),
        legend.text=element_text(size = 12,
                                 face = "bold"),
        legend.box.margin=margin(-10,-10,-10,-10),
        legend.margin=margin(0,0,0,0),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        strip.text.x = element_text(size = 12),
        strip.text = element_text(face="bold")
  )+
  xlab("") + ylab("Cone index")+
  scale_fill_manual(values=cbPaletteFill[c(1:4,8)])+
  scale_x_discrete(breaks=NULL)+
  scale_shape_manual(values = c(21:(length(unique(com_cu$zone1))+21)))+
  facet_wrap(~shore)+
  guides(fill=guide_legend(nrow=1))+
  labs(title = paste0("Sediment compaction recorded in the ",cur.yr," monitoring programme"),
       subtitle = "Higher values indicate more compacted sediments")
dev.off()

## Get summary ####
#require(dplyr)
(mean_cur <- com_cu %>%
   group_by(zone1, shore) %>%
   summarise(mean(value,na.rm=TRUE), sd(value,na.rm=TRUE)))

(mean_cur_zone <- com_cu %>%
    group_by(zone1) %>%
    summarise(mean(value,na.rm=TRUE), sd(value,na.rm=TRUE)))

(mean_all <- subset(df, type == "cone") %>%
    group_by(zone1, shore) %>%
    summarise(mean(as.numeric(value),na.rm=TRUE),
              sd(as.numeric(value),na.rm=TRUE)))

(mean_cur_tran <- com_cu %>%
    group_by(transect,shore) %>%
    summarise(mean(value,na.rm=TRUE), sd(value,na.rm=TRUE)))

### mean by station
mn_st_cur <- subset(df, type == "cone") %>%
  group_by(transect, shore) %>% 
  summarise(mean(as.numeric(value),na.rm=TRUE),
            sd(as.numeric(value),na.rm=TRUE),
            .groups = "drop")

## Time series ####
df_tm <- df
df_tm$shore <- factor(df_tm$shore, levels=c("Upper","Mid","Low"))

png(file = "output/figs/sed.ts.mor.pen.png",
    width=12*ppi, height=6*ppi, res=ppi)
df %>% 
  filter(.,type=="cone") %>% 
  filter(., zone1 != "Wash") %>% droplevels(.) %>% 
  mutate(value = as.numeric(value)) %>% 
  filter(., value >= 0) %>% 
  dplyr::select(.,c(year,shore,type,value,zone1)) %>% 
  ggplot(.,
       aes(y = as.numeric(value), x = year, fill = zone1))+
  # geom_vline(xintercept = seq(2008,cur.yr,by=1),linetype=2, colour="lightgrey")+
  # geom_hline(yintercept = seq(0,40,by=10),linetype=2, colour="lightgrey")+
  geom_boxplot(aes(group=year),outlier.shape = NA)+
  geom_jitter(width = 0.1, height = 0,alpha=0.3)+
  geom_smooth(method = "loess", colour = "red", span = .9)+
  # geom_smooth(method = "gam", colour = "red", span = .9)+
  facet_grid(shore~zone1)+
  scale_colour_manual(name = "", values=cbPalette)+
  scale_fill_manual(name = "", values=cbPaletteFill)+
  scale_x_continuous(breaks = seq(2008, 2024, by = 4))+
  scale_y_continuous(limits = c(0, NA), expand = c(0, 0))+
  xlab("Year") + ylab("Cone index")+
  theme(plot.title = element_text(face = "bold"),
        legend.position="none",
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12),
        strip.text = element_text(face="bold"),
        axis.title = element_text(face="bold"))+
  labs(title = paste0("Sediment compaction recorded since 2008 as part of the SFGPBM programme"),
       subtitle = "Higher values indicate more compacted sediments")
dev.off()

## Statistical comparison ####
## remove Wash data
com_cu_lnc <- droplevels(com_cu %>% filter(.,zone1!="Wash"))
##re-level to compare all with Inside
com_cu_lnc$zone1 <- factor(com_cu_lnc$zone1,levels=c("Inside","Above","Inside2","Below"))

com_cur_mod1 <- lmerTest::lmer(value ~ zone1 + (1|shore) , data = com_cu_lnc,REML=FALSE)
com_cur_mod2 <- lmerTest::lmer(value ~ zone1 + (transect|shore) , data = com_cu_lnc,REML=FALSE)
anova(com_cur_mod3 <- lmerTest::lmer(value ~ zone1 + (1|transect) + (1|shore) , data = com_cu_lnc,REML=FALSE))
anova(com_cur_mod3.1 <- lmerTest::lmer(value ~ zone1 + (shore|transect) , data = com_cu_lnc,REML=TRUE))
anova(com_cur_mod3.2 <- lmerTest::lmer(value ~ zone1*shore + (shore|transect) , data = com_cu_lnc,REML=TRUE))
anova(com_cur_mod4 <- lmerTest::lmer(value ~ zone1*shore + (1|transect), data = com_cu_lnc,REML=FALSE))

anova(com_cur_mod1,com_cur_mod2,com_cur_mod3,com_cur_mod3.1,com_cur_mod4)
AIC(com_cur_mod1,com_cur_mod2,com_cur_mod3,com_cur_mod3.1,com_cur_mod4)
### Lowest AIC = com_cur_mod3.1

summary(com_cur_mod3.1)
print(com_cur_mod3.1,cor=FALSE)
anova(com_cur_mod3.1)
lmerTest::ls_means(com_cur_mod3.1, test.effs = "Group",pairwise = TRUE)
sjPlot::plot_model(com_cur_mod3.1,show.values=TRUE, show.p=TRUE)
visreg::visreg(com_cur_mod3.1)
performance::check_posterior_predictions(com_cur_mod3.1)
ggplot(data=com_cu, aes(x=zone1,y=value,colour=transect))+
  # geom_point()+facet_wrap(.~shore)+
  geom_jitter(width = 0.2)+
  facet_wrap(.~shore)

## time series ####
png(file = "output/figs/sed.cone.ts.png",
    width=12*ppi, height=6*ppi, res=ppi)
df %>% 
  filter(.,type=="cone") %>% 
  mutate(value = as.numeric(value)) %>% 
  dplyr::select(.,c(year,shore,type,value,zone1)) %>% 
  ggplot(data=., aes(x=year, y=value, fill=zone1))+
  geom_boxplot(outlier.shape = NA,
               aes(group=as.factor(year)), show.legend = FALSE)+
  geom_jitter(show.legend = FALSE, alpha=0.25)+
  facet_grid(shore~zone1)+
  scale_fill_manual(values=cbPaletteFill[c(1:4,8)])+
  geom_smooth(span=0.9, show.legend = FALSE, col="red",
              # method = "gam"
              method = "loess"
  )+
  ylab("Cone index")+
  theme(axis.title.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12),
        strip.text = element_text(face="bold"))
dev.off()

# Wave Class #### not interesting!
# df %>% filter(.,type == "WaveClass") -> df_wave
# plot(df_wave$value)

# create summary table for appendices ####

glimpse(cur_dat)

cur_dat %>% 
  filter(.,type=="cone") %>% 
  mutate(value=as.numeric(value)) %>% 
  group_by(.,transect,shore,year) %>% 
  summarise(mean = mean(value, na.rm=TRUE),
            sd = sd(value, na.rm=TRUE)) %>% 
  View(.)

cur_dat %>% 
  filter(.,type=="angle") %>% 
  mutate(value=as.numeric(value)) %>% 
  group_by(.,transect,shore,year) %>% 
  summarise(mean = mean(value, na.rm=TRUE),
            sd = sd(value, na.rm=TRUE)) %>% 
  View(.)

# smry$value <- as.numeric(smry$value)

# smry <- smry %>% 
#   select(.,transect,shore,type,value,zone1) %>%
#   group_by(transect,shore,type,zone1) %>% 
#   summarise(mean=mean(value,na.rm=TRUE),sd=sd(value,na.rm=TRUE)) %>% ungroup()
# 
# smry$ID <- paste0(smry$transect,".",smry$shore)
# 
# smry.cone <- smry %>% 
#   filter(.,type=="cone") %>% ungroup() %>% 
#   rename(., "Mean cone" = mean, "SD cone"=sd)
# smry.ang <- smry %>% 
#   filter(.,type=="angle") %>% 
#   select(.,-(transect:zone1)) %>% ungroup() %>% 
#   rename(., "Mean angle" = mean, "SD angle"=sd)
# 
# oo <- left_join(smry.cone,smry.ang,by="ID")
# write.csv(oo,file="output/forDoc/2021.sed.bulk.csv",row.names = FALSE)

#### tidy up ####
### Unload data
rm(list = ls(pattern = "^ang"))
rm(list = ls(pattern = "^com"))
rm(list = ls(pattern = "^df"))
rm(list = ls(pattern = "^mean_"))
rm(list = ls(pattern = "^cur_"))
rm(list = ls(pattern = "^pl_"))
rm(cbPalette, ppi,mn_st_cur, cbPaletteTxt, cur.yr, perm)

### Unload packages
detach("package:tidyverse", unload=TRUE)
detach("package:effects", unload=TRUE)
# detach("package:flexplot", unload=TRUE)
detach("package:ggthemes", unload=TRUE)
detach("package:lmerTest", unload=TRUE)
detach("package:sjPlot", unload=TRUE)
detach("package:visreg", unload=TRUE)
