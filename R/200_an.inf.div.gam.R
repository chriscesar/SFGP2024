# an.inf.div.gam.R ####
# calculate & analyse diversity indices using GAMs and 'standard' approaches ####

# Set up ####
## set local library ####

## load packages ####
ld_pkgs <- c("tidyverse","mgcv","gratia","lmerTest")
vapply(ld_pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)
rm(ld_pkgs)
# library(tidyverse);library(mgcv);library(gratia)

##set universals ####
perms <- 9999 ###number of permutations
ppi <- 300 ###resolution of images
theme_set(ggthemes::theme_few())###set theme for all ggplot objects
cbPalette <- c("#0072B2","#e79f00","#009E73", "#9ad0f3", "#000000", 
               "#D55E00", "#CC79A7", "#F0E442")
cur.yr <- 2024

### load data ####
df <- read.csv("data/out/inf_div.csv")

df$transect <- factor(df$transect,levels=c("T1N","T1", "T1S",
                                           "T4","T7","T8",
                                           "T11","T12","T13",
                                           "T15","T17","T20","T21",
                                           "T22","T23",
                                           "T24","T25","T26",
                                           "WA1","WA5","WA6"))
### shore
df$shore <- factor(df$shore,levels = c("Mid","Low"))

###zone
df$zone1 <- factor(df$zone1,levels=c("Above","Inside",
                                     "Inside2","Below", "Wash"))

# run a GAM!####
### 
#chop off pre2007 data
df <- df[df$year>2006,]
summary(m.S.nb <- gam(S ~ zone1 + s(year,by=zone1,m=c(2,0)),
                      data=df, family = nb))
# summary(m.S.pois <- gam(S ~ zone1 + s(year,by=zone1,m=c(2,0)),
#                         data=df, family = poisson))
# summary(m.S.pois0 <- gam(S ~ zone1 + s(year,by=zone1),
#                          data=df, family = poisson))
# AIC(m.S.nb,m.S.pois,m.S.pois0) # m.S.nb is'best'
# summary(m.S.nb2 <- gam(S ~ zone1 + year + s(year,by=zone1,m=c(2,0)),data=df, family = nb))
# summary(m.S.pois2 <- gam(S ~ zone1 +year + s(year,by=zone1,m=c(2,0)),data=df, family = poisson))

# draw(m.S.pois,residuals = TRUE,scales = "fixed")#,constant = coef(m.S)[1])
draw(m.S.nb,residuals = TRUE,scales = "fixed")#,constant = coef(m.S)[1])

appraise(m.S.nb,method = "simulate")

#use predict() to generate data from the model
new.yr <- with(df,tibble(year=seq(min(year),max(year),length.out = 400)))
new.yrA <- new.yr;new.yrB <- new.yr;new.yrI <- new.yr;new.yrI2 <- new.yr
new.yrA$zone1 <- "Above";new.yrB$zone1 <- "Below";new.yrI$zone1 <- "Inside";new.yrI2$zone1 <- "Inside2"
nu <- rbind(new.yrA,new.yrB,new.yrI,new.yrI2)
nu$zone1 <- factor(nu$zone1,levels=c("Inside","Above","Inside2","Below"))
rm(new.yr,new.yrA,new.yrB,new.yrI,new.yrI2)
pred <- predict(m.S.nb,newdata = nu,se.fit = TRUE,type="link")
pred <- bind_cols(nu,as_tibble(as.data.frame(pred)))

### convert to response scale
ilink <- inv_link(m.S.nb)
crit <- 1.96
pred <- mutate(pred, richness = ilink(fit),
               lwr = ilink(fit - (crit * se.fit)), # lower...
               upr = ilink(fit + (crit * se.fit)))
pred$zone1 <- factor(pred$zone1, levels = c("Above","Inside","Inside2","Below"))

ggplot(pred,aes(x=year))+
  geom_vline(xintercept = c(2007:2022),lty=2,col="lightgrey")+
  geom_ribbon(aes(ymin = lwr, ymax = upr,fill=zone1),
              alpha = 0.2,show.legend = FALSE) +
  # geom_point(data=df,aes(x=year,y=S),colour="darkgrey") +
  geom_jitter(height=0,width=0.15,data=df,aes(x=year,y=S),colour="darkgrey") + 
  geom_line(aes(y = richness)) + labs(y = "Species richness", x = NULL)+
  geom_hline(yintercept = mean(pred$richness),lty=2,col="red")+
  geom_hline(yintercept = mean(df$S),lty=3,col="blue")+
  scale_fill_manual(values=cbPalette)+
  facet_wrap(~zone1,scales = "fixed")


### see https://stats.stackexchange.com/questions/544695/mgcvgamm-extract-breakpoints

deriv1 <- gratia::derivatives(m.S.nb, type="central",order=1)
deriv2 <- gratia::derivatives(m.S.nb, type="central",order=2)

# draw(deriv1,scale="fixed")
# draw(deriv2,scale="fixed")


### change pts - first derivative ####
deriv1 <- 
  deriv1 %>% 
  mutate(change = ifelse(.lower_ci < 0 & .upper_ci > 0, NA_real_, .derivative)) %>% 
  mutate(zone1 = ifelse(grepl("Above",.smooth),"Above",
                        ifelse(grepl("Inside2",.smooth),"Inside2",
                               ifelse(grepl("Below",.smooth),"Below",
                                      ifelse(grepl("Wash",.smooth),"Wash",
                                      "Inside")))))
deriv1$zone1 <- factor(deriv1$zone1, levels = c("Above","Inside","Inside2","Below","Wash"))

# draw(deriv1)+
#   geom_line(data=deriv1, aes(x=data, y= change), lwd=2)+facet_wrap(.~.smooth)

ggplot(deriv1, aes(x = year, y = .derivative)) +
  geom_vline(xintercept = c(2007:2024),lty=2,col="lightgrey")+
  geom_hline(yintercept = 0, col="lightgrey")+
  geom_ribbon(aes(ymin=.lower_ci,ymax=.upper_ci, fill=zone1), alpha=0.4) +
  geom_line() +
  scale_fill_manual(values=cbPalette)+
  facet_wrap(. ~ zone1) +
  geom_line(aes(x=year, y= change), col=2,lwd=2)+
  theme(legend.position = "none")+
  xlab("")+ylab("Derivative(Taxon richness)")


### change pts - second derivative ####
deriv2 <- deriv2 %>% 
  mutate(change = ifelse(.lower_ci < 0 & .upper_ci > 0, NA_real_, .derivative)) %>% 
  mutate(zone1 = ifelse(grepl("Above",.smooth),"Above",
                        ifelse(grepl("Inside2",.smooth),"Inside2",
                               ifelse(grepl("Below",.smooth),"Below",
                                      ifelse(grepl("Wash",.smooth),"Wash",
                                             "Inside")))))
deriv2$zone1 <- factor(deriv2$zone1, levels = c("Above","Inside","Inside2","Below", "Wash"))

# draw(deriv2)+
#   geom_line(data=deriv2, aes(x=data, y= change), lwd=2)+facet_wrap(.~smooth)

ggplot(deriv2, aes(x = year, y = .derivative)) +
  geom_vline(xintercept = c(2007:2024),lty=2,col="lightgrey")+
  geom_hline(yintercept = 0, col="lightgrey")+
  geom_ribbon(aes(ymin=.lower_ci,ymax=.upper_ci, fill=zone1), alpha=0.4) +
  geom_line() +
  scale_fill_manual(values=cbPalette)+
  facet_wrap(. ~ zone1) +
  geom_line(aes(x=year, y= change), col=2,lwd=2)+
  theme(legend.position = "none")+
  xlab("")+ylab("Derivative(Taxon richness)")

#### According to ChatGPT:
# To analyze whether your different treatments show differing trends over time
# and account for the expected nonlinear trends, using a Generalized Additive
# Model (GAM) approach is a good choice. You can specify your GAM model in R as
# follows:

summary(fit2 <- gam(S ~ relevel(zone1,ref = "Inside") + s(year, by = zone1),
                    data = df, family = nb))

#### According to ChatGPT:
#Interpreting the Model:
# With this setup, you can interpret the model as follows:
# The linear term for zone1 represents the overall effect of treatments on
# species counts while ignoring time.
# The smooth term s(year, by = zone1) represents how the effect of time (year)
# on species counts varies for each treatment group.

#Interpreting Results:
# After fitting the model, you can use summary(fit2) to view the results.
# This summary will provide information about the estimated coefficients
# for zone1 and the smooth terms for Year within each treatment group

#Comparing Treatment Trends:
# To determine whether the different treatments show differing trends over time,
# you can compare the estimated smooth functions for Year within each treatment
# group. Look for differences in the shapes of these functions. If they exhibit
# distinct nonlinear trends, it suggests that the treatments have varying
# effects on species counts over time.
