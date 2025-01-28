# an.sed.dat.R ####

# Set up ####
### load packages ####
ld_pkgs <- c("tidyverse","readxl","tictoc","ggridges")
vapply(ld_pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)
rm(ld_pkgs)

tictoc::tic.clearlog();tic("Proj set up")
# load data & prep ####
# load
source("R/100_imp.int.sed.R")

#prep
df_psa <- df_sed %>% 
  filter(.,str_detect(DetUse, 'phi')) %>% 
  mutate(.,phi = as.numeric(substr(DetUse, 1, nchar(DetUse)-3))) %>% 
  mutate(., year = lubridate::year(SAMP_SAMPLE_DATE)) %>% 
  dplyr::select(REGION,SAMP_SAMPLE_DATE,year,zone1,Transect,Shore,method,phi,MEAS_RESULT,
                DetUse,SAMP_ID,Status
                )

df_sed_old_trm <- df_sed_old %>% 
  filter(year <2015)

y1 <- df_sed_old_trm$year;unique(y1)
y2 <- df_psa$year;unique(y2)
yr <- c(y1,y2);rm(y1,y2);unique(yr)

p1 <- round(df_sed_old_trm$phi,1);unique(p1)
p2 <- round(df_psa$phi,1);unique(p2)
p <- c(p1,p2);rm(p1,p2);unique(p)

z11 <- as.factor(df_sed_old_trm$zone1);unique(z11)
z12 <- df_psa$zone1;unique(z12)
z1 <- factor(c(z11,z12),levels = c("Above","Inside","Inside2","Below","Wash"));rm(z11,z12);unique(z1)

s1 <- as.factor(df_sed_old_trm$shore);unique(s1)
s2 <- df_psa$Shore;unique(s2)
s <- factor(c(s1,s2),levels=c("Upper","Mid","Low","Surf"));rm(s1,s2);unique(s)

m1 <- df_sed_old_trm$method;unique(m1)
m2 <- df_psa$method;unique(m2)
m <- c(m1,m2);rm(m1,m2);unique(m)

r1 <- df_sed_old_trm$perc
r2 <- df_psa$MEAS_RESULT
r <- c(r1,r2);rm(r1,r2)

t1 <- df_sed_old_trm$transect
t2 <- df_psa$Transect
t <- c(t1,t2);rm(t1,t2)

df_pl <- as_tibble(data.frame("year" = yr,"transect"=t,"method"=m,"phi" = p,
                              "zone1" = z1, "shore" = s, "perc"=r))
rm(p,yr,z1,s,r,m,t)
toc(log=TRUE)

# calculate mean phi contributions by nourishment group & shore ####
df_pl %>% 
  filter(method != "15cm") %>% 
  filter(zone1 != "Wash") %>% 
  dplyr::select(-method) %>% 
  group_by(year, zone1,shore,phi) %>% 
  summarise("perc"=mean(perc),
            .groups = "drop") %>%
  ungroup() %>% 
  mutate(label=paste0(shore," shore: ",zone1)) %>% 
  ggplot(data = .,
             aes(x = phi, y=as.factor(year), height = perc))+
  geom_rect(aes(xmin = -5,xmax = 0,ymin = -Inf,ymax = Inf),fill="grey",alpha=0.01)+
  geom_rect(aes(xmin = 0,xmax = 4,ymin = -Inf,ymax = Inf),fill="yellow",alpha=0.005)+
  geom_rect(aes(xmin = 4,xmax = 9,ymin = -Inf,ymax = Inf),fill="sienna",alpha=0.005)+
  geom_vline(xintercept = c(0,4), colour="darkgrey",lty=2)+
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
  theme(strip.text = element_text(size = 14, face="bold")) -> pl
png("output/figs/sed.ts.psahires.png", width=14,height = 14,units = "in",res=ppi)
print(pl)
dev.off()

# prep data for Gradistat ####
## create mm and um values from phi
df_pl$dia_um <- (2^-(df_pl$phi))/.001
df_pl$dia_mm <- 2^-(df_pl$phi)

# generate sample code
df_pl$code <- paste0(df_pl$year,"_",df_pl$transect,"_",df_pl$shore,"_",df_pl$method)
# df_pl %>% 
#   dplyr::select(.,code,dia_um,perc) %>% 
#   pivot_wider(names_from = code,values_from = perc) -> df_grad_all


df_pl %>%
  dplyr::select(.,code,dia_um,perc) %>% 
  group_by(code) %>%
  mutate(row = row_number()) %>%
  tidyr::pivot_wider(names_from = code, values_from = perc,values_fill = 0) %>%
  select(-row) ->df_grad_all

df_grad_all %>% 
  dplyr::select(dia_um, starts_with("2024")) -> df_cur
  
  # dplyr::summarise(n = dplyr::n(), .by = c(dia_um, code)) |>
  # dplyr::filter(n > 1L) -> x

write.csv(df_cur, file="data/Gradistat/sample.data.csv",row.names = FALSE)

# tidy up ####
rm(list=ls(pattern = "^df"))
rm(list=ls(pattern = "^cb"))
rm(sum_zero,cur.yr,fol,gisfol,perm,ppi,projfol)
