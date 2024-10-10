### 200_an.sedvol.R ####
### import and plot time series of nourishment loads ###

# Set up ####
### set universals & load packages ####
### load packages
ld_pkgs <- c("ggplot2","scales","dplyr")
vapply(ld_pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)
rm(ld_pkgs)

source("R/00_datfol.R")
cur.yr <- 2024
ggplot2::theme_set(ggthemes::theme_few())
cbPalette <- c("#0072B2","#e79f00","#009E73", "#9ad0f3",
               "#000000", "#D55E00", "#CC79A7", "#F0E442")
ppi <- 300                        
### import data  ####
vols <- read.csv(paste0(fol,"nourish_vol_and_landings.csv"), header = T)

####range of values ####
vols %>% filter(., notes == "annual_nourishment") %>% 
  summarise(min_val = min(.$value),
            max_val = max(.$value))

#### plot ####
png(file = "output/figs/sedvol.png",
    width=12*ppi, height=6*ppi, res=ppi)
vols %>% filter(., notes == "annual_nourishment") %>%
  ggplot(., aes(x=year,y=value))+
  geom_hline(
    yintercept = c(0, 500000, 1000000, 1500000, 2000000, 2500000),
    colour = "darkgrey",
    linetype = "dashed")+
  geom_bar(stat = "identity", colour = "black", fill = "lightgrey")+
  theme(legend.title=element_blank(),
        legend.direction="horizontal",
        legend.position = c(-0.05,1.2),
        axis.text.x = element_text(size = 12, angle = 90, vjust=0.5),
        axis.text.y = element_text(size = 12))+
  ylab(bquote("Nourishment volume ("~m^3*")")) + xlab("")+
  scale_x_continuous(breaks = seq(1994, max(vols$year), by = 1))+
  scale_y_continuous(labels=comma)
dev.off()

## mean ± SD since 2005
vols %>% filter(.,
                notes == "annual_nourishment",
                year > 2004) %>%
  summarise(mn=mean(value,na.rm = TRUE),
            sd=sd(value,na.rm = TRUE),
            current = value[.$year==cur.yr])

### highest by area
vols %>% filter(notes == "nourishment by location") %>% 
  arrange(desc(value))

vols %>% filter(notes == "nourishment by location") %>% 
  slice_max(value, with_ties = FALSE)

### Tidy up
rm(vols,ppi,cbPalette, fol,gisfol,cur.yr)
detach("package:ggplot2", unload=TRUE)
detach("package:scales", unload=TRUE)
