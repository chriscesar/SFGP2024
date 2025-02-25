# 200_an.inf_Wash.R ####
### Display of Wash assemblages

# Set up ####
## load packages ####
ld_pkgs <- c("tidyverse","ggplot2","vegan",
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

# format Wash data ####
# sum duplicated taxa ####
df %>% #names()
  filter(!is.na(count)) %>%
  filter(.,zone1 == "Wash") %>% #names()
  ## remove zero values
  filter(.,count !=0) %>% #names()
  ## convert -9999 to 1
  mutate(count = ifelse(count < 0, #replace <0 values with 1
                        1,
                        count)) %>%
  dplyr::select(.,
                -taxonReported,
                -zone2.1,-zone2.2,
                -yr.trn,-yr.trn.sh,-yr.trn.sh.meth,-yr.trn.sh.meth.rep,
                -Kingdom,
                -Phylum,
                -Class,
                -Order,
                -Family,
                -Genus,
                -Species,
                -Comment
  ) %>% 
  group_by(across(c(!count)
  )) %>% 
  summarise(.,count=sum(count), .groups = "drop") %>% ungroup() %>% 
  ## remove superfluous cols
  ### widen and fill gaps with 0:
  pivot_wider(.,
              names_from=taxonUSE,
              values_from=count,
              values_fill=list(count = 0)
  ) -> dfwash_all
  

# plots ####
dfwash_all %>% 
  ## lengthen
  pivot_longer(.,cols = -c("year","transect","shore","rep","zone1","mesh",
                           "core.area_m2"),
               names_to = "Taxon",values_to="Abundance") %>% 
  ## calculate mean across reps
  dplyr::select(.,-rep) %>% 
  group_by(across(!"Abundance")) %>% 
  summarise(., Abundance = sum(Abundance),.groups = "drop") %>% ungroup() %>% 
  filter(.,mesh=="1.0mm") -> dfwash_mean

Taxon <- unique(dfwash_mean$Taxon)
xxx <- vegan::make.cepnames(Taxon)

xx <- data.frame("Taxon" = Taxon, "Taxon_lab" = xxx)

dfwash_mean <- left_join(dfwash_mean, xx, by = "Taxon")
dfwash_mean$shore <- factor(dfwash_mean$shore, levels=c("Mid","Low"))

ntax <- length(unique(dfwash_mean$Taxon))
minyr <- min(dfwash_mean$year)

dfwash_mean <- dfwash_mean %>% filter(Abundance!=0)

dfwash_mean <- bind_rows(dfwash_mean,tibble(year=c(2020,2022,2023),
                                            shore=c("Low","Low","Mid"),
                                            Taxon_lab=c("Ceraedul","Ceraedul","Ceraedul")))

dfwash_mean$shore <- factor(dfwash_mean$shore, levels=c("Mid","Low"))

pl <- dfwash_mean %>% 
  #filter(Abundance != 0) %>% 
  # filter(!is.na(shore)) %>% 
  ggplot(.,
         aes(
           y = as.factor(year),
           x = Taxon_lab
         )) +
  geom_vline(xintercept = seq(from = 1, to = ntax,by=1), colour="grey",lty=2)+
  geom_point(aes(size = log(Abundance+1),
                 colour = log(Abundance+1)),show.legend = FALSE)+
  facet_wrap(.~shore)+
  #scale_y_discrete(limits=rev)+
  scale_x_discrete(limits=rev)+
  labs(title = paste0(
    "Abundances of taxa recorded in transects in The Wash since ",
    minyr),
    subtitle = "Point colours and sizes indicate the relative abundances of different taxa",
    caption = "Taxon names abbreviated using the vegan::makecepnames() function in R.
    Note that the 2024 data include taxa gathered across three transects. Previous data gathered across a single transect")+
  theme(
    axis.text.x = element_text(
      #angle = 90,
      face=2#,
      # hjust = 1,
      # vjust=.5
      ),
    axis.text.y = element_text(face=2),
    plot.title = element_text(face=2),
    strip.text = element_text(face=2),
    axis.title = element_blank()
  )+
  coord_flip()
ggsave(plot = pl, filename = "output/figs/WashTaxa2.png",
         width = 16,height = 8,units = "in");rm(pl)

# tidy up ####
rm(list=ls(pattern = "^df"))
rm(list=ls(pattern = "^cb"))
rm(xxx,xx,cur.yr,fol,gisfol,minyr,ntax,perm,ppi,projfol,source_file,Taxon,sum_zero)

detach("package:tidyverse", unload=TRUE)
detach("package:vegan", unload=TRUE)
detach("package:ggplot2", unload=TRUE)
detach("package:tictoc", unload=TRUE)
