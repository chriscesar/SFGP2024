# 300_expt_an_inf_zero_infl.R ####
# Playing with zero-inflated dissimilarity analysis ####

df.cur %>% 
  dplyr::select(.,-AFAUNAL) %>% 
  pivot_longer(
    cols =-c(year:core.area_m2),
    names_to = "taxon",values_to = "abundance") %>% 
  ## drop 'rep' and calculated mean abundances
  dplyr::select(.,-rep) %>% 
  group_by(across(c(!abundance))) %>% 
  summarise(abundance = mean(abundance), .groups = "drop") %>%
  ungroup() %>% #View()
  ## remove Wash Samples %>% 
  filter(., zone1 != "Wash") %>% 
  #convert presence values (-9999) to 1 for ordination
  mutate(abundance = ifelse(abundance < 0, #replace <0 values with 1
                            1,
                            abundance)) %>%
  #rewiden for ordination
  pivot_wider(.,names_from = taxon,
              values_from = abundance,
              values_fill=list(abundance = 0)) %>% #names()
  ungroup() %>% 
  ## keep metadata and remove zero-sum cols
  dplyr::select(.,year,transect,shore,zone1,core.area_m2,
                where(~any(. !=0))) %>% 
  ungroup() -> df.cur.w.trm

## create zero-inflated distance
inf.dist <- vegan::designdist(df.cur.w.trm %>%
  dplyr::select(.,-c(year,transect,shore,zone1,
                     core.area_m2)),
  method = "(A+B-2*J)/(A+B+2*min(x[x>0]))", terms = "minimum")#bray0
  #method = "(A+B-2*J)/(A+B)",terms="minimum")#bray
dim(as.matrix(inf.dist))

set.seed(pi);ord <- vegan::metaMDS(inf.dist,
                                   k = 2,
                                   try = 100,
                                   trymax = 500
                                   )
plot(ord)

data.scores <- as.data.frame(scores(ord))
names(data.scores) <- c("NMDS1","NMDS2")
data.scores$transect <- df.cur.w.trm$transect
data.scores$shore <- df.cur.w.trm$shore
data.scores$zone1 <- df.cur.w.trm$zone1

data.scores$zone1 <- factor(data.scores$zone1,levels=c("Above","Inside","Inside2","Below"))
data.scores$shore <- factor(data.scores$shore, levels = c("Mid","Low"))

data.scores %>% 
  ggplot2::ggplot(.,
                  aes(x= NMDS1,y=NMDS2)
                  )+
  ggplot2::geom_hline(yintercept = 0,lty=2)+
  ggplot2::geom_vline(xintercept = 0, lty=2)+
  ggplot2::geom_point(aes(colour = shore,
                          shape = zone1,
                          fill=zone1),
                      size=5,
                      stroke=1.5)+
  ggplot2::scale_fill_manual(values=cbPalette)+
  ggplot2::scale_colour_manual(values=c(1,2))+
  ggplot2::scale_shape_manual(values = c(21,22,23,24))+
  ggplot2::coord_fixed()
