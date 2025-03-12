# 100_imp.Wash.sed.R ####
## import sediment data for the Wash and prep for Gradistat ###

# Set up ####
### load packages ####
ld_pkgs <- c("tidyverse","readxl","tictoc","ggridges","lme4","lmerTest")
vapply(ld_pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)
rm(ld_pkgs)

tictoc::tic.clearlog();tic("SET UP")

source("R/100_imp.int.sed.R")

### prep for Gradistat

# prep data for Gradistat ####
df_sed %>% 
  filter(.,str_detect(DetUse, 'phi')) %>% 
  filter(.,year == cur.yr) %>% 
  filter(str_starts(Transect, "WA")) %>% #names(.)
  mutate("code" = paste0(Transect,".",Shore,".",method)) %>% #View(.)
  dplyr::select(.,code,sediment_um,MEAS_RESULT) %>% 
  pivot_wider(.,names_from = code,values_from = MEAS_RESULT) -> sed_wash

# dplyr::summarise(n = dplyr::n(), .by = c(dia_um, code)) |>
# dplyr::filter(n > 1L) -> x

write.csv(sed_wash, file="data/Gradistat/sample.data_wash.csv",row.names = FALSE)
