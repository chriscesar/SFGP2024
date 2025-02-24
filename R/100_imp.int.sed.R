## 100_imp.int.sed.R ##
## Import intertidal sediment data for later assessment

# Set up ####
### load packages ####
ld_pkgs <- c("tidyverse","readxl","tictoc")
vapply(ld_pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)
rm(ld_pkgs)

tictoc::tic.clearlog();tic("SET UP")

### load metadata ####
source("R/00_meta_setMeta.R")

ab <- c("T1N","T1","T1S")
inside <- c("T4","T7","T8","T11","T12")
inside2 <- "T13"
bel <- c("T15","T21","T22","T23","T24","T25","T26")
wash <- c("WA1")

df_sed <- readxl::read_xlsx(paste0(fol,"sed.data.ALL.USE.xlsx"), sheet = "AllDat") %>% 
  filter(., DetUse != "Remove: metadata") %>%  # drop unneeded rows
  mutate(., year = lubridate::year(SAMP_SAMPLE_DATE))
df_sed$zone1 <- ifelse(
  df_sed$Transect %in% ab,"Above",
  ifelse(
    df_sed$Transect %in% inside,"Inside",
    ifelse(
      df_sed$Transect %in% inside2,"Inside2",
      ifelse(
        df_sed$Transect %in% bel,"Below",
        ifelse(
          df_sed$Transect %in% wash,"Wash",NA
        )))))

df_sed$zone1 <- factor(df_sed$zone1, levels = c("Above","Inside","Inside2","Below","Wash"))
df_sed$Shore <- factor(df_sed$Shore, levels = c("Upper","Mid","Low","Surf"))
df_sed$Transect <- factor(df_sed$Transect,
                          levels = c(
                            "T1N", "T1","T1S",
                            "T4","T7", "T8", "T11", "T12",
                            "T13",
                            "T15", "T21", "T22", "T23","T24", "T25","T26",
                            "WA1"
                          ))

# load and append older data ####
df_sed_old <- read.csv(file = paste0(fol,"sed.psa.hi.ts.csv"))

df_sed_bulk <- readxl::read_xlsx(paste0(fol,"sed.psa.bulkWIP_use.xlsx"),
                                 sheet="sed.bulk.ts.out")
df_sed_bulk$transect <- factor(df_sed_bulk$transect, levels=c(
  "T1N","T1","T1S","T4","T11","T7", "T8", "T12", "T13", "T15", "T17",
  "T20","T21", "T22", "T23", "T24", "T25", "T26","WA1"
))

df_sed_bulk$shore <- factor(df_sed_bulk$shore,levels=c("Upper","Mid","Low","Surf"))
df_sed_bulk$zone1 <- factor(df_sed_bulk$zone1, levels=c("Above","Inside","Inside2","Below","Wash"))

names(df_sed)
names(df_sed_old)

rm(ab,inside,inside2,bel,wash)
toc(log=TRUE)
