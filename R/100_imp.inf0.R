## 100_imp.inf0.R ##
## Import infaunal data for later assessment

# Set up ####
### load packages ####
ld_pkgs <- c("tidyverse","readxl")
vapply(ld_pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)
rm(ld_pkgs)

### load metadata ####
source("R/00_meta_setMeta.R")

# copy & load data ####

file_name <- "inf_ts_longRAW_USE.xlsx"

# Set the source and destination file paths
source_file <- paste0(fol,file_name)
destination_file <- paste0("data/in/",file_name)

# Check if the file exists in the data folder
if (!file.exists(destination_file)) {
  # If not, copy the file from the source folder to the data folder
  file.copy(source_file, destination_file)
  cat("File copied successfully.\n")
} else {
  # If the file already exists in the data folder, do nothing
  cat("File already exists in the data folder.\n")
}

df0 <- as_tibble(read_xlsx(destination_file,
                           sheet = "dat_all",
                           guess_max = 10000))

# drop nuicance/non-marine taxa ####
df <- df0 %>%
  filter(., is.na(Order) | Order != "Diptera") %>% ###drop flies
  filter(., is.na(Order) | Order != "Hemiptera") %>% ###drop bugs 
  filter(., is.na(Order) | Order != "Lepidoptera") %>% ###drop butterflies/moths
  filter(., is.na(Order) | Order != "Hymenoptera") %>% ###drop ants/bees/wasps
  filter(., taxonUSE != "Animalia") %>%  ###drop taxa flagged only as Animalia
  filter(., is.na(Kingdom) | Kingdom != "Chromista") %>% ### drop chromists
  filter(., is.na(Kingdom) | Kingdom != "Bacteria") %>% ### drop bacteria
  filter(., is.na(Kingdom) | Kingdom != "Plantae") %>% ### drop plants
  filter(., !str_detect(taxonReported, "fragment")) ###remove fragments of taxa

# sum duplicated taxa ####
df %>% #names()
  filter(!is.na(count)) %>%
  dplyr::select(.,
                -taxonReported,
                #-units,
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
  filter(., mesh == "1.0mm") %>% ##keep only 1mm mesh
  ## remove superfluous cols
  
  ### widen and fill gaps with 0:
  pivot_wider(.,
              names_from=taxonUSE,
              values_from=count,
              values_fill=list(count = 0)
              ) -> dfw
  # ### re-lengthen for summarising:
  # pivot_longer(.,13:ncol(.),names_to="taxon",values_to="count") %>%names()
  # select(.,
  #        -rep,
  #        -yr.trn.sh.meth.rep
  #        ) %>% ###drop 'rep' and code variables
  # ### calculate mean across replicates:
  # group_by(across(c(!count))) %>%
  # summarise(.,count=mean(count, na.rm = TRUE),
  #           .groups = "drop") %>%
  # ungroup() %>%
  # ##re-widen:
  # pivot_wider(.,names_from=taxon,values_from=count) %>% ungroup()

# TIDY UP ####
# rm(list = ls(pattern = "^df"))
# rm(list = ls(pattern = "^cb"))
# rm(cur.yr,destination_file,file_name,fol,gisfol,perm,ppi,source_file,projfol)

detach(package:readxl, unload=TRUE)
detach(package:tidyverse, unload=TRUE)
