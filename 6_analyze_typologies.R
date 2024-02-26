library(sf)
library(dplyr)
library(reticulate)

# Load udp typologies from python pickle ####
udp_typologies <-
  py_load_object(paste0(getwd(), '/data/pickle_files/final_typology_output.pkl'))

# Save udp typologies to RData file
# save(
#   udp_typologies,
#   file = paste0(
#     getwd(),
#     '/data/outputs/typologies/final_typology_output.RData'
#   )
# )

# Fatal encounters with 2019 GEOID (already joined) ####
load(file = paste0(getwd(), '/data/R_data/fatal_enc_2019.RData'))

## Rename -- orig. name isn't clear ####
fatal_enc_2019 <- joined_2019
rm(joined_2019) # remove -- not needed

# Load all census tracts -- 2019 data ####
load(file = paste0(getwd(), '/data/R_data/all_tracts_2019.RData'))

## Rename ####
all_tracts_2019 <- income_population_quintiles_2019
rm(income_population_quintiles_2019)

# Add UDP to 2019 census data ####
all_tracts_2019 <- all_tracts_2019 |> 
  left_join(
    x = all_tracts_2019,
    y = udp_typologies[c('GEOID', 'typology', 'typology_text')],
    by = join_by(GEOID)
  )

# Remove geometries since I can join with GEOIDs/FIPS ####
fatal_enc_2019 <- st_drop_geometry(fatal_enc_2019)
udp_typologies <- st_drop_geometry(udp_typologies)

# Bring UDP typologies into fatal encounters df ####
fatal_enc_2019_udp <- fatal_enc_2019 |>
  left_join(x = fatal_enc_2019,
            y = udp_typologies[, c(
              "NAME",
              "SAE",
              "AdvG",
              "ARE",
              "BE",
              "SMMI",
              "ARG",
              "EOG",
              "OD",
              "OD_loss",
              "LISD",
              "GEOID",
              "double_counted",
              "typology",
              "typology_text"
            )],
            by = join_by(GEOID))





udp_populations <- aggregate(pop_18 ~ typology_text, data = udp_typologies, FUN = sum)

