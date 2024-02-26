library(sf)
library(dplyr)
library(reticulate)
udp_typologies <- py_load_object(paste0(getwd(), '/data/pickle_files/final_typology_output.pkl'))

save(udp_typologies, file = paste0(getwd(), '/data/outputs/typologies/final_typology_output.RData'))

load(file = paste0(getwd(), '/data/R_data/fatal_enc_2019.RData'))
fatal_enc_2019 <- joined_2019
rm(joined_2019)

fatal_enc_2019 <- st_drop_geometry(fatal_enc_2019)
udp_typologies <- st_drop_geometry(udp_typologies)

fatal_enc_2019_udp <- fatal_enc_2019 |>
  left_join(
    x = fatal_enc_2019,
    y = udp_typologies[,c("NAME", "SAE", "AdvG", "ARE", "BE", "SMMI",
                          "ARG", "EOG", "OD", "OD_loss", "LISD", "GEOID",
                          "double_counted", "typology", "typology_text")],
    by = join_by(GEOID)
  )
