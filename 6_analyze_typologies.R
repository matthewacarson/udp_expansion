rm(list = ls())
# Load libraries ----------------------------------------------
library(sf)
library(tidyverse)
conflicted::conflicts_prefer(dplyr::filter)
conflicted::conflicts_prefer(dplyr::filter)
library(reticulate)
library(jmv)
library(openxlsx)

# Load data ---------------------------------------------------

## UDP typologies from python pickle --------------------------

udp_typologies <-
  py_load_object(paste0(getwd(), '/data/pickle_files/final_typology_output.pkl'))


### Save udp typologies to RData ------------------------------

# save(
#   udp_typologies,
# file = paste0(
#   getwd(),
#   '/data/outputs/typologies/final_typology_output.RData'
# )
# )

## Load udp typologies from RData -----------------------------

# load(
#   paste0(
#     getwd(), 
#     '/data/outputs/typologies/final_typology_output.RData'))

### Fatal encounters with 2019 GEOID (joined) -----------------

load(file = paste0(getwd(), '/data/R_data/fatal_enc_2019.RData'))

##### Recode race and race_imputed & rename --------------------------------------------

fatal_enc_2019 <- joined_2019 |>
  mutate(
    race =
      case_when(
        race == "European-American/White" ~ "White",
        race == "African-American/Black" ~ "Black",
        race == "Hispanic/Latino" ~ race,
        TRUE ~ "Other/Unknown"
      ),
    race_imputed =
      case_when(
        race_imputed == "European-American/White" ~ "White",
        race_imputed == "African-American/Black" ~ "Black",
        race_imputed == "Hispanic/Latino" ~ race_imputed,
        TRUE ~ "Other/Unknown"
      )
  ) |> filter(race_imputed != "Other/Unknown")

### All census tracts -- 2019 data ----------------------------

load(file = paste0(getwd(), '/data/R_data/all_tracts_2019.RData'))

#### Rename all_tracts_2019 -----------------------------------

all_tracts_2019 <- income_population_quintiles_2019
rm(income_population_quintiles_2019)
# Condense UDP categories -------------------------------------
# There are currently too many categories -- combine

udp_typologies <- udp_typologies |>
  mutate(
    'UDP Typology' = case_when(
      # "LIR", # Low-income or at-risk
      typology %in% c('LISD', 'ARG') ~ 'L-income/At-risk',
      # 'GIP', # Gentrification in progress
      typology %in% c('EOG', 'OD', 'BE') ~ 'Gentrifying',
      # "SMOHI", # Stable: mixed or high-income
      typology %in% c('ARE', 'SAE', 'SMMI', 'AdvG') ~ 'Stable',
      
      TRUE ~ NA
    )
    
    # ,typology = case_when(
    #   # "LIR", # Low-income or at-risk
    #   typology %in% c('LISD', 'OD', 'ARG') ~ 'LIR',
    #   # ' Low-income or at-risk',
    #   # 'GIP', # Gentrification in progress
    #   typology %in% c('EOG', 'AdvG', 'SMMI') ~ 'GIP',
    #   # 'Gentrification in progress',
    #   # "SMOHI", # Stable: mixed or high-income
    #   typology %in% c('ARE', 'BE', 'SAE') ~ 'SMOHI',
    #   # 'Stable: mixed or high-income',
    #   TRUE ~ NA
    # )
  )


### Make typologies ordered -----------------------------------

typology_order <- c("L-income/At-risk",
                    "Gentrifying",
                    "Stable")

## Remove geometries since I can join with GEOIDs/FIPS --------

fatal_enc_2019 <- st_drop_geometry(fatal_enc_2019)
udp_typologies <- st_drop_geometry(udp_typologies)
all_tracts_2019 <- st_drop_geometry(all_tracts_2019)

# Combining -----------------------------------------------

## Add UDP to 2019 census data --------------------------------

### Columns to keep -------------------------------------------

keep_udp_cols <- c(
  'GEOID',
  'pop_18',
  'white_18',
  "pop_00",
  "white_00",
  "hu_00",
  "ohu_00",
  "rhu_00",
  "total_25_00",
  "male_25_col_bd_00",
  "male_25_col_md_00",
  "male_25_col_psd_00",
  "male_25_col_phd_00",
  "female_25_col_bd_00",
  "female_25_col_md_00",
  "female_25_col_psd_00",
  "female_25_col_phd_00",
  "mhval_00",
  "mrent_00",
  "hh_00",
  "hinc_00",
  "trtid00",
  "pop_90",
  "hh_90",
  "white_90",
  "total_25_col_9th_90",
  "total_25_col_12th_90",
  "total_25_col_hs_90",
  "total_25_col_sc_90",
  "total_25_col_ad_90",
  "total_25_col_bd_90",
  "total_25_col_gd_90",
  "hinc_90",
  "ohu_90",
  "rhu_90",
  "mrent_90",
  "mhval_90",
  "trtid90",
  "inc80_18",
  "inc120_18",
  "inc80_00",
  "inc120_00",
  "inc80_90",
  "low_80120_18",
  "mod_80120_18",
  "high_80120_18",
  "low_pdmt_medhhinc_18",
  "high_pdmt_medhhinc_18",
  "mod_pdmt_medhhinc_18",
  "mix_low_medhhinc_18",
  "mix_mod_medhhinc_18",
  "mix_high_medhhinc_18",
  "inc_cat_medhhinc_18",
  "inc_cat_medhhinc_encoded18",
  "low_80120_00",
  "mod_80120_00",
  "high_80120_00",
  "low_pdmt_medhhinc_00",
  "high_pdmt_medhhinc_00",
  "mod_pdmt_medhhinc_00",
  "mix_low_medhhinc_00",
  "mix_mod_medhhinc_00",
  "mix_high_medhhinc_00",
  "inc_cat_medhhinc_00",
  "inc_cat_medhhinc_encoded00",
  "per_all_li_90",
  "per_all_li_00",
  "per_all_li_18",
  "all_li_count_90",
  "all_li_count_00",
  "all_li_count_18",
  "real_mhval_90",
  "real_mrent_90",
  "real_hinc_90",
  "real_mhval_00",
  "real_mrent_00",
  "real_hinc_00",
  "real_mhval_12",
  "real_mrent_12",
  "real_mhval_18",
  "real_mrent_18",
  "real_hinc_18",
  "per_nonwhite_90",
  "per_nonwhite_00",
  "per_nonwhite_18",
  "hu_90",
  "per_rent_90",
  "per_rent_00",
  "hu_18",
  "per_rent_18",
  "total_25_90",
  "per_col_90",
  "male_25_col_00",
  "female_25_col_00",
  "total_25_col_00",
  "per_col_00",
  "per_col_18",
  "per_units_pre50_18",
  "per_limove_18",
  "mov_tot_w_income_18",
  "per_limove_12",
  "mov_tot_w_income_12",
  "lmh_flag_encoded",
  "lmh_flag_category",
  "pctch_real_mhval_00_18",
  "pctch_real_mrent_12_18",
  "rent_decrease",
  "rent_marginal",
  "rent_increase",
  "rent_rapid_increase",
  "house_decrease",
  "house_marginal",
  "house_increase",
  "house_rapid_increase",
  "tot_decrease",
  "tot_marginal",
  "tot_increase",
  "tot_rapid_increase",
  "change_flag_encoded",
  "change_flag_category",
  "per_ch_zillow_12_18",
  "ab_50pct_ch",
  "ab_90percentile_ch",
  "rent_50pct_ch",
  "rent_90percentile_ch",
  "hv_abrm_ch",
  "rent_abrm_ch",
  "pctch_real_mhval_90_00",
  "pctch_real_mrent_90_00",
  "pctch_real_hinc_90_00",
  "pctch_real_mrent_00_18",
  "pctch_real_hinc_00_18",
  "ch_all_li_count_90_00",
  "ch_all_li_count_00_18",
  "ch_per_col_90_00",
  "ch_per_col_00_18",
  "ch_per_limove_12_18",
  "pop00flag",
  "aboverm_per_all_li_90",
  "aboverm_per_all_li_00",
  "aboverm_per_all_li_18",
  "aboverm_per_nonwhite_18",
  "aboverm_per_nonwhite_90",
  "aboverm_per_nonwhite_00",
  "aboverm_per_rent_90",
  "aboverm_per_rent_00",
  "aboverm_per_rent_18",
  "aboverm_per_col_90",
  "aboverm_per_col_00",
  "aboverm_per_col_18",
  "aboverm_real_mrent_90",
  "aboverm_real_mrent_00",
  "aboverm_real_mrent_12",
  "aboverm_real_mrent_18",
  "aboverm_real_mhval_90",
  "aboverm_real_mhval_00",
  "aboverm_real_mhval_18",
  "aboverm_pctch_real_mhval_00_18",
  "aboverm_pctch_real_mrent_00_18",
  "aboverm_pctch_real_mrent_12_18",
  "aboverm_pctch_real_mhval_90_00",
  "aboverm_pctch_real_mrent_90_00",
  "lostli_00",
  "lostli_18",
  "aboverm_pctch_real_hinc_90_00",
  "aboverm_pctch_real_hinc_00_18",
  "aboverm_ch_per_col_90_00",
  "aboverm_ch_per_col_00_18",
  "aboverm_per_units_pre50_18",
  "presence_ph_LIHTC",
  "vul_gent_90",
  "vul_gent_00",
  "vul_gent_18",
  "hotmarket_00",
  "hotmarket_18",
  "gent_90_00",
  "gent_90_00_urban",
  "gent_00_18",
  "gent_00_18_urban",
  "dp_PChRent",
  "dp_RentGap",
  "tr_rent_gap",
  "rm_rent_gap",
  "dense",
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
  "double_counted",
  "typology",
  'UDP Typology'
)


### Join ------------------------------------------------------

all_tracts_2019 <-
  left_join(x = all_tracts_2019,
            y = udp_typologies[keep_udp_cols],
            by = join_by(GEOID))


## Add LUOF counts to all_tracts_2019 -------------------------

### Creating LUOF by GEOID frequency table --------------------

GEOID_count <- table(fatal_enc_2019$GEOID) |> as.data.frame() |>
  rename(GEOID = Var1, luof_count = Freq)

### Join frequency table with all_tracts_2019 -----------------

all_tracts_2019 <-
  left_join(all_tracts_2019,
            GEOID_count) |> 
  mutate(luof_count =
           case_when(is.na(luof_count) ~ 0,
                     TRUE ~ luof_count))

## Bring UDP typologies into fatal encounters df --------------

fatal_enc_2019_udp <-
  left_join(x = fatal_enc_2019,
            y = all_tracts_2019)


# Summarizing -------------------------------------------------

## Tables: by UDP typology only -------------------------------

### Population by UDP typology --------------------------------

pop_udp_only <-
  aggregate(pop_18 ~ `UDP Typology`, data = all_tracts_2019, FUN = sum)


### LUOF by UDP typology --------------------------------------

luof_by_udp <-
  table(fatal_enc_2019_udp$`UDP Typology`) |> as.data.frame() |>
  rename('UDP Typology' = Var1, luof_count = Freq)


### Combine 'pop_udp_only' and 'LUOF_by_UDP' ------------------

summary_udp_luof <-
  full_join(luof_by_udp,
            pop_udp_only,
            by = join_by('UDP Typology')) |>
  mutate('Annual Rate Per 10 Million Population' = luof_count / pop_18 / 6 * 10000000)

summary_udp_luof$`UDP Typology` <-
  factor(summary_udp_luof$`UDP Typology`, levels = typology_order)

#### Plot -----------------------------------------------------

# ggplot(summary_udp_luof,
#        aes(x = `UDP Typology`, y = `Annual Rate Per 10 Million Population`)) +
#   geom_col(color = 'black', fill = '#a6c4f1') +
#   geom_text(
#     aes(label = round(`Annual Rate Per 10 Million Population`, 1)),
#     position = position_dodge(width = 0.85),
#     vjust = -0.4,
#     color = "black",
#     size = 3
#   ) +
#   labs(title = "Rate of Police Lethal Uses of Force",
#        # subtitle = "Years: [2015-2020]",
#        y = "Annual Rate Per 10 Million",
#        x = "Urban Displacement Project Typology",
#        fill = "Majority") +
#   ylim(0, 65) +
#   theme_classic() +
#   theme(
#     axis.text.x = element_text(color = "black"),
#     axis.text.y = element_text(color = "black"),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank()
#   )

descriptives(
  data = summary_udp_luof,
  vars = "Annual Rate Per 10 Million Population",
  splitBy = 'UDP Typology',
  mean = F,
  median = F,
  min = F,
  max = F,
  n = F,
  missing = F,
  se = F,
  sd = F,
  bar = T,
  barCounts = T
)

ggsave(
  'plots/udp_only.png',
  dpi = 'retina',
  width = 10.4,
  height = 4.81
)

wb <- createWorkbook()

addWorksheet(wb, "UDP Only")
writeData(wb, "UDP Only", summary_udp_luof, startCol = 1, startRow = 1)
# write_csv(x = summary_udp_luof, file = "summary_udp_luof.csv", )

## Rate by Majority & UDP typology ----------------------------

### Population by UDP and Majority race -----------------------

pop_udp_majority <- all_tracts_2019 |>
  aggregate(NH_WhiteE ~ `UDP Typology`, FUN = sum, data = _) |>
  mutate(all_tracts_2019 |>
           aggregate(Hisp_LatinoE ~ `UDP Typology`, FUN = sum, data = _)) |>
  mutate(all_tracts_2019 |>
           aggregate(NH_BlackE ~ `UDP Typology`, FUN = sum, data = _)) |>
  rename('White' = NH_WhiteE,
         'Hispanic/Latino' = Hisp_LatinoE,
         'Black' = NH_BlackE) |>
  pivot_longer(
    names_to = "Majority",
    cols = c("White", "Hispanic/Latino", "Black"),
    values_to = "population"
  )


### LUOF count by typology & Majority -------------------------


luof_by_udp_majority <-
  table(fatal_enc_2019_udp$`UDP Typology`,
        fatal_enc_2019_udp$Majority) |>
  as.data.frame() |> rename(`UDP Typology` = Var1,
                            Majority = Var2,
                            luof_count = Freq)



### Join population and LUOF count ----------------------------

summary_udp_majority_luof <-
  left_join(
    as.data.frame(luof_by_udp_majority),
    pop_udp_majority,
    by = join_by(`UDP Typology`, Majority)
  ) |>
  mutate('Annual Rate Per 10 Million Population' = luof_count / population / 6 * 10000000)


summary_udp_majority_luof$`UDP Typology` <-
  factor(summary_udp_majority_luof$`UDP Typology`,
         levels = typology_order,
         ordered = TRUE)
summary_udp_majority_luof$Majority <-
  factor(summary_udp_majority_luof$Majority, ordered = TRUE)


##### Plot ----------------------------------------------------

# ggplot(summary_udp_majority_luof,
#        aes(x = `UDP Typology`, y = `Annual Rate Per 10 Million Population`, fill = Majority)) +
#   geom_col(position = 'dodge', color = 'black') +
#   geom_text(
#     aes(label = round(`Annual Rate Per 10 Million Population`, 1)),
#     position = position_dodge(width = 0.85),
#     vjust = -0.4,
#     color = "black",
#     size = 3
#   ) +
#   labs(title = "Rate of Police Lethal Uses of Force",
#        # subtitle = "Years: [2015-2020]",
#        y = "Annual Rate Per 10 Million",
#        x = "Urban Displacement Project Typology",
#        fill = "Majority") +
#   ylim(0, 65) +
#   theme_classic() +
#   theme(
#     axis.text.x = element_text(color = "black"),
#     axis.text.y = element_text(color = "black"),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank()
#   )


descriptives(
  data = summary_udp_majority_luof,
  vars = "Annual Rate Per 10 Million Population",
  splitBy = vars("UDP Typology", "Majority"),
  mean = F,
  median = F,
  min = F,
  max = F,
  n = F,
  missing = F,
  se = F,
  sd = F,
  bar = T,
  barCounts = TRUE
)

ggsave(
  'plots/udp_majority.png',
  dpi = 'retina',
  width = 10.4,
  height = 4.81)

addWorksheet(wb, "UDP & Majority")
writeData(wb, "UDP & Majority", summary_udp_majority_luof, startCol = 1, startRow = 1)

# write_csv(x = summary_udp_majority_luof,
          # file = 'summary_udp_majority_luof.csv')

######################################################################## #
## Tables: Rate by victim race and UDP typology ------------------------------------

# Using pop_udp_majority for population counts

### LUOF count ------------------------------------------------

luof_udp_victim_race <- fatal_enc_2019_udp |>
  count(`UDP Typology`, race_imputed) |>
  rename(Victim = race_imputed, luof_count = n) |>
  na.omit()

### Join population & LUOF count tables -----------------------

summary_udp_victim_race <-
  left_join(
    luof_udp_victim_race |> filter(Victim != 'Other/Unknown'),
    pop_udp_majority |> rename(Victim = Majority),
    by = join_by(`UDP Typology`, Victim)
  ) |>  mutate('Annual Rate Per 10 Million Population' = luof_count / population / 6 * 10000000)

summary_udp_victim_race$`UDP Typology` <-
  factor(summary_udp_victim_race$`UDP Typology`,
         levels = typology_order,
         ordered = T)
summary_udp_victim_race$Victim <-
  factor(summary_udp_victim_race$Victim, ordered = T)
#### Plot -----------------------------------------------------

# ggplot(summary_udp_victim_race,
#        aes(x = `UDP Typology`, y = `Annual Rate Per 10 Million Population`, fill = Victim)) +
#   geom_col(position = 'dodge', color = 'black') +
#   geom_text(
#     aes(label = round(`Annual Rate Per 10 Million Population`, 1)),
#     position = position_dodge(width = 0.85),
#     vjust = -0.4,
#     color = "black",
#     size = 3
#   ) +
#   labs(title = "Rate of Police Lethal Uses of Force",
#        # subtitle = "Years: [2015-2020]",
#        y = "Annual Rate Per 10 Million",
#        x = "Urban Displacement Project Typology",
#        fill = "Victim's Race") +
#   ylim(0, 100) +
#   theme_classic() +
#   theme(
#     axis.text.x = element_text(color = "black"),
#     axis.text.y = element_text(color = "black"),
#     panel.grid.major.x = element_blank(),
#     panel.grid.minor.x = element_blank()
#   )

descriptives(
  data = summary_udp_victim_race,
  vars = "Annual Rate Per 10 Million Population",
  splitBy = vars("UDP Typology", "Victim"),
  mean = F,
  median = F,
  min = F,
  max = F,
  n = F,
  missing = F,
  se = F,
  sd = F,
  bar = T,
  
)

ggsave(
  'plots/udp_victim_race.png',
  dpi = 'retina',
  width = 10.4,
  height = 4.81)

addWorksheet(wb, "UDP & Victim Race")
writeData(wb, "UDP & Victim Race", summary_udp_victim_race, startCol = 1, startRow = 1)


# write_csv(x = summary_udp_victim_race,
          # file = 'summary_udp_victim_race.csv')

############################################################# #
## Tables: UDP & Majority race & victim race ------------------

### Population by racial group in UDP & Majority tract ------
# For example, the number of blacks living in Majority-white, gentrifying tracts


pop_udp_majority_victim <- all_tracts_2019 |>
  aggregate(NH_WhiteE ~ `UDP Typology` + Majority,
            FUN = sum,
            data = _) |>
  mutate(all_tracts_2019 |>
           aggregate(
             Hisp_LatinoE ~ `UDP Typology` + Majority,
             FUN = sum,
             data = _
           )) |>
  mutate(all_tracts_2019 |>
           aggregate(
             NH_BlackE ~ `UDP Typology` + Majority,
             FUN = sum,
             data = _
           )) |>
  rename('White' = NH_WhiteE,
         'Hispanic/Latino' = Hisp_LatinoE,
         'Black' = NH_BlackE) |>
  pivot_longer(
    names_to = "Victim",
    cols = c("White", "Hispanic/Latino", "Black"),
    values_to = "population"
  )

### LUOF count by UDP & Majority race & victim race ---------


# table(
#   fatal_enc_2019_udp$`UDP Typology`,
#   fatal_enc_2019_udp$Majority,
#   fatal_enc_2019_udp$race_imputed
# )


luof_udp_majority_victim <- fatal_enc_2019_udp |>
  count(`UDP Typology`, Majority, race_imputed) |>
  rename(Victim = race_imputed, luof_count = n)


### Join population & LUOF count tables ---------------------

summary_udp_majority_victim_luof <-
  full_join(luof_udp_majority_victim, pop_udp_majority_victim) |>
  dplyr::filter(Victim != "Other/Unknown") |>
  mutate('Annual Rate Per 10 Million Population' = luof_count / population / 6 * 10000000) |>
  mutate(
    `UDP Typology` = factor(
      `UDP Typology`,
      labels = unique(`UDP Typology`)[c(2, 1, 3)],
      ordered = TRUE
    ),
    Victim = factor(Victim)
  ) |>
  rename('UDP Typology' = 'UDP Typology', Victim = Victim) |>
  na.omit()

descriptives(
  data = summary_udp_majority_victim_luof, # mutate(Majority = paste('Majority:', Majority))
  vars = "Annual Rate Per 10 Million Population",
  splitBy = vars("Majority", "Victim", "UDP Typology"),
  mean = F,
  median = F,
  min = F,
  max = F,
  n = F,
  missing = F,
  se = F,
  sd = F,
  bar = T,
  barCounts = TRUE
)

ggsave(
  'plots/udp_race_majority_victim_legend.png',
  dpi = 'retina',
  width = 10.4 * 1.5,
  height = 4.81 * 1.5
)

# print(summary_udp_majority_victim_luof |> arrange('Annual Rate Per 10 Million Population'), n = 50)

addWorksheet(wb, "UDP, Victim & Majority")
writeData(wb, "UDP, Victim & Majority", summary_udp_majority_victim_luof, startCol = 1, startRow = 1)

# write_csv(summary_udp_majority_victim_luof, file = 'summary_udp_majority_victim_luof_original.csv')

saveWorkbook(wb, "all_udp_summary_tables.xlsx", overwrite = TRUE)

#### Plot: made in Minitab ----------------------------------

descriptives(
  data = summary_udp_majority_victim_luof,
  vars = "Annual Rate Per 10 Million Population",
  splitBy = vars( "Victim", "Majority", "UDP Typology"),
  mean = F,
  median = F,
  min = F,
  max = F,
  n = F,
  missing = F,
  se = F,
  sd = F,
  bar = T,
  barCounts = TRUE
)

ggsave(
  'plots/udp_victim_race_majority_legend.png',
  dpi = 'retina',
  width = 10.4 * 1.5,
  height = 4.81 * 1.5
)
