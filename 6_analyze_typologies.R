rm(list = ls())
# Load libraries ----------------------------------------------------------
library(sf)
library(tidyverse)
library(reticulate)

# Load data ---------------------------------------------------------------

## UDP typologies from python pickle ----------------------------------

udp_typologies <-
  py_load_object(paste0(getwd(), '/data/pickle_files/final_typology_output.pkl'))


### Save udp typologies to RData ---------------------------------------

# save(
#   udp_typologies,
  # file = paste0(
  #   getwd(),
  #   '/data/outputs/typologies/final_typology_output.RData'
  # )
# )

## Load udp typologies from RData ---------------------------------------

# load(paste0(getwd(), '/data/outputs/typologies/final_typology_output.RData'))

### Fatal encounters with 2019 GEOID (joined) -------------------------------

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
)

rm(joined_2019) # remove -- not needed

### All census tracts -- 2019 data -------------------------------------

load(file = paste0(getwd(), '/data/R_data/all_tracts_2019.RData'))

#### Rename all_tracts_2019 ------------------------------------------------

all_tracts_2019 <- income_population_quintiles_2019
rm(income_population_quintiles_2019)

# Condense UDP categories -------------------------------------------------
# There are currently too many categories -- combine

udp_typologies <- udp_typologies |> 
  mutate(
    typology_text = case_when(
      # "LIR", # Low-income or at-risk
      typology %in% c('LISD', 'OD', 'ARG') ~ 'Low-income or at-risk', 
      # 'GIP', # Gentrification in progress
      typology %in% c('EOG', 'AdvG', 'SMMI') ~ 'Gentrification in progress', 
      # "MHIS", # Stable: mixed or high-income
      typology %in% c('ARE', 'BE', 'SAE') ~ 'Stable: mixed or high-income', 
      
      # typology %in% c('LISD', 'OD', 'ARG') ~ ' Low-income or at-risk', # "LIR", 
      # typology %in% c('EOG', 'AdvG', 'BE') ~ ' Gentrification in progress', # 'GIP
      # typology %in% c('SMMI', 'ARE', 'SAE') ~ 'Stable: mixed or high-income', # "MHIS",
      TRUE ~ NA),
      
      typology = case_when(
        # "LIR", # Low-income or at-risk
        typology %in% c('LISD', 'OD', 'ARG') ~ 'LIR', # ' Low-income or at-risk', 
        # 'GIP', # Gentrification in progress
        typology %in% c('EOG', 'AdvG', 'SMMI') ~ ' GIP', # 'Gentrification in progress', 
        # "MHIS", # Stable: mixed or high-income
        typology %in% c('ARE', 'BE', 'SAE') ~ 'MHIS', # 'Stable: mixed or high-income', 
        TRUE ~ NA
    )
  )


### Make typologies ordered -------------------------------------------------

typology_order <- c("Low-income or at-risk",
                    "Gentrification in progress",
                    "Stable: mixed or high-income")

## Remove geometries since I can join with GEOIDs/FIPS ---------------------

fatal_enc_2019 <- st_drop_geometry(fatal_enc_2019)
udp_typologies <- st_drop_geometry(udp_typologies)

# Combining -----------------------------------------------

## Add UDP to 2019 census data ---------------------------------------------

### Columns to keep ---------------------------------------------------------

keep_udp_cols <- c(
  'GEOID', 'pop_18', 'white_18', "pop_00", 
  "white_00", "hu_00", "ohu_00", "rhu_00", "total_25_00", 
  "male_25_col_bd_00", "male_25_col_md_00", "male_25_col_psd_00", 
  "male_25_col_phd_00", "female_25_col_bd_00", "female_25_col_md_00", 
  "female_25_col_psd_00", "female_25_col_phd_00", "mhval_00", "mrent_00", 
  "hh_00", "hinc_00", "trtid00", "pop_90", "hh_90", "white_90", 
  "total_25_col_9th_90", "total_25_col_12th_90", 
  "total_25_col_hs_90", "total_25_col_sc_90", "total_25_col_ad_90", 
  "total_25_col_bd_90", "total_25_col_gd_90", "hinc_90", "ohu_90", 
  "rhu_90", "mrent_90", "mhval_90", "trtid90", "inc80_18", "inc120_18", 
  "inc80_00", "inc120_00", "inc80_90", "low_80120_18", "mod_80120_18", 
  "high_80120_18", "low_pdmt_medhhinc_18", "high_pdmt_medhhinc_18", 
  "mod_pdmt_medhhinc_18", "mix_low_medhhinc_18", "mix_mod_medhhinc_18", 
  "mix_high_medhhinc_18", "inc_cat_medhhinc_18", "inc_cat_medhhinc_encoded18", 
  "low_80120_00", "mod_80120_00", "high_80120_00", "low_pdmt_medhhinc_00", 
  "high_pdmt_medhhinc_00", "mod_pdmt_medhhinc_00", "mix_low_medhhinc_00", 
  "mix_mod_medhhinc_00", "mix_high_medhhinc_00", "inc_cat_medhhinc_00", 
  "inc_cat_medhhinc_encoded00", "per_all_li_90", "per_all_li_00", 
  "per_all_li_18", "all_li_count_90", "all_li_count_00", "all_li_count_18", 
  "real_mhval_90", "real_mrent_90", "real_hinc_90", "real_mhval_00", 
  "real_mrent_00", "real_hinc_00", "real_mhval_12", "real_mrent_12", 
  "real_mhval_18", "real_mrent_18", "real_hinc_18", "per_nonwhite_90", 
  "per_nonwhite_00", "per_nonwhite_18", "hu_90", "per_rent_90", 
  "per_rent_00", "hu_18", "per_rent_18", "total_25_90", "per_col_90", 
  "male_25_col_00", "female_25_col_00", "total_25_col_00", "per_col_00", 
  "per_col_18", "per_units_pre50_18", "per_limove_18", "mov_tot_w_income_18", 
  "per_limove_12", "mov_tot_w_income_12", "lmh_flag_encoded", "lmh_flag_category", 
  "pctch_real_mhval_00_18", "pctch_real_mrent_12_18", "rent_decrease", 
  "rent_marginal", "rent_increase", "rent_rapid_increase", "house_decrease", 
  "house_marginal", "house_increase", "house_rapid_increase", "tot_decrease", 
  "tot_marginal", "tot_increase", "tot_rapid_increase", "change_flag_encoded", 
  "change_flag_category", "per_ch_zillow_12_18", "ab_50pct_ch", 
  "ab_90percentile_ch", "rent_50pct_ch", "rent_90percentile_ch", 
  "hv_abrm_ch", "rent_abrm_ch", "pctch_real_mhval_90_00", "pctch_real_mrent_90_00", 
  "pctch_real_hinc_90_00", "pctch_real_mrent_00_18", "pctch_real_hinc_00_18", 
  "ch_all_li_count_90_00", "ch_all_li_count_00_18", "ch_per_col_90_00", 
  "ch_per_col_00_18", "ch_per_limove_12_18", "pop00flag", "aboverm_per_all_li_90", 
  "aboverm_per_all_li_00", "aboverm_per_all_li_18", "aboverm_per_nonwhite_18", 
  "aboverm_per_nonwhite_90", "aboverm_per_nonwhite_00", "aboverm_per_rent_90", 
  "aboverm_per_rent_00", "aboverm_per_rent_18", "aboverm_per_col_90", 
  "aboverm_per_col_00", "aboverm_per_col_18", "aboverm_real_mrent_90", 
  "aboverm_real_mrent_00", "aboverm_real_mrent_12", "aboverm_real_mrent_18", 
  "aboverm_real_mhval_90", "aboverm_real_mhval_00", "aboverm_real_mhval_18", 
  "aboverm_pctch_real_mhval_00_18", "aboverm_pctch_real_mrent_00_18", 
  "aboverm_pctch_real_mrent_12_18", "aboverm_pctch_real_mhval_90_00", 
  "aboverm_pctch_real_mrent_90_00", "lostli_00", "lostli_18", 
  "aboverm_pctch_real_hinc_90_00", "aboverm_pctch_real_hinc_00_18", 
  "aboverm_ch_per_col_90_00", "aboverm_ch_per_col_00_18", 
  "aboverm_per_units_pre50_18", "presence_ph_LIHTC", 
  "vul_gent_90", "vul_gent_00", "vul_gent_18", "hotmarket_00", 
  "hotmarket_18", "gent_90_00", "gent_90_00_urban", "gent_00_18", 
  "gent_00_18_urban", "dp_PChRent", "dp_RentGap", "tr_rent_gap", 
  "rm_rent_gap", "dense", "SAE", "AdvG", "ARE", "BE", "SMMI", "ARG", 
  "EOG", "OD", "OD_loss", "LISD", "double_counted", 
  "typology", "typology_text")


### Join --------------------------------------------------------------------

all_tracts_2019 <- 
  left_join(
    x = all_tracts_2019,
    y = udp_typologies[keep_udp_cols])
rm(udp_typologies)


## Add LUOF counts to all_tracts_2019 --------------------------------------

### Creating LUOF by GEOID frequency table -------------------------------

GEOID_count <- table(fatal_enc_2019$GEOID) |> as.data.frame() |> 
  rename(GEOID = Var1, luof_count = Freq)

### Join frequency table with all_tracts_2019 -------------------------------

all_tracts_2019 <- 
  full_join(
    all_tracts_2019,
    GEOID_count
  )

## Bring UDP typologies into fatal encounters df ---------------------------

fatal_enc_2019_udp <-
  left_join(x = fatal_enc_2019,
            y = all_tracts_2019)

rm(fatal_enc_2019)

# Summarizing -------------------------------------------------------------

## Rate by UDP typology only -----------------------------------------------

### Population by UDP typology ----------------------------------------------

udp_population <- aggregate(pop_18 ~ typology_text, data = all_tracts_2019, FUN = sum)


### LUOF by UDP typology ----------------------------------------------------

luof_by_udp <-  table(fatal_enc_2019_udp$typology_text) |> as.data.frame() |> 
  rename(typology_text = Var1, luof_count = Freq)


### Combine 'udp_population' and 'LUOF_by_UDP' ------------------------------

summary_udp_luof <- 
  full_join(
    luof_by_udp,
    udp_population,
    by = join_by(typology_text)
  ) |> 
  mutate(
    rt_annual_10m = luof_count / pop_18 / 6 * 10000000
  )

rm(udp_population, luof_by_udp)

summary_udp_luof$typology_text <- factor(summary_udp_luof$typology_text, levels = typology_order)

#### Plot --------------------------------------------------------------------

ggplot(summary_udp_luof,
       aes(x = typology_text, y = rt_annual_10m)) +
  geom_col(color = 'black', fill = 'skyblue') +
  geom_text(
    aes(label = round(rt_annual_10m, 1)),
    position = position_dodge(width = 0.85),
    vjust = -0.4,
    color = "black",
    size = 3
  ) +
  labs(title = "Rate of Police Lethal Uses of Force",
       # subtitle = "Years: [2015-2020]",
       y = "Annual Rate Per 10 Million",
       x = "Urban Displacement Project Typology",
       # "Based on Median Household Income in Census Tracts Where a Lethal Use of Force Occurred",
       fill = "Majority") +
  ylim(0, 65) +
  theme_classic() +
  theme(
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
ggsave(
  'plots/udp_only.png',
  dpi = 'retina',
  width = 10.4,
  height = 4.81
)

## Rate by majority & UDP typology -----------------------------------------

### Population by UDP and majority race ------------------------------------

pop_udp_majority <- all_tracts_2019 |>  
  aggregate(NH_WhiteE ~ typology_text, FUN = sum, data = _) |> 
  mutate(all_tracts_2019 |> aggregate(Hisp_LatinoE ~ typology_text, FUN = sum, data = _)) |> 
  mutate(all_tracts_2019 |> aggregate(NH_BlackE ~ typology_text, FUN = sum, data = _)) |> 
  rename(White = NH_WhiteE, 'Hispanic/Latino' = Hisp_LatinoE, Black = NH_BlackE) |> 
  pivot_longer(names_to = "majority", cols = c("White", "Hispanic/Latino", "Black"), values_to = "population")
  
pop_udp_majority$typology_text <- factor(pop_udp_majority$typology_text, levels = typology_order, ordered = TRUE)
pop_udp_majority$majority <- factor(pop_udp_majority$majority, ordered = TRUE)

### LUOF count by typology & majority --------------------------------------


luof_by_udp_majority <- 
  table(fatal_enc_2019_udp$typology_text, fatal_enc_2019_udp$Majority) |> 
  as.data.frame() |> rename(typology_text = Var1, majority = Var2, luof_count = Freq)

luof_by_udp_majority$typology_text <- factor(luof_by_udp_majority$typology_text, levels = typology_order, ordered = TRUE)
luof_by_udp_majority$majority <- factor(luof_by_udp_majority$majority, ordered = TRUE)

### Join population and LUOF count -----------------------------------------

summary_udp_majority_luof <- 
  full_join(
    luof_by_udp_majority,
    pop_udp_majority,
    by = join_by(typology_text, majority)) |> 
  mutate(
    rt_annual_10m = luof_count / population / 6 * 10000000 
  )

rm(luof_by_udp_majority)


##### Plot -------------------------------------------------------------------

ggplot(summary_udp_majority_luof,
       aes(x = typology_text, y = rt_annual_10m, fill = majority)) +
  geom_col(position = 'dodge', color = 'black') +
  geom_text(
    aes(label = round(rt_annual_10m, 1)),
    position = position_dodge(width = 0.85),
    vjust = -0.4,
    color = "black",
    size = 3
  ) +
  labs(title = "Rate of Police Lethal Uses of Force",
       # subtitle = "Years: [2015-2020]",
       y = "Annual Rate Per 10 Million",
       x = "Urban Displacement Project Typology",
       # "Based on Median Household Income in Census Tracts Where a Lethal Use of Force Occurred",
       fill = "Majority") +
  ylim(0, 60) +
  theme_classic() +
  theme(
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
ggsave(
  'plots/udp_majority.png',
  dpi = 'retina',
  width = 10.4,
  height = 4.81)

## Tables: UDP & majority race & victim race -------------------------------



### Population by racial group in UDP & majority tract ----------------------
# For example, the number of blacks living in majority-white, gentrifying tracts


pop_udp_majority_victim <- all_tracts_2019 |>
  aggregate(NH_WhiteE ~ typology_text + Majority,
            FUN = sum, 
            data = _) |>
  mutate(all_tracts_2019 |>
           aggregate(
             Hisp_LatinoE ~ typology_text + Majority,
             FUN = sum,
             data = _
           )) |>
  mutate(all_tracts_2019 |>
           aggregate(NH_BlackE ~ typology_text + Majority, FUN = sum, data = _)) |>
  rename(White = NH_WhiteE,
         'Hispanic/Latino' = Hisp_LatinoE,
         Black = NH_BlackE) |>
  pivot_longer(
    names_to = "victim_race",
    cols = c("White", "Hispanic/Latino", "Black"),
    values_to = "population"
  )

### LUOF count by UDP & majority race & victim race -------------------------


# table(
#   fatal_enc_2019_udp$typology_text, 
#   fatal_enc_2019_udp$Majority, 
#   fatal_enc_2019_udp$race_imputed
# )


luof_udp_maj_victim <- fatal_enc_2019_udp |> 
  count(typology_text, Majority, race_imputed) |> 
  rename(victim_race = race_imputed, luof_count = n)


### Join population & LUOF count tables -------------------------------------

summary_udp_majority_victim_luof <- 
  full_join(
    luof_udp_maj_victim,
    pop_udp_majority_victim
  ) |>  mutate(
    rt_annual_10m = luof_count / population / 6 * 10000000 
  ) |> na.omit()

# print(summary_udp_majority_victim_luof |> arrange(rt_annual_10m), n = 50)
write_csv(summary_udp_majority_victim_luof, file = 'summary_udp_majority_victim_luof.csv')


