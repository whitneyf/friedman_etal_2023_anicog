# read 'KPR all sighting' data
# score # of KPR males seen in first 5min
# determine 2' and 3' association surveys
# summarise activity context

library(tidyverse)
library(here)
library(janitor)
library(skim)
library(readxl)

fn_path <- "/Volumes/WorkDrive/DAP/GoogleDriveSync/DAP_sync/Aerial/DataAnalysis/"

# ids
kpr_ids <- read_csv(paste0(fn_path, "Network/KsPdRr09-14/KSPDRR_Supplemental.csv")) %>% 
  clean_names()

ks <- kpr_ids %>% filter(alliance == "KS") %>%  pull(id)
pd <- kpr_ids %>% filter(alliance == "PD") %>%  pull(id)
rr <- kpr_ids %>% filter(alliance == "RR") %>%  pull(id)

# surveys
db <- read_excel(paste0(fn_path, "Network/KsPdRr09-14/DAP2014 Surveys 150330 KSPDRR ONLY.xlsx")) %>% 
  clean_names() %>% 
  select(ref_num, date, survey_number, resight, reason_depart, 
         reason_depart_other, survey_duration_time, num_ds_1st_5min_code_knowns_only_2014_onward,
         activity_predom) %>% 
  rename(allid_first5 = num_ds_1st_5min_code_knowns_only_2014_onward) %>% 
  filter(reason_depart %in% c('5','F'), 
         resight == "no") %>% 
  mutate(activity_predom = recode(activity_predom, 
                                  "n/a" = "unknown", 
                                  "missed" = "unknown", 
                                  "Travel" = "travel", 
                                  "Forage" = "forage",
                                  "lateral line forage" = "forage",
                                  "respond" = "unknown",
                                  "rst" = "rest",
                                  "resting" = "rest",
                                  "social" = "socialize",
                                  "socialize, travel" = "socialize")) %>% 
  # restrict to aug-dec
  mutate(month = lubridate::month(date)) %>% 
  filter(month >= 8)


# only care about 3' so any two members of different alliances are good

id_list <- function(x){
  str_split(x, "[:blank:]")[[1]]
}

a3 <- db %>% 
  rowwise() %>% 
  mutate(pd_num = sum(pd %in% id_list(allid_first5)),
         ks_num = sum(ks %in% id_list(allid_first5)), 
         rr_num = sum(rr %in% id_list(allid_first5))) %>% 
  mutate(pd_ks = if_else((pd_num > 0 & ks_num > 0), 1, 0),
         pd_rr = if_else((pd_num > 0 & rr_num > 0), 1, 0),
         ks_rr = if_else((rr_num > 0 & ks_num > 0), 1, 0),
         pd_ks_rr = if_else((pd_num > 0 & ks_num > 0 & rr_num > 0), 1, 0)) %>% 
  mutate(order3 = if_else((pd_ks > 0 | pd_rr > 0 | ks_rr > 0), 1, 0), 
         total_kpr = sum(pd_num, ks_num, rr_num),
         total_kspd = sum(pd_num, ks_num)) %>% 
  filter(total_kpr > 0)

# summary stats: 
# all r/s/t contexts (>=1 kpr); n = 246
a3_rts <- a3 %>% 
  filter(activity_predom %in% c("rest","travel","socialize"))

# all r/s/t with 3' (n = 35)
a3_rts %>% 
  filter(order3  == 1) %>% 
  tabyl(activity_predom) %>% 
  adorn_totals()

# number of ks_pd, ks_rr, pd_rr S/R/T surveys: 
a3_rts$pd_ks %>% sum() # 23
a3_rts$pd_rr %>% sum() # 3
a3_rts$ks_rr %>% sum() # 9

# only ks-pd surveys (n = 173)
a3_rts_ks_or_pd <- 
  a3 %>% 
  filter(activity_predom %in% c("rest","travel","socialize")) %>% 
  mutate(pd_or_ks = if_else((ks_num >0 | pd_num >0), 1, 0)) %>% 
  filter(pd_or_ks == 1)

a3_rts_ks_or_pd$pd_ks %>% sum() # n = 23 (23/173)


# third order - all association contexts ----
a3 %>% 
  filter(order3 == 1) %>% 
  tabyl(activity_predom) %>% 
  adorn_totals()

# third order - r/s/t contexts
a3 %>%   
  filter(order3 == 1) %>% 
  filter(activity_predom %in% c("rest","travel","socialize")) %>% 
  tabyl(activity_predom) %>% 
  adorn_totals()

# rr surveys in T1, T2, T3
a3 %>% 
  mutate(year = lubridate::year(date)) %>% 
  filter(activity_predom %in% c("rest","travel","socialize")) %>% 
  filter(rr_num > 0, 
         year >= 2009,
         year < 2011) %>% nrow()

a3 %>% 
  mutate(year = lubridate::year(date)) %>%
  filter(activity_predom %in% c("rest","travel","socialize")) %>% 
  filter(rr_num > 0, 
         year >= 2011,
         year < 2013) %>% nrow()

# 2013 only (prior to inclusion in KPR)
a3 %>% 
  mutate(year = lubridate::year(date)) %>% 
  filter(rr_num > 0, 
         year == 2013) %>% view()

# T3
a3 %>% 
  mutate(year = lubridate::year(date)) %>% 
  #filter(activity_predom %in% c("rest","travel","socialize")) %>% 
  filter(rr_num > 0, 
         year >= 2013,
         year < 2015) %>% nrow()

