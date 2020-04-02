library(tidyverse)
library(janitor)
library(here)


# Import ------------------------------------------------------------------
# Ontario Ministry Data
cases <- readr::read_csv("https://data.ontario.ca/dataset/f4112442-bdc8-45d2-be3c-12efae72fb27/resource/455fd63b-603d-4608-8216-7d8647f43350/download/conposcovidloc.csv")
cases <- janitor::clean_names(cases)
cases <- cases %>% 
  mutate(case_id_ON = row_number()) #add a pk
test <- readr::read_csv("https://data.ontario.ca/dataset/f4112442-bdc8-45d2-be3c-12efae72fb27/resource/455fd63b-603d-4608-8216-7d8647f43350/download/conposcovidloc.csv")

# COVID19Canada Data
CANcases <- readr::read_csv("https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/cases.csv")
CANcases <- CANcases %>%
  filter(province == "Ontario")

CANmort <- readr::read_csv("https://raw.githubusercontent.com/ishaberry/Covid19Canada/master/mortality.csv")
CANmort <- CANmort %>%
  filter(province == "Ontario")

# Standardize Names -------------------------------------------------------
# ID for each dataset (case_id_*)
# date (Date class)
# age group
# sex (M, F, Other)
# case_acq

ONcases <- cases %>%
  select(
    case_id_ON,
    date = accurate_episode_date,
    age_group,
    sex = client_gender,
    case_acq = case_acquisitioninfo,
    outcome1,
    phu = reporting_phu,
    lat = reporting_phu_latitude,
    long = reporting_phu_longitude
  )

CANdata <- CANcases %>%
  select(
    case_id_CAN = case_id, #remember this
    provincial_case_id,
    date_report,
    provincial_case_id,
    age_group = age,
    sex,
    phu = health_region,
    travel_yn,
    locally_acquired
  ) %>%
  bind_rows(
    CANmort %>%
      select(
        case_id_CAN = province_death_id, #remember this
        province_death_id,
        date_death_report,
        age_group = age,
        sex,
        phu = health_region
      )
  ) %>%
  mutate(CANdata_id = row_number())

# Standardize Variables ---------------------------------------------------
names(ONcases)
on1 <- ONcases %>%
  mutate(age_group = str_remove(age_group, "s$")) %>%
  mutate(sex = case_when(tolower(sex) == "male" ~ 1L,
                         tolower(sex) == "female" ~ 2L,
                         TRUE ~ 3L)) %>%
  mutate(
    case_acq = case_when(
      case_acq == "Travel-Related" ~ 1L,
      case_acq == "Contact of a confirmed case" ~ 2L,
      case_acq == "Neither" ~ 3L,
      case_acq == "Information pending" ~ 4L
    )
  ) %>%
  mutate(outcome1 = tolower(outcome1))

names(CANdata)
can1 <- CANdata %>%
  mutate(age_group = str_extract(age_group, "^[0-9]{2}")) %>%
  mutate(sex = case_when(tolower(sex) == "male" ~ 1L,
                         tolower(sex) == "female" ~ 2L,
                         TRUE ~ 3L)) %>%
  mutate(outcome1 = ifelse(!is.na(date_death_report), "fatal", NA_character_)) %>%
  mutate(case_acq = case_when(
      travel_yn == "1" ~ 1L, #matches "Travel-Related
      travel_yn == "0" &
        locally_acquired == "Close contact" ~ 2L, #matches Contact of a confirmed case
      travel_yn == "0" &
        locally_acquired == "Community" | travel_yn == "Not Reported" ~ 3L, #matches Neither
      is.na(travel_yn) ~ 4L #matches information pending (subject to change)
    )
  ) %>%
  mutate(
    date_report = lubridate::dmy(date_report),
    date_death_report = lubridate::dmy(date_death_report)
  ) %>%
  select(-travel_yn, -locally_acquired)

# standardize PHUs
join_phu <- read_csv(here("data", "join_phu.csv")) %>%
  rename(phu_on = phu.x, phu_can = phu.y)

can1 <- left_join(can1, join_phu, by = c("phu" = "phu_can")) %>%
  select(-phu) %>% 
  select(everything(), phu = phu_on)


# Match Cases -------------------------------------------------------------
# Block by PHU, remove NA in age group
# join on Age Group, Sex code, Case Acquisition code and PHU
matched_cases <- map2(
  on1 %>%
    split(.$phu) %>%
    map(~ filter(., !is.na(age_group))),
  can1 %>%
    split(.$phu) %>%
    map(~ filter(., !is.na(age_group))),
  function(x, y)
    full_join(x, y, by = c("age_group", "sex", "case_acq", "phu"))
  ) %>% 
  keep( ~ nrow(.) > 0)

filtered_matched_cases <- matched_cases %>%
  map( ~ filter(., !is.na(CANdata_id))) %>%
  map( ~ filter(., date < date_report)) %>%
  map( ~ filter(., as.numeric(date_report - date) < 14)) %>%
  map(
    ~ select(
      .,
      case_id_ON,
      case_id_CAN,
      date,
      date_report,
      age_group,
      sex,
      outcome1.x,
      outcome1.y,
      date_death_report,
      case_acq,
      phu
    )
  )

filter_matches_cases_ON <- filtered_matched_cases %>%
  map_df( ~ filter(., !duplicated(.$case_id_ON)))
message(nrow(filter_matches_ON), " unique `case_id` matches to Ontario data")

filter_dups_cases_ON <- filtered_matched_cases %>%
  map_df( ~ filter(.,
    duplicated(.$case_id_ON) |
      duplicated(.$case_id_ON, fromLast = TRUE)
  ))
# nrow(filter_dups_ON)
message(length(unique(filter_dups_cases_ON$case_id_ON)), " duplicated `case_id` matches in Ontario data")
message(" in total ", nrow(filter_dups_cases_ON), " duplicated `case_id` combinations")

# Join Originals based on Filtered IDs 
output_cases <- left_join(
  inner_join(cases,
          filter_matches_cases_ON %>% select(case_id_ON, case_id_CAN),
          by = c("case_id_ON")),
  inner_join(CANcases,
          filter_matches_cases_ON %>% select(case_id_ON, case_id_CAN),
          by = c("case_id" = "case_id_CAN")),
  by = c("case_id_ON")
)

# EXPORT
# matches by Age Group, Sex code, Case Acquisition code and PHU
output_cases
write_csv(output_cases, here::here("matches_cases_agegrp_sex_case_acq.csv"))



# Deaths ------------------------------------------------------------------
matched_deaths <- map2(
  on1 %>%
    split(.$phu) %>%
    map(~ filter(., !is.na(age_group))) %>%
    map(~ filter(., outcome1 == "fatal")),
  can1 %>%
    split(.$phu) %>%
    map(~ filter(., !is.na(age_group))) %>%
    map(~ filter(., !is.na(date_death_report))),
  function(x, y)
    # case_acq removed since reporting delays and manual binning
    full_join(x, y, by = c("age_group", "sex", "phu"))) %>%
  keep( ~ nrow(.) > 0)
# length(matched_deaths)
# map_dbl(matched_deaths, nrow)

filtered_matched_deaths <- matched_deaths %>%
  map( ~ filter(., outcome1.x == outcome1.y)) %>% # lgl to match "fatal", could have done this in full_join()
  map( ~ filter(., !is.na(CANdata_id))) %>%
  map( ~ filter(., !is.na(case_id_ON))) %>% 
  map( ~ filter(., date_death_report >= date)) %>% #death date should be after or on reported date
  map(
    ~ select(
      .,
      case_id_ON,
      case_id_CAN,
      date,
      date_report,
      age_group,
      sex,
      outcome1.x,
      outcome1.y,
      date_death_report,
      case_acq.x,
      case_acq.y,
      phu
    )
  )
# length(filtered_matched_deaths)
# map_dbl(filtered_matched_deaths, nrow)


filter_matches_deaths_ON <- filtered_matched_deaths %>%
  map_df( ~ filter(., !duplicated(.$case_id_ON)))
message(nrow(filter_matches_deaths_ON), " unique `case_id` matches to Ontario data for deaths")

filter_dups_deaths_ON <- filtered_matched_deaths %>%
  map_df( ~ filter(.,
                   duplicated(.$case_id_ON) |
                     duplicated(.$case_id_ON, fromLast = TRUE)
  ))
# nrow(filter_dups_deaths_ON)
message(length(unique(filter_dups_deaths_ON$case_id_ON)), " duplicated `case_id` matches in Ontario data")
message(" in total ", nrow(filter_dups_deaths_ON), " duplicated `case_id` combinations")

output_deaths <- left_join(
  inner_join(cases,
             filter_matches_deaths_ON %>% select(case_id_ON, case_id_CAN),
             by = c("case_id_ON")),
  inner_join(CANmort,
             filter_matches_deaths_ON %>% select(case_id_ON, case_id_CAN),
             by = c("province_death_id"="case_id_CAN")),
  by = c("case_id_ON")
)

# EXPORT
# matches deaths by Age Group, Sex code, and PHU
output_deaths
write_csv(output_deaths, here::here("matches_deaths_agegrp_sex.csv"))


