# Import b6c8ec's CSV in a manner that is compatible with the SoSciSurvey format
# Note that the datasets generated here are not completely cleaned yet -- some
# processing steps need to happen in data_preparation.R afterwards!

library(qualtRics)
library(tidyverse)

#### Import b6c8ec ####

d.turk.raw <- read_survey("data/b6c8ec_with_order.csv")

add_cond_order <- function(df) {
  vign_1 <- str_sub(df$vignette_order, 1, 1)
  vign_2 <- str_sub(df$vignette_order, 2, 2)
  vign_3 <- str_sub(df$vignette_order, 3, 3)
  cond_1 <- case_when(vign_1 == "D" ~ df$d_cond, vign_1 == "G" ~ df$g_cond, vign_1 == "E" ~ df$e_cond)
  cond_2 <- case_when(vign_2 == "D" ~ df$d_cond, vign_2 == "G" ~ df$g_cond, vign_2 == "E" ~ df$e_cond)
  cond_3 <- case_when(vign_3 == "D" ~ df$d_cond, vign_3 == "G" ~ df$g_cond, vign_3 == "E" ~ df$e_cond)
  mutate(df, cond_order = str_c(cond_1, cond_2, cond_3))
}

recode_ethnicity <- function(df) {
  df %>%
    mutate(ethn_wh = str_detect(ethnicity, "(^|,)White/European(,|$)"),
           ethn_bl = str_detect(ethnicity, "(^|,)Black/African American(,|$)"),
           ethn_lat = str_detect(ethnicity, "(^|,)Hispanic Latino(,|$)"),
           ethn_as_sea_pac = str_detect(ethnicity, "(^|,)East or Southeast Asian / Pacific Islander"),
           ethn_sa = str_detect(ethnicity, "(^|,)South Asian"),
           ethn_none = str_detect(ethnicity, "(^|,)I prefer not to answer this question(,|$)"),
           ethn_other = str_detect(ethnicity, "(^|,)Other(,|$)")) %>%
    mutate(ethn_selected = rowSums(across(starts_with("ethn_"))),
           ethn_other_text= ethnicity_7_TEXT)
}

d.turk <- d.turk.raw %>%
  # need the as.character here because country_now_2_TEXT is always empty and
  # NAs are thus interpreted as numeric, clashing with the "United States" string
  mutate(country = if_else(country_now == "United States",
                           "United States", as.character(country_now_2_TEXT)),
         birth_country = if_else(country_birth == "United States",
                                 "United States", as.character(country_birth_2_TEXT))) %>%
  # Replace -99 with NA in all columns
  mutate(across(everything(), na_if, -99)) %>%
  recode_ethnicity() %>%
  select(vignette_order=FL_85_DO,
         case=ResponseId,
         d_cond=FL_78_DO,
         d_know_vas=d_knows_1, d_compr=d_fake, d_reason_vas=d_reasonable_1,
         d_ri_wr=d_right, d_luck_vas=d_ability_1, d_know_alt=d_situation,
         g_cond=FL_41_DO,
         g_know_vas=g_knows_1, g_compr=g_fake, g_reason_vas=g_reasonable_1,
         g_ri_wr=g_right, g_luck_vas=g_ability_1, g_know_alt=g_situation,
         e_cond=FL_50_DO,
         e_know_vas=e_knows_1, e_compr=e_fake, e_reason_vas=e_reasonable_1,
         e_ri_wr=e_right, e_luck_vas=e_ability_1, e_know_alt=e_situation,

         purpose=purpose_study, materials=impression_study, previous=similar_study,
         sx_enjoy=enjoy_study, sx_nervous=nervous_study, sx_difficult=difficult_study,
         sx_boring=boring_study, sx_tiring=tiring_study, sx_speed=quickly_study,
         sx_experience=regularly_study, sx_selfconscious=selfcons_study,
         sx_motivation=motivated_study, sx_contribution=important_study,
         sx_goal=aim_study, sx_comments=comment_study,
         
         age, gender, gender_other=gender_3_TEXT,
         country, birth_country, education_level=lvl_edu, language=english,
         ethn_selected, ethn_wh, ethn_bl, ethn_lat, ethn_as_sea_pac,
         ethn_sa, ethn_none, ethn_other, ethn_other_text,
         
         start=StartDate, # TODO: timezones
         end=EndDate, duration=`Duration (in seconds)`,
         consent) %>%
  mutate(across(c(start, end),
                as.POSIXct, format="%Y-%m/%d %H:%M", tz="America/Denver")) %>%
  mutate(d_cond = fct_recode(d_cond, K="FL_90", G="FL_91", I="FL_92"),
         g_cond = fct_recode(g_cond, K="FL_62", G="FL_66", I="FL_64"),
         e_cond = fct_recode(e_cond, K="FL_68", G="FL_70", I="FL_69")) %>%
  mutate(vignette_order = vignette_order %>%
           str_replace("FL_89", "D") %>% str_replace("FL_93", "G") %>%
           str_replace("FL_94", "E") %>% str_replace_all(fixed("|"), "")) %>%
  add_cond_order() %>%
  mutate(lab_id = "b6c8ec", id = paste0("q_turk_", case),
         source = "Qualtrics",
         bin_order = "mixed", scale="vas") %>%
  mutate_at(vars(starts_with("sx_"), -"sx_comments"), ~as.integer(sub("\\..*", "", .))) %>%
  #mutate(gender_other = as.character(NA)) %>%
  mutate(consent = consent == "I agree to participate in this study.") %>%
  mutate(survey_lang = "eng") %>%
  mutate(d_compr = factor(d_compr, c("ground squirrel","prairie dog"), labels=c("real", "fake")),
         g_compr = factor(g_compr, c("real","fake"), labels=c("real", "fake")),
         e_compr = factor(e_compr, c("diamonds","cubic zirconium stones"), labels=c("real", "fake")),
         d_know_alt = factor(d_know_alt,
                             c("Darrel knows that the animal he saw is a red speckled ground squirrel.",
                               "Darrel feels like he knows that the animal he saw is a red speckled ground squirrel, but he doesn't actually know that it is."),
                             labels=c("knows", "believes")),
         g_know_alt = factor(g_know_alt,
                             c("Gerald knows that the house he is pointing at is a real house.",
                               "Gerald feels like he knows that the house he is pointing at is a real house, but he doesn't actually know that it is."),
                             labels=c("knows", "believes")),
         e_know_alt = factor(e_know_alt,
                             c("Emma knows that she chose a necklace made of diamonds.",
                               "Emma feels like she knows that she chose a necklace made of diamonds, but she doesn't actually know that it is."),
                             labels=c("knows", "believes")),
         across(ends_with("_ri_wr"), factor, c("right", "wrong"), labels=c("right", "wrong")),
         across(ends_with("_cond"), as_factor),
         gender = factor(gender, c("Female", "Male", "Other (please specify):"),
                         c("female", "male", "other")),
         gender_other = as.character(gender_other), # ensure correct type even if always NA
         education_level = ordered(education_level,
                                   c("Less than high school",
                                     "High school diploma (or GED)",
                                     "Some college or a 2-year college degree (A.A.)",
                                     "4-year college degree (B.A., B.S.)",
                                     "Master’s degree (M.A., M.S.)",
                                     "Graduate or professional degree (J.D., Ph.D., M.D.)")),
         language = ordered(tolower(language),
                            c("not well at all", "not very well", "well", "very well")))

#### Import 87d100 ####


d.2054.raw <- read_survey("data/87d100_with_order.csv")

d.2054 <- d.2054.raw %>%
  # This lab is Portugal-based, however, the country of Portugal is coded as
  # United States.
  mutate(country = if_else(country_now == "United States",
                           "Portugal", as.character(country_now_2_TEXT)),
         birth_country = if_else(country_birth == "Portugal",
                                 "United States", as.character(country_birth_2_TEXT))) %>%
  # Replace -99 with NA in all columns
  mutate(across(everything(), na_if, -99)) %>%
  recode_ethnicity() %>%
  select(vignette_order=FL_40_DO,
         case=ResponseId,
         d_cond=FL_56_DO,
         d_know_vas=d_knows_1, d_compr=d_fake, d_reason_vas=d_reasonable_1,
         d_ri_wr=d_right, d_luck_vas=d_ability_1, d_know_alt=d_situation,
         g_cond=FL_41_DO,
         g_know_vas=g_knows_1, g_compr=g_fake, g_reason_vas=g_reasonable_1,
         g_ri_wr=g_right, g_luck_vas=g_ability_1, g_know_alt=g_situation,
         e_cond=FL_50_DO,
         e_know_vas=e_knows_1, e_compr=e_fake, e_reason_vas=e_reasonable_1,
         e_ri_wr=e_right, e_luck_vas=e_ability_1, e_know_alt=e_situation,
         
         purpose=purpose_study, previous=similar_study,
         sx_enjoy=enjoy_study, sx_nervous=nervous_study, sx_difficult=difficult_study,
         sx_boring=boring_study, sx_tiring=tiring_study, sx_speed=quickly_study,
         sx_experience=regularly_study, sx_selfconscious=selfcons_study,
         sx_motivation=motivated_study, sx_contribution=important_study,
         sx_goal=aim_study, sx_comments=comments_study,
         
         age, gender, gender_other=gender_3_TEXT,
         country, birth_country, education_level=lvl_edu,
         language=english, # this is presumably actually portuguese-proficiency
         ethn_selected, ethn_wh, ethn_bl, ethn_lat, ethn_as_sea_pac,
         ethn_sa, ethn_none, ethn_other, ethn_other_text,
         
         start=StartDate, # TODO: timezones
         end=EndDate, duration=`Duration (in seconds)`,
         consent) %>%
  mutate(across(c(start, end),
                as.POSIXct, format="%m/%d/%Y %H:%M", tz="America/Denver")) %>%
  mutate(d_cond = fct_recode(d_cond, K="FL_71", G="FL_72", I="FL_73"),
         g_cond = fct_recode(g_cond, K="FL_62", G="FL_66", I="FL_64"),
         e_cond = fct_recode(e_cond, K="FL_68", G="FL_70", I="FL_69")) %>%
  mutate(vignette_order = vignette_order %>%
           str_replace("FL_55", "D") %>% str_replace("FL_43", "G") %>%
           str_replace("FL_49", "E") %>% str_replace_all(fixed("|"), "")) %>%
  add_cond_order() %>%
  mutate(lab_id = "87d100", id = paste0("q_87d100_", case),
         source = "Qualtrics",
         bin_order = "normal", scale="vas") %>%
  mutate_at(vars(starts_with("sx_"), -"sx_comments"), ~as.integer(sub("\\..*", "", .))) %>%
  mutate(consent = consent == "I agree to participate in this study") %>%
  mutate(survey_lang = "prt") %>%
  # knowledge/reasonable VAS scales were coded reversely in the 87d100 survey!
  mutate(across(ends_with(c("_know_vas", "_reason_vas")), ~(100-.))) %>%
  mutate(d_compr = factor(d_compr, c("ground squirrel","prairie dog"), labels=c("real", "fake")),
         g_compr = factor(g_compr, c("real","fake"), labels=c("real", "fake")),
         e_compr = factor(e_compr, c("diamonds","cubic zirconium stones"), labels=c("real", "fake")),
         d_know_alt = factor(d_know_alt,
                             c("Darrel knows that the animal he saw is a red speckled ground squirrel.",
                               "Darrel feels like he knows that the animal he saw is a red speckled ground squirrel, but he doesn't actually know that it is."),
                             labels=c("knows", "believes")),
         g_know_alt = factor(g_know_alt,
                             c("Gerald knows that the house he is pointing at is a real house.",
                               "Gerald feels like he knows that the house he is pointing at is a real house, but he doesn't actually know that it is."),
                             labels=c("knows", "believes")),
         e_know_alt = factor(e_know_alt,
                             c("Emma knows that she chose a necklace made of diamonds.",
                               "Emma feels like she knows that she chose a necklace made of diamonds, but she doesn't actually know that it is."),
                             labels=c("knows", "believes")),
         across(ends_with("_ri_wr"), factor, c("right", "wrong"), labels=c("right", "wrong")),
         across(ends_with("_cond"), as_factor),
         gender = factor(gender, c("Female", "Male", "Other (please specify):"),
                         c("female", "male", "other")),
         gender_other = as.character(gender_other), # ensure correct type even if always NA
         education_level = ordered(education_level,
                                   c("Less than high school",
                                     "High school diploma (or GED)",
                                     "Some college or a 2-year college degree (A.A.)",
                                     "4-year college degree (B.A., B.S.)",
                                     "Master’s degree (M.A., M.S.)",
                                     "Graduate or professional degree (J.D., Ph.D., M.D.)")),
         language = ordered(tolower(language),
                            c("not well at all", "not very well", "well", "very well")))
# 
# d2054.raw %>%
#   select(ends_with("_study")) %>% View()
#   pivot_longer(everything()) %>%
#   mutate(value=as.integer(sub("\\..*", "", na_if(value, -99)))) %>%
#   group_by(name) %>%
#   summarize(min=min(value), max=max(value))


# Cleanup
rm(d.turk.raw, d.2054.raw, add_cond_order, recode_ethnicity)
