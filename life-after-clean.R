library(tidyverse)
library(readr)
library(skimr)
library(scales)
library(stringi)
library(janitor)
library(StandardizeText)
library(infer)

race <- c("Asian","Hispanic/Latino","Caucasian","Black/African","Mixed Race","Indigenous","Russian", "Iranian", "Did not specify")
us <- c("United States of America", "The United States", "United States", "USA", "usa", "United states", "US", "united states", "America", "Usa", "U.S.", "Florida")
germany <- c("Getmany", "Germany")
one_filter <- c('Disagree', 'Much Less', 'Much Worse')
two_filter <- c('Somewhat Disagree', 'Less', 'Worse')
three_filter <- c('As','Neutral','Same','The Same Amount', 'The Same Amount of')
four_filter <- c('Somewhat Agree', 'More', 'Better')
five_filter <- c('Agree', 'Much More', 'Much Better')


survey <- read_csv("v4.csv") %>% 
  select(-Timestamp) %>% 
  rowid_to_column("id") %>%
  mutate(`In what country do you currently live?` = case_when(`In what country do you currently live?` %in% us ~ "United States",
                                                              `In what country do you currently live?` %in% germany ~ "Germany",
                                                              TRUE ~ `In what country do you currently live?`),
         `What is your race or ethnicity? (Select all that apply.)` = case_when(str_detect(`What is your race or ethnicity? (Select all that apply.)`, "Caucasian,") ~ "Mixed Race",
                                                                                str_detect(`What is your race or ethnicity? (Select all that apply.)`, "Hispanic/Latino,") ~ "Mixed Race",
                                                                                str_detect(`What is your race or ethnicity? (Select all that apply.)`, "Asian, Hispanic/Latino") ~ "Mixed Race",
                                                                                str_detect(`What is your race or ethnicity? (Select all that apply.)`, "Indigenous, Mixed Race") ~ "Mixed Race",
                                                                                str_detect(`What is your race or ethnicity? (Select all that apply.)`, ";") ~ "Mixed Race",
                                                                                is.na(`What is your race or ethnicity? (Select all that apply.)`) ~ "Did not specify",
                                                                                `What is your race or ethnicity? (Select all that apply.)` == "I have no specific knowledge of my ethnicity" ~ "Other",
                                                                                `What is your race or ethnicity? (Select all that apply.)` == "Iranian" ~ "Other",
                                                                                `What is your race or ethnicity? (Select all that apply.)` == "White" | 
                                                                                  `What is your race or ethnicity? (Select all that apply.)` == "european" | 
                                                                                  `What is your race or ethnicity? (Select all that apply.)` == "European" | 
                                                                                  `What is your race or ethnicity? (Select all that apply.)` == "Jew" ~ "Caucasian",
                                                                                TRUE ~ `What is your race or ethnicity? (Select all that apply.)`))

coded <- read_csv("coded-life-after.csv") %>% clean_names() %>% 
  mutate(q_word = case_when(str_detect(q_condensed, '\\(O\\)')~str_replace(q_condensed, '\\(O\\)', ''),
                            str_detect(q_condensed, '\\(I\\)')~str_replace(q_condensed, '\\(I\\)', '')))
grouped_qs <- read_csv("grouped_qs.csv") %>% clean_names()

age_income_zip <- survey %>%
  select(`In what country do you currently live?`,
         `What is your age?`, 
         `What is your annual income level? (in the currency of your country).`,
         `If you live in the US, what is your 5-digit zipcode?`) %>%
  rename('income' = `What is your annual income level? (in the currency of your country).`) %>%
  mutate(`In what country do you currently live?` = case_when(`In what country do you currently live?` %in% us ~ "United States",
                                                              `In what country do you currently live?` %in% germany ~ "Germany",
                                                              TRUE ~ `In what country do you currently live?`),
         us_currency = case_when(`In what country do you currently live?` == 'Russia' ~ round(income * 0.013, 2),
                                 `In what country do you currently live?` == 'Canada' ~ round(income * 0.71, 2),
                                 `In what country do you currently live?` == 'Hungary' ~ round(income * 0.0031, 2),
                                 `In what country do you currently live?` == 'Mexico' ~ round(income * 0.042, 2),
                                 `In what country do you currently live?` == 'Germany' ~ round(income * 1.09, 2),
                                 `In what country do you currently live?` == 'Argentina' ~ round(income * 0.0151492, 2),
                                 `In what country do you currently live?` == 'Venezuela' ~ round(income * 0.100125, 2),
                                 `In what country do you currently live?` == 'Costa Rica' ~ round(income * 0.00175551, 2),
                                 `In what country do you currently live?` == 'Spain' ~ round(income * 1.09, 2),
                                 `In what country do you currently live?` == 'Colombia' ~ round(income * 0.000247048, 2),
                                 `In what country do you currently live?` == 'France' ~ round(income * 1.09, 2),
                                 `In what country do you currently live?` == 'Paraguay' ~ round(income * 0.000154462, 2),
                                 `In what country do you currently live?` == 'Brazil' ~ round(income * 0.188060, 2),
                                 `In what country do you currently live?` == 'Uruguay' ~ round(income * 0.0230315, 2),
                                 TRUE ~ income))

write_csv(age_income_zip, "age_income_zip.csv")

long_demo <- survey %>%
  select(c(`In what country do you currently live?`:`If you live in the US, what is your 5-digit zipcode?`), 
         -`What is your age?`,
         -`What is your annual income level? (in the currency of your country).`,
         -`If you live in the US, what is your 5-digit zipcode?`) %>%
  mutate_at(vars(names(.)), factor) %>%
  pivot_longer(cols = names(.),
               names_to = "question",
               values_to = "value") %>%
  arrange(question) %>%
  group_by(question, value) %>%
  count()
  
demo_id <- unique(long_demo$question) %>% 
  as_tibble() %>% 
  rename("question" = "value") %>% 
  rowid_to_column("id")

coded_demo <- long_demo %>%
  left_join(demo_id, by = "question") %>%
  group_by(question) %>%
  mutate(per=round(100*n/sum(n),2))

write_csv(coded_demo, "demographics.csv")

long_survey <- survey %>%
  mutate_at(vars(-c(`In what country do you currently live?`:`If you live in the US, what is your 5-digit zipcode?`)), factor) %>%
  select(-c(`In what country do you currently live?`:`If you live in the US, what is your 5-digit zipcode?`)) %>%
  filter(!is.na(`I believe people will return to using hand shakes and/or other physical greetings.`)) %>%
  pivot_longer(cols = -c("id"), 
               names_to = "question", 
               values_to = "value") %>%
  left_join(coded, by = "question") %>%
  left_join(grouped_qs, by = "question") %>%
  rename("id"="id.x") %>%
  select(-id.y) 

write_csv(long_survey, 'long_survey_hm.csv')

#OTHERS = 1, SELF = 2

others_count <- long_survey %>%
  filter(self_or_others == 1) %>% 
  group_by(question, value) %>% 
  count() %>% 
  filter(!grepl("(Optional)", question)) %>%
  left_join(grouped_qs, by = "question")

self_count <- long_survey %>% 
  filter(self_or_others == 2) %>% 
  group_by(question, value) %>% 
  count() %>% 
  filter(!grepl("(Optional)", question)) %>%
  left_join(grouped_qs, by = "question") 

other_id <- unique(others_count$question) %>% 
  as_tibble() %>% 
  rename("question" = "value") %>% 
  rowid_to_column("id")

self_id <- unique(self_count$question) %>% 
  as_tibble() %>% 
  rename("question" = "value") %>% 
  rowid_to_column("id")

OTHERS <- others_count %>% 
  left_join(other_id %>% select("question","id"), by = "question")

SELF <- self_count %>% 
  left_join(self_id %>% select("question","id"), by = "question") 

coded_OTHERS <- OTHERS %>% 
  left_join(coded, by = "question") %>% 
  select(-id.y) %>%
  rename("id"="id.x")

coded_SELF <- SELF %>% 
  left_join(coded, by = "question") %>%
  select(-id.y) %>%
  rename("id"="id.x")

write_csv(coded_OTHERS, "others.csv")
write_csv(coded_SELF, "self.csv")

self_others <- rbind(coded_OTHERS,coded_SELF)

fin_self_others <- self_others %>% 
  group_by(question) %>% 
  mutate(per=round(100*n/sum(n),2))

write_csv(fin_self_others, "self_others.csv")
