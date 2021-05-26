################
### Recoding ###
################

# Remarks on general coding scheme ####
## "keine Angabe" turned into NA

# Attitudes #### 

## Topic 1 (-5 is pro-distribution, 5 is anti-distribution)

data <- data %>% 
  rename(topic1_attitude1 = Q9_5,
         topic1_attitude2 = Q9_6,
         topic1_attitude3 = Q9_9,
         topic1_attitude4 = Q9_10) %>%
  mutate(topic1_attitude1 = (topic1_attitude1 * (-1)) + 5,
         topic1_attitude2 = topic1_attitude2 - 5,
         topic1_attitude3 = (topic1_attitude3 * (-1)) + 5,
         topic1_attitude4 = (topic1_attitude4 * (-1)) + 5)

### Scale reduction

data <- data %>% 
  mutate(topic1_attitudes_average = 
      select(., c("topic1_attitude1",
                   "topic1_attitude2",
                   "topic1_attitude3",
                   "topic1_attitude4")) %>%
      rowMeans(na.rm = TRUE)) 

topic1_attitudes_average_mean <- mean(data$topic1_attitudes_average, na.rm = T)

## Topic 2 (-5 is anti-law-and-order, 5 is pro-law-and-order)

data <- data %>% 
  rename(topic2_attitude1 = Q11_5,
         topic2_attitude2 = Q11_2,
         topic2_attitude3 = Q11_3,
         topic2_attitude4 = Q11_4) %>% 
  mutate(topic2_attitude1 = topic2_attitude1 - 5,
         topic2_attitude2 = (topic2_attitude2 * (-1)) + 5,
         topic2_attitude3 = topic2_attitude3 - 5,
         topic2_attitude4 = topic2_attitude4 - 5)

### Scale reduction

data <- data %>% 
  mutate(topic2_attitudes_average = 
           select(., c("topic2_attitude1",
                       "topic2_attitude2",
                       "topic2_attitude3",
                       "topic2_attitude4")) %>%
           rowMeans(na.rm = TRUE))

topic2_attitudes_average_mean <- mean(data$topic2_attitudes_average, na.rm = T)

## Topic 3 (-5 is pro-migration, 5 is anti-migration)

data <- data %>% 
  rename(topic3_attitude1 = Q13_1,
        topic3_attitude2 = Q13_2,
        topic3_attitude3 = Q13_3,
        topic3_attitude4 = Q13_4) %>% 
  mutate(topic3_attitude1 = (topic3_attitude1 * (-1)) + 5,
         topic3_attitude2 = topic3_attitude2 - 5,
         topic3_attitude3 = topic3_attitude3 - 5,
         topic3_attitude4 = topic3_attitude4 - 5)

### Scale reduction

data <- data %>% 
  ungroup() %>%
  mutate(topic3_attitudes_average = 
           select(., matches("topic3_attitude\\d$")) %>%
           rowMeans(na.rm = TRUE))

topic3_attitudes_average_mean <- mean(data$topic3_attitudes_average, na.rm = T)

### Topic 4
# -5 is pro-EU, 5 is anti-EU

data <- data %>% rename(topic4_attitude1 = Q15_1,
                        topic4_attitude2 = Q15_5,
                        topic4_attitude3 = Q15_3,
                        topic4_attitude4 = Q15_4) %>% 
  mutate(topic4_attitude1 = topic4_attitude1 - 5, 
         topic4_attitude2 = topic4_attitude2 - 5,
         topic4_attitude3 = topic4_attitude3 - 5,
         topic4_attitude4 = topic4_attitude4 - 5)

## Scale reduction

data <- data %>% 
  ungroup() %>%
  mutate(topic4_attitudes_average = 
           select(., matches("topic4_attitude\\d$")) %>%
           rowMeans(na.rm = TRUE))
topic4_attitudes_average_mean <- mean(data$topic4_attitudes_average, na.rm = T)

# Treatments ####
## Source: 0 is low-credibility, 1 is high-credibility
## Content: 0 is left-wing, 1 is right-wing

### Topic 1

data <- data %>% 
  mutate(topic1_content = 
           case_when(TopicISozialeGerechtigkeit.Storypost.done12._DO_Q22 == "1" ~ 0,
                     TopicISozialeGerechtigkeit.Storypost.done12._DO_Q20 == "1" ~ 0,
                     TopicISozialeGerechtigkeit.Storypost.done12._DO_Q19 == "1" ~ 1,
                     TopicISozialeGerechtigkeit.Storypost.done12._DO_Q21 == "1" ~ 1),
         topic1_congruence = 
           case_when(topic1_content == 0 & 
                       topic1_attitudes_average < topic1_attitudes_average_mean ~ 1,
                     topic1_content == 0 & 
                       topic1_attitudes_average > topic1_attitudes_average_mean ~ 0,
                     topic1_content == 1 & 
                       topic1_attitudes_average < topic1_attitudes_average_mean ~ 0,
                     topic1_content == 1 & 
                       topic1_attitudes_average > topic1_attitudes_average_mean ~ 1),
         topic1_source = 
           case_when(TopicISozialeGerechtigkeit.Storypost.done12._DO_Q21 == "1" ~ 0,
                     TopicISozialeGerechtigkeit.Storypost.done12._DO_Q22 == "1" ~ 0,
                     TopicISozialeGerechtigkeit.Storypost.done12._DO_Q19 == "1" ~ 1,
                     TopicISozialeGerechtigkeit.Storypost.done12._DO_Q20 == "1" ~ 1),
         topic1_interaction = topic1_content*2 + topic1_source)

## Topic 2

data <- data %>% 
  mutate(topic2_content = 
           case_when(TopicIIInnereSicherheit.Storypost.done12._DO_Q29 == "1" ~ 0,
                     TopicIIInnereSicherheit.Storypost.done12._DO_Q31 == "1" ~ 0,
                     TopicIIInnereSicherheit.Storypost.done12._DO_Q28 == "1" ~ 1,
                     TopicIIInnereSicherheit.Storypost.done12._DO_Q30 == "1" ~ 1),
         topic2_congruence = 
           case_when(topic2_content == 0 & 
                       topic2_attitudes_average < topic2_attitudes_average_mean ~ 1,
                     topic2_content == 0 & 
                       topic2_attitudes_average > topic2_attitudes_average_mean ~ 0,
                     topic2_content == 1 & 
                       topic2_attitudes_average < topic2_attitudes_average_mean ~ 0,
                     topic2_content == 1 & 
                       topic2_attitudes_average > topic2_attitudes_average_mean ~ 1),
         topic2_source = 
           case_when(TopicIIInnereSicherheit.Storypost.done12._DO_Q30 == "1" ~ 0,
                     TopicIIInnereSicherheit.Storypost.done12._DO_Q31 == "1" ~ 0,
                     TopicIIInnereSicherheit.Storypost.done12._DO_Q28 == "1" ~ 1,
                     TopicIIInnereSicherheit.Storypost.done12._DO_Q29 == "1" ~ 1),
         topic2_interaction = topic2_content*2 + topic2_source)

## Topic 3

data <- data %>% 
  mutate(topic3_content = 
           case_when(TopicIIIFlüchtlinge.Storypost.text.done12._DO_Q35 == "1" ~ 0,
                     TopicIIIFlüchtlinge.Storypost.text.done12._DO_Q37 == "1" ~ 0,
                     TopicIIIFlüchtlinge.Storypost.text.done12._DO_Q34 == "1" ~ 1,
                     TopicIIIFlüchtlinge.Storypost.text.done12._DO_Q36 == "1" ~ 1),
         topic3_congruence = 
           case_when(topic3_content == 0 & 
                       topic3_attitudes_average < topic3_attitudes_average_mean ~ 1,
                     topic3_content == 0 & 
                       topic3_attitudes_average > topic3_attitudes_average_mean ~ 0,
                     topic3_content == 1 & 
                       topic3_attitudes_average < topic3_attitudes_average_mean ~ 0,
                     topic3_content == 1 & 
                       topic3_attitudes_average > topic3_attitudes_average_mean ~ 1),
         topic3_source = 
           case_when(TopicIIIFlüchtlinge.Storypost.text.done12._DO_Q36 == "1" ~ 0,
                     TopicIIIFlüchtlinge.Storypost.text.done12._DO_Q37 == "1" ~ 0,
                     TopicIIIFlüchtlinge.Storypost.text.done12._DO_Q34 == "1" ~ 1,
                     TopicIIIFlüchtlinge.Storypost.text.done12._DO_Q35 == "1" ~ 1),
         topic3_interaction = topic3_content*2 + topic3_source)

## Topic 4

data <- data %>% 
  mutate(topic4_content = 
           case_when(TopicIVEuropa.Storypost.text.done12._DO_Q41 == "1" ~ 0,
                     TopicIVEuropa.Storypost.text.done12._DO_Q43 == "1" ~ 0,
                     TopicIVEuropa.Storypost.text.done12._DO_Q40 == "1" ~ 1,
                     TopicIVEuropa.Storypost.text.done12._DO_Q42 == "1" ~ 1),
         topic4_congruence = 
           case_when(topic4_content == 0 & 
                       topic4_attitudes_average < topic4_attitudes_average_mean ~ 1,
                     topic4_content == 0 & 
                       topic4_attitudes_average > topic4_attitudes_average_mean ~ 0,
                     topic4_content == 1 & 
                       topic4_attitudes_average < topic4_attitudes_average_mean ~ 0,
                     topic4_content == 1 & 
                       topic4_attitudes_average > topic4_attitudes_average_mean ~ 1),
         topic4_source = 
           case_when(TopicIVEuropa.Storypost.text.done12._DO_Q42 == "1" ~ 0,
                     TopicIVEuropa.Storypost.text.done12._DO_Q43 == "1" ~ 0,
                     TopicIVEuropa.Storypost.text.done12._DO_Q40 == "1" ~ 1,
                     TopicIVEuropa.Storypost.text.done12._DO_Q41 == "1" ~ 1),
         topic4_interaction = topic4_content*2 + topic4_source)

# Outcomes ####

data <- data %>% 
  rename(topic1_belief = Q23_1) %>% 
  group_by(topic1_content) %>% 
  mutate(topic1_belief_stand = scale(topic1_belief))

data <- data %>% 
  rename(topic2_belief = Q32_1) %>% 
  group_by(topic2_content) %>% 
  mutate(topic2_belief_stand = scale(topic2_belief))

data <- data %>% 
  rename(topic3_belief = Q38_1) %>% 
  group_by(topic3_content) %>% 
  mutate(topic3_belief_stand = scale(topic3_belief))

data <- data %>% rename(topic4_belief = Q44_1) %>% 
  group_by(topic4_content) %>% 
  mutate(topic4_belief_stand = scale(topic4_belief))

# Covariates ####

## Some variable groups

geodata <- read.csv(file = "data/plz_de.csv", 
                    header = T, 
                    sep = ";",
                    stringsAsFactors = F,
                    encoding = "UTF-8") %>%
  select(c(Bundesland, Plz)) %>% 
  rename(postcode = Plz) %>% 
  distinct(postcode, .keep_all = TRUE)

mainstream_sources = c("trustsource_heute",
                       "trustsource_ntv",
                       "trustsource_dlf",
                       "trustsource_sz",
                       "trustsource_welt")

knowledge_vars <- c("know_party_merkel",
                    "know_party_leyen",
                    "know_party_goering",
                    "know_party_oezdemir",
                    "know_party_hoecke",
                    "know_party_schulz",
                    "know_job_bartsch",
                    "know_job_mueller",
                    "know_job_steinmeier",
                    "know_job_schulz",
                    "know_job_lammert")

frequency <- c("Einmal pro Woche oder weniger", 
               "Mehrmals die Woche, aber nicht täglich",
               "Einmal am Tag",
               "Mehrmals täglich",
               "Stündlich oder öfter") 

## Recoding

data <- data %>% 
  
  ## Consent/debriefing
  rename(consent = Q1) %>%
  rename(debriefing = Q75) %>%
  
  ## Age
  rename(birthyear = Q3_1) %>% 
  mutate(age = 2017 - birthyear) %>% 
  mutate(age_group_quotas = case_when(
    age < 30 ~ "18-30ys",
    age >= 30 & age < 50 ~ "31-50ys",
    age >= 50 & age < 65 ~ "50-65ys",
    age >= 65 ~ "65+")) %>% 
  mutate(age_group_census = case_when(
    age < 31 ~ "18-30ys",
    age >= 31 & age < 44 ~ "31-43ys",
    age >= 44 & age < 57 ~ "44-56ys",
    age >= 57 & age < 69 ~ "57-69ys",
    age >= 69 ~ "70+")) %>% 
  
  ## Gender
  rename(sex = Q4) %>% 
  mutate(sex = case_when(
    sex == "männlich" ~ 0,
    sex == "weiblich" ~ 1)) %>% 
  
  ## Education
  rename(education = Q5) %>% 
  mutate(education_en = factor(
    education, 
    ordered = TRUE,
    levels = c("keinen",
               "Haupt-/Volksschulabschluss",
               "Realschulabschluss (Mittlere Reife)", 
               "Fachhochschulreife",
               "allgemeine oder fachgebundene Hochschulreife (Abitur)",
               "Fach-/Hochschulstudium"),
    labels = c("None or still in school",
               "Lower school (Volks-/Hauptschule)",
               "Middle school (Realschule)",
               "Technical high school (Fachabitur)",
               "High school (Abitur)",
               "University"))) %>% 
  mutate(education_num = as.numeric(education_en)) %>% 
  mutate(education_high = ifelse(
    education_num > 3, 1, 0
  )) %>%
  
  ## Income
  rename(income = Q55) %>% 
  mutate(income = factor(
    income,
    ordered = TRUE,
    levels = c("unter 500 Euro",
               "500 bis 800 Euro",
               "800 bis 1100 Euro",
               "1100 bis 1500 Euro",
               "1500 bis 2000 Euro",
               "2000 bis 2500 Euro",
               "2500 bis 3000 Euro",
               "3000 bis 3500 Euro",
               "über 3500"))) %>%
  mutate(income_num = as.numeric(income)) %>%
  
  ## Residence
  rename(postcode = Q56) %>%
  merge(., geodata, by="postcode", all.x = T) %>% 
  rename(federal_state = Bundesland) %>% 
  mutate(federal_state = case_when(
    federal_state == "Schlewig-Holstein" ~ "Schleswig-Holstein",
    TRUE ~ as.character(federal_state))) %>%
  mutate(federal_state_west = ifelse(
    federal_state == "Brandenburg" |
     federal_state == "Mecklenburg-Vorpommern" |
     federal_state == "Sachsen" |
     federal_state == "Sachsen-Anhalt" |
     federal_state == "Thüringen", 0, 1)) %>%

  ## Citizenship
  rename(citizenship = Q53) %>% 
  mutate(citizenship = factor(
    citizenship, levels = c("Ja", "Nein"))) %>%
  mutate(citizenship_num = as.numeric(citizenship) - 1) %>%
  
  ## Religion
  rename(religion = Q54) %>% 
  mutate(religion = factor(
    religion, 
    levels = c("keiner",
               "dem Islam",
               "dem Judentum",
               "der römisch-katholischen Kirche",
               "einer evangelischen Kirche",
               "einer anderen christlichen Religionsgemeinschaft",
               "einer anderen, nicht genannten Religionsgemeinschaft"))) %>%
  
  ## Media trust
  rename(trustsource_heute = Q7_2,
         trustsource_ntv = Q7_26,
         trustsource_dlf = Q7_27,
         trustsource_sz = Q7_3,
         trustsource_welt = Q7_4,
         trustsource_jf = Q7_5,
         trustsource_jw = Q7_7,
         trustsource_24aktuelles = Q7_8,
         trustsource_internetzzeitung = Q7_11,
         trustsource_newsblitz = Q7_12) %>% 
  mutate(trustsource_mainstream =
           select(., all_of(mainstream_sources)) %>%
                     rowMeans(na.rm = TRUE)) %>%
  
  ## Left-right placement
  rename(leftright = Q46_1) %>%
  
  ## Political interest
  rename(political_interest = Q47_1) %>%
  
  ## Political knowledge
  rename(party_merkel = Q49.1_1,
         party_leyen = Q49.1_2,
         party_goering = Q49.1_3,
         party_oezdemir = Q49.1_4,
         party_hoecke = Q49.1_5,
         party_schulz = Q49.1_6) %>% 
  mutate(know_party_merkel = ifelse(
      party_merkel == "CDU", 1, 0),
    know_party_leyen = ifelse(
      party_leyen == "CDU", 1, 0),
    know_party_goering = ifelse(
      party_goering == "Bündnis 90/Die Grünen", 1, 0),
    know_party_oezdemir =  ifelse(
      party_oezdemir == "Bündnis 90/Die Grünen", 1, 0),
    know_party_hoecke = ifelse(
      party_hoecke == "AfD", 1, 0),
    know_party_schulz = ifelse(
      party_schulz == "SPD", 1, 0)) %>%
  rename(job_bartsch = Q51.1_1,
         job_mueller = Q51.1_2,
         job_steinmeier = Q51.1_3,
         job_schulz = Q51.1_4,
         job_lammert = Q51.1_5) %>% 
  mutate(know_job_bartsch = ifelse(
    job_bartsch == "Parteivorsitzender der Linken", 1, 0)) %>% 
  mutate(know_job_mueller = ifelse(
    job_mueller == "Bundesminister für Entwicklung", 1, 0),
    know_job_steinmeier = ifelse(
      job_steinmeier == "Bundespräsident", 1, 0),
    know_job_schulz = ifelse(
      job_schulz == "Parteivorsitzender der SPD", 1, 0),
    know_job_lammert = ifelse(
      job_lammert == "Präsident des Deutschen Bundestages ", 1, 0)) %>%
    mutate(political_knowledge =
           select(., all_of(knowledge_vars)) %>%
           rowMeans(na.rm = TRUE)) %>%
  
  ## Media platform use
  rename(importance_fb = Q58.1_1,
         importance_social = Q58.1_2,
         importance_newssites = Q58.1_3,
         importance_print = Q58.1_4,
         importance_tv = Q58.1_5,
         importance_radio = Q58.1_6) %>% 
  mutate_at(vars(matches("importance")),
            list(~factor(., ordered = TRUE,
                         levels = c("Komplett unwichtig",
                                    "Eher unwichtig",
                                    "Wichtig",
                                    "Sehr wichtig")))) %>%
  mutate(importance_fb_num = as.numeric(importance_fb)) %>%
  
  ## Frequency of media use
  rename(media_frequency = Q59) %>% 
  mutate(media_frequency = factor(
    media_frequency,
    ordered = TRUE,
    levels = frequency),
    media_frequency_num = as.numeric(media_frequency)) %>%
  
  ## Facebook account
  rename(fb_account = Q61) %>% 
  mutate(fb_account = factor(
    fb_account, levels = c("Ja", "Nein"))) %>%
  
  ## Frequency of Facebook use
  rename(fb_frequency = Q62) %>% 
  mutate(fb_frequency =  factor(
    fb_frequency, ordered = TRUE, levels = frequency)) %>%
  
  ## Turnout and voting intention
  rename(turnout_intent = Q64) %>% 
  mutate(turnout_intent = factor(
    turnout_intent, 
    levels = c("Ja", "Nein", "Ich weiß es noch nicht")),
    turnout_intent_num = case_when(
      turnout_intent == "Ja" ~ 1,
      turnout_intent == "Nein" | 
        turnout_intent == "Ich weiß es noch nicht" ~ 0)) %>% 
  rename(voting_intent = Q65) %>% 
  mutate(voting_intent_7 =  case_when(
    voting_intent == "Ich weiß es noch nicht" ~ "Other",
    voting_intent == "Keine Angabe" ~ "Other",
     voting_intent == "Eine andere" ~ "Other",
     voting_intent == "" ~ "Other",
     TRUE ~ as.character(voting_intent))) %>% 
  rename(voting_intent_others = Q66) %>%
  
  ## Attention checks
  rename(truestory_belief = Q26_1) %>%
  rename(truestory_belief_submit = Q27_Page.Submit)

