###########################################################
### Recoding Study 1 from "The Ocean of Possible Truth" ###
###########################################################

#### Remarks on general coding scheme ####
# "keine Angabe" turned into NA

#### Delete unused variables ####

data_1 %<>% select(-c(Duration..in.seconds., Status,
                      RecipientLastName, RecipientFirstName, RecipientEmail,
                      ExternalReference, DistributionChannel, UserLanguage,
                      term, gc, opp, age_group, year))

#### Treatments ####

# Source: 0 is unprofessional, 1 is professional
# Content: 0 is left-wing, 1 is right-wing

### Delete randomize variables for outcome measure and timing blocks (seen by all subjects)

data_1 %<>% select(-c("TopicISozialeGerechtigkeit.Storypost.done12._DO_Q23",
                      "TopicISozialeGerechtigkeit.Storypost.done12._DO_Q24",
                      "TopicIIInnereSicherheit.Storypost.done12._DO_Q32",
                      "TopicIIInnereSicherheit.Storypost.done12._DO_Q33",
                      "TopicIIIFlüchtlinge.Storypost.text.done12._DO_Q38",
                      "TopicIIIFlüchtlinge.Storypost.text.done12._DO_Q39",
                      "TopicIVEuropa.Storypost.text.done12._DO_Q44",
                      "TopicIVEuropa.Storypost.text.done12._DO_Q45"))

### Topic 1

data_1 %<>% 
  mutate(topic1_content = 
           case_when(TopicISozialeGerechtigkeit.Storypost.done12._DO_Q22 == "1" ~ 0,
                     TopicISozialeGerechtigkeit.Storypost.done12._DO_Q20 == "1" ~ 0,
                     TopicISozialeGerechtigkeit.Storypost.done12._DO_Q19 == "1" ~ 1,
                     TopicISozialeGerechtigkeit.Storypost.done12._DO_Q21 == "1" ~ 1))

data_1 %<>% 
  mutate(topic1_source = 
           case_when(TopicISozialeGerechtigkeit.Storypost.done12._DO_Q21 == "1" ~ 0,
                     TopicISozialeGerechtigkeit.Storypost.done12._DO_Q22 == "1" ~ 0,
                     TopicISozialeGerechtigkeit.Storypost.done12._DO_Q19 == "1" ~ 1,
                     TopicISozialeGerechtigkeit.Storypost.done12._DO_Q20 == "1" ~ 1))

data_1 %<>% mutate(topic1_treatment = 
                     case_when(topic1_content == 0 & topic1_source == 0 ~ "A",
                               topic1_content == 0 & topic1_source == 1 ~ "B",
                               topic1_content == 1 & topic1_source == 0 ~ "C",
                               topic1_content == 1 & topic1_source == 1 ~ "D"))

## Topic 2


data_1 %<>% 
  mutate(topic2_content = 
           case_when(TopicIIInnereSicherheit.Storypost.done12._DO_Q29 == "1" ~ 0,
                     TopicIIInnereSicherheit.Storypost.done12._DO_Q31 == "1" ~ 0,
                     TopicIIInnereSicherheit.Storypost.done12._DO_Q28 == "1" ~ 1,
                     TopicIIInnereSicherheit.Storypost.done12._DO_Q30 == "1" ~ 1))

data_1 %<>% 
  mutate(topic2_source = 
           case_when(TopicIIInnereSicherheit.Storypost.done12._DO_Q30 == "1" ~ 0,
                     TopicIIInnereSicherheit.Storypost.done12._DO_Q31 == "1" ~ 0,
                     TopicIIInnereSicherheit.Storypost.done12._DO_Q28 == "1" ~ 1,
                     TopicIIInnereSicherheit.Storypost.done12._DO_Q29 == "1" ~ 1))

data_1 %<>% mutate(topic2_treatment = 
                     case_when(topic2_content == 0 & topic2_source == 0 ~ "A",
                               topic2_content == 0 & topic2_source == 1 ~ "B",
                               topic2_content == 1 & topic2_source == 0 ~ "C",
                               topic2_content == 1 & topic2_source == 1 ~ "D"))

## Topic 3

data_1 %<>% 
  mutate(topic3_content = 
           case_when(TopicIIIFlüchtlinge.Storypost.text.done12._DO_Q35 == "1" ~ 0,
                     TopicIIIFlüchtlinge.Storypost.text.done12._DO_Q37 == "1" ~ 0,
                     TopicIIIFlüchtlinge.Storypost.text.done12._DO_Q34 == "1" ~ 1,
                     TopicIIIFlüchtlinge.Storypost.text.done12._DO_Q36 == "1" ~ 1))

data_1 %<>% 
  mutate(topic3_source = 
           case_when(TopicIIIFlüchtlinge.Storypost.text.done12._DO_Q36 == "1" ~ 0,
                     TopicIIIFlüchtlinge.Storypost.text.done12._DO_Q37 == "1" ~ 0,
                     TopicIIIFlüchtlinge.Storypost.text.done12._DO_Q34 == "1" ~ 1,
                     TopicIIIFlüchtlinge.Storypost.text.done12._DO_Q35 == "1" ~ 1))

data_1 %<>% mutate(topic3_treatment = 
                     case_when(topic3_content == 0 & topic3_source == 0 ~ "A",
                               topic3_content == 0 & topic3_source == 1 ~ "B",
                               topic3_content == 1 & topic3_source == 0 ~ "C",
                               topic3_content == 1 & topic3_source == 1 ~ "D"))

## Topic 4

data_1 %<>% 
  mutate(topic4_content = 
           case_when(TopicIVEuropa.Storypost.text.done12._DO_Q41 == "1" ~ 0,
                     TopicIVEuropa.Storypost.text.done12._DO_Q43 == "1" ~ 0,
                     TopicIVEuropa.Storypost.text.done12._DO_Q40 == "1" ~ 1,
                     TopicIVEuropa.Storypost.text.done12._DO_Q42 == "1" ~ 1))

data_1 %<>% 
  mutate(topic4_source = 
           case_when(TopicIVEuropa.Storypost.text.done12._DO_Q42 == "1" ~ 0,
                     TopicIVEuropa.Storypost.text.done12._DO_Q43 == "1" ~ 0,
                     TopicIVEuropa.Storypost.text.done12._DO_Q40 == "1" ~ 1,
                     TopicIVEuropa.Storypost.text.done12._DO_Q41 == "1" ~ 1))

data_1 %<>% mutate(topic4_treatment = 
                     case_when(topic4_content == 0 & topic4_source == 0 ~ "A",
                               topic4_content == 0 & topic4_source == 1 ~ "B",
                               topic4_content == 1 & topic4_source == 0 ~ "C",
                               topic4_content == 1 & topic4_source == 1 ~ "D"))

#### Outcomes ####

data_1 %<>% rename(topic1_belief = Q23_1)
data_1 %<>% group_by(topic1_content) %>% mutate(topic1_belief_stand = scale(topic1_belief))

data_1 %<>% rename(topic2_belief = Q32_1)
data_1 %<>% group_by(topic2_content) %>% mutate(topic2_belief_stand = scale(topic2_belief))

data_1 %<>% rename(topic3_belief = Q38_1)
data_1 %<>% group_by(topic3_content) %>% mutate(topic3_belief_stand = scale(topic3_belief))

data_1 %<>% rename(topic4_belief = Q44_1)
data_1 %<>% group_by(topic4_content) %>% mutate(topic4_belief_stand = scale(topic4_belief))

#### Covariates ####

### Consent

data_1 %<>% rename(consent = Q1)

### Debriefing

data_1 %<>% rename(debriefing = Q75)

### Age

data_1 %<>% rename(birthyear = Q3_1)
data_1 %<>% mutate(age = 2017 - birthyear)
data_1 %<>% mutate(age_group_quotas = case_when(age < 30 ~ "18-30ys",
                                                age >= 30 & age < 50 ~ "31-50ys",
                                                age >= 50 & age < 65 ~ "50-65ys",
                                                age >= 65 ~ "65+"))
data_1 %<>% mutate(age_group_census = case_when(age < 31 ~ "18-30ys",
                                                age >= 31 & age < 44 ~ "31-43ys",
                                                age >= 44 & age < 57 ~ "44-56ys",
                                                age >= 57 & age < 69 ~ "57-69ys",
                                                age >= 69 ~ "70+"))

### Gender
# 0 is male, 1 is female

data_1 %<>% rename(sex = Q4)
data_1 %<>% mutate(sex = case_when(sex == "männlich" ~ 0,
                                   sex == "weiblich" ~ 1))

### Education

data_1 %<>% rename(education = Q5)
data_1 %<>% mutate(education = 
                     factor(education, 
                            ordered = TRUE,
                            levels = c("keinen",
                                       "Haupt-/Volksschulabschluss",
                                       "Realschulabschluss (Mittlere Reife)", 
                                       "Fachhochschulreife",
                                       "allgemeine oder fachgebundene Hochschulreife (Abitur)",
                                       "Fach-/Hochschulstudium")))
data_1 %<>% 
  mutate(education_6 = 
           case_when(education == "keinen" ~  "None or still in school",
                     education == "Haupt-/Volksschulabschluss" ~  "Lower school (Volks-/Hauptschule)",
                     education == "Realschulabschluss (Mittlere Reife)" ~  "Middle school (Realschule)",
                     education == "Fachhochschulreife" ~  "Technical high school (Fachabitur)",
                     education == "allgemeine oder fachgebundene Hochschulreife (Abitur)" ~  "High school (Abitur)",
                     education == "Fach-/Hochschulstudium" ~  "University"))
data_1$education_6 <- factor(data_1$education_6, ordered = TRUE)

### Income

data_1 %<>% rename(income = Q55)
data_1 %<>% mutate(income = 
                     factor(income,
                            ordered = TRUE,
                            levels = c("unter 500 Euro",
                                       "500 bis 800 Euro",
                                       "800 bis 1100 Euro",
                                       "1100 bis 1500 Euro",
                                       "1500 bis 2000 Euro",
                                       "2000 bis 2500 Euro",
                                       "2500 bis 3000 Euro",
                                       "3000 bis 3500 Euro",
                                       "über 3500")))

### Residence

data_1 %<>% rename(postcode = Q56)

# Import data set with postcode-state pairs
geodata <- read.csv(file = "plz_de.csv", 
                    header = T, 
                    sep = ";",
                    stringsAsFactors = F,
                    encoding = "UTF-8")
geodata %<>% select(c(Bundesland, Plz))
geodata %<>% rename(postcode = Plz)
geodata %<>% distinct(postcode, .keep_all = TRUE)

data_1 <- merge(data_1, geodata, by="postcode", all.x = T)
data_1 %<>% rename(federal_state = Bundesland)
data_1 %<>% mutate(federal_state = case_when(
  federal_state == "Schlewig-Holstein" ~ "Schleswig-Holstein",
  TRUE ~ as.character(federal_state)))

data_1 %<>% mutate(federal_state_west = ifelse(federal_state == "Brandenburg" |
                                                 federal_state == "Mecklenburg-Vorpommern" |
                                                 federal_state == "Sachsen" |
                                                 federal_state == "Sachsen-Anhalt" |
                                                 federal_state == "Thüringen", 0, 1))

rm(geodata)

### Citizenship

data_1 %<>% rename(citizenship = Q53)
data_1 %<>% mutate(citizenship = 
                     factor(citizenship,
                            levels = c("Ja",
                                       "Nein")))

### Religion

data_1 %<>% rename(religion = Q54)
data_1 %<>% mutate(religion = 
                     factor(religion,
                            levels = c("keiner",
                                       "dem Islam",
                                       "dem Judentum",
                                       "der römisch-katholischen Kirche",
                                       "einer evangelischen Kirche",
                                       "einer anderen christlichen Religionsgemeinschaft",
                                       "einer anderen, nicht genannten Religionsgemeinschaft")))

### Media trust

data_1 %<>% rename(trustsource_heute = Q7_2)
data_1 %<>% rename(trustsource_ntv = Q7_26)
data_1 %<>% rename(trustsource_dlf = Q7_27)
data_1 %<>% rename(trustsource_sz = Q7_3)
data_1 %<>% rename(trustsource_welt = Q7_4)
data_1 %<>% rename(trustsource_jf = Q7_5)
data_1 %<>% rename(trustsource_jw = Q7_7)
data_1 %<>% rename(trustsource_24aktuelles = Q7_8)
data_1 %<>% rename(trustsource_internetzzeitung = Q7_11)
data_1 %<>% rename(trustsource_newsblitz = Q7_12)

mainstream_sources = c("trustsource_heute",
                       "trustsource_ntv",
                       "trustsource_dlf",
                       "trustsource_sz",
                       "trustsource_welt")

data_1 %<>% mutate(trustsource_mainstream =
                     select(., mainstream_sources) %>%
                     rowMeans(na.rm = TRUE))

### Left-right placement

data_1 %<>% rename(leftright = Q46_1)

### Political interest

data_1 %<>% rename(political_interest = Q47_1)

### Political knowledge

## Party of a politician

data_1 %<>% rename(party_merkel = Q49.1_1)
data_1 %<>% rename(party_leyen = Q49.1_2)
data_1 %<>% rename(party_goering = Q49.1_3)
data_1 %<>% rename(party_oezdemir = Q49.1_4)
data_1 %<>% rename(party_hoecke = Q49.1_5)
data_1 %<>% rename(party_schulz = Q49.1_6)

data_1 %<>% mutate(know_party_merkel = 
                     ifelse(party_merkel == "CDU", 1, 0))
data_1 %<>% mutate(know_party_leyen = 
                     ifelse(party_leyen == "CDU", 1, 0))
data_1 %<>% mutate(know_party_goering = 
                     ifelse(party_goering == "Bündnis 90/Die Grünen", 1, 0))
data_1 %<>% mutate(know_party_oezdemir = 
                     ifelse(party_oezdemir == "Bündnis 90/Die Grünen", 1, 0))
data_1 %<>% mutate(know_party_hoecke = 
                     ifelse(party_hoecke == "AfD", 1, 0))
data_1 %<>% mutate(know_party_leyen = 
                     ifelse(party_schulz == "SPD", 1, 0))

## Job of a politician

data_1 %<>% rename(job_bartsch = Q51.1_1)
data_1 %<>% rename(job_mueller = Q51.1_2)
data_1 %<>% rename(job_steinmeier = Q51.1_3)
data_1 %<>% rename(job_schulz = Q51.1_4)
data_1 %<>% rename(job_lammert = Q51.1_5)

data_1 %<>% mutate(know_job_bartsch = 
                     ifelse(job_bartsch == "Parteivorsitzender der Linken", 1, 0))
data_1 %<>% mutate(know_job_mueller = 
                     ifelse(job_mueller == "Bundesminister für Entwicklung", 1, 0))
data_1 %<>% mutate(know_job_steinmeier = 
                     ifelse(job_steinmeier == "Bundespräsident", 1, 0))
data_1 %<>% mutate(know_job_schulz = 
                     ifelse(job_schulz == "Parteivorsitzender der SPD", 1, 0))
data_1 %<>% mutate(know_job_lammert = 
                     ifelse(job_lammert == "Präsident des Deutschen Bundestages ", 1, 0))

### Media platform use

data_1 %<>% rename(importance_fb = Q58.1_1)
data_1 %<>% rename(importance_social = Q58.1_2)
data_1 %<>% rename(importance_newssites = Q58.1_3)
data_1 %<>% rename(importance_print = Q58.1_4)
data_1 %<>% rename(importance_tv = Q58.1_5)
data_1 %<>% rename(importance_radio = Q58.1_6)

data_1 %<>% mutate_at(vars(matches("importance")),
                      list(~factor(., ordered = TRUE,
                                   levels = c("Komplett unwichtig",
                                              "Eher unwichtig",
                                              "Wichtig",
                                              "Sehr wichtig"))))

### Frequency of media use

frequency <- c("Einmal pro Woche oder weniger", 
               "Mehrmals die Woche, aber nicht täglich",
               "Einmal am Tag",
               "Mehrmals täglich",
               "Stündlich oder öfter") # categories for use frequency

data_1 %<>% rename(media_frequency = Q59)
data_1 %<>% mutate(media_frequency = 
                     factor(media_frequency,
                            ordered = TRUE,
                            levels = frequency))

### Facebook account

data_1 %<>% rename(fb_account = Q61)
data_1 %<>% mutate(fb_account = 
                     factor(fb_account,
                            levels = c("Ja",
                                       "Nein")))

### Frequency of Facebook use

data_1 %<>% rename(fb_frequency = Q62)
data_1 %<>% mutate(fb_frequency = 
                     factor(fb_frequency,
                            ordered = TRUE,
                            levels = frequency))

### Turnout and voting intention

data_1 %<>% rename(turnout_intent = Q64)
data_1 %<>% mutate(turnout_intent = 
                     factor(turnout_intent,
                            levels = c("Ja",
                                       "Nein",
                                       "Ich weiß es noch nicht")))

data_1 %<>% rename(voting_intent = Q65)
data_1 %<>% mutate(voting_intent = 
                     factor(voting_intent,
                            levels = c("Die Linke",
                                       "SPD",
                                       "Bündnis 90/Die Grünen",
                                       "CDU/CSU",
                                       "FDP",
                                       "AfD",
                                       "Ich weiß es noch nicht")))

data_1 %<>% rename(voting_intent_others = Q66)

#### Pre-treatment attitudes #### 

### Topic 1
# -5 is pro-distribution, 5 is anti-distribution

data_1 %<>% rename(topic1_attitude1 = Q9_5)
data_1 %<>% rename(topic1_attitude2 = Q9_6)
data_1 %<>% rename(topic1_attitude3 = Q9_9)
data_1 %<>% rename(topic1_attitude4 = Q9_10)

data_1 %<>% mutate(topic1_attitude1 = (topic1_attitude1 * (-1)) + 5)
data_1 %<>% mutate(topic1_attitude2 = topic1_attitude2 - 5)
data_1 %<>% mutate(topic1_attitude3 = (topic1_attitude3 * (-1)) + 5)
data_1 %<>% mutate(topic1_attitude4 = (topic1_attitude4 * (-1)) + 5)

## Scale reduction

# Cronbach's alpha

topic1_alpha <- data_1 %>% 
  ungroup() %>% 
  select(matches("topic1_attitude\\d$")) %>% 
  psych::alpha() # Since reliability is low, no composite index calculated

# Principal factor analysis

topic1_factor_analysis <- data_1 %>% 
  ungroup() %>% 
  select(matches("topic1_attitude\\d$")) %>% 
  fa(., nfactors = 1, rotate = "none", fm = "pa", max.iter = 1)

# Standardize single items to make them comparable to factors for other topics

data_1 %<>%
  ungroup() %>%
  mutate_at(vars(matches("topic1_attitude\\d$")), .funs = list(stand = ~scale(.)))

data_1 %<>%
  mutate(topic1_attitude1_stand = as.vector(topic1_attitude1_stand))
data_1 %<>%
  mutate(topic1_attitude2_stand = as.vector(topic1_attitude1_stand))
data_1 %<>%
  mutate(topic1_attitude3_stand = as.vector(topic1_attitude1_stand))
data_1 %<>%
  mutate(topic1_attitude4_stand = as.vector(topic1_attitude1_stand))

### Topic 2
# -5 is anti-law-and-order, 5 is pro-law-and-order

data_1 %<>% rename(topic2_attitude1 = Q11_5)
data_1 %<>% rename(topic2_attitude2 = Q11_2)
data_1 %<>% rename(topic2_attitude3 = Q11_3)
data_1 %<>% rename(topic2_attitude4 = Q11_4)

data_1 %<>% mutate(topic2_attitude1 = topic2_attitude1 - 5)
data_1 %<>% mutate(topic2_attitude2 = (topic2_attitude2 * (-1)) + 5)
data_1 %<>% mutate(topic2_attitude3 = topic2_attitude3 - 5)
data_1 %<>% mutate(topic2_attitude4 = topic2_attitude4 - 5)

## Scale reduction

# Cronbach's alpha 

topic2_alpha <- data_1 %>% 
  ungroup() %>% 
  select(matches("topic2_attitude\\d$")) %>% 
  psych::alpha() # High reliability for three out of four measures

# Composite index as average index of three measures 

data_1 %<>% 
  ungroup() %>%
  mutate(topic2_attitudes134_average = 
           select(., c("topic2_attitude1",
                       "topic2_attitude3",
                       "topic2_attitude4")) %>%
           rowMeans(na.rm = TRUE))

# Principal factor analysis

topic2_factor_analysis <- data_1 %>% 
  ungroup() %>% 
  select(matches("topic2_attitude\\d$")) %>% 
  fa(., nfactors = 2, rotate = "none", fm = "pa", max.iter = 1)

data_1$topic2_attitudes_factor <- topic2_factor_analysis$scores[, 1]

## Topic 3
# -5 is pro-migration, 5 is anti-migration

data_1 %<>% rename(topic3_attitude1 = Q13_1)
data_1 %<>% rename(topic3_attitude2 = Q13_2)
data_1 %<>% rename(topic3_attitude3 = Q13_3)
data_1 %<>% rename(topic3_attitude4 = Q13_4)

data_1 %<>% mutate(topic3_attitude1 = (topic3_attitude1 * (-1)) + 5)
data_1 %<>% mutate(topic3_attitude2 = topic3_attitude2 - 5)
data_1 %<>% mutate(topic3_attitude3 = topic3_attitude3 - 5)
data_1 %<>% mutate(topic3_attitude4 = topic3_attitude4 - 5)

## Scale reduction

# Cronbach's alpha 

topic3_alpha <-data_1 %>% 
  ungroup() %>% 
  select(matches("topic3_attitude\\d$")) %>% 
  psych::alpha() # All four measures reliable

# Composite index as average index of four measures 

data_1 %<>% 
  ungroup() %>%
  mutate(topic3_attitudes_average = 
           select(., matches("topic3_attitude\\d$")) %>%
           rowMeans(na.rm = TRUE))

# Principal factor analysis

topic3_factor_analysis <- data_1 %>% 
  ungroup() %>% 
  select(matches("topic3_attitude\\d$")) %>% 
  fa(., nfactors = 1, rotate = "none", fm = "pa", max.iter = 1)

data_1$topic3_attitudes_factor <- topic3_factor_analysis$scores[, 1]

### Topic 4
# -5 is pro-EU, 5 is anti-EU

data_1 %<>% rename(topic4_attitude1 = Q15_1)
data_1 %<>% rename(topic4_attitude2 = Q15_5)
data_1 %<>% rename(topic4_attitude3 = Q15_3)
data_1 %<>% rename(topic4_attitude4 = Q15_4)

data_1 %<>% mutate(topic4_attitude1 = topic4_attitude1 - 5)
data_1 %<>% mutate(topic4_attitude2 = topic4_attitude2 - 5)
data_1 %<>% mutate(topic4_attitude3 = topic4_attitude3 - 5)
data_1 %<>% mutate(topic4_attitude4 = topic4_attitude4 - 5)

## Scale reduction

# Cronbach's alpha

topic4_alpha <- data_1 %>% 
  ungroup() %>% 
  select(matches("topic4_attitude\\d$")) %>% 
  psych::alpha() # All four measures reliable

# Composite index as average index of four measures 

data_1 %<>% 
  ungroup() %>%
  mutate(topic4_attitudes_average = 
           select(., matches("topic4_attitude\\d$")) %>%
           rowMeans(na.rm = TRUE))

# Principal factor analysis

topic4_factor_analysis <- data_1 %>% 
  ungroup() %>% 
  select(matches("topic4_attitude\\d$")) %>% 
  fa(., nfactors = 1, rotate = "none", fm = "pa", max.iter = 1)

data_1$topic4_attitudes_factor <- topic4_factor_analysis$scores[, 1]

#### Post-treatment attitudes #### 

### Topic 1
# -5 is pro-distribution, 5 is anti-distribution

data_1 %<>% rename(topic1_postattitude1 = Q67_5)
data_1 %<>% rename(topic1_postattitude2 = Q67_6)
data_1 %<>% rename(topic1_postattitude3 = Q67_9)
data_1 %<>% rename(topic1_postattitude4 = Q67_10)

data_1 %<>% mutate(topic1_postattitude1 = (topic1_postattitude1 * (-1)) + 5)
data_1 %<>% mutate(topic1_postattitude2 = topic1_postattitude2 - 5)
data_1 %<>% mutate(topic1_postattitude3 = (topic1_postattitude3 * (-1)) + 5)
data_1 %<>% mutate(topic1_postattitude4 = (topic1_postattitude4 * (-1)) + 5)

## Topic 2
# -5 is anti-law-and-order, 5 is pro-law-and-order

data_1 %<>% rename(topic2_postattitude1 = Q69_5)
data_1 %<>% rename(topic2_postattitude2 = Q69_2)
data_1 %<>% rename(topic2_postattitude3 = Q69_3)
data_1 %<>% rename(topic2_postattitude4 = Q69_4)

data_1 %<>% mutate(topic2_postattitude1 = topic2_postattitude1 - 5)
data_1 %<>% mutate(topic2_postattitude2 = (topic2_postattitude2 * (-1)) + 5)
data_1 %<>% mutate(topic2_postattitude3 = topic2_postattitude3 - 5)
data_1 %<>% mutate(topic2_postattitude4 = topic2_postattitude4 - 5)

# Factor

## Topic 3
# - 5 is pro-migration, 5 is anti-migration

data_1 %<>% rename(topic3_postattitude1 = Q71_1)
data_1 %<>% rename(topic3_postattitude2 = Q71_2)
data_1 %<>% rename(topic3_postattitude3 = Q71_3)
data_1 %<>% rename(topic3_postattitude4 = Q71_4)

data_1 %<>% mutate(topic3_postattitude1 = (topic3_postattitude1 * (-1)) + 5)
data_1 %<>% mutate(topic3_postattitude2 = topic3_postattitude2 - 5)
data_1 %<>% mutate(topic3_postattitude3 = topic3_postattitude3 - 5)
data_1 %<>% mutate(topic3_postattitude4 = topic3_postattitude4 - 5)

## Topic 4
# -5 is pro-EU, 5 is anti-EU

data_1 %<>% rename(topic4_postattitude1 = Q73_1)
data_1 %<>% rename(topic4_postattitude2 = Q73_5)
data_1 %<>% rename(topic4_postattitude3 = Q73_3)
data_1 %<>% rename(topic4_postattitude4 = Q73_4)

data_1 %<>% mutate(topic4_postattitude1 = topic4_postattitude1 - 5)
data_1 %<>% mutate(topic4_postattitude2 = topic4_postattitude2 - 5)
data_1 %<>% mutate(topic4_postattitude3 = topic4_postattitude3 - 5)
data_1 %<>% mutate(topic4_postattitude4 = topic4_postattitude4 - 5)

#### Meta variables ####

### Attention checks

data_1 %<>% rename(truestory_belief = Q26_1)

### Timings

data_1 %<>% rename(consent_firstclick = Q2_First.Click)
data_1 %<>% rename(consent_lastclick = Q2_Last.Click)
data_1 %<>% rename(consent_submit = Q2_Page.Submit)
data_1 %<>% rename(consent_clicks = Q2_Click.Count)

data_1 %<>% rename(demographics1_firstclick = Q6_First.Click)
data_1 %<>% rename(demographics1_lastclick = Q6_Last.Click)
data_1 %<>% rename(demographics1_submit = Q6_Page.Submit)
data_1 %<>% rename(demographics1_clicks = Q6_Click.Count)

data_1 %<>% rename(topic1_attitudes_firstclick = Q10_First.Click)
data_1 %<>% rename(topic1_attitudes_lastclick = Q10_Last.Click)
data_1 %<>% rename(topic1_attitudes_submit = Q10_Page.Submit)
data_1 %<>% rename(topic1_attitudes_clicks = Q10_Click.Count)

data_1 %<>% rename(topic2_attitudes_firstclick = Q12_First.Click)
data_1 %<>% rename(topic2_attitudes_lastclick = Q12_Last.Click)
data_1 %<>% rename(topic2_attitudes_submit = Q12_Page.Submit)
data_1 %<>% rename(topic2_attitudes_clicks = Q12_Click.Count)

data_1 %<>% rename(topic3_attitudes_firstclick = Q14_First.Click)
data_1 %<>% rename(topic3_attitudes_lastclick = Q14_Last.Click)
data_1 %<>% rename(topic3_attitudes_submit = Q14_Page.Submit)
data_1 %<>% rename(topic3_attitudes_clicks = Q14_Click.Count)

data_1 %<>% rename(topic4_attitudes_firstclick = Q16_First.Click)
data_1 %<>% rename(topic4_attitudes_lastclick = Q16_Last.Click)
data_1 %<>% rename(topic4_attitudes_submit = Q16_Page.Submit)
data_1 %<>% rename(topic4_attitudes_clicks = Q16_Click.Count)

data_1 %<>% rename(trust_firstclick = Q8_First.Click)
data_1 %<>% rename(trust_lastclick = Q8_Last.Click)
data_1 %<>% rename(trust_submit = Q8_Page.Submit)
data_1 %<>% rename(trust_clicks = Q8_Click.Count)

data_1 %<>% rename(explanation_firstclick = Q18_First.Click)
data_1 %<>% rename(explanation_lastclick = Q18_Last.Click)
data_1 %<>% rename(explanation_submit = Q18_Page.Submit)
data_1 %<>% rename(explanation_clicks = Q18_Click.Count)

data_1 %<>% rename(topic1Belief_firstclick = Q24_First.Click)
data_1 %<>% rename(topic1_belief_lastclick = Q24_Last.Click)
data_1 %<>% rename(topic1_belief_submit = Q24_Page.Submit)
data_1 %<>% rename(topic1_belief_clicks = Q24_Click.Count)

data_1 %<>% rename(truestory_belief_firstclick = Q27_First.Click)
data_1 %<>% rename(truestory_belief_lastclick = Q27_Last.Click)
data_1 %<>% rename(truestory_belief_submit = Q27_Page.Submit)
data_1 %<>% rename(truestory_belief_clicks = Q27_Click.Count)

data_1 %<>% rename(topic2_belief_firstclick = Q33_First.Click)
data_1 %<>% rename(topic2_belief_lastclick = Q33_Last.Click)
data_1 %<>% rename(topic2_belief_submit = Q33_Page.Submit)
data_1 %<>% rename(topic2_belief_clicks = Q33_Click.Count)

data_1 %<>% rename(topic3_belief_firstclick = Q39_First.Click)
data_1 %<>% rename(topic3_belief_lastclick = Q39_Last.Click)
data_1 %<>% rename(topic3_belief_submit = Q39_Page.Submit)
data_1 %<>% rename(topic3_belief_clicks = Q39_Click.Count)

data_1 %<>% rename(topic4_belief_firstclick = Q45_First.Click)
data_1 %<>% rename(topic4_belief_lastclick = Q45_Last.Click)
data_1 %<>% rename(topic4_belief_submit = Q45_Page.Submit)
data_1 %<>% rename(topic4_belief_clicks = Q45_Click.Count)

data_1 %<>% rename(leftright_interest_firstclick = Q48_First.Click)
data_1 %<>% rename(leftright_interest_lastclick = Q48_Last.Click)
data_1 %<>% rename(leftright_interest_submit = Q48_Page.Submit)
data_1 %<>% rename(leftright_interest_clicks = Q48_Click.Count)

data_1 %<>% rename(know_party_firstclick = Q50_First.Click)
data_1 %<>% rename(know_party_lastclick = Q50_Last.Click)
data_1 %<>% rename(know_party_submit = Q50_Page.Submit)
data_1 %<>% rename(know_party_clicks = Q50_Click.Count)

data_1 %<>% rename(know_job_firstclick = Q52_First.Click)
data_1 %<>% rename(know_job_lastclick = Q52_Last.Click)
data_1 %<>% rename(know_job_submit = Q52_Page.Submit)
data_1 %<>% rename(know_job_clicks = Q52_Click.Count)

data_1 %<>% rename(demographics2_firstclick = Q57_First.Click)
data_1 %<>% rename(demographics2_lastclick = Q57_Last.Click)
data_1 %<>% rename(demographics2_submit = Q57_Page.Submit)
data_1 %<>% rename(demographics2_clicks = Q57_Click.Count)

data_1 %<>% rename(media_frequency_firstclick = Q60_First.Click)
data_1 %<>% rename(media_frequency_lastclick = Q60_Last.Click)
data_1 %<>% rename(media_frequency_submit = Q60_Page.Submit)
data_1 %<>% rename(media_frequency_clicks = Q60_Click.Count)

data_1 %<>% rename(fb_firstclick = Q63_First.Click)
data_1 %<>% rename(fb_lastclick = Q63_Last.Click)
data_1 %<>% rename(fb_submit = Q63_Page.Submit)
data_1 %<>% rename(fb_clicks = Q63_Click.Count)

data_1 %<>% rename(turnout_voting_firstclick = Q84_First.Click)
data_1 %<>% rename(turnout_voting_lastclick = Q84_Last.Click)
data_1 %<>% rename(turnout_voting_submit = Q84_Page.Submit)
data_1 %<>% rename(turnout_voting_clicks = Q84_Click.Count)

data_1 %<>% rename(topic1_postattitudes_firstclick = Q68_First.Click)
data_1 %<>% rename(topic1_postattitudes_voting_lastclick = Q68_Last.Click)
data_1 %<>% rename(topic1_postattitudes_voting_submit = Q68_Page.Submit)
data_1 %<>% rename(topic1_postattitudes_voting_clicks = Q68_Click.Count)

data_1 %<>% rename(topic2_postattitudes_firstclick = Q70_First.Click)
data_1 %<>% rename(topic2_postattitudes_voting_lastclick = Q70_Last.Click)
data_1 %<>% rename(topic2_postattitudes_voting_submit = Q70_Page.Submit)
data_1 %<>% rename(topic2_postattitudes_voting_clicks = Q70_Click.Count)

data_1 %<>% rename(topic3_postattitudes_firstclick = Q72_First.Click)
data_1 %<>% rename(topic3_postattitudes_voting_lastclick = Q72_Last.Click)
data_1 %<>% rename(topic3_postattitudes_voting_submit = Q72_Page.Submit)
data_1 %<>% rename(topic3_postattitudes_voting_clicks = Q72_Click.Count)

data_1 %<>% rename(topic4_postattitudes_firstclick = Q74_First.Click)
data_1 %<>% rename(topic4_postattitudes_voting_lastclick = Q74_Last.Click)
data_1 %<>% rename(topic4_postattitudes_voting_submit = Q74_Page.Submit)
data_1 %<>% rename(topic4_postattitudes_voting_clicks = Q74_Click.Count)

data_1 %<>% rename(debriefing_firstclick = Q76_First.Click)
data_1 %<>% rename(debriefing_voting_lastclick = Q76_Last.Click)
data_1 %<>% rename(debriefing_voting_submit = Q76_Page.Submit)
data_1 %<>% rename(debriefing_voting_clicks = Q76_Click.Count)

### Display orders

# data_1 %<>% rename(trust_order = Q7_DO)
# data_1 %<>% rename(know_party_order = Q49_DO)
# data_1 %<>% rename(attitudes_order = FL_37_DO)
# data_1 %<>% rename(postattitudes_order = FL_38_DO)

