###########################################################
### Recoding Study 1 from "The Ocean of Possible Truth" ###
###########################################################

#### Remarks on general coding scheme ####
# "keine Angabe" turned into NA

### Delete rows related to variable names
data_2 <- data_2[-1:-2,]

#### Delete unused variables ####

data_2 %<>% select(-c(Status, Progress, Duration..in.seconds., Finished,
                      RecipientLastName, RecipientFirstName, RecipientEmail,
                      ExternalReference, DistributionChannel, UserLanguage))


#### Outcomes ####

### Outcome used

data_2 %<>% rename(share_report_2_email.desk = QID758_1)
data_2 %<>% rename(share_report_2_fb.desk = QID758_2)
data_2 %<>% rename(share_report_2_twitter.desk = QID758_3)
data_2 %<>% rename(share_report_2_whatsapp.desk = QID758_4)

data_2 %<>% rename(share_report_2_email.mob = QID759_1)
data_2 %<>% rename(share_report_2_fb.mob = QID759_2)
data_2 %<>% rename(share_report_2_twitter.mob = QID759_3)
data_2 %<>% rename(share_report_2_whatsapp.mob = QID759_4)

### Outcomes only used in Bauer & Clemm (2019)

data_2 %<>% rename(belief_report_1.desk = QID640)
data_2 %<>% rename(share_report_1_email.desk = QID641_1)
data_2 %<>% rename(share_report_1_fb.desk = QID641_2)
data_2 %<>% rename(share_report_1_twitter.desk = QID641_3)
data_2 %<>% rename(share_report_1_whatsapp.desk = QID641_4)

data_2 %<>% rename(belief_report_1.mob = QID738)
data_2 %<>% rename(share_report_1_email.mob = QID646_1)
data_2 %<>% rename(share_report_1_fb.mob = QID646_2)
data_2 %<>% rename(share_report_1_twitter.mob = QID646_3)
data_2 %<>% rename(share_report_1_whatsapp.mob = QID646_4)

data_2 %<>% rename(belief_report_5.desk = QID739)
data_2 %<>% rename(share_report_5_email.desk = QID650_1)
data_2 %<>% rename(share_report_5_fb.desk = QID650_2)
data_2 %<>% rename(share_report_5_twitter.desk = QID650_3)
data_2 %<>% rename(share_report_5_whatsapp.desk = QID650_4)

data_2 %<>% rename(belief_report_5.mob = QID740)
data_2 %<>% rename(share_report_5_email.mob = QID679_1)
data_2 %<>% rename(share_report_5_fb.mob = QID679_2)
data_2 %<>% rename(share_report_5_twitter.mob = QID679_3)
data_2 %<>% rename(share_report_5_whatsapp.mob = QID679_4)

data_2 %<>% rename(belief_report_5_prompt.desk = QID741)
data_2 %<>% rename(share_report_5_prompt_email.desk = QID698_1)
data_2 %<>% rename(share_report_5_prompt_fb.desk = QID698_2)
data_2 %<>% rename(share_report_5_prompt_twitter.desk = QID698_3)
data_2 %<>% rename(share_report_5_prompt_whatsapp.desk = QID698_4)

data_2 %<>% rename(belief_report_5_prompt.mob = QID779)
data_2 %<>% rename(share_report_5_prompt_email.mob = QID780_1)
data_2 %<>% rename(share_report_5_prompt_fb.mob = QID780_2)
data_2 %<>% rename(share_report_5_prompt_twitter.mob = QID780_3)
data_2 %<>% rename(share_report_5_prompt_whatsapp.mob = QID780_4)

data_2 %<>% rename(belief_report_5_prompt.bonly_desk = QID782)
data_2 %<>% rename(belief_report_5_prompt.bonly_mob = QID788)

data_2 %<>% rename(share_report_5_prompt_email.shonly_desk = QID786_1)
data_2 %<>% rename(share_report_5_prompt_fb.shonly_desk = QID786_2)
data_2 %<>% rename(share_report_5_prompt_twitter.shonly_desk = QID786_3)
data_2 %<>% rename(share_report_5_prompt_whatsapp.shonly_desk = QID786_4)

data_2 %<>% rename(share_report_5_prompt_email.shonly_mob = QID792_1)
data_2 %<>% rename(share_report_5_prompt_fb.shonly_mob = QID792_2)
data_2 %<>% rename(share_report_5_prompt_twitter.shonly_mob = QID792_3)
data_2 %<>% rename(share_report_5_prompt_whatsapp.shonly_mob = QID792_4)

### Unite respective desktop and mobile columns

data_2 <- data_2 %>% gather(Var, Val, -ResponseId, na.rm = FALSE) %>%
  separate(Var, into = c("Var1", "Var2"), sep = "\\.") %>%
  group_by(ResponseId, Var1) %>% 
  summarise(Val = paste(Val[!(is.na(Val)|Val=="")], collapse="_")) %>%
  spread(Var1, Val)

### Recode into numeric

data_2 %<>% mutate_at(vars(contains("share_report")),
                      list(~case_when(. == "Nein" ~ "0",
                                      . == "Ja" ~ "1",
                                      TRUE ~ as.character(.))))
data_2 %<>% mutate_at(vars(contains("share_report")),
                      list(~as.numeric(.)))

data_2 %<>% mutate_at(vars(contains("belief_report")),
                      list(~case_when(. == "0 - Gar nicht" ~ "0",
                                      . == "6 - Voll und ganz" ~ "6",
                                      TRUE ~ as.character(.))))
data_2 %<>% mutate_at(vars(contains("belief_report")),
                      list(~as.numeric(.)))

#### Covariates ####

### Consent

data_2 %<>% rename(consent = QID212) 

### Debrief

data_2 <- data_2 %>% rename(debrief_more_info = Q127_1)
data_2 <- data_2 %>% mutate(debrief_more_info_num = ifelse(is.na(debrief_more_info), 0, 1))

data_2 <- data_2 %>% rename(debrief_feedback = Q123)

data_2 <- data_2 %>% rename(debrief_confirm = QID213)
data_2 <- data_2 %>% mutate(debrief_confirm_num = ifelse(is.na(debrief_confirm), 0, 1))

### Age ###

data_2 %<>% rename(age = QID731_1)
data_2$age <- as.numeric(data_2$age)
data_2 %<>% mutate(age_group = case_when(age < 31 ~ "18-30ys",
                                         age >= 31 & age < 44 ~ "31-43ys",
                                         age >= 44 & age < 57 ~ "44-56ys",
                                         age >= 57 & age < 69 ~ "57-69ys",
                                         age >= 69 ~ "70+"))

### Sex ###

data_2 %<>% rename(sex = QID160) 
data_2$sex_fac <- as.factor(data_2$sex)
data_2 %<>% mutate(gender = case_when(sex == "maennlich" ~ 0,
                                      sex == "weiblich" ~ 1))

### Education ###

data_2 %<>% rename(education = QID220)
data_2 %<>% mutate(education = 
                     factor(education,
                            ordered = TRUE,
                            levels = c("Grundschule nicht beendet",
                                       "(Noch) kein Abschluss, aber Grundschule beendet",
                                       "Volks- oder Hauptschulabschluss (bzw. Polytechnische Oberschule der ehem. DDR mit Abschluss der 8. oder 9. Klasse)",
                                       "Mittlere Reife, Realschulabschluss, Fachoberschulreife, Mittlerer Schulabschluss (bzw. Polytechnische Oberschule der ehem. DDR mit Abschluss der 10. Klasse)",
                                       "Abitur, allgemeine oder fachgebundene Hochschulreife (bzw. Erweiterte Oberschule der ehem. DDR mit Abschluss 12. Klasse)", 
                                       "Fachhochschulreife (Abschluss einer Fachoberschule etc.)",
                                       "Fach-/Hochschulstudium")))
data_2 %<>% 
  mutate(education_6 = 
           case_when(education == "(Noch) kein Abschluss, aber Grundschule beendet" ~ "None or still in school",
                     education == "Grundschule nicht beendet"  ~ "None or still in school",
                     education == "Volks- oder Hauptschulabschluss (bzw. Polytechnische Oberschule der ehem. DDR mit Abschluss der 8. oder 9. Klasse)" ~  "Lower school (Volks-/Hauptschule)",
                     education == "Mittlere Reife, Realschulabschluss, Fachoberschulreife, Mittlerer Schulabschluss (bzw. Polytechnische Oberschule der ehem. DDR mit Abschluss der 10. Klasse)" ~  "Middle school (Realschule)",
                     education == "Fachhochschulreife (Abschluss einer Fachoberschule etc.)" ~ "Technical high school (Fachabitur)",
                     education == "Abitur, allgemeine oder fachgebundene Hochschulreife (bzw. Erweiterte Oberschule der ehem. DDR mit Abschluss 12. Klasse)" ~ "High school (Abitur)",
                     education == "Fach-/Hochschulstudium" ~  "University"))
data_2$education_6 <- factor(data_2$education_6, 
                             levels = c("None or still in school",
                                        "Lower school (Volks-/Hauptschule)",
                                        "Middle school (Realschule)",
                                        "Technical high school (Fachabitur)",
                                        "High school (Abitur)",
                                        "University"))

### Income

data_2 <- data_2 %>% rename(income = QID208)
data_2$income <- factor(data_2$income, 
                        ordered = TRUE, 
                        levels = c("Monatlich 0 bis 500 Euro", 
                                   "Monatlich 501 bis 1.000 Euro",
                                   "Monatlich 1.001 bis 1.500 Euro", 
                                   "Monatlich 1.501 bis 2.000 Euro",
                                   "Monatich 2.001 bis 2.500 Euro",
                                   "Monatlich 2.501 bis 3.000 Euro",
                                   "Monatlich 3.001 bis 3.500 Euro",
                                   "Monatlich 3.501 bis 4.000 Euro",
                                   "Monatlich 4.001 bis 4.500 Euro",
                                   "Monatlich 4.501 bis 5.000 Euro",
                                   "Monatlich 5.001 Euro oder mehr"))

### Federal state

data_2 %<>% rename(federal_state = QID222_1)
data_2$federal_state <- factor(data_2$federal_state)
data_2 %<>% mutate(federal_state = case_when(
  federal_state == "Baden-Wuerttemberg" ~ "Baden-Württemberg",
  federal_state == "Thueringen" ~ "Thüringen",
  TRUE ~ as.character(federal_state)))
data_2 %<>% mutate(federal_state_west = ifelse(federal_state == "Brandenburg" |
                                                 federal_state == "Mecklenburg-Vorpommern" |
                                                 federal_state == "Sachsen" |
                                                 federal_state == "Sachsen-Anhalt" |
                                                 federal_state == "Thüringen", 0, 1))

### Turnout

data_2 %<>% rename(turnout = QID221)
data_2$turnout <- factor(data_2$turnout)
data_2 %<>% mutate(turnout_binary = 
                     case_when(turnout == "Ja" ~ 1,
                               turnout == "Nein" ~ 0))

### Vote choice

data_2 %<>% rename(vote_choice = QID219)
data_2 %<>% mutate(vote_choice_6 = ifelse(vote_choice == "" |
                                            vote_choice == "Weiss ich nicht" |
                                            vote_choice == "Eine andere Partei", 
                                          "No vote/other party", vote_choice))
data_2$vote_choice <- factor(data_2$vote_choice)
data_2$vote_choice_6 <- factor(data_2$vote_choice_6, 
                               ordered = TRUE,
                               levels = c("Die Linke", "SPD", "Buendnis 90/Die Gruenen",
                                          "FDP", "CDU/CSU", "AfD", "No vote/other party"))
data_2 %<>% rename(vote_hyp_1 = QID754)
data_2$vote_hyp_1 <- factor(data_2$vote_hyp_1)
data_2 %<>% rename(vote_hyp_2 = QID756)
data_2$vote_hyp_2 <- factor(data_2$vote_hyp_2)

### Political knowledge 

data_2 %<>% rename(knowpol_merkel = QID721_1)
data_2 %<>% rename(knowpol_altmaier = Q131_1)
data_2 %<>% rename(knowpol_schulz = Q139_1)
data_2 %<>% rename(knowpol_maas = Q135_1)
data_2 %<>% rename(knowpol_lindner = Q137_1)
data_2 %<>% rename(knowpol_hofreiter = Q141_1)
data_2 %<>% rename(knowpol_goering = Q143_1)
data_2 %<>% rename(knowpol_bartsch = Q145_1)
data_2 %<>% rename(knowpol_weidel = Q147_1)

data_2 %<>% ungroup() %>% 
  mutate_at(vars(contains("knowpol_")), as.factor)

data_2 %<>% mutate(knowpol_merkel_correct = ifelse(knowpol_merkel == "CDU", 1, 0))
data_2 %<>% mutate(knowpol_altmaier_correct = ifelse(knowpol_altmaier == "CDU", 1, 0))
data_2 %<>% mutate(knowpol_schulz_correct = ifelse(knowpol_schulz == "SPD", 1, 0))
data_2 %<>% mutate(knowpol_maas_correct = ifelse(knowpol_maas == "SPD", 1, 0))
data_2 %<>% mutate(knowpol_lindner_correct = ifelse(knowpol_lindner == "FDP", 1, 0))
data_2 %<>% mutate(knowpol_hofreiter_correct = ifelse(knowpol_hofreiter == "Die Gruenen", 1, 0))
data_2 %<>% mutate(knowpol_goering_correct = ifelse(knowpol_goering == "Die Gruenen", 1, 0))
data_2 %<>% mutate(knowpol_bartsch_correct = ifelse(knowpol_bartsch == "Die Linke", 1, 0))
data_2 %<>% mutate(knowpol_weidel_correct = ifelse(knowpol_weidel == "AfD", 1, 0))

data_2 %<>% ungroup() %>%
  mutate(knowpol_total = select(., contains("_correct")) %>% 
           rowSums(na.rm = TRUE))

### Overconfidence

data_2 %<>% rename(confidence = QID734)
data_2$confidence <- as.numeric(data_2$confidence)
data_2$overconfidence <- (data_2$confidence - data_2$knowpol_total)

### Recognize news source

data_2 %<>% rename(knowsource_tagesschau = QID189_1)
data_2 %<>% rename(knowsource_heute = QID189_2)
data_2 %<>% rename(knowsource_sz = QID189_3)
data_2 %<>% rename(knowsource_faz = QID189_4)
data_2 %<>% rename(knowsource_focus = QID189_5)
data_2 %<>% rename(knowsource_bild = QID189_6)
data_2 %<>% rename(knowsource_nachrichten360 = QID189_7)
data_2 %<>% rename(knowsource_berliner = QID189_8)
data_2 %<>% rename(knowsource_spiegel = QID189_9)
data_2 %<>% rename(knowsource_rtdeutsch = QID189_10)
data_2 %<>% rename(knowsource_newsblitz = QID189_11)

data_2 %<>% mutate_at(vars(contains("knowsource_")), as.factor)

### Read/watched news source

data_2 %<>% rename(readsource_tagesschau = QID712_1)
data_2 %<>% rename(readsource_heute = QID712_2)
data_2 %<>% rename(readsource_sz = QID712_3)
data_2 %<>% rename(readsource_faz = QID712_4)
data_2 %<>% rename(readsource_focus = QID712_5)
data_2 %<>% rename(readsource_bild = QID712_6)
data_2 %<>% rename(readsource_nachrichten360 = QID712_7)
data_2 %<>% rename(readsource_berliner = QID712_8)
data_2 %<>% rename(readsource_spiegel = QID712_9)
data_2 %<>% rename(readsource_rtdeutsch = QID712_10)
data_2 %<>% rename(readsource_newsblitz = QID712_11)

data_2 %<>% mutate_at(vars(contains("readsource_")), as.factor)

### Trust source

data_2 %<>% rename(trustsource_tagesschau = Q126)
data_2 %<>% rename(trustsource_heute = Q128)
data_2 %<>% rename(trustsource_sz = Q129)
data_2 %<>% rename(trustsource_faz = Q130)
data_2 %<>% rename(trustsource_focus = Q131)
data_2 %<>% rename(trustsource_bild = Q132)
data_2 %<>% rename(trustsource_nachrichten360 = Q133)
data_2 %<>% rename(trustsource_berliner = Q134)
data_2 %<>% rename(trustsource_spiegel = Q135)
data_2 %<>% rename(trustsource_rtdeutsch = Q136)
data_2 %<>% rename(trustsource_newsblitz = Q137)

data_2 %<>% mutate_at(vars(contains("trustsource_")), 
                      list(~factor(., ordered = TRUE,
                                   levels = c("Ueberhaupt nicht",
                                              "Eher nicht",
                                              "Teils/teils",
                                              "Eher",
                                              "Voll und ganz"))))

data_2 %<>% mutate_at(vars(contains("trustsource_")), as.numeric)

mainstream_sources_2 = c("trustsource_tagesschau",
                         "trustsource_heute",
                         "trustsource_sz",
                         "trustsource_faz",
                         "trustsource_spiegel")

## Mainstream source trust index 

data_2 %<>% ungroup() %>%
  mutate(trustsource_mainstream =
           select(., mainstream_sources_2) %>%
           rowMeans(na.rm = TRUE))

### Account for email/FB/Twitter/Whatsapp

data_2 %<>% rename(account_email = QID634)
data_2 %<>% rename(account_fb = QID635)
data_2 %<>% rename(account_twitter = QID637)
data_2 %<>% rename(account_whatsapp = QID636)

data_2 %<>% mutate_at(vars(contains("account_")), as.factor)

## Variable for having any account

data_2 %<>% mutate(account = ifelse(account_email == "Ja" |
                                      account_fb == "Ja" |
                                      account_twitter == "Ja" |
                                      account_whatsapp == "Ja", 1, 0))

### Sharing frequency via email/FB/Twitter/Whatsapp

data_2 %<>% rename(sharing_email = QID744)
data_2 %<>% rename(sharing_fb = QID745)
data_2 %<>% rename(sharing_twitter = QID746)
data_2 %<>% rename(sharing_whatsapp = QID747)

data_2 %<>% mutate_at(vars(contains("sharing_")), 
                      list(~factor(., ordered = TRUE,
                                   levels = c("Seltener",
                                              "Ein paar Mal im Jahr",
                                              "Ein paar Mal im Monat",
                                              "Einmal pro Woche",
                                              "Taeglich"))))

data_2 %<>% mutate_at(vars(contains("sharing_")), as.numeric)

### HTML knowledge

data_2 <- data_2 %>% rename(know_html = QID295)
data_2$know_html_fac <- factor(data_2$know_html)
data_2 <- data_2 %>% mutate(know_html_correct = ifelse(know_html == "HTML", 1, 0))

#### Attitudes ####

data_2 %<>% rename(immigrant_attitude_culture = QID183)
data_2 %<>% rename(immigrant_attitude_economy = QID184)
data_2 %<>% rename(immigrant_attitude_security = QID185)
data_2 %<>% rename(immigrant_attitude_life = QID186)
data_2 %<>% rename(immigrant_attitude_border = QID218)

data_2 %<>% 
  ungroup() %>%
  mutate_at(vars(contains("immigrant_attitude_")), 
            list(~case_when(grepl("^0\\s+",.) ~ "0",
                            grepl("^10\\s+",.) ~ "10",
                            TRUE ~ as.character(.))))

### Function for turning into numeric and recoding
# Recode so that 0 is pro-migration, 10 is anti-migration like in Study 1

numrec <- function(attitude) {
  attitude_rec <- as.numeric(attitude)
  attitude_rec <- attitude_rec*(-1) + 10
  return(attitude_rec)
}

data_2 %<>% 
  ungroup() %>%
  mutate_at(vars(contains("immigrant_attitude_")), numrec)

data_2 %<>% mutate(immigrant_attitude_culture = immigrant_attitude_culture - 5)
data_2 %<>% mutate(immigrant_attitude_economy = immigrant_attitude_economy - 5)
data_2 %<>% mutate(immigrant_attitude_security = immigrant_attitude_security - 5)
data_2 %<>% mutate(immigrant_attitude_life = immigrant_attitude_life - 5)
data_2 %<>% mutate(immigrant_attitude_border = immigrant_attitude_border - 5)

### Scale reduction

## Cronbach's alpha

data_2 %>% 
  ungroup() %>%
  select(contains("immigrant_attitude_")) %>% psych::alpha()

## Additive index

data_2 %<>% 
  ungroup() %>%
  mutate(immigrant_attitudes_average = 
           select(., contains("immigrant_attitude_")) %>%
           rowMeans(na.rm = TRUE))

## Principle factor analysis

immigrant_factor_analysis <- data_2 %>% 
  ungroup() %>% 
  select(matches("immigrant_attitude_")) %>% 
  fa(., nfactors = 1, rotate = "none", fm = "pa", max.iter = 1)

data_2$immigrant_attitudes_factor <- immigrant_factor_analysis$scores[, 1]

#### Treatments ####

### Raw treatments

data_2 %<>% rename(treatment_a = FL_358_DO_FL_359)
data_2 %<>% rename(treatment_b = FL_358_DO_FL_384)
data_2 %<>% rename(treatment_c = FL_358_DO_FL_419)
data_2 %<>% rename(treatment_d = FL_358_DO_FL_454)
data_2 %<>% rename(treatment_e = FL_358_DO_FL_489)
data_2 %<>% rename(treatment_f = FL_358_DO_FL_524)
data_2 %<>% rename(treatment_g = FL_358_DO_FL_559)
data_2 %<>% rename(treatment_h = FL_358_DO_FL_594)

### Source treatment variable: 0 is "Nachrichten 360", 1 is "Tagesschau"

data_2 %<>% mutate(treatment_source = 
                     case_when(treatment_e == "1" |
                                 treatment_f == "1" |
                                 treatment_g == "1" |
                                 treatment_h == "1" ~ 0,
                               treatment_a == "1" |
                                 treatment_b == "1" |
                                 treatment_c == "1" |
                                 treatment_d == "1" ~ 1))

### Platform treatment: 0 is Facebook, 1 is website 

data_2 %<>% mutate(treatment_channel = 
                     case_when(treatment_c == "1" |
                                 treatment_d == "1" |
                                 treatment_g == "1" |
                                 treatment_h == "1" ~ 0,
                               treatment_a == "1" |
                                 treatment_b == "1" |
                                 treatment_e == "1" |
                                 treatment_f == "1" ~ 1))

## Content treatment: 0 is pro-migration content, 1 is anti-migration content

data_2 %<>% mutate(treatment_content = 
                     case_when(treatment_a == "1" |
                                 treatment_c == "1" |
                                 treatment_e == "1" |
                                 treatment_g == "1" ~ 0,
                               treatment_b == "1" |
                                 treatment_d == "1" |
                                 treatment_f == "1" |
                                 treatment_h == "1" ~ 1))

#### Manipulation checks ####
# NA coded as 0 (i.e. check not answered correctly)

### Content manipulation

## Report 2 (0 is pro-migration content)

data_2 %<>% rename(report_2_check = QID736)

data_2 %<>% mutate(report_2_check_correct = 
                     ifelse(treatment_content == 0,
                            ifelse(report_2_check == "Zuwanderer sind in Deutschland unter Tatverdaechtigen nicht ueberrepraesentiert.", 1, 0), 
                            ifelse(report_2_check == "Zuwanderer sind in Deutschland unter Tatverdaechtigen ueberrepraesentiert.", 1, 0)))

## Report 3 (0 is pro-migration content)

data_2 %<>% rename(report_3_check = QID353)

data_2 %<>% mutate(report_3_check_correct = 
                     ifelse(treatment_content == 0,
                            ifelse(report_3_check == "Die Mehrheit der Fluechtinge schafft den Deutschtest.", 1, 0), 
                            ifelse(report_3_check == "Die Mehrheit der Fluechtinge schafft den Deutschtest nicht.", 1, 0)))

## Report 4 (0 is pro-migration content)

data_2 %<>% rename(report_4_check = QID506)

data_2 %<>% mutate(report_4_check_correct = 
                     ifelse(treatment_content == 0,
                            ifelse(report_4_check == "Private Seenotrettung im Mittelmeer erzeugt keine Sogwirkung.", 1, 0), 
                            ifelse(report_4_check == "Private Seenotrettung im Mittelmeer erzeugt eine Sogwirkung.", 1, 0)))

## Source (0 is N360)

data_2 %<>% rename(source_check = Q140)

data_2 %<>% mutate(source_check_correct = 
                     ifelse(treatment_source == 1,
                            ifelse(source_check == "Tagesschau", 1, 0), 
                            ifelse(source_check == "Nachrichten 360", 1, 0)))

### Attention across survey

data_2 %<>% ungroup() %>%
  mutate(attention_correct_total = 
           select(., c("source_check_correct", 
                       "report_2_check_correct", 
                       "report_3_check_correct", 
                       "report_4_check_correct")) %>% rowSums(na.rm = TRUE))

#### Timing variables ####

### Time of survey

data_2$StartDate <- as.POSIXct(data_2$StartDate)

### Duration

data_2 %<>% mutate(duration = as.numeric(Q_TotalDuration))

### Timing and click count per page

data_2 %<>% rename(consent_firstclick = `QID318_First`)
data_2 %<>% rename(consent_lastclick = `QID318_Last`)
data_2 %<>% rename(consent_submit = `QID318_Page`)
data_2 %<>% rename(consent_clickcount = `QID318_Click`)

data_2 %<>% rename(gender_age_state_firstclick = `QID162_First`)
data_2 %<>% rename(gender_age_state_lastclick = `QID162_Last`)
data_2 %<>% rename(gender_age_state_submit = `QID162_Page`)
data_2 %<>% rename(gender_age_state_clickcount = `QID162_Click`)

data_2 %<>% rename(know_firstclick = `Q126_First`)
data_2 %<>% rename(know_lastclick = `Q126_Last`)
data_2 %<>% rename(know_submit = `Q126_Page`)
data_2 %<>% rename(know_clickcount = `Q126_Click`)

data_2 %<>% rename(read_firstclick = `QID319_First`)
data_2 %<>% rename(read_lastclick = `QID319_Last`)
data_2 %<>% rename(read_submit = `QID319_Page`)
data_2 %<>% rename(read_clickcount = `QID319_Click`)

data_2 %<>% rename(trust_firstclick = `QID633_First`)
data_2 %<>% rename(trust_lastclick = `QID633_Last`)
data_2 %<>% rename(trust_submit = `QID633_Page`)
data_2 %<>% rename(trust_clickcount = `QID633_Click`)

data_2 %<>% rename(attitudes_firstclick = `QID322_First`)
data_2 %<>% rename(attitudes_lastclick = `QID322_Last`)
data_2 %<>% rename(attitudes_submit = `QID322_Page`)
data_2 %<>% rename(attitudes_clickcount = `QID322_Click`)

data_2 %<>% rename(platforms_firstclick = `QID685_First`)
data_2 %<>% rename(platforms_lastclick = `QID685_Last`)
data_2 %<>% rename(platforms_submit = `QID685_Page`)
data_2 %<>% rename(platforms_clickcount = `QID685_Click`)

data_2 %<>% rename(sharing_firstclick = `QID720_First`)
data_2 %<>% rename(sharing_lastclick = `QID720_Last`)
data_2 %<>% rename(sharing_submit = `QID720_Page`)
data_2 %<>% rename(sharing_clickcount = `QID720_Click`)

data_2 %<>% rename(knowledge_firstclick = `QID760_First`)
data_2 %<>% rename(knowledge_lastclick = `QID760_Last`)
data_2 %<>% rename(knowledge_submit = `QID760_Page`)
data_2 %<>% rename(knowledge_clickcount = `QID760_Click`)

data_2 %<>% rename(estimate_correct_firstclick = `Q127_First`)
data_2 %<>% rename(estimate_correct_lastclick = `Q127_Last`)
data_2 %<>% rename(estimate_correct_submit = `Q127_Page`)
data_2 %<>% rename(estimate_correct_clickcount = `Q127_Click`)

data_2 %<>% rename(introduction_firstclick = `QID334_First`)
data_2 %<>% rename(introduction_lastclick = `QID334_Last`)
data_2 %<>% rename(introduction_submit = `QID334_Page`)
data_2 %<>% rename(introduction_clickcount = `QID334_Click`)

data_2 %<>% rename(report_1_desk_firstclick = `QID687_First`)
data_2 %<>% rename(report_1_desk_lastclick = `QID687_Last`)
data_2 %<>% rename(report_1_desk_submit = `QID687_Page`)
data_2 %<>% rename(report_1_desk_clickcount = `QID687_Click`)

data_2 %<>% rename(report_1_mob_firstclick = `QID688_First`)
data_2 %<>% rename(report_1_mob_lastclick = `QID688_Last`)
data_2 %<>% rename(report_1_mob_submit = `QID688_Page`)
data_2 %<>% rename(report_1_mob_clickcount = `QID688_Click`)

data_2 %<>% rename(report_2_desk_firstclick = `QID686_First`)
data_2 %<>% rename(report_2_desk_lastclick = `QID686_Last`)
data_2 %<>% rename(report_2_desk_submit = `QID686_Page`)
data_2 %<>% rename(report_2_desk_clickcount = `QID686_Click`)

data_2 %<>% rename(report_2_mob_firstclick = `QID689_First`)
data_2 %<>% rename(report_2_mob_lastclick = `QID689_Last`)
data_2 %<>% rename(report_2_mob_submit = `QID689_Page`)
data_2 %<>% rename(report_2_mob_clickcount = `QID689_Click`)

data_2 %<>% rename(report_2_check_firstclick = `QID516_First`)
data_2 %<>% rename(report_2_check_lastclick = `QID516_Last`)
data_2 %<>% rename(report_2_check_submit = `QID516_Page`)
data_2 %<>% rename(report_2_check_clickcount = `QID516_Click`)

data_2 %<>% rename(report_3_desk_firstclick = `QID690_First`)
data_2 %<>% rename(report_3_desk_lastclick = `QID690_Last`)
data_2 %<>% rename(report_3_desk_submit = `QID690_Page`)
data_2 %<>% rename(report_3_desk_clickcount = `QID690_Click`)

data_2 %<>% rename(report_3_mob_firstclick = `QID691_First`)
data_2 %<>% rename(report_3_mob_lastclick = `QID691_Last`)
data_2 %<>% rename(report_3_mob_submit = `QID691_Page`)
data_2 %<>% rename(report_3_mob_clickcount = `QID691_Click`)

data_2 %<>% rename(report_3_check_firstclick = `QID692_First`)
data_2 %<>% rename(report_3_check_lastclick = `QID692_Last`)
data_2 %<>% rename(report_3_check_submit = `QID692_Page`)
data_2 %<>% rename(report_3_check_clickcount = `QID692_Click`)

data_2 %<>% rename(report_4_desk_firstclick = `QID693_First`)
data_2 %<>% rename(report_4_desk_lastclick = `QID693_Last`)
data_2 %<>% rename(report_4_desk_submit = `QID693_Page`)
data_2 %<>% rename(report_4_desk_clickcount = `QID693_Click`)

data_2 %<>% rename(report_4_mob_firstclick = `QID694_First`)
data_2 %<>% rename(report_4_mob_lastclick = `QID694_Last`)
data_2 %<>% rename(report_4_mob_submit = `QID694_Page`)
data_2 %<>% rename(report_4_mob_clickcount = `QID694_Click`)

data_2 %<>% rename(report_4_check_firstclick = `QID510_First`)
data_2 %<>% rename(report_4_check_lastclick = `QID510_Last`)
data_2 %<>% rename(report_4_check_submit = `QID510_Page`)
data_2 %<>% rename(report_4_check_clickcount = `QID510_Click`)

data_2 %<>% rename(meas2_desk_firstclick = `QID705_First`)
data_2 %<>% rename(meas2_desk_lastclick = `QID705_Last`)
data_2 %<>% rename(meas2_desk_submit = `QID705_Page`)
data_2 %<>% rename(meas2_desk_clickcount = `QID705_Click`)

data_2 %<>% rename(meas2_mob_firstclick = `QID706_First`)
data_2 %<>% rename(meas2_mob_lastclick = `QID706_Last`)
data_2 %<>% rename(meas2_mob_submit = `QID706_Page`)
data_2 %<>% rename(meas2_mob_clickcount = `QID706_Click`)

data_2 %<>% rename(prompt_full_desk_firstclick = `QID507_First`)
data_2 %<>% rename(prompt_full_desk_lastclick = `QID507_Last`)
data_2 %<>% rename(prompt_full_desk_submit = `QID507_Page`)
data_2 %<>% rename(prompt_full_desk_clickcount = `QID507_Click`)

data_2 %<>% rename(prompt_full_mob_firstclick = `QID781_First`)
data_2 %<>% rename(prompt_full_mob_lastclick = `QID781_Last`)
data_2 %<>% rename(prompt_full_mob_submit = `QID781_Page`)
data_2 %<>% rename(prompt_full_mob_clickcount = `QID781_Click`)

data_2 %<>% rename(prompt_bonly_desk_firstclick = `QID784_First`)
data_2 %<>% rename(prompt_bonly_desk_lastclick = `QID784_Last`)
data_2 %<>% rename(prompt_bonly_desk_submit = `QID784_Page`)
data_2 %<>% rename(prompt_bonly_desk_clickcount = `QID784_Click`)

data_2 %<>% rename(prompt_bonly_mob_firstclick = `QID790_First`)
data_2 %<>% rename(prompt_bonly_mob_lastclick = `QID790_Last`)
data_2 %<>% rename(prompt_bonly_mob_submit = `QID790_Page`)
data_2 %<>% rename(prompt_bonly_mob_clickcount = `QID790_Click`)

data_2 %<>% rename(prompt_shonly_desk_firstclick = `QID787_First`)
data_2 %<>% rename(prompt_shonly_desk_lastclick = `QID787_Last`)
data_2 %<>% rename(prompt_shonly_desk_submit = `QID787_Page`)
data_2 %<>% rename(prompt_shonly_desk_clickcount = `QID787_Click`)

data_2 %<>% rename(prompt_shonly_mob_firstclick = `QID793_First`)
data_2 %<>% rename(prompt_shonly_mob_lastclick = `QID793_Last`)
data_2 %<>% rename(prompt_shonly_mob_submit = `QID793_Page`)
data_2 %<>% rename(prompt_shonly_mob_clickcount = `QID793_Click`)

data_2 %<>% rename(education_turnout_firstclick = `QID330_First`)
data_2 %<>% rename(education_turnout_lastclick = `QID330_Last`)
data_2 %<>% rename(education_turnout_submit = `QID330_Page`)
data_2 %<>% rename(education_turnout_clickcount = `QID330_Click`)

data_2 %<>% rename(vote_choice_firstclick = `QID707_First`)
data_2 %<>% rename(vote_choice_lastclick = `QID707_Last`)
data_2 %<>% rename(vote_choice_submit = `QID707_Page`)
data_2 %<>% rename(vote_choice_clickcount = `QID707_Click`)

data_2 %<>% rename(vote_hyp_1_firstclick = `QID755_First`)
data_2 %<>% rename(vote_hyp_1_lastclick = `QID755_Last`)
data_2 %<>% rename(vote_hyp_1_submit = `QID755_Page`)
data_2 %<>% rename(vote_hyp_1_clickcount = `QID755_Click`)

data_2 %<>% rename(vote_hyp_2_firstclick = `QID757_First`)
data_2 %<>% rename(vote_hyp_2_lastclick = `QID757_Last`)
data_2 %<>% rename(vote_hyp_2_submit = `QID757_Page`)
data_2 %<>% rename(vote_hyp_2_clickcount = `QID757_Click`)

data_2 %<>% rename(income_html_firstclick = `QID333_First`)
data_2 %<>% rename(income_html_lastclick = `QID333_Last`)
data_2 %<>% rename(income_html_submit = `QID333_Page`)
data_2 %<>% rename(income_html_clickcount = `QID333_Click`)

data_2 %<>% rename(debrief_firstclick = `QID331_First`)
data_2 %<>% rename(debrief_lastclick = `QID331_Last`)
data_2 %<>% rename(debrief_submit = `QID331_Page`)
data_2 %<>% rename(debrief_clickcount = `QID331_Click`)

#### Randomizers ####

### Recognize source - number between 1 and 11
data_2 %<>% rename(know_tagesschau_pos = `QID189_DO_1`)
data_2 %<>% rename(know_heute_pos = `QID189_DO_2`)
data_2 %<>% rename(know_sz_pos = `QID189_DO_3`)
data_2 %<>% rename(know_faz_pos = `QID189_DO_4`)
data_2 %<>% rename(know_focus_pos = `QID189_DO_5`)
data_2 %<>% rename(know_bild_pos = `QID189_DO_6`)
data_2 %<>% rename(know_nachrichten360_pos = `QID189_DO_7`)
data_2 %<>% rename(know_berliner_pos = `QID189_DO_8`)
data_2 %<>% rename(know_spiegel_pos = `QID189_DO_9`)
data_2 %<>% rename(know_rtdeutsch_pos = `QID189_DO_10`)
data_2 %<>% rename(know_newsblitz_pos = `QID189_DO_11`)

### Read source - number between 1 and 11
data_2 %<>% rename(read_tagesschau_pos = `QID712_DO_1`)
data_2 %<>% rename(read_heute_pos = `QID712_DO_2`)
data_2 %<>% rename(read_sz_pos = `QID712_DO_3`)
data_2 %<>% rename(read_faz_pos = `QID712_DO_4`)
data_2 %<>% rename(read_focus_pos = `QID712_DO_5`)
data_2 %<>% rename(read_bild_pos = `QID712_DO_6`)
data_2 %<>% rename(read_nachrichten360_pos = `QID712_DO_7`)
data_2 %<>% rename(read_berliner_pos = `QID712_DO_8`)
data_2 %<>% rename(read_spiegel_pos = `QID712_DO_9`)
data_2 %<>% rename(read_rtdeutsch_pos = `QID712_DO_10`)
data_2 %<>% rename(read_newsblitz_pos = `QID712_DO_11`)

### Trust source - number between 2 and 12
data_2 %<>% rename(trust_tagesschau_pos = `media_trust_DO_Q126`)
data_2 %<>% rename(trust_heute_pos = `media_trust_DO_Q128`)
data_2 %<>% rename(trust_sz_pos = `media_trust_DO_Q129`)
data_2 %<>% rename(trust_faz_pos = `media_trust_DO_Q130`)
data_2 %<>% rename(trust_focus_pos = `media_trust_DO_Q131`)
data_2 %<>% rename(trust_bild_pos = `media_trust_DO_Q132`)
data_2 %<>% rename(trust_nachrichten360_pos = `media_trust_DO_Q133`)
data_2 %<>% rename(trust_berliner_pos = `media_trust_DO_Q134`)
data_2 %<>% rename(trust_spiegel_pos = `media_trust_DO_Q135`)
data_2 %<>% rename(trust_rtdeutsch_pos = `media_trust_DO_Q136`)
data_2 %<>% rename(trust_newsblitz_pos = `media_trust_DO_Q137`)

# table(data$media_trust_DO_QID633, exclude = NULL)
# table(data$media_trust_DO_QID684, exclude = NULL) # media_trust_DO_QID684 and media_trust_DO_QID633 is always "1"/"13" and thus uninformative
data_2 %<>% select (-c(media_trust_DO_QID633, media_trust_DO_QID684))

### Attitudes - number between 1 and 6
data_2 %<>% rename(immigrant_culture_pos = `attitudes_DO_QID183`)
data_2 %<>% rename(immigrant_economy_pos = `attitudes_DO_QID184`)
data_2 %<>% rename(immigrant_security_pos = `attitudes_DO_QID185`)
data_2 %<>% rename(immigrant_life_pos = `attitudes_DO_QID186`)
data_2 %<>% rename(immigrant_border_pos = `attitudes_DO_QID218`)

# table(data$attitudes_DO_QID322, exclude = NULL) # attitudes_DO_QID322 is always "6" and thus uninformative
data_2 %<>% select (-c(attitudes_DO_QID322))

## Political knowledge - number between 2 and 10
# For some reason, Qualtrics named explanation and Schulz question the same (Q139)
# Hence, no information about position of Schulz
data_2 %<>% rename(know_merkel_pos = `politicalknowledge_DO_QID721`)
data_2 %<>% rename(know_altmaier_pos = `politicalknowledge_DO_Q131`)
data_2 %<>% rename(know_maas_pos = `politicalknowledge_DO_Q135`)
data_2 %<>% rename(know_lindner_pos = `politicalknowledge_DO_Q137`)
data_2 %<>% rename(know_hofreiter_pos = `politicalknowledge_DO_Q141`)
data_2 %<>% rename(know_goering_pos = `politicalknowledge_DO_Q143`)
data_2 %<>% rename(know_bartsch_pos = `politicalknowledge_DO_Q145`)
data_2 %<>% rename(know_weidel_pos = `politicalknowledge_DO_Q147`)

# table(data$politicalknowledge_DO_Q139, exclude = NULL)
# table(data$politicalknowledge_DO_QID760, exclude = NULL) # politicalknowledge_DO_Q139/politicalknowledge_DO_QID760 are always "11" and thus uninformative
data_2 %<>% select (-c(politicalknowledge_DO_Q139, politicalknowledge_DO_QID760))

### Building 1 manipulation check
data_2 %<>% rename(report_2_check1_pos = `QID736_DO_1`)
data_2 %<>% rename(report_2_check2_pos = `QID736_DO_2`)
data_2 %<>% rename(report_2_check3_pos = `QID736_DO_4`)

### Building 2 manipulation check
data_2 %<>% rename(report_3_check1_pos = `QID353_DO_1`)
data_2 %<>% rename(report_3_check2_pos = `QID353_DO_2`)
data_2 %<>% rename(report_3_check3_pos = `QID353_DO_4`)

### Building 3 manipulation check
data_2 %<>% rename(report_4_check1_pos = `QID506_DO_1`)
data_2 %<>% rename(report_4_check2_pos = `QID506_DO_2`)
data_2 %<>% rename(report_4_check3_pos = `QID506_DO_4`)

### Source manipulation check
data_2 %<>% rename(source_check1_pos = `Q140_DO_1`)
data_2 %<>% rename(source_check2_pos = `Q140_DO_2`)
data_2 %<>% rename(source_check3_pos = `Q140_DO_3`)
data_2 %<>% rename(source_check4_pos = `Q140_DO_4`)
data_2 %<>% rename(source_check5_pos = `Q140_DO_5`)
data_2 %<>% rename(source_check6_pos = `Q140_DO_6`)

### HTML Knowledge
data_2 %<>% rename(know_html1_pos = `QID295_DO_6`)
data_2 %<>% rename(know_html2_pos = `QID295_DO_7`)
data_2 %<>% rename(know_html3_pos = `QID295_DO_8`)
data_2 %<>% rename(know_html4_pos = `QID295_DO_9`)
data_2 %<>% rename(know_html5_pos = `QID295_DO_10`)
data_2 %<>% rename(know_html6_pos = `QID295_DO_11`)
data_2 %<>% rename(know_html7_pos = `QID295_DO_12`)
data_2 %<>% rename(know_html8_pos = `QID295_DO_13`)

#### Focus data ####

### Get all question IDs that appear 

page_ids <- unique(unlist(str_extract_all(data_2$FocusData, "QID[0-9][0-9][0-9]")))

# This yields 43 QIDs; given questionnaire, it should be 44; "QID792" does not appear, because this configuration (prompt share only mobile) never materialized:
# data_2$share_report_5_total <- data_2$share_report_5_email_num + data_2$share_report_5_fb_num + data_2$share_report_5_twitter_num + data_2$share_report_5_whatsapp_num
# data_2[is.na(data_2$report_5_belief) & data_2$share_report_5_total > 0, ]

### Extract one variable for each question ID

data_2$FocusData <- str_replace_all(data_2$FocusData, "QID","--QID")
for(i in page_ids){
  varname <- i
  data_2 <- data_2 %>% mutate(!!varname := NA) %>% 
    mutate(!!varname :=  gsub("QID[0-9]{3}: |QID[0-9]{3}:", "",
                              gsub("--Q.*","",
                                   str_extract(FocusData, 
                                               paste(varname, ":\\sD.*;  ", sep = "")))))
}

### Rename variables and unite variables per questionnaire page if necessary

data_2 %<>% rename(consent_focus_raw = QID212)
data_2 %<>% rename(gender_age_state_focus_raw = QID160)
data_2 %<>% rename(know_focus_raw = QID189)
data_2 %<>% rename(read_focus_raw = QID712)
data_2 %<>% rename(trust_focus_raw = QID684)
data_2 %<>% unite("attitudes_focus_raw", 
                  c("QID186", "QID218", "QID184", "QID183", "QID185"))
data_2 %<>% mutate(attitudes_focus_raw = gsub("NA_|_NA", "", attitudes_focus_raw))
data_2 %<>% rename(services_focus_raw = QID634)
data_2 %<>% unite("sharing_focus_raw", 
                  c("QID744", "QID747", "QID745", "QID746"))
data_2 %<>% mutate(sharing_focus_raw = gsub("NA_|_NA", "", sharing_focus_raw))
data_2 %<>% rename(knowledge_focus_raw = QID808)
data_2 %<>% rename(estimate_correct_focus_raw = QID734)
data_2 %<>% rename(introduction_focus_raw = QID299)
data_2 %<>% unite("report_1_focus_raw", 
                  c("QID638", "QID643"))
data_2 %<>% mutate(report_1_focus_raw = gsub("NA_|_NA", "", report_1_focus_raw))
data_2 %<>% unite("report_2_focus_raw", 
                  c("QID659", "QID668"))
data_2 %<>% mutate(report_2_focus_raw = gsub("NA_|_NA", "", report_2_focus_raw))
data_2 %<>% rename(report_2_check_focus_raw = QID736)
data_2 %<>% unite("report_3_focus_raw", 
                  c("QID664", "QID672"))
data_2 %<>% mutate(report_3_focus_raw = gsub("NA_|_NA", "", report_3_focus_raw))
data_2 %<>% rename(report_3_check_focus_raw = QID353)
data_2 %<>% unite("report_4_focus_raw", 
                  c("QID666", "QID674"))
data_2 %<>% mutate(report_4_focus_raw = gsub("NA_|_NA", "", report_4_focus_raw))
data_2 %<>% rename(report_4_check_focus_raw = QID506)
data_2 %<>% unite("report_5_focus_raw", 
                  c("QID647", "QID676"))
data_2 %<>% mutate(report_5_focus_raw = gsub("NA_|_NA", "", report_5_focus_raw))
data_2 %<>% unite("prompt_focus_raw", 
                  c("QID741", "QID782", "QID786", "QID779", "QID788"))
data_2 %<>% mutate(prompt_focus_raw = gsub("NA_|_NA", "", prompt_focus_raw))
data_2 %<>% rename(source_check_focus_raw = QID809)
data_2 %<>% rename(education_turnout_focus_raw = QID220)
data_2 %<>% unite("vote_focus_raw", 
                  c("QID219", "QID754", "QID756"))
data_2 %<>% mutate(vote_focus_raw = gsub("NA_|_NA", "", vote_focus_raw))
data_2 %<>% rename(income_html_focus_raw = QID208)
data_2 %<>% rename(debrief_focus_raw = QID831)

### Define focus variables

pages_focus <- c("consent_focus_raw", "gender_age_state_focus_raw", "know_focus_raw", "read_focus_raw", "trust_focus_raw", "attitudes_focus_raw", "services_focus_raw", "sharing_focus_raw", "knowledge_focus_raw", "estimate_correct_focus_raw", "introduction_focus_raw", "report_1_focus_raw", "report_2_focus_raw", "report_2_check_focus_raw", "report_3_focus_raw", "report_3_check_focus_raw", "report_4_focus_raw", "report_4_check_focus_raw", "report_5_focus_raw", "prompt_focus_raw", "source_check_focus_raw", "education_turnout_focus_raw", "vote_focus_raw", "income_html_focus_raw", "debrief_focus_raw")

## Load and run PageFocus functions by Diedenhofen & Musch

# source("pagefocus.r")

rtrim <- function (x) sub("\\s+$", "", x)  # returns string w/o trailing whitespace

winsorize <- function(x, q=.05, lower.winsorize=TRUE, upper.winsorize=TRUE) {
  extrema <- quantile(x, c(q, 1 - q))
  if(lower.winsorize) x[x < extrema[1]] <- extrema[1]
  if(upper.winsorize) x[x > extrema[2]] <- extrema[2]
  x
}

pagefocus <- function(data, page, statistics=NULL, lower.cut=-Inf, upper.cut=Inf, lower.winsorize=FALSE, upper.winsorize=FALSE, winsorize.percentil=.05) {  # lower.cut, upper.cut, upper.winsorize in milliseconds
  if(length(data) == 0) stop("No data found")
  if(length(page) == 0) stop("No page found")
  if(is.null(statistics)) statistics <- c("loss", "count", "error", "exclusions", "duration", "duration.min", "duration.mean", "duration.max", "duration.log.sum", "duration.log.mean", "duration.log.max", "duration.log.min")
  
  if(length(page) == 1) {  # analyze one page
    if(is.factor(data)) data <- as.character(data)
    data <- rtrim(data)
    
    absence.count <-  absence.durations <- NULL
    
    result <- list()
    for(d in 1:length(data)) {  # iterate through all participants
      result[[d]] <- {
        if(!is.null(data[d]) && !is.na(data[d]) && grepl("^(D[[:digit:]]*;R[[:digit:]]*;)+$", data[d])) {
          clicks.raw <- strsplit(data[d], ";")[[1]]
          
          timestamp <- c()
          for(x in clicks.raw) {
            timestamp <- c(timestamp, as.numeric(substr(x,2,nchar(x))))
          }
          timestamp.diffs <- diff(timestamp)
          absence.duration <- timestamp.diffs[seq(1, length(timestamp.diffs), by=2)]
          
          ## cut-offs
          include <- absence.duration > lower.cut & absence.duration < upper.cut
          absence.duration <- absence.duration[include]
          
          absence.durations <- c(absence.durations, absence.duration)  # collect durations for winsorizing (use the same percentile value for all participants on a page)
          
          list(
            absence.duration=absence.duration,
            absence.count=length(absence.duration),
            exclusions=sum(!include),
            error=0
          )
        } else if(is.na(data[d]) || grepl("^-(66|77|99)*$", data[d])) {  ## no pagefocus lost
          list(
            absence.count=0,
            exclusions=0,
            error=0
          )
        } else {  ## error occured
          list(
            absence.count=NA,
            exclusions=NA,
            error=1
          )
        }
      }
    }
    
    ## winsorize
    if(!is.null(absence.durations) && (upper.winsorize || lower.winsorize)) {
      lim <- quantile(absence.durations, c(winsorize.percentil, 1 - winsorize.percentil))  # use the same percentile value for all participants on a page
      absence.durations <- NULL
      
      for(d in 1:length(result)) {  # iterate through all participants
        if(!is.null(absence.duration)) {
          x <- result[[d]]$absence.duration
          if(lower.winsorize) x[x < lim[1]] <- lim[1]
          if(upper.winsorize) x[x > lim[2]] <- lim[2]
          result[[d]]$absence.duration <- x
          absence.durations <- c(absence.durations, x)
        }
      }
    }
    
    result.table <- NULL
    
    for(d in 1:length(result)) {  # iterate through all participants
      x <- result[[d]]$absence.duration
      
      if(is.na(result[[d]]$absence.count) || result[[d]]$absence.count == 0) {  # if there are no pagefocus losses or an error occured
        absence.duration <- absence.duration.min <- absence.duration.mean <- absence.duration.max <- absence.duration.log.sum <- absence.duration.log.min <- absence.duration.log.mean <- absence.duration.log.max <- NA
      } else {  # there have been pagefocus losses
        x <- x/1000  # convert milliseconds to seconds
        absence.duration <- sum(x)
        absence.duration.min <- min(x)
        absence.duration.mean <- mean(x)
        absence.duration.max <- max(x)
        absence.duration.log.sum <- sum(log(x))
        absence.duration.log.min <- min(log(x))
        absence.duration.log.mean <- mean(log(x))
        absence.duration.log.max <- max(log(x))
      }
      result.table <- rbind(result.table, c(loss=result[[d]]$absence.count > 0, count=result[[d]]$absence.count, error=result[[d]]$error, exclusions=result[[d]]$exclusions, duration=absence.duration, duration.min=absence.duration.min, duration.mean=absence.duration.mean, duration.max=absence.duration.max, duration.log.sum=absence.duration.log.sum, duration.log.min=absence.duration.log.min, duration.log.mean=absence.duration.log.mean, duration.log.max=absence.duration.log.max)[statistics])
    }
    
    colnames(result.table) <- paste(page, statistics, sep="_")
  } else {
    stop("This function can only analyze one page at a time. Please use pagefocus.analysis().")
  }
  
  result.table
}

pagefocus.analysis <- function(data, pages, statistics=NULL, lower.cut=-Inf, upper.cut=Inf, lower.winsorize=FALSE, upper.winsorize=FALSE, winsorize.percentil=.05) {
  result <- NULL
  
  for(v in 1:length(pages)) {
    if(length(data[[pages[v]]]) == 0) stop(paste0("Could not find data for page '", pages[v], "'"))
    result <- cbind(result, pagefocus(data[[pages[v]]], pages[v], statistics, lower.cut=lower.cut, upper.cut=upper.cut, lower.winsorize=lower.winsorize, upper.winsorize=upper.winsorize, winsorize.percentil=winsorize.percentil))
  }
  
  result
}

data_2 <- cbind(data_2, pagefocus.analysis(data_2,
                                           pages_focus,
                                           statistics=c("loss", "count", "error", "duration")))

### Defocus count across questionnaire

data_2 %<>% mutate(defocus_count = rowSums(select(., ends_with("_count")), na.rm = T))

### Defocussed pages across questionnaire

data_2 %<>% mutate(defocussed_page_count = rowSums(select(., ends_with("_loss")), na.rm = T))

### Total defocussing duration across questionnaire

data_2 %<>% mutate(defocus_duration = rowSums(select(., ends_with("_duration")), na.rm = T))


