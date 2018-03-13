## libraries
library(stringr)
library(tidyverse)

# read entire datasets: wci and blk
wci_path <- "/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_1995/csv/amps 1995 wci_tm_20120227.csv"
blk_path <- "/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_1995/csv/amps 1995 blk_tm_20120227.csv"

wci_data <- read.csv(wci_path)
blk_data <- read.csv(blk_path)

# creating single dataframe of both races:
all_data_95 <- merge(wci_data, blk_data, all = TRUE)

# save
saveRDS(all_data_95, "all_data_95.rds")
all_data_95 <- readRDS("all_data_95.rds")

# read metadata files
wci_labels_path <- "/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_1995/csv/metadata/variable_labels/AMPS 1995 WCI_tm_20120227_variable_labels.txt"
blk_labels_path <- "/Users/HansPeter/Dropbox/Statistics/UCTDataScience/Thesis/amps_1995/csv/metadata/variable_labels/AMPS 1995 BLK_tm_20120227_variable_labels.txt"
wci_labels <- readLines(wci_labels_path)
blk_labels <- readLines(blk_labels_path)

saveRDS(wci_labels, "wci_labels.rds")
saveRDS(blk_labels, "blk_labels.rds")

wci_labels <- readRDS("wci_labels.rds")
blk_labels <- readRDS("blk_labels.rds")

# 
# # print
# names_print_95a <- str_subset(wci_labels,
#                              '[Ii]ssues of') %>%
#         str_replace('.+[Ii]ssues of\\s', '') %>%
#         str_replace('(usually read)| (do you usually read or page through\\?)', '') %>%
#         str_trim()
# names_print_95a[20:83] <- names_print_95
# 
# fix(names_print_95a)
# 
# names_print_95 <- names_print_95a



# names_print_95_thorough <- str_subset(wci_labels,
#                                       '[Tt]horoughly')
# 
# names_thorough_95 <- str_subset(wci_labels,
#                              'thoroughly') %>%
#         str_replace('.+How many different issues of\\s', '') %>%
#         str_replace('do you usually read or page through\\?', '') %>%
#         str_trim()


# create print dataset

# issues
issues_95 <- all_data_95[,str_detect(names(all_data_95), '(qb2)|(qc2)|(qd2)|(qe2)|(qf2)')]
# thorough
thorough_95 <-  all_data_95[,str_detect(names( all_data_95), '(qb6)|(qc6)|(qd6)|(qe6)|(qf6)')]

# one missing in thorough... what the fuck???
names(thorough_95)
names(issues_95)

# get rid of two extras in issues ie "qb2_13a" and "qb2_16a" (issues c(14,18)) and in names
# get rid of "qf6_4" De Kat in thorough... not in issues (c(59))

names_print_95 <- names_print_95[-c(14,18)]
issues_95 <- issues_95[,-c(14,18)]
thorough_95 <- thorough_95[,-59]

# and need to reverse number thorough
thorough_95 <- 7 - thorough_95

# change NAs to 0
issues_95[is.na(issues_95)] <- 0
thorough_95[is.na(thorough_95)] <- 0

saveRDS(names_print_95, "names_print_95.rds")
saveRDS(issues_95, "issues_95.rds")
saveRDS(thorough_95, "thorough_95.rds")

names_print_95 <- readRDS("names_print_95.rds")
issues_95 <- readRDS("issues_95.rds")
thorough_95 <- readRDS("thorough_95.rds")

# create print engagement set
print_engagement_95 <- issues_95 * thorough_95
print_engagement_95_simple <- issues_95

# names
names(print_engagement_95) <- names_print_95
names(print_engagement_95_simple) <- names_print_95

# seperate into magazines and newspapers
newspapers_engagement_95 <- print_engagement_95[,1:36]
magazines_engagement_95 <- print_engagement_95[,37:81]

newspapers_engagement_95_simple <- print_engagement_95_simple[,1:36]
magazines_engagement_95_simple <- print_engagement_95_simple[,37:81]

saveRDS(newspapers_engagement_95, "newspapers_engagement_95.rds")
saveRDS(magazines_engagement_95, "magazines_engagement_95.rds")
saveRDS(newspapers_engagement_95_simple, "newspapers_engagement_95_simple.rds")
saveRDS(magazines_engagement_95_simple, "magazines_engagement_95_simple.rds")

##### RADIO


# # RADIO (NB... in variable list refer to 6 months... questionaire refers to past 7 days)

radio_names_95 <- wci_labels %>%
        str_subset('^qg1_') %>%
        str_replace('qg1_..?\\sPersonally\\slistened\\sto\\s','') %>%
        str_replace('\\sin\\sthe\\spast\\s6\\smonths\\?', '')
radio_names_95 <- radio_names_95[-c(37:38)] # get rid of unsure and no radio

fix(radio_names_95)

saveRDS(radio_names_95, "radio_names_95.rds")

radio_7days_95 <- all_data_95[,str_detect(names(all_data_95), "^qg1_.+")]
radio_7days_95 <- radio_7days_95[-c(37:38)]
names(radio_7days_95) <- radio_names_95
#
# # replace NAs with zeros
radio_7days_95[is.na(radio_7days_95)] <- 0

# create one variable "other radio" by summing the two columns into one
radio_7days_95 <- radio_7days_95 %>%
        mutate(Other.Radio = Other1 + Other2) %>%
        select(-Other1, -Other2)

# # Radio- yesterday

radio_yesterday_95 <- all_data_95[,str_detect(names(all_data_95), "^qg2_.+")]
radio_yesterday_95 <- radio_yesterday_95[-c(37:38)]
names(radio_yesterday_95) <- radio_names_95
#
# # replace NAs with zeros
radio_yesterday_95[is.na(radio_yesterday_95)] <- 0

# create one variable "other radio" by summing the two columns into one
radio_yesterday_95 <- radio_yesterday_95 %>%
        mutate(Other.Radio = Other1 + Other2) %>%
        select(-Other1, -Other2)

# # # 0 = not
# # # 1 = in past 7 days
# # # 2 = yesterday and in past 7 days
# 
radio_engagement_95 <- radio_7days_95 + radio_yesterday_95

saveRDS(radio_engagement_95, "radio_engagement_95.rds")
radio_engagement_95 <- readRDS("radio_engagement_95.rds")

# # # TELEVISION (same as radio...information on times is missing!!)
# # 
tv_days7_vars <- wci_labels %>%
         str_subset('^qh1_') %>%
         str_extract('^qh1_\\d{1,2}')
# # 
tv_names_95 <- wci_labels %>%
         str_subset('^qh1_') %>%
         str_replace('qh1_..?\\sWhich\\sof\\sthese\\sservices\\shave\\syou\\swatched\\sin\\sthe\\spast\\s7\\sdays\\?\\s','')

fix(tv_names_95)

tv_names_95 <- tv_names_95[c(1:14,19)]
saveRDS(tv_names_95, "tv_names_95.rds")
tv_names_95 <- readRDS("tv_names_95.rds")

# 
tv_days7_95 <- all_data_95[,str_detect(names(all_data_95), "^qh1_.+")]
tv_days7_95 <- tv_days7_95[,c(1:14,19)]
names(tv_days7_95) <- tv_names_95
# 
# # # replace NAs with zeros
tv_days7_95[is.na(tv_days7_95)] <- 0

# # # # tv yesterday
tv_yesterday_95 <- all_data_95[,str_detect(names(all_data_95), "^qh2_.+")]
tv_yesterday_95 <- tv_yesterday_95[,c(1:14,19)]
names(tv_yesterday_95) <- tv_names_95

# # # replace NAs with zeros
tv_yesterday_95[is.na(tv_yesterday_95)] <- 0

# # # # 0 = not
# # # # 1 = in past 7 days
# # # # 2 = yesterday

tv_engagement_95 <- tv_yesterday_95 + tv_days7_95

saveRDS(tv_engagement_95, "tv_engagement_95.rds")
tv_engagement_95 <- readRDS("tv_engagement_95.rds")

internet_engagement_95 <- rep(0,nrow(all_data_95))

# Level 1: Type
media_type_95 <- data.frame(cbind(qn = all_data_95$quesno,
                                  scale(rowSums(newspapers_engagement_95)),
                                  scale(rowSums(magazines_engagement_95)),
                                  scale(rowSums(radio_engagement_95)),
                                  scale(rowSums(tv_engagement_95)),
                                  internet_engagement_95))

media_type_95_simple <- data.frame(cbind(qn = all_data_95$quesno,
                                         scale(rowSums(newspapers_engagement_95_simple)),
                                         scale(rowSums(magazines_engagement_95_simple)),
                                         scale(rowSums(radio_engagement_95)),
                                         scale(rowSums(tv_engagement_95)),
                                         internet_engagement_95))
names(media_type_95) <- c("qn",
                          "newspapers",
                          "magazines",
                          "radio",
                          "tv",
                          "internet")

names(media_type_95_simple) <- c("qn",
                                 "newspapers",
                                 "magazines",
                                 "radio",
                                 "tv",
                                 "internet")
# Level 2: Vehicles
media_vehicles_95 <- data.frame(cbind(qn = all_data_95$quesno,
                                      newspapers_engagement_95,
                                      magazines_engagement_95,
                                      radio_engagement_95,
                                      tv_engagement_95,
                                      internet = internet_engagement_95))

media_vehicles_95_simple <- data.frame(cbind(qn = all_data_95$quesno,
                                             newspapers_engagement_95_simple,
                                             magazines_engagement_95_simple,
                                             radio_engagement_95,
                                             tv_engagement_95,
                                             internet = internet_engagement_95))

saveRDS(media_type_95, 'media_type_95.rds')
saveRDS(media_type_95_simple, 'media_type_95_simple.rds')
saveRDS(media_vehicles_95, 'media_vehicles_95.rds')
saveRDS(media_vehicles_95_simple, 'media_vehicles_95_simple.rds')

media_type_95 <- readRDS('media_type_95.rds')
media_type_95_simple <- readRDS('media_type_95_simple.rds')
media_vehicles_95 <- readRDS('media_vehicles_95.rds')
media_vehicles_95_simple <- readRDS('media_vehicles_95_simple.rds')

# Demographic stuff (NB... will need to double check comparibility....)


# # # drawing demographic data:
# 
# 
# # want to add. personal income 
# 
# table(all_data$qpd16_1a)
# table(all_data$qpd16_1b)
# table(all_data$qpd16_1c)
# 
# p_inc <- all_data[,str_detect(names(all_data), '^qpd16_+')]
# p_inc$qpd16_1b <- p_inc$qpd16_1b + 24
# p_inc$qpd16_1c <- p_inc$qpd16_1c + 29
# 
# 
# table(p_inc$qpd16_1a)
# table(p_inc$qpd16_1b)
# table(p_inc$qpd16_1c)
# personal_income <- rowSums(p_inc, na.rm = TRUE)
# 
# table(personal_income)
# 
# personal_income[personal_income == 0] <- NA # 0 becomes NA
# personal_income[personal_income == 1 |
#                         personal_income == 2 |
#                         personal_income == 3 |
#                         personal_income == 4 |
#                         personal_income == 5] <- 1 # 1,2,3,4,5 becomes 1
# personal_income[personal_income == 6 |
#                         personal_income == 7 |
#                         personal_income == 8 |
#                         personal_income == 9] <- 2 # 6,7,8,9 becomes 2
# 
# personal_income[personal_income == 10 |
#                         personal_income == 11 |
#                         personal_income == 12 |
#                         personal_income == 13] <- 3 # 10,11,12,13 becomes 3
# personal_income[personal_income == 14 |
#                         personal_income == 15 |
#                         personal_income == 16] <- 4 # 14,15,16 becomes 4
# personal_income[personal_income == 17 |
#                         personal_income == 18] <-  5 # 17,18 becomes 5
# personal_income[personal_income == 19 |
#                         personal_income == 20] <-  6 # 19,20 becomes 6
# personal_income[personal_income == 21 |
#                         personal_income == 22 |
#                         personal_income == 23] <- 7 # 21,22,23 becomes 7
# personal_income[personal_income == 24 |
#                         personal_income == 25 |
#                         personal_income == 26 |
#                         personal_income == 27 |
#                         personal_income == 28 |
#                         personal_income == 29] <- 8 # 24,25,26,27,28,29 becomes 8
# 
# personal_income[personal_income == 30] <- 1 # 30 becomes 1
# personal_income[personal_income == 31] <- NA # 31 becomes 1
# table(personal_income)
# 
# # first without personal_income
# demogr_vars <- c('educatn', # education level
#                  'h_inc_g1', # household income level
#                  'sex',
#                  'age',
#                  'lang',
#                  'race_1',
#                  'province',
#                  'wrk_stat',
#                  'qpd1') #'qpd1' # marital status
# 
# demogrs <- all_data[,demogr_vars] %>%
#         mutate(pers_inc = personal_income) %>%
#         mutate(quesno = all_data$quesno) %>%
#         mutate(popwght = all_data$popwght) %>%
#         select(quesno, popwght, everything())
# # 
# names(demogrs) <- c("quesno",
#                     "popwght",
#                     "edu",
#                     "hh_inc",
#                     "sex",
#                     "age",
#                     "lang",
#                     "race",
#                     "prov",
#                     "wrk_status",
#                     "mart_status",
#                     "pers_inc")
# # 
# # 
# # actually also want to add a variable for metropole:
# 
# metropole <- all_data[,str_detect(names(all_data),"metrop\\d" )]
# 
# # enumerate the metropoles
# vec <- seq(0,12)
# for(i in 1: nrow(metropole)){
#         metropole[i,] <- metropole[i,] + vec
# }
# 
# metropole <- metropole %>%
#         mutate(metro = rowSums(metropole, na.rm = TRUE))
# 
# table(metropole$metro) # seems that all the 19s are the sum of 7 & 12s (ie, Soweto)
# 
# # code as such, ie all 19s are actually 12s (so keeping the double count in the 7s)
# metropole$metro <- ifelse(metropole$metro == 19, 12, metropole$metro)
# 
# demogrs95 <- demogrs95 %>%
#         mutate(metro = metropole$metro)
#
# problematic response 1542
length(which(is.na(media95)))
length(which(is.na(demogrs95))) # all in pers_inc

# get rid of problematic record 1542
demogrs95 <- demogrs95[-1542,]


# ensure correct typing:
# education
# to create ordered factor took 8 and fit it after matric (ie 6) then change 6 to 7 and 7 to eight
demogrs95 <- readRDS("demogrs95.rds")
table(demogrs95$edu)
for(i in 1: nrow(demogrs95)) {
        if(demogrs95$edu[i] %in% c(6,7)) {
                demogrs95$edu[i] <- demogrs95$edu[i] + 1
                }
        else if(demogrs95$edu[i] == 8) {
                demogrs95$edu[i] <- 6
                }
}

demogrs95$edu <- factor(demogrs95$edu, ordered = TRUE)
demogrs95$hh_inc <- factor(demogrs95$hh_inc, ordered = TRUE)
demogrs95$sex <- factor(demogrs95$sex, ordered = FALSE)
demogrs95$age <- factor(demogrs95$age, ordered = TRUE)
demogrs95$lang <- factor(demogrs95$lang, ordered = FALSE)
demogrs95$race <- factor(demogrs95$race, ordered = FALSE)
demogrs95$prov <- factor(demogrs95$prov, ordered = FALSE)
demogrs95$wrk_status <- factor(demogrs95$wrk_status, ordered = FALSE)
demogrs95$mart_status <- factor(demogrs95$mart_status, ordered = FALSE)
demogrs95$pers_inc <- factor(demogrs95$pers_inc, ordered = TRUE)
demogrs95$metro <- factor(demogrs95$metro, ordered = FALSE)

saveRDS(demogrs95, "demogrs95.rds")
demogrs95 <- readRDS("demogrs95.rds")


