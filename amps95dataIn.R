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

magazines_engagement_95 <- readRDS("magazines_engagement_95.rds")
newspapers_engagement_95 <- readRDS("newspapers_engagement_95.rds")
magazines_engagement_95_simple <- readRDS("magazines_engagement_95_simple.rds")
newspapers_engagement_95_simple <- readRDS("newspapers_engagement_95_simple.rds")

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

# Level 1: Type
media_type_95 <- data.frame(cbind(qn = all_data_95$quesno,
                                  rowSums(newspapers_engagement_95),
                                  rowSums(magazines_engagement_95),
                                  rowSums(radio_engagement_95),
                                  rowSums(tv_engagement_95)))

media_type_95_simple <- data.frame(cbind(qn = all_data_95$quesno,
                                         rowSums(newspapers_engagement_95_simple),
                                         rowSums(magazines_engagement_95_simple),
                                         rowSums(radio_engagement_95),
                                         rowSums(tv_engagement_95)))
names(media_type_95) <- c("qn",
                          "newspapers",
                          "magazines",
                          "radio",
                          "tv")

names(media_type_95_simple) <- c("qn",
                                 "newspapers",
                                 "magazines",
                                 "radio",
                                 "tv")
media_type_95 <- media_type_95 %>%
        mutate(all = as.vector(newspapers + magazines + radio + tv))
media_type_95_simple <- media_type_95_simple %>%
        mutate(all = as.vector(newspapers + magazines + radio + tv))

# Level 2: Vehicles
media_vehicles_95 <- data.frame(cbind(qn = all_data_95$quesno,
                                      newspapers_engagement_95,
                                      magazines_engagement_95,
                                      radio_engagement_95,
                                      tv_engagement_95))

media_vehicles_95_simple <- data.frame(cbind(qn = all_data_95$quesno,
                                             newspapers_engagement_95_simple,
                                             magazines_engagement_95_simple,
                                             radio_engagement_95,
                                             tv_engagement_95))

saveRDS(media_type_95, 'media_type_95.rds')
saveRDS(media_type_95_simple, 'media_type_95_simple.rds')
saveRDS(media_vehicles_95, 'media_vehicles_95.rds')
saveRDS(media_vehicles_95_simple, 'media_vehicles_95_simple.rds')

media_type_95 <- readRDS('media_type_95.rds')
media_type_95_simple <- readRDS('media_type_95_simple.rds')
media_vehicles_95 <- readRDS('media_vehicles_95.rds')
media_vehicles_95_simple <- readRDS('media_vehicles_95_simple.rds')

# Demographic stuff (NB... will need to double check comparibility....)

age <- all_data_95[,'age'] # only four levels (deal with this...)
#1: 16-24
#2: 25-34
#3: 35-49
#4: 50-64
#5: 65+

age <- all_data_95[,'qpd9_1']
age <- ifelse(age > 15 & age < 25, 1, age)
age <- ifelse(age > 24 & age < 45, 2, age)
age <- ifelse(age > 44 & age < 55, 3, age)
age <- ifelse(age > 54, 4, age)

sex <- all_data_95[,'sex']
# 1 male
# 2 female
edu <- all_data_95[,'educatn']
for(i in 1: length(edu)) {
        if(edu[i] %in% c(6,7)) {
                edu[i] <- edu[i] + 1
        }
        else if(edu[i] == 8) {
                edu[i] <- 6
        }
}
hh_inc <- all_data_95[,'h_inc_g1'] # nb double check scales...

race <- all_data_95[,'race_1']
# # dataset 95: 1 = white, 2 = black, 3 = coloured, 4 = indian.
# # 2012 dataset: 1 = black, 2 = coloured, 3 = indian, 4 = white
# # change 1995 codes to 2012 codes for consistency: 1 to 4; 2 to 1; 3 to 2 and 4 to 3
# 
race <- ifelse(race == 1, 9, race)
race <- ifelse(race == 2, 6, race)
race <- ifelse(race == 3, 7, race)
race <- ifelse(race == 4, 8, race)
race <- race - 5

province <- all_data_95[,'province']

metro1 <- all_data_95[,'metrop1']
metro2 <- all_data_95[,'metrop2']
metro3 <- all_data_95[,'metrop3']
metro4 <- all_data_95[,'metrop4']
metro5 <- all_data_95[,'metrop5']
metro6 <- all_data_95[,'metrop6']
metro7 <- all_data_95[,'metrop7']
metro8 <- all_data_95[,'metrop8']
metro9 <- all_data_95[,'metrop9']
metro10 <- all_data_95[,'metrop10']
metro11 <- all_data_95[,'metrop11']
metro12 <- all_data_95[,'metrop12']
metro13 <- all_data_95[,'metrop13']


# think should be able to place into single set:

metro1[metro1 == 1] <- 1
metro2[metro2 == 1] <- 2
metro3[metro3 == 1] <- 3
metro4[metro4 == 1] <- 4
metro5[metro5 == 1] <- 5
metro6[metro6 == 1] <- 6
metro7[metro7 == 1] <- 7
metro8[metro8 == 1] <- 8
metro9[metro9 == 1] <- 9
metro10[metro10 == 1] <- 10
metro11[metro11 == 1] <- 11
# metro12[metro12 == 1] <- 12
metro13[metro13 == 1] <- 12 # vaal

test <- cbind(metro1,
         metro2,
         metro3,
         metro4,
         metro5,
         metro6,
         metro7,
         metro8,
         metro9,
         metro10,
         metro11,
         # metro12,
         metro13)

metro <- rowSums(test, na.rm = TRUE)

# # collect and code into single metro set:
# #0 = no metro
# #1 Cape Town
# #2 Cape Town Fringe Area
# #3 Port Elizabeth/Uitenhage
# #4 East London
# #5 Durban
# #6 Bloemfontein
# #7 Greater Johannesburg
# #8 Reef
# #9 Pretoria
# #10 Kimberley
# ##11 Pietermaritzburg
# ##12 Vaal

table(metro) # yes, continue

lang <- all_data_95[,'lang']
# 1 English
# 2 Afrikaans
# 3 African 1
# 4 African 2

mar_status <- all_data_95[,'qpd1']

# problematic response 1542

mar_status[1542] <- 2 # place it in largest category...

demographics_95 <- data.frame(qn = all_data_95$quesno, # no lifestyle or attitudes yet
                              pwgt = all_data_95$popwght,
                              age,
                              sex,
                              edu,
                              hh_inc,
                              race,
                              province,
                              metro,
                              lang,
                              mar_status)


#reducing levels of categorical variables and setting factor types for demographics:

# # age:
demographics_95$age <- factor(demographics_95$age, ordered = TRUE)

# sex:
demographics_95$sex <- factor(demographics_95$sex, ordered = FALSE)

#edu:
demographics_95$edu <- ifelse(demographics_95$edu %in% c(1,2,3,4), 1, demographics_95$edu)
demographics_95$edu <- ifelse(demographics_95$edu %in% c(5), 2, demographics_95$edu)
demographics_95$edu <- ifelse(demographics_95$edu %in% c(6,7,8), 3, demographics_95$edu)
demographics_95$edu <- factor(demographics_95$edu, ordered = TRUE)

#hh_inc
demographics_95$hh_inc <- ifelse(demographics_95$hh_inc %in% c(1,2,3,4), 1, demographics_95$hh_inc)
demographics_95$hh_inc <- ifelse(demographics_95$hh_inc %in% c(5,6), 2, demographics_95$hh_inc)
demographics_95$hh_inc <- ifelse(demographics_95$hh_inc %in% c(7), 3, demographics_95$hh_inc)
demographics_95$hh_inc <- ifelse(demographics_95$hh_inc %in% c(8), 4, demographics_95$hh_inc)
demographics_95$hh_inc <- factor(demographics_95$hh_inc, ordered = TRUE)

demographics_95$race <- factor(demographics_95$race, ordered = FALSE)
demographics_95$province <- factor(demographics_95$province, ordered = FALSE)
demographics_95$metro <- factor(demographics_95$metro, ordered = FALSE)
demographics_95$lang <- factor(demographics_95$lang, ordered = FALSE)
demographics_95$mar_status <- factor(demographics_95$mar_status, ordered = FALSE)

saveRDS(demographics_95, "demographics_95.rds")
demographics_95 <- readRDS("demographics_95.rds")

# read datafiles again if necessary
magazines_engagement_95 <- readRDS("magazines_engagement_95.rds")
newspapers_engagement_95 <- readRDS("newspapers_engagement_95.rds")
radio_engagement_95 <- readRDS("radio_engagement_95.rds")
tv_engagement_95 <- readRDS("tv_engagement_95.rds")
internet_engagement_95 <- readRDS("internet_engagement_95.rds")

media_type_95 <- readRDS("media_type_95.rds")
media_type_95_simple <- readRDS("media_type_95_simple.rds")
media_vehicles_95 <- readRDS("media_vehicles_95.rds")
media_vehicles_95_simple <- readRDS("media_vehicles_95_simple.rds")
demographics_95 <- readRDS("demographics_95.rds")

# #create single dataset minus non metropolitans
set95 <- demographics_95 %>%
        left_join(media_type_95) %>%
        left_join(media_vehicles_95) %>%
        filter(metro != 0)
set95_simple <- demographics_95 %>%
        left_join(media_type_95_simple) %>%
        left_join(media_vehicles_95_simple) %>%
        filter(metro != 0)

# scale media type and media vehicles
set95[,12:147] <- scale(set95[,12:147])
set95_simple[,12:147] <- scale(set95_simple[,12:147])

# save them
saveRDS(set95, "set95.rds")
saveRDS(set95_simple, "set95_simple.rds")

# end for now

