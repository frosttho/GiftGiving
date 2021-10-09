#######################################
#                                     #
#            gift giving              #
#          survey analysis            #
#                                     #
#            Thomas FROST             #
#           Antonia GERKEN            #
#           Anita LINDBERG            #
#             Diane OWIN              #
#                                     #
#       emlyon business school        #
#                                     #
#       https://lyonexchange.fr       #
#     gift-giving@lyonexchange.fr     #
#                                     #
#######################################



# 00 - Setup #############################

# setwd("~/Studium/Master/Module/5A2M21 Advanced Topics in Digital Marketing/Gruppenarbeit/GiftGiving")

# install.packages("tidyverse")
# install.packages("fastDummies")
# install.packages("openxlsx")
# install.packages("data.table")


library(tidyverse)
library(fastDummies)
library(openxlsx)
library(data.table)


# 01 - Read data #########################

surveydata <- read_csv(file = "results-survey282822_20211009-1210.csv")

# 02 - Clean data #########################

# delete incomplete answers
surveydata <- filter(surveydata, !is.na(submitdate))

# delete answer where control question is wrong
surveydata <- surveydata[surveydata$`G02Q10[SQ053]`  == 4, ]

# remove control question
surveydata1 <- subset(surveydata, select = -c(`G02Q10[SQ053]`) )

# remove time stamp
surveydata1 <- subset(surveydata1, select = -c(submitdate) )

# rename categorical columns----
oldnames <-  c('G01Q04','G01Q17', 'G01Q18', 'G01Q19', 'G01Q05', 'G02Q02','G03Q03', 'G03Q13', 'G03Q14',
             'G03Q15','G03Q16')
newnames <-  c('situation_fit','last_minute', 'remb_bday', 'online_gift', 
             'suitablegift','services_fit','age', 'gender', 'student', 'profession','country')
for(i in 1:length(newnames)) names(surveydata1)[names(surveydata1) == oldnames[i]] = newnames[i]

# rename G02Q07 ----

# How suitable are the following items in your opinion for an individualized gift box? 
# Consider as criteria the probability of sending a box with this to someone else by yourself 
# and how much you would find it a good gift if you would receive it yourself. 
# Consider 1 as a bad gift and 5 as the best possible option.

gifts_numbers <- c('G02Q07[SQ001]', 'G02Q07[SQ012]','G02Q07[SQ022]','G02Q07[SQ032]','G02Q07[SQ042]',
                   'G02Q07[SQ052]','G02Q07[SQ062]','G02Q07[SQ072]','G02Q07[SQ082]','G02Q07[SQ092]')
gifts_names <- c('Seads', 'Spices', 'Chocolate, Pralines', 'Coffee / Tea', 'Food / sweats in general', 
                 'Specialties from the whole country / EU / world', 'Flowers', 'Candles', 'Cosmetics', 
                 'Alcohol')
for(i in 1:length(gifts_names)) names(surveydata1)[names(surveydata1) == gifts_numbers[i]] = gifts_names[i]

# rename G01Q06 -----

# What do you usually give your friends (who do not live close to you) for their birthday?
gifts_numbers2 <- c('G01Q06[SQ001]', 'G01Q06[SQ002]', 'G01Q06[SQ003]', 'G01Q06[SQ004]', 'G01Q06[SQ005]')
gifts_names2 <- c('package', 'post_card','flowers', 'nothing_no_change','nothing_want_change')
for(i in 1:length(gifts_names2)) names(surveydata1)[names(surveydata1) == gifts_numbers2[i]] = gifts_names2[i]

# recode yes/no to 1/0 as dummies
surveydata1 <- dummy_cols(surveydata1, select_columns = gifts_names2, remove_selected_columns = TRUE)
# delete douplicated columns
surveydata1 <- subset(surveydata1, select = - c(package_No, post_card_No, flowers_No,
                                                nothing_no_change_No,nothing_want_change_No))

# rename G02Q10 ----

# Imagine that this service wants to help you not only for sending gifts to others but also
# wants to be your central point for all matters related to gifts. Which of the following 
# functionalities should the platform provide to be useful for you?(1 = not important at all
# and 5 = really important)
gifts_numbers3 <- c('G02Q10[SQ012]','G02Q10[SQ022]','G02Q10[SQ032]','G02Q10[SQ042]',
                    'G02Q10[SQ052]')
gifts_names3 <- c('Int_User_Design', 'Customized_packaging','per_calendar',
                  'recommendation','wishlist')
for(i in 1:length(gifts_names3)) names(surveydata1)[names(surveydata1) == gifts_numbers3[i]] = gifts_names3[i]


# recode variables in dummies ----

# situation_fit (G01Q04): yes = 1, last_minute (G01Q17): last min = 1, 
# hard_remb_bday(G01Q18): hard to remb = 1,; online_gift(G01Q19): yes = 1; 
# suitablegift (G01Q05): hard = yes = 1; services_fit (G02Q02): yes = 1;
# gender (G03Q13): male = 1; student (G03Q14): yes = 1;

# might adding age and country to the dummy list ?

dummy_var <- c('situation_fit','last_minute', 'remb_bday', 'online_gift', 
               'suitablegift','services_fit', 'gender', 'student')

surveydata1 <- dummy_cols(surveydata1, select_columns = dummy_var, remove_selected_columns = TRUE)

# delete douplicated columns
surveydata1 <- subset(surveydata1, select = - c(situation_fit_No, remb_bday_No,online_gift_No, 
                                                suitablegift_No, services_fit_No, 
                                                gender_Female, student_No))
# problem with deleting 'last_minute_In advance', because of '.. ..' in it

# Format Factor Variables ----
surveydata1$age <- as.factor(surveydata1$age)
surveydata1$profession <- as.factor(surveydata1$profession)
surveydata1$country <- as.factor(surveydata1$country)

# dummies as numeric 
surveydata1[,24:37] <- sapply(surveydata1[,24:37],as.numeric)


# 03 - Extracting text answers to excel ##############

text_answers <- c('G01Q11', 'G02Q08','G02Q09', 'G02Q12')

answers_G01Q11 <- unique(surveydata1['G01Q11'])
answers_G02Q08 <- unique(surveydata1['G02Q08'])
answers_G02Q09 <- unique(surveydata1['G02Q09'])
answers_G02Q12 <- unique(surveydata1['G02Q12'])

# export as  one excel
list_of_text_answers <- list("G01Q11" = answers_G01Q11, "G02Q08" = answers_G02Q08,
                             "G02Q09" = answers_G02Q09,
                         "G02Q12" = answers_G02Q12)
write.xlsx(list_of_text_answers, file = "survey_open_question_v1.xlsx", overwrite = TRUE)

# deleting open questions from data frame
surveydata1 <- subset(surveydata1, select = - c(G01Q11, G02Q08, G02Q09, G02Q12))

# 04 - Descriptive analysis ##############
summary(surveydata1)

# Plot Histogram of One Variable
hist(surveydata1$`Chocolate, Pralines`)





# 04.01 - popularity of the different gift items (G02Q07) #####
items <- c('Seads', 'Spices', 'Chocolate, Pralines', 'Coffee / Tea', 'Food / sweats in general', 'Specialties from the whole country / EU / world', 'Flowers', 'Candles', 'Cosmetics', 'Alcohol')
labels <- c('Seads', 'Spices', 'Chocolate,\nPralines', 'Coffee /\nTea', 'Food / sweats\nin general', 'Specialties from\nthe whole country /\nEU / world', 'Flowers', 'Candles', 'Cosmetics', 'Alcohol')

giftitems <- reshape(as.data.frame(surveydata[15:24]), times = items,
                     timevar = "gift", direction = "long",
                     varying = 1:10, v.names = "rating", idvar = "participant")

rownames(giftitems) <- 1:nrow(giftitems)
giftitems$gift <- as.factor(giftitems$gift)

itemsplot <- ggplot(giftitems, aes(x = gift, y = rating, fill = gift)) +
  theme_classic() +
  geom_boxplot() +
  ylab("Rating (1 to 5)") +
  xlab("") +
  ggtitle("G02Q07 Popularity of gift box items") +
  scale_x_discrete(labels= labels) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="PuBuGn")
itemsplot



# 04.02 - popularity of different site functionalities (G02Q07) #####
functionalities <- c('intuitive user design', 'Customized packaging with personal pictures', 'a connection to your personal calendar with a reminder of important birthday dates', 'recommendation of presents based on characteristics of the recipient', 'a personal wishlist in your profile with your own personal preferences to help others finding the perfect gift for you (milk or dark chocolate, coffee or tea, ...)')
func_labels <- c('intuitive user design', 'Customized packaging\nwith personal pictures', 'a connection to your\npersonal calendar with a reminder\nof important birthday dates', 'recommendation of presents\nbased on characteristics\nof the recipient', 'a personal wishlist in your profile\nwith your own personal preferences')

func <- reshape(as.data.frame(surveydata[c(27:29, 31:32)]), times = functionalities,
                     timevar = "functionality", direction = "long",
                     varying = 1:5, v.names = "rating", idvar = "participant")

rownames(func) <- 1:nrow(func)
func$functionality <- as.factor(func$functionality)

funcplot <- ggplot(func, aes(x = functionality, y = rating, fill = functionality)) +
  theme_classic() +
  geom_boxplot() +
  ylab("Rating (1 to 5)") +
  xlab("") +
  ggtitle("G02Q10 Popularity of service functionalities") +
  scale_x_discrete(labels= func_labels) +
  theme(legend.position="none") +
  scale_fill_brewer(palette="PuBuGn")
funcplot



# 04.03 - Some simple percentages #############

# Format yes/no answer as Factor Variable
surveydata$G01Q04 <- as.factor(surveydata$G01Q04)


# Show percentage of the two answers
summary(surveydata$G01Q04)/nrow(surveydata)



# Share of early / late buyers
surveydata$G01Q17 <- as.factor(surveydata$G01Q17)
summary(surveydata$G01Q17)/nrow(surveydata)

#Share of online gifts
surveydata$G01Q19 <- as.factor(surveydata$G01Q19)
summary(surveydata$G01Q19)/nrow(surveydata)


# plot a simple bar diagram
plot <- ggplot(surveydata) +
  geom_bar(aes(x = G01Q04)) +
  ylab("Number of respondents") +
  xlab("Answer") +
  ggtitle("G01Q04 Familiarity of problem situation") +
  coord_flip()
plot


# 04.04 - Sustainability ##########################

summary(surveydata$G02Q09)
