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

library(tidyverse)
library(fastDummies)


# 01 - Read data #########################

# surveydata <- read_csv(file = "results-survey282822_20211004-1449.csv")
surveydata <- read_csv(file = "results-survey282822_20211005-1218.csv")

# 02 - Clean data #########################

# delete incomplete answers
surveydata <- filter(surveydata, !is.na(submitdate))

# delete answer where control question is wrong
surveydata <- surveydata[surveydata$`G02Q10[SQ053]`  == 4, ]

# remove control question
surveydata <- subset(surveydata, select = -c(`G02Q10[SQ053]`) )

# rename columns
oldnames = c('G01Q04','G01Q17', 'G01Q18', 'G01Q19', 'G01Q05', 'G02Q02','G03Q03', 'G03Q13', 'G03Q14','G03Q15',
             'G03Q16')
newnames = c('situation_fit','last_minute', 'remb_bday', 'online_gift', 
             'suitablegift','services_fit','age', 'gender', 'student', 'profession','country')
for(i in 1:length(newnames)) names(surveydata)[names(surveydata) == oldnames[i]] = newnames[i]

# recode variables in dummies

# situation_fit (G01Q04): yes = 1, last_minute (G01Q17): last min = 1, hard_remb_bday(G01Q18): hard to remb = 1
# online_gift(G01Q19): yes = 1; suitablegift (G01Q05): hard = yes = 1; services_fit (G02Q02): yes = 1;
# gender (G03Q13): male = 1; student (G03Q14): yes = 1;

dummy_var <- c('situation_fit','last_minute', 'remb_bday', 'online_gift', 
               'suitablegift','services_fit', 'gender', 'student')
surveydata <- dummy_cols(surveydata, select_columns = dummy_var, remove_selected_columns = TRUE)

# delete douplicated columns
colnames(surveydata)
surveydata1 <- subset(surveydata, select = - c(situation_fit_No, remb_bday_No,online_gift_No, 
                                                suitablegift_No, services_fit_No, 
                                                gender_Female, student_No))
# problem with deleting 'last_minute_In advance', because of '.. ..' in it 


# 03 - Descriptive analysis ##############


# different gift items (G02Q07)
items <- c('Seads', 'Spices', 'Chocolate, Pralines', 'Coffee / Tea', 'Food / sweats in general', 'Specialties from the whole country / EU / world', 'Flowers', 'Candles', 'Cosmetics', 'Alcohol')
labels <- c('Seads', 'Spices', 'Chocolate,\nPralines', 'Coffee /\nTea', 'Food / sweats\nin general', 'Specialties from\nthe whole country /\nEU / world', 'Flowers', 'Candles', 'Cosmetics', 'Alcohol')

giftitems <- reshape(surveydata[15:24], times = items, timevar = "gift", direction = "long", 
                     varying = 1:10, v.names = "rating")
rownames(giftitems) <- 1:nrow(giftitems)
giftitems$gift <- as.factor(giftitems$gift)

itemsplot <- ggplot(giftitems) +
  theme_classic() +
  geom_boxplot(aes(x = gift, y = rating)) +
  ylab("Rating (1 to 5)") +
  xlab("") +
  ggtitle("G02Q07 Popularity of gift box items") +
  scale_x_discrete(labels= labels)
itemsplot



# Format yes/no answer as Factor Variable
surveydata$G01Q04 <- as.factor(surveydata$G01Q04)


# Show percentage of the two answers
summary(surveydata$G01Q04)/nrow(surveydata)

# plot a simple bar diagram
plot <- ggplot(surveydata) +
  geom_bar(aes(x = G01Q04)) +
  ylab("Number of respondents") +
  xlab("Answer") +
  ggtitle("G01Q04 Familiarity of problem situation") +
  coord_flip()
plot
