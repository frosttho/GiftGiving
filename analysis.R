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


# 01 - Read data #########################

# surveydata <- read_csv(file = "results-survey282822_20211004-1449.csv")
surveydata <- read_csv(file = "results-survey282822_20211005-1218.csv")

# delete incomplete answers
surveydata <- filter(surveydata, !is.na(submitdate))





# 02 - Descriptive analysis ##############


# different gift items (G02Q07)
items <- c('Seads', 'Spices', 'Chocolate, Pralines', 'Coffee / Tea', 'Food / sweats in general', 'Specialties from the whole country / EU / world', 'Flowers', 'Candles', 'Cosmetics', 'Alcohol')
labels <- c('Seads', 'Spices', 'Chocolate,\nPralines', 'Coffee /\nTea', 'Food / sweats\nin general', 'Specialties from\nthe whole country /\nEU / world', 'Flowers', 'Candles', 'Cosmetics', 'Alcohol')

giftitems <- reshape(surveydata[15:24], times = items, timevar = "gift", direction = "long", varying = 1:10, v.names = "rating")
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
