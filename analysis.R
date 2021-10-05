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
