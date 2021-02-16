## app.R ##
library(shiny)
library(shinydashboard)
library(tidyr)
library(ggplot2)
library(dplyr)
library(readxl)
library(ggthemes)
library(rsconnect)
library(ggpointdensity)
library(ggscatter)
library(hrbrthemes)
library(viridis)
library(ggpmisc)
library(tidyr)
library(plyr)
library(dplyr)
library(ggmap)
library(ggpubr)
library(ggExtra)
setwd("~/MSBA/MOD3/Hilton")
HiltonUS <- read_excel("HiltonUSEmployeeDataAll.xlsx")
HotelData <- read_excel('HiltonUSTotalHotelLevelData.xlsx')

# Perceptual IVs (5-point composite scales):
#   • WorkLifeBalance
# • Communication
# • RewardsBenefits
# • LearningDevelopment
# • Voice
# • TeamWork
# • WorkEnvironment

# Perceptual DVs (5 point scales or dichotomous converted):
#   • Engagement
# • RecommendToWork
# • RecommendToStay
# • JobSatisfaction
# • PaySatisfaction
# • IntentToStayFivePoint
# • IntentToStayDichotomous


jsf_model = lm(IntentToStayFivePoint ~ WorkLifeBalance + RewardsBenefits + WorkEnvironment, data = HiltonUS)
coef(jsf_model)
summary(jsf_model)

