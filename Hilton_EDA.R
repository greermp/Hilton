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


ggplotRegression <- function (fit, lineCol) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point(shape=21, color="#273443", fill="#1EBEA5", position = 'jitter') +
    stat_smooth(method = "lm", col = lineCol,) +
    labs(caption =  paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                          "Intercept =",signif(fit$coef[[1]],5 ),
                          " Slope =",signif(fit$coef[[2]], 5),
                          " P =",signif(summary(fit)$coef[2,4], 5))) + theme_classic2()
}

ggplotRobustRegression <- function (fit, lineCol) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point(shape=21, color="#273443", fill="#1EBEA5", position = 'jitter') +
    stat_smooth(method = "rlm", col = lineCol,) +
    labs(caption =  paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                          "Intercept =",signif(fit$coef[[1]],5 ),
                          " Slope =",signif(fit$coef[[2]], 5),
                          " P =",signif(summary(fit)$coef[2,4], 5))) + theme_classic2()
}


ggplot(data = HotelData) + 
  geom_bar(aes(x = HotelBrand))
nrow(filter(HotelData, HotelBrand == 10))

str(HotelData)
str(HiltonUS)
levels= c('HotelA','HotelB','HotelC','HotelD','HotelE',
          'HotelF','HotelG','HotelH','HotelI','HotelJ','HotelK')
HiltonUS$HotelName <- factor(HiltonUS$HotelBrand, labels = levels)


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


ggscatter(HiltonUS, x="WorkLifeBalance", y="JobSatisfaction", color="lightgray")  + geom_density_2d() +stat_density_2d(aes(fill=..level..), geom="polygon")+  gradient_fill("YlOrRd")
ggscatter(HiltonUS, x="RewardsBenefits", y="JobSatisfaction", color="lightgray")  + stat_density_2d(aes(fill=..level..), geom="polygon")+  gradient_fill("YlOrRd")
ggscatter(HiltonUS, x="WorkEnvironment", y="JobSatisfaction", color="lightgray")  + geom_density_2d() +stat_density_2d(aes(fill=..level..), geom="polygon")+  gradient_fill("YlOrRd")

jsf_model = lm(JobSatisfaction ~ WorkLifeBalance + RewardsBenefits + WorkEnvironment, data = HiltonUS)
coef(jsf_model)
summary(jsf_model)

# ggscatter(filter(HiltonUS,HotelName=='HotelA'), x = "JobSatisfaction", y = "WorkLifeBalance",
#              color = "HotelName",size = , position = 'jitter', title="Facebook Regard v Privacy", mean.point = TRUE) + geom_density2d() + theme_classic()
ggscatter(filter(HiltonUS,HotelName=='HotelA'), x = "WorkLifeBalance", y = "JobSatisfaction",
          color = "HotelName",size = .5, position = 'jitter', title="Facebook Regard v Privacy", mean.point = TRUE) + theme_classic()
ggscatter(filter(HiltonUS,HotelName=='HotelA'), x = "RewardsBenefits", y = "JobSatisfaction",
          color = "HotelName",size = .5, position = 'jitter', title="Facebook Regard v Privacy", mean.point = TRUE) + theme_classic()
ggscatter(filter(HiltonUS,HotelName=='HotelA'), x = "WorkEnvironment", y = "JobSatisfaction",
          color = "HotelName",size = .5, position = 'jitter', title="Hotel A Environment V Satisfaction", mean.point = TRUE) + theme_classic()



ggplot(HiltonUS, aes(factor(WorkEnviron5), IntentToStayFivePoint)) + 
  geom_violin() 

ggplot(HiltonUS, aes(factor(MgtTrust), IntentToStayFivePoint, color=factor(MgtTrust))) + 
  geom_violin() 



# Perceptual IVs (5-point composite scales):
#   • WorkLifeBalance
# • Communication
# • RewardsBenefits
# • LearningDevelopment
# • Voice
# • TeamWork
# • WorkEnvironment

HiltonUS %>% sample_frac(.01) %>% 
ggplot(aes(x=WorkLifeBalance, y= WorkEnvironment, color=IntentToStayDichotomous)) + 
  geom_point(alpha=.9) + geom_jitter()

HiltonUS %>% sample_frac(.01) %>% 
  ggplot(aes(x=Communication, y= Voice, color=IntentToStayDichotomous)) + 
  geom_point(alpha=.1) + geom_jitter()

HiltonUS %>% sample_frac(.01) %>% 
  ggplot(aes(x=RewardsBenefits, y= IntentToStayFivePoint)) + 
  geom_point(alpha=.9) + geom_jitter()

ggplot(HiltonUS, aes(factor(RewardsBenefits), IntentToStayFivePoint)) + 
  geom_boxplot() 

ggplot(HiltonUS, aes(factor(WorkEnvironment), IntentToStayFivePoint)) + 
  geom_boxplot() 

ggplot(HiltonUS, aes(WorkEnvironment, IntentToStayFivePoint))+geom_hex(binwidth = c(2, 2))

ggplot(HiltonUS, aes(WorkEnvironment, IntentToStayFivePoint))+stat_binhex(binwidth = c(1, 1)) 


ggplot(HiltonUS, aes(WorkEnvironment, IntentToStayFivePoint)) +
  stat_density_2d(aes(fill = stat(density)), geom = 'raster', contour = FALSE) +       
  scale_fill_viridis_c() +
  coord_cartesian(expand = FALSE)



ggplot(HiltonUS, aes(WorkEnvironment, IntentToStayFivePoint)) +
  geom_point(colour="blue", alpha=0.9) +
  geom_density2d(colour="black")

ggplot(HiltonUS, aes(WorkEnvironment, IntentToStayFivePoint)) +geom_jitter()+ geom_pointdensity() + scale_color_viridis_c()

# ggplot(HiltonUS, aes(WorkEnvironment, RewardsBenefits, fill=IntentToStayFivePoint)) +geom_tile()+  scale_fill_gradient(low="red", high="blue") +  theme_ipsum()

# ggplot(HiltonUS, aes(WorkEnvironment, RewardsBenefits, fill=IntentToStayFivePoint)) +geom_tile()+  scale_fill_viridis() +  theme_ipsum()

# ggplot(HiltonUS, aes(WorkEnvironment, RewardsBenefits, fill=IntentToStayFivePoint)) +geom_tile()+    scale_fill_distiller(palette = "RdPu") +  theme_ipsum()


# ggplot(HiltonUS, aes(WorkEnvironment, RewardsBenefits, fill=IntentToStayDichotomous)) +geom_tile()+    scale_fill_distiller(palette = "RdPu") +  theme_ipsum()+
#   geom_density2d()


# ggplot(HiltonUS, aes(WorkEnvironment, RewardsBenefits)) +
#   stat_density2d(aes(fill = stat(level)), geom="polygon") +
#   scale_fill_viridis_c(option = "plasma") +
#   theme(legend.position = "magma")
# 
# ggplot(HiltonUS, aes(WorkEnvironment, IntentToStayFivePoint)) +
#   stat_density2d(aes(fill = stat(level)), geom="polygon") +
#   scale_fill_viridis_c(option = "plasma") +
#   theme(legend.position = "magma")
# 
# ggplot(HiltonUS, aes(WorkEnvironment, RewardsBenefits)) + geom_point(alpha=.005)+geom_jitter() + geom_density_2d_filled(alpha = 0.7, contour_var = 'ndensity') + facet_wrap(vars(IntentToStayFivePoint))
# 
# ggplot(HiltonUS, aes(WorkEnvironment, RewardsBenefits)) + geom_density_2d_filled( alpha = 0.5, contour_var = 'ndensity')+ facet_wrap(vars(IntentToStayDichotomous))
# 
# ggplot(HiltonUS, aes(WorkEnvironment, RewardsBenefits))+ geom_density_2d_filled(alpha = 0.9, contour_var = 'ndensity') + facet_wrap(vars(IntentToStayFivePoint))

ggplot(HiltonUS, aes(x=WorkLifeBalance, y=JobSatisfaction))+ geom_density_2d_filled(alpha = 0.9, contour_var = 'ndensity') + facet_wrap(vars(IntentToStayFivePoint)) 

ggplot(HiltonUS, aes(x=WorkLifeBalance, y=JobSatisfaction))+ geom_density_2d_filled(alpha = 0.9, contour_var = 'ndensity') + facet_wrap(vars(IntentToStayFivePoint)) 

ggplot(HiltonUS, aes(x=Voice, y=Communication))+ geom_density_2d_filled(alpha = 0.9, contour_var = 'ndensity') + facet_wrap(vars(IntentToStayFivePoint))

ggplot(HiltonUS, aes(x=Teamwork, y=LearningDevelopment))+ geom_density_2d_filled(contour_var = 'ndensity') + facet_wrap(vars(IntentToStayFivePoint))

ggplot(HiltonUS, aes(x=Teamwork, y=LearningDevelopment))+ geom_density_2d_filled(contour_var = 'ndensity') + facet_wrap(vars(IntentToStayDichotomous))

ggplot(HiltonUS, aes(x=Teamwork, y=LearningDevelopment)) + stat_density_2d(geom = "point", aes(size = after_stat(density)), n = 75, contour = FALSE)

ggplot(HiltonUS, aes(x=Teamwork, y=LearningDevelopment)) + stat_density_2d(geom = "point", aes(size = after_stat(density)), n = 5, contour = FALSE)+ facet_wrap(vars(IntentToStayDichotomous))
