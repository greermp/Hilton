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
ggscatter(HiltonUS, x="RewardsBenefits", y="JobSatisfaction", color="lightgray")  + geom_density_2d() +stat_density_2d(aes(fill=..level..), geom="polygon")+  gradient_fill("YlOrRd")
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



ggplot(HiltonUS, aes(x = "WorkLifeBalance", y = "JobSatisfaction")) +
  geom_hex(bins=30) +
  scale_fill_viridis_c() +
  geom_point(shape = '.', col = 'white')

# palette = c("#00AFBB", "#E7B800", "#FC4E07")



ggplot(filter(HiltonUS,HotelName=='HotelA'), aes(x=WorkLifeBalance, y=JobSatisfaction)) + geom_pointdensity() + scale_color_viridis_c() + geom_jitter()

## Data in a data.frame
x1 <- rnorm(n=1E3, sd=2)
x2 <- x1*1.2 + rnorm(n=1E3, sd=2)
df <- data.frame(x1,x2)

## Use densCols() output to get density at each point
x <- densCols(x1,x2, colramp=colorRampPalette(c("black", "white")))
df$dens <- col2rgb(x)[1,] + 1L

## Map densities to colors
cols <-  colorRampPalette(c("#000099", "#00FEFF", "#45FE4F", 
                            "#FCFF00", "#FF9400", "#FF3100"))(256)
df$col <- cols[df$dens]

## Plot it, reordering rows so that densest points are plotted on top
plot(x2~x1, data=df[order(df$dens),], pch=20, col=col, cex=2)