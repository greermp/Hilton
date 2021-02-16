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

library(corrplot)
library(psych)
library(ellipse)
library(caret)
library(readxl)
library(DescTools)
library(effects)
setwd("~/MSBA/MOD3/Hilton")
HiltonUS <- read_excel("HiltonUSEmployeeDataAll.xlsx")

draw_confusion_matrix <- function(cm) {
  #https://stackoverflow.com/questions/23891140/r-how-to-visualize-confusion-matrix-using-the-caret-package
  total <- sum(cm$table)
  res <- as.numeric(cm$table)
  
  # Generate color gradients. Palettes come from RColorBrewer.
  greenPalette <- c("#F7FCF5","#E5F5E0","#C7E9C0","#A1D99B","#74C476","#41AB5D","#238B45","#006D2C","#00441B")
  redPalette <- c("#FFF5F0","#FEE0D2","#FCBBA1","#FC9272","#FB6A4A","#EF3B2C","#CB181D","#A50F15","#67000D")
  getColor <- function (greenOrRed = "green", amount = 0) {
    if (amount == 0)
      return("#FFFFFF")
    palette <- greenPalette
    if (greenOrRed == "red")
      palette <- redPalette
    colorRampPalette(palette)(100)[10 + ceiling(90 * amount / total)]
  }
  
  # set the basic layout
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('HILTON CONFUSION MATRIX', cex.main=2)
  
  # create the matrix 
  classes = colnames(cm$table)
  rect(150, 430, 240, 370, col=getColor("green", res[1]))
  text(195, 435, classes[1], cex=1.2)
  rect(250, 430, 340, 370, col=getColor("red", res[3]))
  text(295, 435, classes[2], cex=1.2)
  text(125, 370, 'Predicted Intent', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual Intent', cex=1.3, font=2)
  rect(150, 305, 240, 365, col=getColor("red", res[2]))
  rect(250, 305, 340, 365, col=getColor("green", res[4]))
  text(140, 400, classes[1], cex=1.2, srt=90)
  text(140, 335, classes[2], cex=1.2, srt=90)
  
  # add in the cm results
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[4], cex=1.6, font=2, col='white')
  
  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
  text(10, 85, names(cm$byClass[1]), cex=1.2, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  text(30, 85, names(cm$byClass[2]), cex=1.2, font=2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  text(50, 85, names(cm$byClass[5]), cex=1.2, font=2)
  text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
  text(70, 85, names(cm$byClass[6]), cex=1.2, font=2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
  text(90, 85, names(cm$byClass[7]), cex=1.2, font=2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)
  
  # add in the accuracy information 
  text(30, 35, names(cm$overall[1]), cex=1.5, font=2)
  text(30, 20, round(as.numeric(cm$overall[1]), 3), cex=1.4)
  text(70, 35, names(cm$overall[2]), cex=1.5, font=2)
  text(70, 20, round(as.numeric(cm$overall[2]), 3), cex=1.4)
}


forLogR = HiltonUS[,c('FullTimePartTime','JobSatisfaction','Generation','HotelBrand','Tenure','WorkLifeBalance','Communication','RewardsBenefits','LearningDevelopment','Voice','Teamwork','WorkEnvironment','IntentToStayFivePoint', 'IntentToStayDichotomous')]

forLogR <- forLogR %>% 
  mutate(Tenure = case_when(Tenure <= 3 ~ '4', TRUE ~ as.character(forLogR$Tenure)))
forLogR$Tenure <- factor(forLogR$Tenure, labels = 
                           c("< 12mo", "1-2 Years","2-5 Years","5-10 Years","10+ Years"))

forLogR$IntentToStayDichotomous <- factor(forLogR$IntentToStayDichotomous, labels = 
                           c("0" = "Intend to Leave", "1" = "Intend to Stay"))

forLogR$Generation <- factor(forLogR$Generation, labels = 
                           c('1'="Millennial", '2'="Gen X",'3'="Baby Boomer"))

forLogR$HotelBrand <- factor(forLogR$HotelBrand, labels = 
                           c("Conrad", "DoubleTree","Hampton Inn",
                             "Hilton", "Hilton Garden Inn","Waldorf Astoria",
                             "Curio", "Embassy Suites","Home2 Suites","Homewood Suites","Ricks Hotel"))
forLogR$FullTimePartTime <- factor(forLogR$FullTimePartTime, labels = 
                                     c("Part Time", "Full Time"))
forLogR$HotelChainScale <- factor(forLogR$HotelChainScale, labels = c("Luxury", "Upper Midscale","Upper Upscale","Upscale","RicksHotel"))

forLogR$IntentToStayFivePoint = as.factor(forLogR$IntentToStayFivePoint)

summary(forLogR)


# Feature Plot - Perceptual
tidy <-  HiltonUS[,c('WorkLifeBalance','JobSatisfaction','Communication','RewardsBenefits','IntentToStayDichotomous')] %>% 
  pivot_longer(c('WorkLifeBalance','JobSatisfaction','Communication','RewardsBenefits'), names_to='categories', values_to='rating' )
tidy$IntentToStayDichotomous <- factor(tidy$IntentToStayDichotomous, labels = 
                                            c("0" = "Intend to Leave", "1" = "Intend to Stay"))
ggplot(tidy, aes(x=rating, fill=IntentToStayDichotomous)) +
  geom_density(alpha=0.5, adjust=5) + 
  facet_wrap(~categories)+ labs(fill="", title="Employee Rated Perceptual IVs") +
  labs(title="Survey Response by Intent",x="Survey Response", y = "Intent to Stay") +  theme_tufte(base_size = 13)

#Feature Plot - Demographic/Categorical
hiltonsm <-  HiltonUS[,c('HotelChainScale','HotelBrand','IntentToStayDichotomous','Department','FullTimePartTime','Generation','Tenure')]
hiltonsm$IntentToStayDichotomous <- factor(hiltonsm$IntentToStayDichotomous, labels = 
                                            c("0" = "Intend to Leave", "1" = "Intend to Stay"))
hiltonsm$Generation <- factor(hiltonsm$Generation, labels = 
                               c('1'="Millennial", '2'="Gen X",'3'="Baby Boomer"))
hiltonsm$HotelBrand <- factor(hiltonsm$HotelBrand, labels = 
                               c("Conrad", "DoubleTree","Hampton Inn",
                                 "Hilton", "Hilton Garden Inn","Waldorf Astoria",
                                 "Curio", "Embassy Suites","Home2 Suites","Homewood Suites","Ricks Hotel"))
hiltonsm$FullTimePartTime <- factor(hiltonsm$FullTimePartTime, labels = 
                                     c("Part Time", "Full Time"))
hiltonsm$HotelChainScale <- factor(hiltonsm$HotelChainScale, labels = c("Luxury", "Upper Midscale","Upper Upscale","Upscale","RicksHotel"))
hiltonsm <- hiltonsm %>% 
  mutate(Tenure = case_when(Tenure <= 3 ~ '4', TRUE ~ as.character(hiltonsm$Tenure)))
hiltonsm$Tenure <- factor(hiltonsm$Tenure, labels = 
                           c("< 12mo", "1-2 Years","2-5 Years","5-10 Years","10+ Years"))

ggplot(hiltonsm, aes(factor(IntentToStayDichotomous), fill=factor(IntentToStayDichotomous)))+
  geom_histogram(stat = 'count', show.legend = FALSE) + 
  facet_wrap(~Tenure) +
  scale_x_discrete(labels=c("0" = "Leave", "1" = "Stay")) +
  labs(title="Intent by Tenure", x="",y="") +  theme_tufte(base_size = 13) + scale_fill_brewer(palette="Set1")


ggplot(hiltonsm, aes(factor(IntentToStayDichotomous), fill=HotelChainScale))+
  geom_bar(stat = 'count',position="dodge") + 
  labs(title="Intent to Stay by HotelChainScale", x="Intent to Stay",y="") +  
  theme_tufte(base_size = 13) + scale_fill_brewer(palette="Set1")


#Correlation plot
correlations <- cor(forLogR[, sapply(forLogR, is.numeric)])
corrplot(correlations, method='number')
corrplot.mixed(correlations, upper='ellipse', tl.pos = 'lt')

#Feature plot / EDA
xAxis <- forLogR[,c('WorkLifeBalance','JobSatisfaction','Communication','RewardsBenefits')]
yAxis <- forLogR$IntentToStayDichotomous
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=xAxis, y=yAxis, plot="density", auto.key = list(columns = 2), scales=scales)
featurePlot(x=xAxis, y=yAxis, plot="box", auto.key = list(columns = 2), scales=scales)
xtabs(~IntentToStayDichotomous + WorkLifeBalance, data = HiltonUS)


obj <- featurePlot(x=forLogR[,c('WorkLifeBalance','Communication','RewardsBenefits')],
                   y = forLogR$IntentToStayDichotomousF,
                   plot="density")

print(obj)
forLogR = HiltonUS[,c('FullTimePartTime','JobSatisfaction','Generation','HotelBrand','Tenure','WorkLifeBalance','Communication','RewardsBenefits','LearningDevelopment','Voice','Teamwork','WorkEnvironment','IntentToStayFivePoint', 'IntentToStayDichotomous')]
# Calculate model
logit <- glm(IntentToStayDichotomous ~ WorkLifeBalance + JobSatisfaction + Communication + RewardsBenefits, data = forLogR, family = binomial(link="logit"))
summary(logit)

#Summary
AIC(logit)
# exp(confint(logit))
# exp(coef(logit))
# Shows exp Odds ratio and confidence interval
exp(cbind(OddsRatio = coef(logit), confint(logit)))
PseudoR2(logit)
# plot(allEffects(logit))
# plot(allEffects(logit), type = "response")

cm <- confusionMatrix(factor(ifelse(predict(logit, type = "response") >= .5, 1, 0)), reference = factor(forLogR$IntentToStayDichotomous))

draw_confusion_matrix(cm)

anova(logit, test="Chisq")

predicted.data <- data.frame(
  probability.of.ITS=logit$fitted.values,
  valGroup=forLogR$IntentToStayDichotomousF)

predicted.data <- predicted.data[
  order(predicted.data$probability.of.ITS, decreasing=FALSE),]

predicted.data$rank <- 1:nrow(predicted.data) 

predicted.data %>% sample_frac(0.1) %>% 
  ggplot(aes(x=rank, y=probability.of.ITS)) +
  geom_point(aes(color=valGroup), alpha=.2, shape=1, stroke=2, size=.5) +
  xlab("Respondants (1-30,000)") +
  ylab("Predicted probability of Intent to Stay") + theme_minimal()+ scale_fill_brewer(palette="Set1")