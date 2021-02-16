library(readxl)
library(ggthemes)
library(ggscatter)
library(hrbrthemes)
library(viridis)
library(corrplot)
library(psych)
library(ellipse)
library(caret)
library(ggthemes)
library(effects)
library(tidyverse)
library(RColorBrewer)
library(ggpubr)

# ggplotRegression <- function (fit, lineCol) {
#   ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
#     geom_point(shape=21, color="#273443", fill="#1EBEA5", position = 'jitter') +
#     stat_smooth(method = "lm", col = lineCol,) +
#     labs(caption =  paste("R2 = ",signif(summary(fit)$r.squared, 3),
#                           "Intercept =",signif(fit$coef[[1]],2),
#                           " Slope =",signif(fit$coef[[2]], 5),
#                           " P =",signif(summary(fit)$coef[2,4], 2)))
# }

ggplotRegression <- function (fit, lineCol) {
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_bar(color="#273443", fill="#1EBEA5", position = 'jitter') +
    stat_smooth(method = "lm", col = lineCol,) +
    labs(caption =  paste("R2 = ",signif(summary(fit)$r.squared, 3),
                          "Intercept =",signif(fit$coef[[1]],2),
                          " Slope =",signif(fit$coef[[2]], 5),
                          " P =",signif(summary(fit)$coef[2,4], 2)))
}

setwd("~/MSBA/MOD3/Hilton")
HiltonUS <- read_excel("HiltonUSEmployeeDataAll.xlsx")

hiltonsm <-  HiltonUS[,c('FullTimePartTime','HotelBrand','Generation','HotelBrand','Tenure','WorkLifeBalance','Communication','RewardsBenefits','LearningDevelopment','Voice','Teamwork','WorkEnvironment','IntentToStayDichotomous')]
tidy2pt <- hiltonsm %>% pivot_longer(c('WorkLifeBalance','Communication','RewardsBenefits','LearningDevelopment','Voice','Teamwork','WorkEnvironment'), names_to='categories', values_to='rating' )


generation_names <- c(
  '1'="Millennial",
  '2'="Gen X",
  '3'="Baby Boomer"
)

ftpt_names <- c(
  '0'="Full Time",
  '1'="Part Time"
)

tenure_labels <- c("4" = "< 12mo", 
                   "5" = "1-2 Years",
                   "6" = "2-5 Years",
                   "7" = "5-10 Years",
                   "8" = "10+ Years"
)

metric_labels <- c("Communication" = "Communication", 
                   "LearningDevelopment" = "Development",
                   "RewardsBenefits" = "Benefits",
                   "Teamwork" = "Teamwork",
                   "Voice" = "Voice",
                   "WorkEnvironment" = "Work Env",
                   "WorkLifeBalance" = "WorkLifeBal"
)

intent_labels <- c("0" = "Intend to Leave", "1" = "Intend to Stay")

# Survey responses by category/intent to say (yyes/no)
ggplot(tidy2pt, aes(x=rating,color=categories, fill=categories)) +
  geom_density(alpha=0.5, adjust=5,show.legend = FALSE) + 
  facet_grid(IntentToStayDichotomous~categories, labeller = labeller(IntentToStayDichotomous = intent_labels,categories  = metric_labels))+
  labs(title="Survey Response by Intent",x="Survey Response", y = "") +  theme_tufte(base_size = 13)


ggplot(tidy2pt, aes(x=rating,color=categories, fill=categories)) +
  geom_bar(aes(y=..count.., fill=categories),show.legend = FALSE)+ 
  facet_grid(IntentToStayDichotomous~categories, labeller = labeller(IntentToStayDichotomous = intent_labels,categories  = metric_labels)) +
  labs(title="Survey Response by Intent",x="Survey Response", y = "")+ theme_tufte(base_size = 13)


# Same thing without facets
ggplot(tidy2pt, aes(x=rating,color=factor(IntentToStayDichotomous), fill=factor(IntentToStayDichotomous))) +
  geom_density(alpha=0.6, adjust=3) + 
  facet_wrap(~categories, labeller = labeller(categories  = metric_labels), nrow=2) +
  labs(title="Survey Response by Intent",x="Survey Response", y = "") +  theme_tufte(base_size = 13) +
  labs(fill="Intent:",color="Intent:") +  
  scale_fill_discrete(labels = c("Leave", "Stay"))+ scale_color_discrete(labels = c("Leave", "Stay"))


ggplot(tidy2pt, aes(x=rating,color=factor(IntentToStayDichotomous), fill=factor(IntentToStayDichotomous))) +
  geom_bar(alpha=.8,position="dodge")+   
  facet_wrap(~categories, labeller = labeller(categories  = metric_labels), nrow=2) +
  labs(title="Survey Response by Intent",x="Survey Response", y = "Count")+ theme_tufte(base_size = 13) +
  labs(fill="Intent:",color="Intent:") +
  scale_fill_discrete(labels = c("Leave", "Stay"))+ scale_color_discrete(labels = c("Leave", "Stay"))


############################################################################################################## 

hiltonsm <-  HiltonUS[,c('JobSatisfaction','HotelChainScale','HotelBrand','IntentToStayDichotomous','Department','FullTimePartTime','HotelBrand','Generation','HotelBrand','Tenure','WorkLifeBalance','Communication','RewardsBenefits','LearningDevelopment','Voice','Teamwork','WorkEnvironment','IntentToStayFivePoint')]
tidy_gen <- hiltonsm %>% pivot_longer(c('JobSatisfaction','WorkLifeBalance','Communication','RewardsBenefits','LearningDevelopment','Voice','Teamwork','WorkEnvironment'), names_to='categories', values_to='rating' )

# Histograms and Density plots by intent and survey responses..

# Hist dichotomous all
ggplot(tidy_gen, aes(factor(IntentToStayDichotomous), fill=factor(IntentToStayDichotomous)))+
  geom_histogram(stat = 'Count', show.legend = FALSE) + 
  labs(title="Intent to Stay (All)", x="", y="") +  theme_tufte(base_size = 13) + scale_fill_brewer(palette="Set1") +
  scale_x_discrete(labels=c("0" = "Intend to Leave", "1" = "Intend to Stay"))

ggplot(tidy_gen, aes(factor(IntentToStayFivePoint)))+
  geom_histogram(stat = 'Count', show.legend = FALSE, fill='#002C51') + 
  labs(title="Intent to Stay 5 pt", x="", y="") +  theme_tufte(base_size = 13) 

# Hist by Tenure
tidy_ten <- tidy_gen
# Set levels to 4, 5, 6, 7, 8
tidy_ten <- tidy_ten %>% 
  mutate(Tenure = case_when(Tenure <= 3 ~ '4', TRUE ~ as.character(tidy_gen$Tenure)))

tidy_ten$Tenure <- factor(tidy_ten$Tenure, labels = 
                            c("< 12mo", "1-2 Years","2-5 Years","5-10 Years","10+ Years"))

ggplot(tidy_ten, aes(factor(IntentToStayDichotomous), fill=factor(IntentToStayDichotomous)))+
  geom_histogram(stat = 'count', show.legend = FALSE) + 
  facet_wrap(~Tenure) +
  scale_x_discrete(labels=c("0" = "Leave", "1" = "Stay")) +
  labs(title="Intent by Tenure", x="",y="") +  theme_tufte(base_size = 13) + scale_fill_brewer(palette="Set1")


ggplot(tidy_ten, aes(factor(IntentToStayFivePoint), fill=Tenure))+
  geom_bar(stat = 'count',position="dodge") + 
  labs(title="Intent by Tenure", x="",y="", fill="Tenure", color="Tenure") +  
  theme_tufte(base_size = 13) + scale_fill_brewer(palette="Set1")

##
ggplot(tidy_ten, aes(x=IntentToStayFivePoint,fill=Tenure))+
  geom_density(adjust=5, alpha=.2) + 
  labs(title="Intent by Tenure", x="Intent to Stay",y="", fill="Tenure") +  
  theme_tufte(base_size = 13) + scale_fill_brewer(palette="Set1")
## 
ggplot(tidy_ten, aes(IntentToStayFivePoint, fill=Tenure))+
  geom_bar(alpha=.8, position="dodge") +
  labs(title="Intent by Tenure", x="Intent to Stay", fill="Tenure") +
  theme_tufte(base_size = 13) + scale_fill_brewer(palette="Set1")

# Categories by HotelChainScale
tidy_gen$HotelChainScale <- factor(tidy_gen$HotelChainScale, labels = c("Luxury", "Upper Midscale","Upper Upscale","Upscale","RicksHotel"))
ggplot(tidy_gen, aes(x=rating, color=HotelChainScale, fill=HotelChainScale)) +
  geom_bar(alpha=0.8, position="dodge")  +  facet_wrap(~categories) + 
  labs(title="Rating by HotelChainScale",x="Rating", y = "Density") +  theme_tufte(base_size = 13)

ggplot(tidy_gen, aes(x=rating, color=HotelChainScale, fill=HotelChainScale)) +
  geom_density(alpha=0.05, adjust=4)  +  facet_wrap(~categories) + 
  labs(title="Rating by HotelChainScale",x="Rating", y = "Density") +  theme_tufte(base_size = 13)

ggplot(tidy_gen, aes(x=IntentToStayFivePoint, color=HotelChainScale, fill=HotelChainScale)) + geom_density(alpha=0.1, adjust=5) + 
  labs(title="Intent to stay by HotelChainScale",x="Intent to Stay", y = "Density") +  theme_tufte(base_size = 13)

ggplot(tidy_gen, aes(factor(IntentToStayDichotomous), fill=factor(IntentToStayDichotomous)))+
  geom_bar(stat = 'count', show.legend = FALSE) + 
  facet_wrap(~HotelChainScale, labeller = labeller(Tenure = generation_names)) +
  scale_x_discrete(labels=c("0" = "Leave", "1" = "Stay")) +
  labs(title="Intent by HotelChainScale", x="",y="") +  theme_tufte(base_size = 13) + scale_fill_brewer(palette="Set1")

ggplot(tidy_gen, aes(factor(IntentToStayFivePoint), fill=HotelChainScale))+
  geom_bar(stat = 'count', show.legend = FALSE) +
  facet_wrap(~HotelChainScale, labeller = labeller(Tenure = generation_names)) +
  labs(title="Intent by HotelChainScale 5pt", x="",y="") +  theme_tufte(base_size = 13) + scale_fill_brewer(palette="Set1")

ggplot(tidy_gen, aes(factor(IntentToStayFivePoint), fill=HotelChainScale))+
  geom_bar(stat = 'count',position="dodge") + 
  labs(title="Intent to Stay by HotelChainScale", x="Intent to Stay",y="") +  
  theme_tufte(base_size = 13) + scale_fill_brewer(palette="Set1")

ggplot(tidy_gen, aes(factor(IntentToStayDichotomous), fill=HotelChainScale))+
  geom_bar(stat = 'count',position="dodge") + 
  labs(title="Intent to Stay by HotelChainScale", x="Intent to Stay",y="") +  
  theme_tufte(base_size = 13) + scale_fill_brewer(palette="Set1")

ggplot(tidy_gen, aes(factor(IntentToStayDichotomous), fill=HotelChainScale))+
  geom_bar(stat = 'count',position="dodge") + 
  facet_wrap(~HotelChainScale) +
  labs(title="Intent to Stay by HotelChainScale", x="Intent to Stay",y="") +  
  theme_tufte(base_size = 13) + scale_fill_brewer(palette="Set1")


# Categories by HotelBrand
tidy_gen$HotelBrand <- factor(tidy_gen$HotelBrand, 
        labels = c("Conrad", "DoubleTree","Hampton Inn",
                   "Hilton", "Hilton Garden Inn","Waldorf Astoria",
                   "Curio", "Embassy Suites","Home2 Suites","Homewood Suites","Ricks Hotel"))
ggplot(tidy_gen, aes(x=rating, color=HotelBrand, fill=HotelBrand)) +
  geom_bar(alpha=0.8, position="dodge")  +  facet_wrap(~categories) +
  labs(title="Rating by HotelBrand",x="Rating", y = "Density") +  theme_tufte(base_size = 13)

ggplot(tidy_gen, aes(x=rating, color=HotelBrand, fill=HotelBrand)) +
  geom_density(alpha=0.05, alpha=4)  +  facet_wrap(~categories) +
  labs(title="Rating by HotelBrand",x="Rating", y = "Density") +  theme_tufte(base_size = 13)

ggplot(tidy_gen, aes(x=IntentToStayFivePoint, color=HotelBrand, fill=HotelBrand)) + geom_density(alpha=0.1, adjust=5) +
  labs(title="Intent to stay by HotelBrand",x="Intent to Stay", y = "Density") +  theme_tufte(base_size = 13)

ggplot(tidy_gen, aes(factor(IntentToStayDichotomous), fill=factor(IntentToStayDichotomous)))+
  geom_bar(stat = 'count', show.legend = FALSE) +
  facet_wrap(~HotelBrand, labeller = labeller(Tenure = generation_names)) +
  scale_x_discrete(labels=c("0" = "Leave", "1" = "Stay")) +
  labs(title="Intent by HotelBrand", x="",y="") +  theme_tufte(base_size = 13) + scale_fill_brewer(palette="Set1")


tidygen2 <- tidy_gen %>% group_by(HotelBrand) %>%   mutate(count_name_occurr = n())
tidygen2 <- 

  
tidygen2 <-ungroup(tidygen2)
ggplot(tidygen2, aes(x=reorder(HotelBrand, -count_name_occurr), fill=factor(IntentToStayDichotomous))) +
  geom_bar( position="dodge") +
  labs(title="Intent by HotelBrand", x="",y="", fill="Intent to Stay") +  
  theme_tufte(base_size = 13) + scale_fill_brewer(palette="Set1") +
  theme(axis.text.x = element_text(face = "bold", color = "black", size = 12, angle = 45, hjust=1))
  
  
tidygen2 <-ungroup(tidygen2)
ggplot(tidygen2, aes(x=HotelBrand, fill=factor(IntentToStayDichotomous))) +
  geom_bar( position="dodge") 
  labs(title="Intent by HotelBrand", x="Hotel Brand",y="") +  
  theme_tufte(base_size = 13) + scale_fill_brewer(palette="Set1")
#
  
  
ggplot(tidy_gen, aes(factor(IntentToStayFivePoint), fill=HotelBrand))+
  geom_bar(stat = 'count', show.legend = FALSE) +
  facet_wrap(~HotelBrand, labeller = labeller(Tenure = generation_names)) +
  labs(title="Intent by HotelBrand 5pt", x="",y="") +  theme_tufte(base_size = 13) + scale_fill_brewer(palette="Paired")

ggplot(tidy_gen, aes(factor(IntentToStayFivePoint), fill=HotelBrand))+
  geom_bar(stat = 'count',position="dodge") +
  labs(title="Intent to Stay by HotelBrand", x="Intent to Stay",y="") +
  theme_tufte(base_size = 13) + scale_fill_brewer(palette="Paired")

# Categories by generation 
tidy_gen$Generation <- factor(tidy_gen$Generation, labels = c("Millennial", "Gen X","Boomer"))
ggplot(tidy_gen, aes(x=rating, color=Generation, fill=Generation)) +
  geom_bar(alpha=0.8, position="dodge")  +  facet_wrap(~categories) + 
  labs(title="Rating by Generation",x="Rating", y = "Density") +  theme_tufte(base_size = 13)

ggplot(tidy_gen, aes(x=rating, color=Generation, fill=Generation)) +
  geom_density(alpha=0.05)  +  facet_wrap(~categories) + 
  labs(title="Rating by Generation",x="Rating", y = "Density") +  theme_tufte(base_size = 13)

ggplot(tidy_gen, aes(x=IntentToStayFivePoint, color=Generation, fill=Generation)) + geom_density(alpha=0.1, adjust=5) + 
  labs(title="Intent to stay by Generation",x="Intent to Stay", y = "Density") +  theme_tufte(base_size = 13)

ggplot(tidy_gen, aes(factor(IntentToStayDichotomous), fill=factor(IntentToStayDichotomous)))+
  geom_bar(stat = 'count', show.legend = FALSE) + 
  facet_wrap(~Generation, labeller = labeller(Tenure = generation_names)) +
  scale_x_discrete(labels=c("0" = "Leave", "1" = "Stay")) +
  labs(title="Intent by Generation", x="",y="") +  theme_tufte(base_size = 13) + scale_fill_brewer(palette="Set1")

ggplot(tidy_gen, aes(factor(IntentToStayFivePoint), fill=Generation))+
  geom_bar(stat = 'count', show.legend = FALSE) +
  facet_wrap(~Generation, labeller = labeller(Tenure = generation_names)) +
  labs(title="Intent by Generation 5pt", x="",y="") +  theme_tufte(base_size = 13) + scale_fill_brewer(palette="Set1")

ggplot(tidy_gen, aes(factor(IntentToStayFivePoint), fill=Generation))+
  geom_bar(stat = 'count',position="dodge") + 
  labs(title="Intent to Stay by Generation", x="Intent to Stay",y="") +  
  theme_tufte(base_size = 13) + scale_fill_brewer(palette="Set1")

#Categories by FTPT
tidy_gen$FullTimePartTime2 <- factor(tidy_gen$FullTimePartTime, labels = c("Part Time", "Full Time"))
ggplot(tidy_gen, aes(x=rating, color=FullTimePartTime2, fill=FullTimePartTime2)) +
  geom_density(alpha=0.2) + facet_wrap(~categories) + 
  labs(title="Rating by FT/PT",x="Survey Response", y = "Density", fill="Shift Type",color="Shift Type") +  
  theme_tufte(base_size = 13)

ggplot(tidy_gen, aes(x=IntentToStayFivePoint, color=FullTimePartTime2, 
                     fill=FullTimePartTime2)) +
  geom_bar(alpha=0.8, position="dodge") + 
  labs(title="Intent to Stay by FT/PT",
       x="Intent to Stay", y = "Density", fill="Position:", color="Position:") +  
  theme_tufte(base_size = 13)+ scale_fill_brewer(palette="Set1")

ggplot(tidy_gen, aes(factor(IntentToStayDichotomous), fill=factor(IntentToStayDichotomous)))+
  geom_bar(stat = 'count', show.legend = FALSE) + 
  facet_wrap(~FullTimePartTime2, labeller = labeller(Tenure = ftpt_names)) +
  scale_x_discrete(labels=c("0" = "Leave", "1" = "Stay")) +
  labs(title="Intent by Full Time/Part Time", x="",y="") +  theme_tufte(base_size = 13) + scale_fill_brewer(palette="Set1")

ggplot(tidy_gen, aes(factor(IntentToStayFivePoint), fill=factor(FullTimePartTime2)))+
  geom_bar(stat = 'count',position="dodge") + 
  scale_x_discrete(labels=c("0" = "Leave", "1" = "Stay")) +
  labs(title="Intent by Full Time/Part Time", x="",y="") +  theme_tufte(base_size = 13) + scale_fill_brewer(palette="Set1")

# Department density 2pt
x <- HiltonUS %>% group_by(Department) %>% 
  summarise(count)
x <- HiltonUS %>% group_by(Department) %>% 
  summarize(avgIntent = mean(IntentToStayDichotomous), num=n()) %>% 
  arrange(avgIntent)
x <-  x %>% 
  mutate(Department = case_when(Department == 14 ~ 'Cashiers',
                                Department == 1 ~ 'Catering',
                                Department == 2 ~ 'Exec Office',
                                Department == 15 ~ 'Finance',
                                Department %in% c(12,13,34,35,36,37) ~ 'Kitchen',
                                Department %in% c(5,6,7,8,9,32,10) ~ 'Food/Beverage',
                                Department == 3 ~ 'Casino',
                                Department == 16 ~ 'Front Off',
                                Department == 17 ~ 'Admin/IT',
                                Department == 18 ~ 'Golf',
                                Department == 19 ~ 'Guest Activ',
                                Department == 20 ~ 'Guest Services',
                                Department == 33 ~ 'Guest Services',
                                Department == 21 ~ 'House Keeping',
                                Department == 22 ~ 'Human Res.',
                                Department == 23 ~ 'House Keeping',
                                Department == 24 ~ 'Conventions',
                                Department == 25 ~ 'Engineering',
                                Department == 26 ~ 'Purchasing',
                                Department == 27 ~ 'Reservations',
                                Department == 28 ~ 'Sales',
                                Department == 29 ~ 'Security',
                                Department == 30 ~ 'Spa',
                                Department == 31 ~ 'Telecomm',
                                Department == 38 ~ 'Dep1',
                                Department == 39 ~ 'Dep2',
                                Department == 40 ~ 'Dep3',
                                Department == 41 ~ 'Dep4',
                                Department == 42 ~ 'Dep5',
                                TRUE ~ as.character(x$Department)))
tidyDep <-  x %>% group_by(Department) %>% 
  summarise(intentByDep=mean(avgIntent), total=sum(num)) %>% 
  arrange(intentByDep)
ggplot(data=tidyDep, aes(x=reorder(Department, -intentByDep), y=intentByDep)) + 
  geom_col(show.legend = FALSE, fill="#993333") +
  labs(title="Average Intent to Stay by Dept (Dichotomous)", x="",y="Avg Intent to Stay (0/1)") +  theme_tufte(base_size = 13) + 
  theme(axis.text.x = element_text(face = "bold", color = "black", size = 12, angle = 45, hjust=1))+
  scale_color_brewer(palette = "Spectral")+ 
  geom_label(aes(x=Department, y=intentByDep, label=total), hjust = 0, show.legend = FALSE,
              nudge_x = -.5, nudge_y = .05, size = 2.5, fontface = "italic")


y <- HiltonUS %>% group_by(Department) %>% 
  summarize(avgIntent = mean(IntentToStayFivePoint), num=n()) %>% 
  arrange(avgIntent)

  
y <-  y %>% 
  mutate(Department = case_when(Department == 14 ~ 'Cashiers',
                                Department == 1 ~ 'Catering',
                                Department == 2 ~ 'Exec Office',
                                Department == 15 ~ 'Finance',
                                Department %in% c(12,13,34,35,36,37) ~ 'Kitchen',
                                Department %in% c(5,6,7,8,9,32,10) ~ 'Food/Beverage',
                                Department == 3 ~ 'Casino',
                                Department == 16 ~ 'Front Off',
                                Department == 17 ~ 'Admin/IT',
                                Department == 18 ~ 'Golf',
                                Department == 19 ~ 'Guest Activ',
                                Department == 20 ~ 'Guest Services',
                                Department == 33 ~ 'Guest Services',
                                Department == 21 ~ 'House Keeping',
                                Department == 22 ~ 'Human Res.',
                                Department == 23 ~ 'House Keeping',
                                Department == 24 ~ 'Conventions',
                                Department == 25 ~ 'Engineering',
                                Department == 26 ~ 'Purchasing',
                                Department == 27 ~ 'Reservations',
                                Department == 28 ~ 'Sales',
                                Department == 29 ~ 'Security',
                                Department == 30 ~ 'Spa',
                                Department == 31 ~ 'Telecomm',
                                Department == 38 ~ 'Dep1',
                                Department == 39 ~ 'Dep2',
                                Department == 40 ~ 'Dep3',
                                Department == 41 ~ 'Dep4',
                                Department == 42 ~ 'Dep5',
                                TRUE ~ as.character(x$Department)))
tidyDep <-  y %>% group_by(Department) %>% 
  summarise(intentByDep=mean(avgIntent), total=sum(num)) %>% 
  arrange(intentByDep)
ggplot(data=tidyDep, aes(x=reorder(Department, -intentByDep), y=intentByDep)) + 
  geom_col(show.legend = FALSE, fill="#993333") +
  labs(title="Average Intent to Stay by Dept (5pt Scale)", x="",y="Avg Intent to Stay (0-5)") +  theme_tufte(base_size = 13) + 
  theme(axis.text.x = element_text(face = "bold", color = "black", size = 12, angle = 45, hjust=1))+
  scale_color_brewer(palette = "Spectral")+ 
  geom_label(aes(x=Department, y=intentByDep, label=total), hjust = 0, show.legend = FALSE,
             nudge_x = -.5, nudge_y = .05, size = 2.5, fontface = "italic")

#################################################################################




# tidy_gen1 <- tidy_gen
# tidy_gen1$IntentToStayDichotomous <-  as.factor(tidy_gen1$IntentToStayDichotomous)
# 
# ggplot(tidy_gen1, aes(x=rating, color=IntentToStayDichotomous, fill=IntentToStayDichotomous)) +
#   geom_density(alpha=0.2, adjust=3) +  facet_wrap(~categories) + 
#   labs(title="Survey Response by Intent",x="Response", y = "Density", 
#        fill="Intend To Stay:", color="Intend To Stay:") +
#   theme_tufte(base_size = 13)
#   


HiltonUS %>%
  ggplot(aes(factor(WorkLifeBalance), IntentToStayDichotomous, fill=factor(WorkLifeBalance))) + 
  geom_violin() + scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() + labs(x = "Work Life Balance") +
  theme(legend.position="none",plot.title = element_text(size=11)) +ggtitle("Violin chart") 
HiltonUS %>%
  ggplot(aes(factor(Generation), IntentToStayFivePoint, fill=factor(Generation))) + 
  geom_violin() + scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() + labs(x = "Generation") +
  theme(legend.position="none",plot.title = element_text(size=11)) +ggtitle("Violin chart") 

HiltonUS %>%
  ggplot(aes(factor(Tenure), IntentToStayFivePoint, fill=factor(Tenure))) + 
  geom_violin() + scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() + labs(x = "Tenure") +
  theme(legend.position="none",plot.title = element_text(size=11)) +ggtitle("Violin chart") 

HiltonUS %>%
  ggplot(aes(factor(Department), IntentToStayFivePoint, fill=factor(Department))) + 
  geom_violin() + scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() + labs(x = "Department") +
  theme(legend.position="none",plot.title = element_text(size=11)) +ggtitle("Violin chart") 

HiltonUS %>%
  ggplot(aes(factor(HotelBrand), IntentToStayFivePoint, fill=factor(HotelBrand))) + 
  geom_violin() + scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() + labs(x = "HotelBrand") +
  theme(legend.position="none",plot.title = element_text(size=11)) +ggtitle("Violin chart") 

HiltonUS %>%
  ggplot(aes(factor(FullTimePartTime), IntentToStayFivePoint, fill=factor(FullTimePartTime))) + 
  geom_violin() + scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() + labs(x = "FullTimePartTime") +
  theme(legend.position="none",plot.title = element_text(size=11)) +ggtitle("Violin chart")

HiltonUS %>%
  ggplot(aes(factor(ManagementLevel), IntentToStayFivePoint, fill=factor(ManagementLevel))) + 
  geom_violin() + scale_fill_viridis(discrete = TRUE, alpha=0.6, option="A") +
  theme_ipsum() + labs(xlab(label = "Work Life Balance")) +
  theme(legend.position="none",plot.title = element_text(size=11)) +ggtitle("Violin chart")


#############################################################################################

#Start linear models
hilton_tidy <- hiltonsm %>% pivot_longer(c('JobSatisfaction','WorkLifeBalance','Communication','RewardsBenefits','LearningDevelopment','Voice','Teamwork','WorkEnvironment'), names_to='categories', values_to='rating' )

hilton_tidy %>% filter(categories != 'WorkLifeBalance' & categories != 'Voice' & categories != 'RewardsBenefits' &categories != 'JobSatisfaction') %>% 
  ggplot(aes(x=rating, y=IntentToStayFivePoint, group=categories, color=categories)) + 
  geom_smooth(formula =y ~ s(x, bs = "cs"))  +
  labs(color="Category Ratings", title = "Generalized Additive Model (GAM)", caption = 'formula =y ~ s(x, bs = "cs")')


hilton_tidy %>% filter(categories != 'WorkLifeBalance' & categories != 'Voice' & categories != 'RewardsBenefits' ) %>% 
  ggplot(aes(x=rating, y=IntentToStayFivePoint, group=categories, color=categories)) + 
  geom_smooth(method = 'lm')  + 
  labs(color="Category Ratings", title = "Linear Regression Lines")


fit <- lm(IntentToStayFivePoint ~ rating, data= filter(hilton_tidy,categories=='WorkEnvironment'))
a <- ggplot(filter(hilton_tidy,categories=='WorkEnvironment'), aes(x=rating, y=IntentToStayFivePoint)) +
  geom_density_2d_filled(contour_var = 'density') +guides( guide_legend(nrow = 1)) +
  stat_smooth(method = "lm", col = "white", size=1)+ 
  geom_density_2d(color='white', alpha=.1) +
  labs(title="Work Environment Regression",y="Intent to Stay",x="Work Environment",
       caption =  paste("R2 = ",signif(summary(fit)$r.squared, 3),
                        "Intercept =",signif(fit$coef[[1]],2),
                        " Slope =",signif(fit$coef[[2]], 5),
                        " P =",signif(summary(fit)$coef[2,4], 2)))+  theme_tufte(base_size = 13)

fit <- lm(IntentToStayFivePoint ~ rating, data= filter(hilton_tidy,categories=='RewardsBenefits'))
b <- ggplot(filter(hilton_tidy,categories=='RewardsBenefits'), aes(x=rating, y=IntentToStayFivePoint)) +
  geom_density_2d_filled(contour_var = 'density') +guides( guide_legend(nrow = 1)) +
  stat_smooth(method = "lm", col = "white", size=1)+ 
  geom_density_2d(color='white', alpha=.1) +
  labs(title="Rewards Benefits Regression",y="Intent to Stay",x="Rewards/Benefits",
       caption =  paste("R2 = ",signif(summary(fit)$r.squared, 3),
                       "Intercept =",signif(fit$coef[[1]],2),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 2)))+  theme_tufte(base_size = 13)

fit <- lm(IntentToStayFivePoint ~ rating, data= filter(hilton_tidy,categories=='JobSatisfaction'))
c <- ggplot(filter(hilton_tidy,categories=='JobSatisfaction'), aes(x=rating, y=IntentToStayFivePoint)) +
  geom_density_2d_filled(contour_var = 'density') +guides( guide_legend(nrow = 1)) +
  stat_smooth(method = "lm", col = "white", size=1)+ 
  geom_density_2d(color='white', alpha=.1) +
  labs(title="JobSatisfaction Regression",y="Intent to Stay",x="Learning Development",
       caption =  paste("R2 = ",signif(summary(fit)$r.squared, 3),
                        "Intercept =",signif(fit$coef[[1]],2),
                        " Slope =",signif(fit$coef[[2]], 5),
                        " P =",signif(summary(fit)$coef[2,4], 2)))+  theme_tufte(base_size = 13)

fit <- lm(IntentToStayFivePoint ~ rating, data= filter(hilton_tidy,categories=='Teamwork'))
d <- ggplot(filter(hilton_tidy,categories=='Teamwork'), aes(x=rating, y=IntentToStayFivePoint)) +
  geom_density_2d_filled(contour_var = 'density') +guides( guide_legend(nrow = 1)) +
  stat_smooth(method = "lm", col = "white", size=1)+ 
  geom_density_2d(color='white', alpha=.1) +
  labs(title="Teamwork Regression",y="Intent to Stay",x="Teamwork",
       caption =  paste("R2 = ",signif(summary(fit)$r.squared, 3),
                        "Intercept =",signif(fit$coef[[1]],2),
                        " Slope =",signif(fit$coef[[2]], 5),
                        " P =",signif(summary(fit)$coef[2,4], 2)))+  theme_tufte(base_size = 13)


fit <- lm(IntentToStayFivePoint ~ rating, data= filter(hilton_tidy,categories=='Communication'))
e <- ggplot(filter(hilton_tidy,categories=='Communication'), aes(x=rating, y=IntentToStayFivePoint)) +
  geom_density_2d_filled(contour_var = 'density') +guides( guide_legend(nrow = 1)) +
  stat_smooth(method = "lm", col = "white", size=1)+ 
  geom_density_2d(color='white', alpha=.1) +
  labs(title="Learning Development Regression",y="Intent to Stay",x="Communication",
       caption =  paste("R2 = ",signif(summary(fit)$r.squared, 3),
                        "Intercept =",signif(fit$coef[[1]],2),
                        " Slope =",signif(fit$coef[[2]], 5),
                        " P =",signif(summary(fit)$coef[2,4], 2)))+  theme_tufte(base_size = 13)

fit <- lm(IntentToStayFivePoint ~ rating, data= filter(hilton_tidy,categories=='WorkLifeBalance'))
f <- ggplot(filter(hilton_tidy,categories=='WorkLifeBalance'), aes(x=rating, y=IntentToStayFivePoint)) +
  geom_density_2d_filled(contour_var = 'density') +guides( guide_legend(nrow = 1)) +
  stat_smooth(method = "lm", col = "white", size=1)+ 
  geom_density_2d(color='white', alpha=.1) +
  labs(title="Work/Life Balance Regression",y="Intent to Stay",x="Work-Life Balance",
       caption =  paste("R2 = ",signif(summary(fit)$r.squared, 3),
                        "Intercept =",signif(fit$coef[[1]],2),
                        " Slope =",signif(fit$coef[[2]], 5),
                        " P =",signif(summary(fit)$coef[2,4], 2)))+  theme_tufte(base_size = 13)

ggarrange(a, b, c, d,e,f, ncol = 3, nrow = 2, common.legend = TRUE, legend = 'right')
#################################################################################################

ggplot(HiltonUS, aes(x=factor(Generation), y=IntentToStayFivePoint, fill=factor(Generation))) +
  geom_violin(contour_var = 'density') 

dat <- filter(hilton_tidy,categories=='WorkLifeBalance')
ggplot(dat, aes(x=rating, y=IntentToStayFivePoint)) +
  geom_density_2d_filled(contour_var = 'density',show.legend = FALSE) +
  stat_smooth(method = "lm", col = "white", size=1)+ 
   geom_density_2d(color='white', alpha=.1)

    
    geom_point(sample_frac(filter(hilton_tidy,categories=='WorkLifeBalance'), .2), aes(x=rating, y=IntentToStayFivePoint))



##############################################################################################################################


#Oneway ANOVA Check

alcoa$CustomerValueGroup = as.numeric(alcoa$CustomerValueGroup)


alcoa <- as.data.frame(alcoa)
anova_one_way <- aov(CustomerValueGroup ~ pricefle+quality+speed,  data = alcoa)

summary(anova_one_way)
