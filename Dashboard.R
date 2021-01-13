## app.R ##
library(shiny)
library(shinydashboard)
library(tidyr)
library(ggplot2)
library(dplyr)
library(readxl)
library(ggthemes)
library(rsconnect)
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody()
)
HiltonUS <- read_excel("HiltonUSEmployeeDataAll.xlsx")
HotelData <- read_excel('HiltonUSTotalHotelLevelData.xlsx')
# ggplot(my_data, aes(x=Year, y = Value, group=Metric)) + 
#   geom_line(aes(color=Metric))+ labs(title = "Oculus Revenue Decision Analysis") 
# continuous_scale('Year', breaks=1)


#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Hitlon Decision Analysis")  
#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))))
    # menuItem("Geographic Survey Results", icon = icon("send",lib='glyphicon'), 
    #          href = "https://rpubs.com/greermp/Regard_by_location")))



frow1 <- fluidRow(
  valueBoxOutput("value1")
  ,valueBoxOutput("value2")
  ,valueBoxOutput("value3")
)
frow2 <- fluidRow( 
  box(
    title = "Oculus Return on Investment with and without Advertising Revenue"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("two_L", height = "300px")
  )
  ,box(
    title = "Units Sold"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("two_R", height = "300px")
  ) 
)
frow3 <- fluidRow( 
  box(
    title = "Revenues - Units and Software Store"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("three_L", height = "300px")
  )
  ,box(
    title = "Investment in R&D, Acquistions, and P&E"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,plotOutput("three_R", height = "300px")
  ) 
)
# combine the two fluid rows to make the body
body <- dashboardBody(frow1, frow2, frow3)

#completing the ui part with dashboardPage
ui <- dashboardPage(title = 'This is my Page title', header, sidebar, body, skin='blue')



# my_data <- read_excel("OculusData_v5_dash.xlsx", sheet = "df")

# create the server functions for the dashboard  
server <- function(input, output) { 
  #some data manipulation to derive the values of KPI boxes
  total.revenue <- round((max(filter(my_data, my_data$Metric=='Total Facebook VR Revenue')$Value) /1000000000), 2)
  total.hardware <- round((max(filter(my_data, my_data$Metric=='Oculus Hardware Revenue')$Value) /1000000000), 2)
  total.software <- round((max(filter(my_data, my_data$Metric=='Oculus Software Revenues')$Value) /1000000), 2)
  #creating the valueBoxOutput content
  output$value1 <- renderValueBox({
    valueBox(
      formatC(total.software, format="f", big.mark=',')
      ,'Total Oculus Store Revenues by 2025 (Millions)'
      ,icon = icon("stats",lib='glyphicon')
      ,color = "purple")  
  })
  output$value2 <- renderValueBox({ 
    valueBox(
      formatC(total.revenue, format="f", big.mark=',')
      ,'Total Expected Revenue from Oculus by 2025 (Billions)'
      ,icon = icon("facebook-square",lib='font-awesome')
      ,color = "green")  
  })
  output$value3 <- renderValueBox({
    valueBox(
      formatC(total.hardware, format="f", big.mark=',')
      ,'Total Oculus Hardware Revenue by 2025 (Billions)'
      ,icon = icon("dollar-sign",lib='font-awesome')
      ,color = "yellow")   
  })
  #creating the plotOutput content
  output$two_L <- renderPlot({
    x <- filter(my_data, Metric =="with 1% Boost to Advertising" | Metric =="with Boost to Advertising as % of Oculus Users"| Metric =="Return after CAPEX and R&D")
    ggplot(x, aes(x=Year, y = Value,size=10, group=Metric)) + 
      geom_line(aes(color=Metric, alpha=0.9))+ 
      labs(title = "Oculus Revenue Decision Analysis") +
      ylab("$ US Billion") + 
      xlab("Year") + theme(legend.position="bottom" 
                           ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("Oculus Return on Investment Breakdown") + theme_classic()+ scale_y_continuous(labels = scales::number_format(accuracy = 0.01, decimal.mark = ','))
    #   + scale_x_date(limits = c(min, max))
    # scale_y_continuous(name, breaks, labels, limits, trans)
    # 
    #     
    #     
    #     
    #     
    #     
    #     ggplot(data = recommendation, 
    #            aes(x=Product, y=Revenue, fill=factor(Region))) + 
    #       geom_bar(position = "dodge", stat = "identity") 
  })
  scaleFUN <- function(x) sprintf("%.2f", x)
  output$two_R <- renderPlot({
    x <- filter(my_data, Metric =="Headsets Sold")
    
    ggplot(data = x, aes(x=Year, y=Value)) + 
      geom_bar(position = "dodge", stat = "identity", color='#3B5998', fill='#ADB9D3') + ylab("US Billions)") + 
      xlab("Account") + theme(legend.position="bottom" 
                              ,plot.title = element_text(size=15, face="bold", color='#3B5998')) + 
      ggtitle("Oculus Units Sold by Year")+ theme_excel_new() +  scale_x_continuous(breaks = seq(2016, 2025, 1))+ scale_y_continuous(labels = scales::number_format(accuracy = 0.01, decimal.mark = ','))
  })
  
  output$three_L <- renderPlot({
    x <- filter(my_data, Metric =="Headsets Sold")
    x <- filter(my_data, Metric =="Oculus Hardware Revenue" | Metric =="Oculus Software Revenues")
    ggplot(x, aes(x=Year, y = Value, size=10, group=Metric)) + 
      geom_line(aes(color=Metric, alpha=0.9))+ 
      theme(legend.position="bottom" ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("Oculus Revenue Breakdown") + theme_classic() +  scale_x_continuous(breaks = seq(2016, 2025, 1))+ scale_y_continuous(labels = scales::number_format(accuracy = 0.01, decimal.mark = ','))
  })
  
  output$three_R <- renderPlot({
    x <- filter(my_data, Metric =="Oculus R&D" | Metric =="Acquisitions + CAPEX")
    y <- filter(my_data,  Metric =="Acquisitions + CAPEX_cum")
    x <- x %>% mutate(ValuePos = Value*-1 )
    y <- y %>% mutate(ValuePos = Value*-1 )
    ggplot(x, aes(x=Year, y = ValuePos, group=Metric))+
      geom_col(aes(fill=Metric, alpha=0.9, size=25))+ 
      theme(legend.position="bottom" ,plot.title = element_text(size=5, face="bold")) + labs(title="Cumulative Facebook Investment", subtitle = "Inlcuding Oculus Acquistion in 2014 (3B)") +
      theme_classic() +  scale_x_continuous(breaks = seq(2016, 2025, 1))+ scale_y_continuous(labels = scales::number_format(accuracy = 0.01, decimal.mark = ',')) + 
      geom_line(data= y,mapping=aes(x=Year,y=ValuePos), alpha=.5, size=1, color="#3B5998")
    # geom_line(y, aes(x=Year, y = Value, size=10, group=Metric))
    # ggplot(x, aes(x=Year, y = Value, group=Metric))+
    #   geom_line(aes(color=Metric, alpha=0.9, size=25))+ 
    #   theme(legend.position="bottom" ,plot.title = element_text(size=5, face="bold")) + 
    #   ggtitle("Cumulative Facebook Investment") + theme_classic() +  scale_x_continuous(breaks = seq(2016, 2025, 1))+ scale_y_continuous(labels = scales::number_format(accuracy = 0.01, decimal.mark = ','))
  })
}



# ggplot(x, aes(x=Year, y = ValuePos, group=Metric))+
#   geom_col(aes(fill=Metric, alpha=0.9, size=25))+ 
#   theme(legend.position="bottom" ,plot.title = element_text(size=5, face="bold")) + 
#   ggtitle("Cumulative Facebook Investment in Oculus") + theme_classic() +  scale_x_continuous(breaks = seq(2016, 2025, 1))+ scale_y_continuous(labels = scales::number_format(accuracy = 0.01, decimal.mark = ',')) + 
#   geom_line(data= y,mapping=aes(x=Year,y=ValuePos), alpha=.5, size=1, color="#3B5998") + labs()

# rsconnect::deployApp('C:\Users\greer\Documents\MSBA\R\dashboard\test.R')

#run/call the shiny app
# shinyApp(ui, server)