require(shinydashboard)
require(shiny)
require(tidyverse)
sidebar=dashboardSidebar(sidebarMenu(
  menuItem('INTRODUCTION',tabName = 'Introduction'),
  menuItem('Mean Life Expectancy',tabName = 'Life_Expectancy'),
  menuItem('GNI Per Capita',tabName='GNI'),
  menuItem('HDI For Two Years',tabName = 'HDI'),
  menuItem('Medium Developed Countries',tabName = 'IHDI_Medium_Developed_Countries'),
  menuItem('Low Developed Countries', tabName = 'IHDI_High_Developed_Countries'),
  menuItem('CONCLUSION',tabName='Conclusion')
))
ui<-dashboardPage(skin='green',
                  dashboardHeader(title='HDI & IHDI Analysis'),
                  sidebar,
                  dashboardBody(tabItems(
                    tabItem(tabName = 'Introduction',
                            h4(box(width=12, 
    
                                   p("The UNDP (United Nations Development Program, headquartered in New York) is the most significant United Nations Development Aid Agency consisting of 170 countries, with an ambition of eradicating poverty & inequality of human resource distributions. It is another significant footstep towards encouraging sustainable economic development."), 
                                   p("Each & every year UNDP releases two major indices to parametrize progress & depreciation as well, in term of growth in every major social aspects. These are named as:"),
              
                                   p("1.HDI (Human Development Index)"),
                                   p("2.IHDI (Inequality-adjusted Human Development Index)."),
                                   br(),
                                    p("HDI: A statistic depending on longevity, access to knowledge & monetary income of different strata of population. According to this index value, countries are categorized into four different categories- Very High developed, High Developed, Medium Developed & Low Developed."),
                                   p("IHDI: This is similarly other indicator of country's development, by exclusively discounting average value of each dimension (education, income, life expectancy) gauging the scenario of inequality."),
                                   p("In my project I have taken UNDP HDI & IHDI Datasets. After taking randomly 15 countries from each category, I've subgrouped the country categories into two. I have analysed the HDI for Highly Developed & Developed countries, and parallelly analysed IHDI for Medium & Low Developed countries."),
                                   p("

The data sets I am working on are:-

1. Human Development Index and its components 
2. Inequality-adjusted Human Development Index 
Source: https://hdr.undp.org/data-center/documentation-and-downloads")))),
                    tabItem(tabName = 'Life_Expectancy',
                            h4(
                              fluidRow(
                                box(width=12,checkboxInput('hdicheck','Check For 
                                                  Highly Developed Countries, Uncheck for 
                                                  Very Highly Developed countries',value=TRUE))
                              ),
                              fluidRow(
                                box(width=12,plotOutput('graph1'))
                              ),
                              fluidRow(
                                box(width=12,'Within Very High Developed Countries, China has the highest & New Moldova has the lowest GNI. Within High Developed Countries, Hong Kong has the highest & Germany has the lowest Life Expectancy in the same year (2021)')
                              )
                            )),
                    tabItem(tabName = 'GNI',
                            h4(
                              fluidRow(
                                box(width=12,checkboxInput('hdicheck2','Check For 
                                                  Highly Developed Countries, Uncheck for 
                                                  Very Highly Developed countries',value=TRUE))
                              ),
                              fluidRow(
                                box(width=12,plotOutput('graph2'))
                              ),
                              fluidRow(
                                box(width=12,'Within Very High Developed Countries, Singapore has the highest & New Zealand has the lowest GNI. Within High Developed Countries, Seychelles has the highest & Barbados has the lowest GNI in the same year (2021)')
                              )
                            )),
                    tabItem(tabName = 'HDI',
                            h4(
                              fluidRow(column(width=12,
                                box(width = NULL,'Among Very Highly Developed Countries, Australia, Canada, Hong Kong, Ireland, Netherlands, New Zealand managed to maintaing their HDI intact. Among the rest only Belgium, Finland, Sweden & Switzerland were improving'),
                                box(width = NULL,plotOutput('graph3')),
                                box(width = NULL,'Although the fluctuations were marginal among Highly Developed Countries, only Antigua & Barbadua maintained intact HDI. Rest were proportionately increasing & decreasing.'),
                                box(width = NULL,plotOutput('graph4')),
                              ))
                            )),
                    tabItem(tabName='IHDI_Medium_Developed_Countries',
                            h4(
                              fluidRow(column(width=12,
                                              box(width = NULL,'Among Medium developed countries, Morocco has most wide spectrum indices among IHDI categories.'),
                                              box(width = NULL,plotOutput('graph5')),
                                              box(width = NULL,plotOutput('graph6'))))
                            )),
                    tabItem(tabName = 'IHDI_High_Developed_Countries',
                            h4(
                              box(width = NULL,'Among Low developed countries, Senegal has most wide spectrum indices among IHDI categories'),
                              box(width = NULL,plotOutput('graph7')),
                              box(width = NULL,plotOutput('graph8'))
                            )),
                    tabItem(tabName = 'Conclusion',
                            h4(box(width=12,
                            p("According to the models, we can arrive at these following conclusions:"),
                            p("1.Within Very High Developed Countries, China has the highest & New Moldova has the lowest GNI. Within High Developed Countries, Hong Kong has the highest & Germany has the lowest Life Expectancy in the same year (2021)"),
                            p("2.Within Very High Developed Countries, Singapore has the highest & New Zealand has the lowest GNI. Within High Developed Countries, Seychelles has the highest & Barbados has the lowest GNI in the same year (2021)"),
                            p("3.Among Very Highly Developed Countries, Australia, Canada, Hong Kong, Ireland, Netherlands, New Zealand managed to maintaing their HDI intact. Among the rest only Belgium, Finland, Sweden & Switzerland were improving"),
                            p("4.Although the fluctuations were marginal among Highly Developed Countries, only Antigua & Barbadua maintained intact HDI. Rest were proportionately increasing & decreasing."),
                            p("5.Among Medium developed countries, Morocco has most wide spectrum indices among IHDI categories."),
                            p("6.Among Low developed countries, Senegal has most wide spectrum indices among IHDI categories"))))
                  ))
)
server<-function(input,output){
  data=read.csv("VHD_HDI.csv")
  data2=read.csv("HD_HDI.csv")
  data3=read.csv("3MD.csv")
  data4=read.csv("4LD.csv")
  output$graph1=renderPlot({
    if(input$hdicheck==TRUE){
      key=data2
    } else {
      key=data
    }
    ggplot(key,aes(x=Country,y=Expectancy_2021,fill=Country))+
      geom_bar(stat="identity")+
      theme(axis.text.x=element_text(angle=90,hjust=1))+labs(title="Life Expectancy at birth (2021) for Given Choice")
  })
  output$graph2=renderPlot({
    if(input$hdicheck2==TRUE){
      key2=data2
    } else {
      key2=data
    }
    ggplot(key2,aes(x=Country,y=GNI_per_capita_2021,fill=Country))+
      geom_bar(stat="identity")+
      theme(axis.text.x=element_text(angle=90,hjust=1))+labs(title="GNI per Capita (2021) for Given Choice")
  })
  output$graph3=renderPlot({
    ggplot(data,aes(x=Countries,y=HDI,fill=Years))+
      geom_bar(stat="identity", position="dodge")+
      theme(axis.text.x=element_text(angle=90,hjust=1))+labs(title="HDI Rank of Very Highly Developed Countries for two consecutive years")
  })
  output$graph4=renderPlot({
    ggplot(data2,aes(x=Countries,y=HDI,fill=Years))+
      geom_bar(stat="identity", position="dodge")+
      theme(axis.text.x=element_text(angle=90,hjust=1))+labs(title="HDI Rank of Highly Developed Countries for two consecutive years")
  })
  output$graph5=renderPlot({
    ggplot(data3,aes(x=Countries,y=Values,fill=Inequality_adjusted_index_2021))+
      geom_bar(stat="identity", position="dodge")+
      theme(axis.text.x=element_text(angle=90,hjust=1))+labs(title="Categorical IHDI for Medium Developed Countries")
  })
  output$graph6=renderPlot({
    ggplot(data3,aes(x=Countries,y=Val,fill=Index))+geom_bar(stat="identity", position="dodge")+
      theme(axis.text.x=element_text(angle=90,hjust=1))+labs(title="HDI vs IHDI for Medium Developed Countries")
  })
  
  output$graph7=renderPlot({
    ggplot(data4,aes(x=Countries,y=Values,fill=Inequality_adjusted_index_2021))+
      geom_bar(stat="identity", position="dodge")+
      theme(axis.text.x=element_text(angle=90,hjust=1))+labs(title="Categorical IHDI for Low Developed Countries")
  })
  output$graph8=renderPlot({
    ggplot(data4,aes(x=Countries,y=Val,fill=Index))+
      geom_bar(stat="identity", position="dodge")+
      theme(axis.text.x=element_text(angle=90,hjust=1))+labs(title="HDI vs IHDI for Low Developed Countries")
  })
}
shinyApp(ui,server)