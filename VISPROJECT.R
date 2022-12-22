require(shinydashboard)
require(shiny)
require(tidyverse)
require(reshape2)


sidebar=dashboardSidebar(sidebarMenu(
  menuItem('INTRODUCTION',tabName = 'Introduction'),
  menuItem('VHDC vs HDC',tabName = 'Tab1'),
  menuItem('HDI for Two Years',tabName = 'Tab2'),
  menuItem('MDC vs LDC',tabName = 'Tab3'),
  menuItem('HDI and IHDI',tabName = 'Tab4'),
  menuItem('Trends in HDI', tabName = 'Tab5'),
  menuItem('CONCLUSION',tabName='Conclusion')
))
ui<-dashboardPage(skin='blue',
                  dashboardHeader(title='HDI & IHDI Analysis'),
                  sidebar,
                  dashboardBody(tabItems(
                    tabItem(tabName = 'Introduction',
                            h5(box(width=12, 
                  
                                   p("The UNDP (United Nations Development Program, headquartered in New York) is the most significant United Nations Development Aid Agency consisting of 170 countries, with an ambition of eradicating poverty & inequality of human resource distributions. It is another significant footstep towards encouraging sustainable economic development."), 
                                   p("Each & every year UNDP releases two major indices to parametrize progress & depreciation as well, in term of growth in every major social aspects. These are named as:"),
                                   
                                   p("1.HDI (Human Development Index)"),
                                   p("2.IHDI (Inequality-adjusted Human Development Index)."),
                                   br(),
                                   p("HDI: A statistic depending on longevity, access to knowledge & monetary income of different strata of population. According to this index value, countries are categorized into four different categories- Very High developed, High Developed, Medium Developed & Low Developed."),
                                   p("IHDI: This is similarly other indicator of country's development, by exclusively discounting average value of each dimension (education, income, life expectancy) gauging the scenario of inequality."),
                                   p("In my project I have taken UNDP HDI & IHDI Datasets. After taking randomly 15 countries from each category, I've subgrouped the country categories into two. I have analysed the HDI for Highly Developed & Developed countries, and parallelly analysed IHDI for Medium & Low Developed countries. Also at the end, there is a trend analysis of all the HDI of the countries of different categories in last 30 years i.e. 1990-2021"),
                                   p("The data sets I am working on are:-"),
p("1. Human Development Index and its components"),
p("2. Inequality-adjusted Human Development Index"),
p("3. Trends in the Human Development Index, 1990-2021"),
p("[Source: https://hdr.undp.org/data-center/documentation-and-downloads]"),
p("ABBREVIATIONS USED"),
p("1. Very Highly Developed Countries - VHDC"),
p("2. Highly Developed Countries      - HDC"),
p("3. Medium Developed Countries      - MDC"),
p("4. Low Developed Countries         - LDC")))),
                    tabItem(tabName = 'Tab1',
                            h4(
                              fluidRow(
                                box(width=6,
                                    selectInput('SelectMeanGNI',
                                                "Select Data",
                                                c("GNI", "Mean Life Expectancy")
                                    )
                                ),
                                box(width=6,
                                    radioButtons("hdicheck3","", c("Very Highly Developed countries", "Highly Developed Countries"))
                                ),
                                uiOutput("tabGNI_Mean")
                                
                              )
                            )
                    ),
                    tabItem(tabName = 'Tab2',
                            
                            h4(
                              fluidRow(
                                box(width=12,
                                    radioButtons("hdicheck4","", c("Very Highly Developed countries", "Highly Developed Countries"))
                                ),
                                uiOutput("tabHDI")
                                
                              )
                            )
                            
                    ),
                    tabItem(tabName='Tab3',
                            h4(
                              fluidRow(
                                box(width=6,
                                    uiOutput("SelectParameterHDI_IHDI")
                                ),
                                box(width=6,
                                    radioButtons("hdicheck5","", c("Medium Developed countries", "Low Developed Countries"))
                                ), 
                               
                                uiOutput("tabIHDI")
                                
                              )
                            )
                            
                    ),
                    tabItem(tabName='Tab4',
                            h4(
                              fluidRow(
                                box(width=12,
                                    radioButtons("hdicheck6","", c("Medium Developed countries", "Low Developed Countries"))
                                ),
                                uiOutput("tabIHDI_HDI")
                                
                              )
                            )
                            
                    ),
                    
                    tabItem(tabName = 'Tab5',
                            h4(
                              fluidRow(
                                box(width=6,
                                    uiOutput("SelectCategory")
                                ),
                                box(width=6,
                                    uiOutput("SelectCountry")
                                ),                                
                                uiOutput("tabTrends_HDI")
                                
                              )
                            )
                            
                            
                            
                    ),
                    
                    tabItem(tabName = 'Conclusion',
                            h5(box(width=12,
                                   p("According to the models, we can arrive at these following conclusions:"),
                                   p("1.In VHDCs, although the fluctuations of values among countries are nearly subtle, SAR is seemingly having the highest life expectancy and Germany is seemingly having the lowest life expectancy at birth in 2021. Very prominently, GNI Per Capita is highest for Singapore and lowest for New Zealand."),
                                   p("2.In HDCs, although the fluctuations of values among countries are nearly subtle, China is seemingly having the highest life expectancy and Moldova is seemingly having the lowest life expectancy at birth in 2021.Very prominently, GNI Per Capita is highest for Seychelles and lowest for Barbados."),
                                   p("3.In VHDCs, HDI rank has been improved (HDI rank in 2020 is greater than HDI rank in 2021) in Belgium, Finland, Sweden & Switzerland. On the contrary, it has been worsened for Denmark, Germany, Norway, Iceland & Singapore. Remaining countries (Australia, Canada, SAR, Ireland, Netherlands & New Zealand) witnessed no change in their HDI rank for consecutive two years."),
                                   p("4.In HDCs, HDI rank has been worsened (HDI rank in 2020 is lesser than HDI rank in 2021) for Bosnia & Herzegivina, Bulgaria and Seychelles. Antigua & Barbuda witnessed no change in their HDI rank for consecutive two years. Rest all has minor but significant improve in their HDI rank in 2021 compared to 2020."),
                                   p("5.Among MDCs, inequality adjusted index for Education, Income and Life Expectancy is lowest for Bhutan, Nicaragua and Ghana whereas they are respectively highest at Kyrgystan, Iraq and Nicaragua."),
                                   p("6.Among LDCs, inequality adjusted index for Education, Income and Life Expectancy is lowest for Senegal, Haiti and Nigeria whereas they are respectively highest at Lesotho, Senegal and Pakistan."),
                                   p("7.In MDCs, Even after eliminating the effect the inequality, Ghana has the lowest & Kyrgyztan has the highest index value. Hence their IHDI rank is just the opposite extremes."),
                                   p("8.Even after eliminating the effect the inequality, Haiti has the lowest & Tanzania has the highest index value. Hence their IHDI rank is just the opposite extremes."))))
                  ))
)

server<-function(input,output){
  data=read.csv("VHD_HDI.csv")
  data2=read.csv("HD_HDI.csv")
  data3=read.csv("3MD.csv")
  data4=read.csv("4LD.csv")
  data5=read.csv("Trends in HDI - Copy.csv")
  data5 = data5[,1:11]
  data5 = melt(data5, id = c("HDI.rank", "Country" , "Category"))  
  names(data5)[4] = "Year"
  names(data5)[5] = "HDI"
  data5$Year = as.numeric( gsub("X","",data5$Year))
  data5$HDI = as.numeric(data5$HDI)
  data5 = na.omit(data5)
  
  
  output$SelectCategory = renderUI({
    selectInput("SelectCategory",
                "Select Category",
                unique(data5$Category)
    )
  })
  
  output$SelectCountry = renderUI({
    temp = data5 %>%
      filter(Category %in% input$SelectCategory)
    
    selectInput("SelectCountry",
                "Select Country",
                unique(temp$Country)
    )
  })
  
  output$SelectParameterHDI_IHDI = renderUI({

    
    selectInput("SelectParameter",
                "Select Parameter",
                unique(data4$Inequality_adjusted_index_2021)
    )
  })
  
  
  
  output$graph3=renderPlot({
    ggplot(data,aes(x=Countries,y=HDI,fill=Years))+
      geom_bar(stat="identity", position="dodge")+
      theme(axis.text.x=element_text(angle=90,hjust=1))+axis_flip()
        +labs(title="HDI Rank of Very Highly Developed Countries for two consecutive years")
  })
  output$graph4=renderPlot({
    ggplot(data2,aes(y=Countries,x=HDI,fill=Years))+
      geom_bar(stat="identity", position="dodge")+
      theme(axis.text.x=element_text(angle=90,hjust=1))+labs(title="HDI Rank of Highly Developed Countries for two consecutive years")
  })
  output$graph5=renderPlot({
    ggplot(data3,aes(y=Countries,x=Values,fill=Inequality_adjusted_index_2021))+
      geom_bar(stat="identity", position="dodge")+
      theme(axis.text.x=element_text(angle=90,hjust=1))+labs(title="Component-wise IHDI for Medium Developed Countries")
  })
  output$graph6=renderPlot({
    ggplot(data3,aes(y=Countries,x=Val,fill=Index))+geom_bar(stat="identity", position="dodge")+
      theme(axis.text.x=element_text(angle=90,hjust=1))+labs(title="HDI vs IHDI for Medium Developed Countries")
  })
  
  output$graph7=renderPlot({
    ggplot(data4,aes(y=Countries,x=Values,fill=Inequality_adjusted_index_2021))+
      geom_bar(stat="identity", position="dodge")+
      theme(axis.text.x=element_text(angle=90,hjust=1))+labs(title="Component-wise IHDI for Low Developed Countries")
  })
  output$graph8=renderPlot({
    ggplot(data4,aes(y=Countries,x=Val,fill=Index))+
      geom_bar(stat="identity", position="dodge")+
      theme(axis.text.x=element_text(angle=90,hjust=1))+labs(title="HDI vs IHDI for Low Developed Countries")
  })
  
  
  output$tabGNI_Mean = renderUI({
    
    UI = list()
    
    if(input$SelectMeanGNI == "GNI"){
      if(input$hdicheck3 == "Very Highly Developed countries"){
        
        UI = append(UI, 
                    list(
                      box(width=12,
                          renderPlot(
                            
                            ggplot(data,aes(x=Country,y=GNI_per_capita_2021,fill=Country))+
                              geom_bar(stat="identity")+
                              theme(axis.text.x=element_text(angle=90,hjust=1))+coord_flip()+labs(title="GNI per Capita (2021) for Given Choice")
                            
                          )
                      )
                      
                    )
        )
        
        UI = append(UI, list(
          box(width=12,'Within Very High Developed Countries, Singapore has the highest & New Zealand has the lowest GNI. Within High Developed Countries, Seychelles has the highest & Barbados has the lowest GNI in the same year (2021)')
        ))
        
        
      }
      
      if(input$hdicheck3 == "Highly Developed Countries"){
        UI = append(UI, 
                    list(
                      box(width=12,
                          renderPlot(
                            
                            ggplot(data2,aes(x=Country,y=GNI_per_capita_2021,fill=Country))+
                              geom_bar(stat="identity")+
                              theme(axis.text.x=element_text(angle=90,hjust=1))+labs(title="GNI per Capita (2021) for Given Choice")+coord_flip()
                            
                          )
                      )
                      
                    )
        )
        
        UI = append(UI, list(
          box(width=12,'Within Very High Developed Countries, Singapore has the highest & New Zealand has the lowest GNI. Within High Developed Countries, Seychelles has the highest & Barbados has the lowest GNI in the same year (2021)')
        ))
      }
    }  
    
    if(input$SelectMeanGNI == "Mean Life Expectancy"){
      if(input$hdicheck3 == "Very Highly Developed countries"){
        UI = append(UI, 
                    list(
                      box(width=12,
                          renderPlot(
                            ggplot(data,aes(y=Country,x=Expectancy_2021,fill=Country))+
                              geom_bar(stat="identity")+
                              theme(axis.text.x=element_text(angle=90,hjust=1))+labs(title="Life Expectancy at birth (2021) for Given Choice")
                            
                          )
                      )
                      
                    )
        )
        
        UI = append(UI, list(
          box(width=12,'Within Very High Developed Countries, China has the highest & New Moldova has the lowest GNI. Within High Developed Countries, Hong Kong has the highest & Germany has the lowest Life Expectancy in the same year (2021)')
        ))
        
      }
      
      if(input$hdicheck3 == "Highly Developed Countries"){
        UI = append(UI, 
                    list(
                      box(width=12,
                          renderPlot(
                            
                            ggplot(data2,aes(y=Country,x=Expectancy_2021,fill=Country))+
                              geom_bar(stat="identity")+
                              theme(axis.text.x=element_text(angle=90,hjust=1))+labs(title="Life Expectancy at birth (2021) for Given Choice")
                            
                          )
                      )
                      
                    )
        )
        
        UI = append(UI, list(
          box(width=12,'Within Very High Developed Countries, China has the highest & New Moldova has the lowest GNI. Within High Developed Countries, Hong Kong has the highest & Germany has the lowest Life Expectancy in the same year (2021)')
        ))
        
      }
    }
    
    UI
    
  })
  
  output$tabHDI = renderUI({
    
    UI = list()
    
    
    if(input$hdicheck4 == "Very Highly Developed countries"){
      
      UI = append(UI, 
                  list(
                    box(width=12,
                        renderPlot(
                          
                          ggplot(data,aes(y=Countries,x=HDI,fill=Years))+
                            geom_bar(stat="identity", position="dodge")+
                            theme(axis.text.x=element_text(angle=90,hjust=1))+labs(title="HDI Rank of Very Highly Developed Countries for two consecutive years")
                          
                        )
                    )
                    
                  )
      )
      
      UI = append(UI, list(
        box(width=12,'Among Very Highly Developed Countries, Australia, Canada, Hong Kong, Ireland, Netherlands, New Zealand managed to maintaing their HDI intact. Among the rest only Belgium, Finland, Sweden & Switzerland were improving')
      ))
      
      
    }
    
    if(input$hdicheck4 == "Highly Developed Countries"){
      UI = append(UI, 
                  list(
                    box(width=12,
                        renderPlot(
                          
                          ggplot(data2,aes(y=Countries,x=HDI,fill=Years))+
                            geom_bar(stat="identity", position="dodge")+
                            theme(axis.text.x=element_text(angle=90,hjust=1))+labs(title="HDI Rank of Highly Developed Countries for two consecutive years")
                          
                        )
                    )
                    
                  )
      )
      
      UI = append(UI, list(
        box(width=12,'Although the fluctuations were marginal among Highly Developed Countries, only Antigua & Barbadua maintained intact HDI. Rest were proportionately increasing & decreasing.')
      ))
    }
    
    
    UI
    
  })
  
  output$tabIHDI = renderUI({
    
    UI = list()
    
    
    if(input$hdicheck5 == "Medium Developed countries"){
      
      UI = append(UI, list(
        box(width=12,'Among Medium developed countries, Morocco has most wide spectrum indices among IHDI categories.')
      ))
      
      temp = data3 %>%
        filter(Inequality_adjusted_index_2021 %in% input$SelectParameter)
      
      UI = append(UI, 
                  list(
                    box(width=12,
                        renderPlot(
                          
                          ggplot(temp,aes(y=Countries,x=Values,fill=Countries))+
                            geom_bar(stat="identity", position="dodge")+
                            theme(axis.text.x=element_text(angle=90,hjust=1))+labs(title="Categorical IHDI for Medium Developed Countries")
                          
                        )
                    )
                    
                  )
      ) 
      
     
      
      
      
    }
    
    if(input$hdicheck5 == "Low Developed Countries"){
      
      UI = append(UI, list(
        box(width=12,'Among Low developed countries, Senegal has most wide spectrum indices among IHDI categories')
      ))
      
      temp = data4 %>%
        filter(Inequality_adjusted_index_2021 %in% input$SelectParameter)
      
      UI = append(UI, 
                  list(
                    box(width=12,
                        renderPlot(
                          
                          ggplot(data4,aes(y=Countries,x=Values,fill=Countries))+
                            geom_bar(stat="identity", position="dodge")+
                            theme(axis.text.x=element_text(angle=90,hjust=1))+labs(title="Categorical IHDI for Low Developed Countries")
                          
                        )
                    )
                    
                  )
      ) 
      
    
      
      
    }
    
    
    UI
    
  })
  output$tabIHDI_HDI = renderUI({
    
    UI = list()
    
    
    if(input$hdicheck6 == "Medium Developed countries"){
      
     
      
      UI = append(UI, 
                  list(
                    box(width=12,
                        renderPlot(
                          
                          ggplot(data3,aes(y=Countries,x=Val,fill=Index))+geom_bar(stat="identity", position="dodge")+
                            theme(axis.text.x=element_text(angle=90,hjust=1))+labs(title="HDI vs IHDI for Medium Developed Countries")
                        )
                    )
                    
                  )
      )
      
      
      
    }
    
    if(input$hdicheck6 == "Low Developed Countries"){
      
      
      UI = append(UI, 
                  list(
                    box(width=12,
                        renderPlot(
                          
                          ggplot(data4,aes(y=Countries,x=Val,fill=Index))+
                            geom_bar(stat="identity", position="dodge")+
                            theme(axis.text.x=element_text(angle=90,hjust=1))+labs(title="HDI vs IHDI for Low Developed Countries")
                        )
                    )
                    
                  )
      )
      
      
    }
    
    
    UI
    
  })
  
  
  output$tabTrends_HDI = renderUI({
    
    UI = list()
    
    temp = data5 %>%
      filter(Category %in% input$SelectCategory) %>%
      filter(Country %in% input$SelectCountry)
    
    temp$Year = as.factor(temp$Year)
    
    UI = append(UI, 
                list(
                  box(width=12,
                      renderPlot(
                        
                        ggplot(temp,aes(x=Year,y=HDI, group=1))+
                          geom_line(color="blue", size = 2)+
                          geom_point(size = 3)+
                          ylim(0, 1)+
                          theme(axis.text.x=element_text(angle=90,hjust=1))+labs(title="Trends in HDI")
                        
                      )
                  )
                  
                )
    ) 
    
    
    UI
    
  })
  
}
shinyApp(ui,server)