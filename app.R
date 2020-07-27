#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shinydashboard)
library(shiny)
library(plotly)
library(leaflet)

# Define UI for application that draws a histogram
ui <- fluidPage(
    dashboardPage(
        dashboardHeader(title = "Climate Change"),
        dashboardSidebar(
            sidebarMenu(
                menuItem("Overview", tabName = "overview"),
                menuSubItem("Causes", tabName = "causes"),
                menuSubItem("Changes", tabName = "changes"),
                menuSubItem("Effects", tabName = "effects"),
                menuSubItem("Global", tabName = "global")
            )),
        dashboardBody(
            tabItems(
                tabItem(tabName = "overview",
                        box(title = "Causes of Global Warming and Impact of Global Warming i.e. CLIMATE CHANGE",solidHeader = TRUE, status = "info", width = 12, background = "navy",br(),
                            h4("What causes climate change? Is Climate change hapenning due to global warming? Why global warming is hapenning? 
                       And why should we care about climate change? Is it a Global Phenomenon? Answering all the question all together is difficult.
                       Therefore, I have divided the climate change phenomenon into FOUR categories."),br(),
                            h3("1. Major causes of Global warming?"),br(), p(">>Click CAUSES tab on left sidebar"), br(),
                            h3("2. Global warming responsible for unnatural changes in nature?" ), br(),p(">>Click CHANGES tab on left sidebar"), br(),
                            h3("3. Are these unnatural changes increasing natural disaster?"), br(),p(">>Click EFFECTS tab on left sidebar"), br(),
                            h3("4. Is Climate Change a Global Phenomenon?"), br(),p(">>Click GLOBAL tab on left sidebar"), br(),
                            h4("I would suggest you to go in sequence one by one for the better understanding. In every tab I have provided
                       few graphs and plots with some interaction, feel free to interact with them."))
                ),
                tabItem(tabName = "causes", fluidRow(box(title = "Major causes of Global warming?", solidHeader = TRUE, status = "danger", width = 12, background = "navy",br(), 
                                                         p("According to science major cause of global warming are greenhouse gases. Now questions is
                                                           which greenhouse gas is more correlated with increase in global temperatue. Since global temperature
                                                           is rising every year, this question becomes more important."), br(), p("To find out the correlation
                                                           betweeen Green house gas and global temprature anomaly I have plotted a scatter plot with data of annual temperature
                                                            anomaly and green house gas emission every year of last 50 years and also calculated
                                                           the correlation between them. Don't forget to interact with the plot using controls given above the plot and see what pop up when you hover mouse on the plots.") )),
                        fluidRow(
                            box(title= "Scatter plot with linear regression line, Green house gas type vs temprature anomaly", width = 9, 
                                status = "primary",
                                solidHeader = T,plotlyOutput("scatter")),
                            box(title= "Drop down select", width = 3, 
                                status = "primary",
                                solidHeader = T, background = "black",
                                selectInput(inputId = "ghg", label = "Choose a Greenhouse gas",
                                            c("CO2" = "CO2_emissions",
                                              "Methane" = "methane_emissions",
                                              "Nitrous Oxide" = "nitrous_oxide",
                                              "HFC, PFC and SF6" = "HFC_PFC_SF6")))
                        )
                ),
                tabItem(tabName = "changes", 
                        fluidRow(box(title = "Global warming causing changes to nature?", solidHeader = TRUE, status = "warning", width = 12, background = "navy",br(), 
                                     p("Due to rise in temperature many aspects of nature are changing, but we are
                                       studying and analysing only sea level and glacier melt. Everything in  nature is
                                       connected, one of them changes, then other changes. Similarly here temperature increase
                                       then sea water expands, sea level rises. Temperature rises, glacier melts and again sea level rise. 
                                       This is how everything is connected, one triggers other."), br(), 
                                     p("Check out the below bubble chart. Size of the bubble show Average cumulative mass balance of reference Glaciers which is
                                         decresing with the rise in temperature anomaly(x-axis). Whereas sea level(y-axis) is rising with decrease in
                                         Average cumulative mass balance of reference Glaciers and increase in temperature anomaly. Must try radio buttons to see separate plots
                                         of sea level vs temperature anomaly and Average cumulative mass balance of reference Glaciers vs temperature anomaly. Again don't forget to interact with the 
                                         plot using controls given above the plot and see what pop up when you hover mouse on the plots.") )),
                        fluidRow(
                            box(plotlyOutput("bubble"), title = "Always try to find out something new", solidHeader = TRUE, status = "success",width = 10),
                            box(radioButtons(inputId = "change", label = "Change Plot?",
                                             choices = c("All"="both",
                                                         "glacier vs temperature" = "glacier_melt", 
                                                         "sea level vs temperature" = "sea_level",
                                                         "glacier vs sea level" = "s_n_g")), width = 2, background = "navy", title = "Radio Buttons", solidHeader = TRUE, status = "success")
                        )
                ),
                tabItem(tabName = "effects",fluidRow(box(title = "Unnatural changes to nature caused by global warming are increasing natural disaster?", solidHeader = TRUE, status = "danger", width = 12, background = "navy",br(), 
                                                         p("We should not play with nature because it can damage more than we can imagine. As we discussed increasing temperature is cause of sea level rise.
                                                           That mean high sea level then it will trigger more number of floods and heavy rainfall. Similary other things in nature are disturbed by
                                                           the increasing temperature and ultimatley can cause other type of natural disaters. Since we already seen how temperature is increasing every year, let us see
                                                           how number of natural disasters are increasing every year in the below stacked area graph. You can also choose to see
                                                           specific type of natural disaster only by using slider bar given on the right of graph. Once again don't forget to interact with the 
                                         graphs using controls given above the graphs and see what pop up when you hover mouse on the graphs."))), 
                        fluidRow(
                            box(plotlyOutput("line"), width = 9),
                            box(selectInput(inputId = "disaster", label = "Choose a natural disaster" ,
                                            choices = c("All natural disasters"="All natural disasters",
                                                        "Floods" = "Flood", 
                                                        "Droughts" = "Drought",
                                                        "Extreme temperature" = "Extreme temperature",
                                                        "Earthquake" = "Earthquake",
                                                        "Extreme weather"="Extreme weather",
                                                        "Landslide"="Landslide",
                                                        "Volcanic activity"="Volcanic activity",
                                                        "Wildfire"="Wildfire")), width = 3, background = 'navy',title = "Drop down select", solidHeader = TRUE, status = "primary")
                        )
                ),
                tabItem(tabName = "global", 
                        fluidRow(box(title = "Is Climate Change a Global Phenomenon?", solidHeader = TRUE, status = "warning", width = 12, background = "navy",br(), 
                                     p("Climate change is not local issue, it is a global issue. That is climate change is not limited to certain locality.
                                       Through global map you can see average CO2 emissions, CO2 emission per capital or Average percentage of population affected by natural disasters by each country, you can choose any ony of these 
                                       variables to be plotted on the map. You will find all the plots are completelty different
                                       because Climate change does not work on believe such as a country producing more CO2 will suffer more natural disaster. That is, if a country or locality
                                       stop producing green house gases then they will not get immune to increasing natural disasters. Whole world need to come together to fight Climate Change."), br(), 
                                     p("Use drop dowm menu below map to change things plotted on map. When user hover over the circular marker, it will show the magnitude of the plotted variable but if user click the circular marker it 
                                       will show percentage of population affected by the natural disaster if the plotted values are either Average CO2 emissions or Average per capita CO2 emissions, but if plotted value is percentage of
                                       population affected by natural disaster in the country then clicking on marker will show the Average CO2 emission by the country. User can also zoom in, zoom out and move the maps and explore it.") )),
                        fluidRow(
                            box(leafletOutput("map"), width = 12, background = "black"),
                            box(selectInput(inputId = "map_input", label = "Choose variables",
                                            choices = c("Avg CO2 emissions"="avg_co2",
                                                        "CO2 emissions per capita" = "per_cap_co2", 
                                                        "percent of population affected by disasters" = "per_pop")),background = "navy" )
                            
                        )
                )
                
                
                
                
            )
            
        )
    )
    
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$scatter <- renderPlotly({
        library(readxl)
        climate_change <- read_excel('climate_change_global.xlsx')
        if (input$ghg == "CO2_emissions"){
            cor <- paste("Correlation =",
                         round(cor(climate_change$CO2_emissions, climate_change$temp, 
                                   method = c("pearson", "kendall", "spearman"), 
                                   use = "complete.obs")*100,digits = 2), "%")
        }
        else if(input$ghg == "methane_emissions"){
            cor <- paste("Correlation =",
                         round(cor(climate_change$methane_emissions, climate_change$temp, 
                                   method = c("pearson", "kendall", "spearman"), 
                                   use = "complete.obs")*100,digits = 2), "%")
        }
        
        else if(input$ghg == "nitrous_oxide"){
            cor <- paste("Correlation =",
                         round(cor(climate_change$nitrous_oxide, climate_change$temp, 
                                   method = c("pearson", "kendall", "spearman"), 
                                   use = "complete.obs")*100,digits = 2), "%")
        }
        else if(input$ghg == "HFC_PFC_SF6"){
            cor <- paste("Correlation =",
                         round(cor(climate_change$HFC_PFC_SF6, climate_change$temp, 
                                   method = c("pearson", "kendall", "spearman"), 
                                   use = "complete.obs")*100,digits = 2), "%")
        }
        
        gg <- ggplot(climate_change, aes(x = !!as.name(input$ghg), y=temp)) + geom_point() + 
            geom_smooth(method="lm") + ggtitle(cor) + ylab("temprature anomaly")
        ggplotly(gg)
    })
    
    output$bubble <- renderPlotly({
        climate_change <- read_excel('climate_change_global.xlsx')
        if(input$change == "both"){
            p <- climate_change %>%
                mutate(temp=round(temp,2)) %>%
                mutate(glacier_melt=round(glacier_melt,2)) %>%
                mutate(sea_level=round(sea_level,2)) %>%
                
                # Reorder countries to having big bubbles on top
                arrange(desc(sea_level)) %>%
                mutate(year = factor(year, year)) %>%
                
                # prepare text for tooltip
                mutate(text = paste("year: ", year, "\nsea_level (M): ", sea_level, "\nAverage cumulative mass balance of reference Glaciers: ", glacier_melt, "\ntemp: ", temp, sep="")) %>%
                
                # Classic ggplot
                ggplot( aes(x=temp, y=sea_level, size = glacier_melt, text=text)) +
                geom_point(alpha=0.7) + ylim(-55,35) + ggtitle("Sea level vs Temperature anomaly vs Average cumulative mass balance of reference Glaciers \n")+
                scale_size(range = c(1.4, 19), name="Population (M)") +
                theme(legend.position="none")
            
            # turn ggplot interactive with plotly
            ggplotly(p, tooltip="text")}
        
        else if(input$change == "glacier_melt"){
            gg <- ggplot(climate_change, aes(x = temp, y=glacier_melt)) + geom_point() + 
                geom_smooth(method="loess") + ggtitle("Average cumulative mass balance of reference Glaciers vs Temperature anomaly") + ylab("Glacier average cumulative mass balance")
            ggplotly(gg)}
        else if(input$change == "sea_level"){
            gg <- ggplot(climate_change, aes(x = temp, y=sea_level)) + geom_point() + 
                geom_smooth(method="loess") + ggtitle("Sea level vs Temperature anomaly")
            ggplotly(gg)
        }
        else if(input$change == "s_n_g"){
            gg <- ggplot(climate_change, aes(x = glacier_melt, y=sea_level)) + geom_point() + 
                geom_smooth(method="loess") + ggtitle("Average cumulative mass balance of reference Glaciers vs Temperature anomaly") + xlab("Glacier average cumulative mass balance")
            ggplotly(gg)
        }
        
    })
    output$line <- renderPlotly({
        natural_disaster <- read_excel('natural_disaster_stacked_graph.xlsx')
        
        
        if(input$disaster == "All natural disasters"){
            p <-
                ggplot(natural_disaster, aes(x=year, y=value, fill=Disaster_type)) +
                geom_area( ) +
                theme(legend.position="none") +
                ggtitle("Number of natural disasters")+ theme(legend.position="right")
            
            ggplotly(p)  
            
        }
        
        else
        {
            natural_disaster <- subset(natural_disaster, Disaster_type == input$disaster)
            p <-
                ggplot(natural_disaster, aes(x=year, y=value)) +
                geom_area( ) +
                theme(legend.position="none") +
                ggtitle("Number of natural disasters")
            
            # Turn it interactive
            ggplotly(p)  
        }
        
        
        
        
    })
    output$map <- renderLeaflet({
        library(dplyr)
        library(maps)
        library(leaflet)
        
        global_map <- read_excel("co22.xlsx")
        
        if(input$map_input == "avg_co2"){
            m = leaflet(global_map) %>% addTiles()%>% addProviderTiles(providers$CartoDB.Positron)
            m %>% addCircleMarkers(radius = ~avg_co2/150000, color = "red",popup = ~as.character(per_pop), label = ~as.character(avg_co2))
        }
        else if(input$map_input == "per_cap_co2"){
            m = leaflet(global_map) %>% addTiles()%>% addProviderTiles(providers$CartoDB.Positron)
            m %>% addCircleMarkers(radius = ~per_cap_co2/1.5, color = "orange", popup = ~as.character(per_pop),label = ~as.character(per_cap_co2))
        }
        else if(input$map_input == "per_pop"){
            m = leaflet(global_map) %>% addTiles()%>% addProviderTiles(providers$CartoDB.Positron)
            m %>% addCircleMarkers(radius = ~per_pop*2.5, color = "purple",popup = ~as.character(avg_co2), label = ~as.character(per_pop))
        }
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
