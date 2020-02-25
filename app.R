

library(shiny)
library(shinydashboard)
library(DT)
library(rgdal)
library(raster)
library(leaflet)
library(RColorBrewer)
library(dplyr)

#loading data
kisumu<-read.csv("./data/kisumu.csv")

#renaming the columns
colnames(kisumu)[c(1,2,3,4,7,8,11,12,13,14,15,16,17,18,19,20)]=c("location Name","Sublocation Name","Area Name",
                                                                   "Area Type","Water Table Dry","Water Table Rainy",
                                                                   "Soil Self Supporting","Area Location","Area Topology",
                                                                   "Population Per KM2",
                                                                   "Percentage Dwellings with Piped Water On Plot",
                                                                   "Percentage Dwellings with Water Source On Plot",
                                                                   "Percentage Dwellings with Flush Toilets",
                                                                   "Percentage Dwellings with Other Improved",
                                                                   "Percentage Dwellings with Unimproved",
                                                                   "Percentage Dwellings with Open Defecation"

)

#selecting columns for plotting
kisumu1<-kisumu[,c(4,6,9)]
kisumu2<-kisumu[,c(14,15,16,17,18,19,20)]

#loading shape file
lias<-readOGR(dsn = path.expand("./LIAS"),
              layer="kisumu")

#removing columns with NAs
lias<-lias[!is.na(lias$lctn_Nm),]

#Unknown Areas
#loading shape file
unknown<-readOGR(dsn = path.expand("./unknown"),
                 layer="unknown")



#UI
ui <- dashboardPage(
  dashboardHeader(title="KISUMU DATA",titleWidth=275),
  dashboardSidebar(
    
    width = 275,
    sidebarMenu(
      menuItem("HOME",tabName = "home",icon = icon("home")),
      menuItem("DATA EXPLORER",tabName = "data",icon = icon("th")),
      menuItem("PLOT EXPLORER",tabName = "plot",icon =icon("bar-chart-o")),
      menuItem("COLOR MAPPING",tabName = "map",icon = icon("map",class = "fas fa-map")),
      menuItem("INTERACTIVE MAPPING",tabName = "map2",icon = icon("map", class="fas fa-map-marked-alt"))
      
    )
  ),
  dashboardBody(
    tabItems(
      
      ## Home
      tabItem(tabName = "home",
              fluidRow(
                column(width = 1),
                column(width = 11,
                       h1("KISUMU SHINY APP."),
                       h2("INTRODUCTION."),
                       h4("The purpose of this Shiny WebApp is to help WASH
                          decision makers in Kisumu County to visualize,interpret
                          and use data on low-income areas."),
                       h4(a(href="https://www.aquaya.org/", "AQUAYA"),"is committed to facilitate the
                          use of such data that can foster faster and
                          easier decision-making."),
                       h2("DATA."),
                       h4("The data were obtained from MajiData(data collected by the government between 2009 and 2011)."),
                       h4("It is important to note that since then,other low-income areas have appeared.
                          However, these have not been mapped yet, so they are not featured in the app."),
                       h4("Data contains information about the legal status, population density,hydrogeology,
                          as well as water and sanitation conditions in low-income areas.
                          "),
                       h4("
                          Figures are expressed as a percentage of the total, e.g. 
                          (Number of Dwellings with Flush_Toilets/Total Number of Dwellings)	× 100%
                          "),
                       
                       h2("HOW TO USE THE APP."),
                       h3("DATA EXPLORER."),
                       h4("Allows the user to interact with the data.
                          Filtering options include:subsetting the data per column,
                          downloading,copying,printing or searching data."),
                       h3("PLOT EXPLORER."),
                       h4("The user can plot the data in different ways and see summary statistics."),
                       h3("COLOR MAPPING."),
                       h4("Values are uniquely identified by colors based on the
                          user selection."),
                       h3("INTERACTIVE MAPPING."),
                       h4("The user can select a variety of options and low-income areas meeting the
                          specified conditions are mapped."),
                       h3("LINK:"),
                       h4("Users can interact with the app through the link:",
                          a(href="https://aquayaapps.shinyapps.io/kisumu_app/", 
                            "https://aquayaapps.shinyapps.io/kisumu_app/"))
                       
                       
                       )
                
                
                
                       )
                       ),
      tabItem(
        tabName = "data",
        h3("INTERACTING WITH DATA"),
        
        fluidRow(
          box(title = "OUTPUT",status = "primary",solidHeader = TRUE,
              
              #datatableoutput
              DT::dataTableOutput("table"),
              width = 12
              
          ))),
      tabItem(
        tabName = "plot",
        h3("PLOT INTERACTION"),
        fluidRow(
          box(
            title = "AXIS VALUES",status = "primary",solidHeader = TRUE,
            radioButtons(inputId = "status",
                         label = "SELECT X VALUE:",
                         choices = colnames(kisumu1)
                         
            ),
            radioButtons(inputId = "status2",
                         label = "SELECT Y VALUE:",
                         choices = colnames(kisumu2)
                         
                         
            ),width=4
          ),
          box(
            title = "PLOT OUTPUT",status = "primary",solidHeader = TRUE,
            plotOutput("stat",click = "plot_click"),
            verbatimTextOutput("info"),
            width = 8
          ))
        
      ),
      tabItem(
        tabName = "map",
        h3("COLOR MAP"),
        fluidPage(
          
          title = "MAP DISPLAY",status = "primary",solidHeader = TRUE,
          leafletOutput("leaf",width="100%",height = 500),
          
          fluidRow(
            column(4,
                   selectInput(inputId = "area1",
                               label = " AREA TYPE:",
                               choices =c(
                                 "All",
                                 unique(as.character(lias$Are_Typ))
                               )
                               
                               
                               
                   )),
            
            column(4,
                   selectInput(inputId = "stl",
                               label = " SETTLEMENT:",
                               choices = c(
                                 "All",
                                 unique(as.character(lias$Sttlmnt))
                               )
                   )),
            column(4,
                   selectInput(inputId = "lg",
                               label = " LEGALIZED:",
                               choices = c(
                                 "All",
                                 unique(as.character(lias$Legalsd))
                               )
                   ))
            
          ),
          fluidRow(
            column(4,
                   selectInput(
                     inputId = "wtd",
                     label = "WATER TABLE DRY:",
                     choices = c(
                       "All",
                       unique(as.character(lias$Wtr_T_D))
                     )
                   )),
            column(4,
                   selectInput(
                     inputId = "wtr",
                     label = "WATER TABLE RAINY:",
                     choices=c(
                       "All",
                       unique(as.character(lias$Wtr_T_R))
                     )
                   )),
            column(4,
                   selectInput(
                     inputId = "fld",
                     label = "FLOODING:",
                     choices = c(
                       "All",
                       unique(as.character(lias$Floodng))
                     )
                   ))),
          fluidRow(
            
            column(4,
                   selectInput(
                     inputId = "at",
                     label = "AREA TOPOLOGY:",
                     choices = c(
                       "All",
                       unique(as.character(lias$Ar_Tplg))
                     )
                   )),
            column(4,
                   selectInput(
                     inputId = "sss",
                     label = "SOIL SELF SUPPORTING:",
                     choices=c(
                       "All",
                       unique(as.character(lias$Sl_Sl_S))
                     )
                   )),
            column(4,
                   selectInput(
                     inputId = "al",
                     label = "AREA LOCATION:",
                     choices = c(
                       "All",
                       unique(as.character(lias$Ar_Lctn))
                     )
                   ))
            
          ),
          fluidRow(
            column(4,
                   selectInput(
                     inputId = "soil",
                     label = "SOIL TYPE:",
                     choices = c(
                       "All",
                       unique(as.character(lias$Soil))
                     )
                   )),
            
            column(4,
                   selectInput(
                     inputId = "pop",
                     label = "POPULATION:",
                     choices=c(
                       "All"=1,
                       "Population Per Km2"=2
                     )
                   )),
            
            column(4,
                   selectInput(
                     inputId = "san",
                     label = "SANITATION TYPE:",
                     choices = c(
                       "All"=1,
                       "Flush Toilets"=2,
                       "Other Improved"=3,
                       "UnImproved"=4,
                       "Open Defecation"=5,
                       "Sanitation Sharing"=6
                     )
                   )
            )
            
          ),
          fluidRow(
            column(6,
                   selectInput(
                     inputId = "pw",
                     label = "WATER ACCESS:",
                     choices = c(
                       "All"=1,
                       "Piped Water On Plot"=2,
                       "Water source On Plot"=3
                     )
                   )
            ),
            column(6,
                   
                   checkboxGroupInput(
                     inputId = "unknown1",
                     label = "AREAS:",
                     choices=c(
                       "Unknown Areas"=1
                     )
                   )
                   
            )
            
          )
        )
      ),
      
      tabItem(
        tabName = "map2",
        h3("INTERACTIVE MAP"),
        fluidPage(
          
          title = "MAP DISPLAY",status = "primary",solidHeader = TRUE,
          leafletOutput("leaf2",height = 500),
          
          
          #h2("USER EXPLORER",style="color:#3474A7"),
          fluidRow(
            column(6,
                   #slider input for population per km2  
                   sliderInput(inputId = "pop2",
                               label = "Population Per km2:",
                               min = min(lias@data$P_P_KM2,na.rm =T),
                               max = max(lias@data$P_P_KM2,na.rm =T),
                               value = c(min(lias@data$P_P_KM2,na.rm =T),
                                         max(lias@data$P_P_KM2,na.rm =T))
                   ),
                   #slider input for piped water on plot  
                   sliderInput(inputId = "pw2",
                               label = "Piped Water On Plot:",
                               min = min(lias@data$P_D__P_,na.rm =T),
                               max = max(lias@data$P_D__P_,na.rm =T),
                               value = c(min(lias@data$P_D__P_,na.rm =T),
                                         max(lias@data$P_D__P_,na.rm =T))
                   )),
            column(6,
                   #slider input for water source on plot  
                   sliderInput(inputId = "ws",
                               label = "Water Source On Plot:",
                               min = min(lias@data$P_D__W_,na.rm =T),
                               max = max(lias@data$P_D__W_,na.rm =T),
                               value = c(min(lias@data$P_D__W_,na.rm =T),
                                         max(lias@data$P_D__W_,na.rm =T))
                   ),
                   
                   #slider input for flush toilets  
                   sliderInput(inputId = "ft",
                               label = "Flush Toilets:",
                               min = min(lias@data$P_D__F_,na.rm =T),
                               max = max(lias@data$P_D__F_,na.rm =T),
                               value = c(min(lias@data$P_D__F_,na.rm =T),
                                         max(lias@data$P_D__F_,na.rm =T))
                   ))),
          fluidRow(
            column(6,
                   
                   #slider input for Other Improved
                   sliderInput(inputId = "om",
                               label = "Other Improved:",
                               min = min(lias@data$P_D__O_I,na.rm =T),
                               max = max(lias@data$P_D__O_I,na.rm =T),
                               value = c(min(lias@data$P_D__O_I,na.rm =T),
                                         max(lias@data$P_D__O_I,na.rm =T))
                   ),
                   
                   #slider input for unimproved
                   sliderInput(inputId = "um",
                               label = "Unimproved:",
                               min = min(lias@data$Pr_D__U,na.rm =T),
                               max = max(lias@data$Pr_D__U,na.rm =T),
                               value = c(min(lias@data$Pr_D__U,na.rm =T),
                                         max(lias@data$Pr_D__U,na.rm =T))
                   )
            ),
            column(6,
                   
                   #slider input for open defecation
                   sliderInput(inputId = "od",
                               label = "Open Defecation:",
                               min = min(lias@data$P_D__O_D,na.rm =T),
                               max = max(lias@data$P_D__O_D,na.rm =T),
                               value = c(min(lias@data$P_D__O_D,na.rm =T),
                                         max(lias@data$P_D__O_D,na.rm =T))
                   )
                   
                   
            )
            
          )
          
        )
        
      )
    )
    
                )
   
)

#server
server <- function(input, output) {
   
  output$table<-DT::renderDataTable({
    DT::datatable(kisumu,filter="top",
                  extensions = "Buttons",options=list(scrollX=TRUE,autoWidth=TRUE,
                                                      dom="Bfrtip",buttons=list("copy","print",list(
                                                        extend="collection",buttons=c("csv","excel","pdf"),
                                                        text="Download"
                                                      ))
                  ))
    
    
  })
  
  #plot explorer 
  output$stat<-renderPlot({
    x<-kisumu[,c(input$status,input$status2)]
    plot(x,col = "#75AADB", pch = 19,main=paste0(input$status2,"  vs ",input$status))
    
  })
  
  #average lias
  output$info <- (renderText({
    paste0("VALUE=", round(as.numeric(input$plot_click$y,0)),
           "\n% AREA TYPE OF DWELLINGS:\nPlanned:32%\nUnplanned:68%",
           "\n% WATER ACCESS TYPE OF DWELLINGS:\nPiped Water On Plot:3%\nWater Source On Plot:19%",
           "\n% SANITATION TYPE OF DWELLINGS:\nFlush Toilets:13%\nUnimproved:60%\nOther Improved:25%\nOpen Defecation:2%"
           
           
    )
    
  })) 
  
  #COLOR MAPPING
  #Reactive functions
  
  #reactive function for area type
  area<-reactive({
    data<-lias
    if(input$area1!="All"){
      data<-subset(data,Are_Typ %in% input$area1)
      #data<-data[data$AreaType==input$area,]
    }
    
    return(data)
  })
  
  #reactive function for settlement
  stlmnt<-reactive({
    data<-lias
    if(input$stl!="All"){
      data<-subset(data,Sttlmnt %in% input$stl)
      #data<-data[data$AreaType==input$area,]
    }
    
    return(data)
  })
  
  #reactive function for legalized
  lglz<-reactive({
    data<-lias
    if(input$lg!="All"){
      data<-subset(data,Legalsd %in% input$lg)
      #data<-data[data$AreaType==input$area,]
    }
    
    return(data)
  })
  
  #reactive function for water table dry
  wtd<-reactive({
    data<-lias
    if(input$wtd!="All"){
      data<-subset(data,Wtr_T_D %in% input$wtd)
      #data<-data[data$AreaType==input$area,]
    }
    
    return(data)
  })
  
  #reactive function for water table rainy
  wtr<-reactive({
    data<-lias
    if(input$wtr!="All"){
      data<-subset(data,Wtr_T_R %in% input$wtr)
      #data<-data[data$AreaType==input$area,]
    }
    
    return(data)
  })
  
  #reactive function for flooding
  fld<-reactive({
    data<-lias
    if(input$fld!="All"){
      data<-subset(data,Floodng %in% input$fld)
      #data<-data[data$AreaType==input$area,]
    }
    
    return(data)
  })
  
  #reactive function for soil
  soil<-reactive({
    data<-lias
    if(input$soil!="All"){
      data<-subset(data,Soil %in% input$soil)
      #data<-data[data$AreaType==input$area,]
    }
    
    return(data)
  })
  
  #reactive function for soil self supporting
  sss<-reactive({
    data<-lias
    if(input$sss!="All"){
      data<-subset(data,Sl_Sl_S %in% input$sss)
      #data<-data[data$AreaType==input$area,]
    }
    
    return(data)
  })
  
  #reactive function for area location
  lctn<-reactive({
    data<-lias
    if(input$al!="All"){
      data<-subset(data,Ar_Lctn %in% input$al)
      #data<-data[data$AreaType==input$area,]
    }
    
    return(data)
  })
  
  #reactive function for area topology
  tplgy<-reactive({
    data<-lias
    if(input$at!="All"){
      data<-subset(data,Ar_Tplg %in% input$at)
      #data<-data[data$AreaType==input$area,]
    }
    
    return(data)
  })
  
  #reactive function for population per km2 
  ppd<-reactive({
    dt<-lias
    if(input$pop==1){
      dt[dt$P_P_KM2<=10000,]
    } else if(input$pop==3){
      dt[dt$P_P_KM2>10000&dt$P_P_KM2<=60000,]
    } else if(input$pop==4){
      dt[dt$P_P_KM2>60000,]
    }
    return(dt)
  })
  
  #reactive function for sanitation type
  st<-reactive({
    dm<-lias
    if(input$san==2){
      dm[dm$P_D__F_<=25,]
    }
    if(input$san==3)
    {
      dm[dm$P_D__O_I<=50,]
    }
    if(input$san==4)
    {
      dm[dm$Pr_D__U<=50,]
    }
    if(input$san==5)
    {
      dm[dm$P_D__O_D<=50,]
    }
    if(input$san==6)
    {
      dm[dm$P_D__S_<=50,]
    }
    
    return(dm)
  })
  
  #reactive function for water access type
  wat<-reactive({
    dm<-lias
    
    if("2" %in% input$pw){
      dm[dm$P_D__P_<=25,]
    }
    
    if("3" %in% input$pw){
      dm[dm$P_D__W_<=25,]
    }
    
    dm
  })
  
  #sliderinput reactive function for all numeric input options
  sld<-reactive({
    subset(lias,lias@data$P_P_KM2>=input$pop2[1]&
             lias@data$P_P_KM2<=input$pop2[2]&
             lias@data$P_D__P_>=input$pw2[1]&
             lias@data$P_D__P_<=input$pw2[2]&
             lias@data$P_D__W_>=input$ws[1]&
             lias@data$P_D__W_<=input$ws[2]& 
             lias@data$P_D__F_>=input$ft[1]&
             lias@data$P_D__F_<=input$ft[2]&
             lias@data$P_D__O_I>=input$om[1]&
             lias@data$P_D__O_I<=input$om[2]&
             lias@data$Pr_D__U>=input$um[1]&
             lias@data$Pr_D__U<=input$um[2]&
             lias@data$P_D__O_D>=input$od[1]&
             lias@data$P_D__O_D<=input$od[2]
             
           
    )
    
  })
  
  #Base map for color mapping
  output$leaf<-renderLeaflet({
    
    leaflet(lias) %>%
      
      #Initializing the map
      #setView(lat=0,long=0,zoom=15)%>%
      
      #default map
      #Add default OpenStreetMap map tiles
      addTiles()%>%
      
      #addProviderTiles("Esri.NatGeoWorldMap")%>%  
      #addProviderTiles("CartoDB.Positron")%>%
      
      #kisumu lias polygons
      addPolygons(
        data = lias,
        fillColor = "blue",
        weight = 1, smoothFactor = 0.5,
        opacity = 1.0, fillOpacity = 1.0,
        highlightOptions = highlightOptions(
          weight = 2,
          color = "red",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label =lias$Name,
        popup = ~paste("<strong>Area Type:</strong>",Are_Typ,
                       "<br>",
                       "<strong>Piped Water On Plot:</strong>",P_D__W_,"%",
                       "<br>",
                       "<strong>Water Source On Plot:</strong>",P_D__P_,"%",
                       "<br>",
                       "<strong>Flash Toilets:</strong>",P_D__F_,"%",
                       "<br>",
                       "<strong>Other Improved:</strong>",P_D__O_I,"%",
                       "<br>",
                       "<strong>Unimproved:</strong>",Pr_D__U,"%",
                       "<br>",
                       "<strong>Open Defecation:</strong>",P_D__O_D,"%",
                       "<br>",
                       "<strong>Population Per km2:</strong>",P_P_KM2
        )
        
      ) 
  })
  
  #Base map for interactive mapping(default)
  
  output$leaf2<-renderLeaflet({
    
    
    
    leaflet(lias) %>%
      
      #Initializing the map
      # setView(lng=36.092245, lat=-00.292115,zoom=15)%>%
      
      #default map
      #Add default OpenStreetMap map tiles
      addTiles()%>%
      
      # addProviderTiles("Esri.NatGeoWorldMap",group = "default")%>%  
      #addProviderTiles("CartoDB.Positron",group = "custom")%>%
      
      #nakuru lias polygons
      addPolygons(
        data = lias,
        fillColor = "blue",
        weight = 1, smoothFactor = 0.5,
        opacity = 1.0, fillOpacity = 1.0,
        highlightOptions = highlightOptions(
          weight = 2,
          color = "red",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label =lias$Name,
        popup = ~paste("<strong>Area Type:</strong>",Are_Typ,
                       "<br>",
                       "<strong>Piped Water On Plot:</strong>",P_D__W_,"%",
                       "<br>",
                       "<strong>Water Source On Plot:</strong>",P_D__P_,"%",
                       "<br>",
                       "<strong>Flash Toilets:</strong>",P_D__F_,"%",
                       "<br>",
                       "<strong>Other Improved:</strong>",P_D__O_I,"%",
                       "<br>",
                       "<strong>Unimproved:</strong>",Pr_D__U,"%",
                       "<br>",
                       "<strong>Open Defecation:</strong>",P_D__O_D,"%",
                       "<br>",
                       "<strong>Population Per km2:</strong>",P_P_KM2
        )
        
      ) 
    
    
    
    
  })
  
  #observe function for slider input numeric options
  observe({
    
    #color mapping function
    pal<-colorNumeric(palette = "plasma",domain=input$pop2)
    #pal1 <- colorBin("plasma",lias$PpDnsty, 15, pretty = TRUE)
    #pal1<- colorBin("Blues", lias$PpDnsty, 2, pretty = FALSE)
    
    
    leafletProxy("leaf2",data=sld()) %>%
      
      #Initializing the map
      #setView(lng=36.092245	, lat=-00.292115,zoom=10)%>%
      # clearMarkers() %>%
      clearControls() %>%
      clearShapes()%>%
      addPolygons(
        weight = 1, smoothFactor = 0.5,
        #fillColor = ~pal(input$pop2),
        opacity = 1.0, fillOpacity = 1.0,
        highlightOptions = highlightOptions(
          weight = 2,
          color = "red",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label =lias$Name,
        popup = ~paste("<strong>Area Type:</strong>",Are_Typ,
                       "<br>",
                       "<strong>Piped Water On Plot:</strong>",P_D__W_,"%",
                       "<br>",
                       "<strong>Water Source On Plot:</strong>",P_D__P_,"%",
                       "<br>",
                       "<strong>Flash Toilets:</strong>",P_D__F_,"%",
                       "<br>",
                       "<strong>Other Improved:</strong>",P_D__O_I,"%",
                       "<br>",
                       "<strong>Unimproved:</strong>",Pr_D__U,"%",
                       "<br>",
                       "<strong>Open Defecation:</strong>",P_D__O_D,"%",
                       "<br>",
                       "<strong>Population Per km2:</strong>",P_P_KM2
        )
        
      ) 
    
  })
  
  #observe function for area type
  observe({
    #color mapping function for area type
    pal<-colorFactor(rainbow(7),lias$Are_Typ)
    
    leafletProxy("leaf",data=area()) %>%
      
      clearMarkers() %>%
      clearControls() %>%
      clearShapes()%>%
      addPolygons(
        weight = 1, smoothFactor = 0.5,
        opacity = 1.0, fillOpacity = 1.0,
        #data=fdata(),
        fillColor = ~pal(Are_Typ),
        highlightOptions = highlightOptions(
          weight = 2,
          color = "red",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label =lias$Name,
        popup = ~paste("<strong>Area Type:</strong>",Are_Typ,
                       "<br>",
                       "<strong>Piped Water On Plot:</strong>",P_D__W_,"%",
                       "<br>",
                       "<strong>Water Source On Plot:</strong>",P_D__P_,"%",
                       "<br>",
                       "<strong>Flash Toilets:</strong>",P_D__F_,"%",
                       "<br>",
                       "<strong>Other Improved:</strong>",P_D__O_I,"%",
                       "<br>",
                       "<strong>Unimproved:</strong>",Pr_D__U,"%",
                       "<br>",
                       "<strong>Open Defecation:</strong>",P_D__O_D,"%",
                       "<br>",
                       "<strong>Population Per km2:</strong>",P_P_KM2
        )
        
      )%>%
      addLegend(title = "AreaType", position = "topleft",
                pal = pal, values = ~Are_Typ, opacity = 1)
  })
  
  #observe function for settlement
  observe({
    #color mapping function for area type
    pal<-colorFactor(rainbow(7),lias$Sttlmnt)
    
    leafletProxy("leaf",data=stlmnt()) %>%
      
      clearMarkers() %>%
      clearControls() %>%
      clearShapes()%>%
      addPolygons(
        weight = 1, smoothFactor = 0.5,
        opacity = 1.0, fillOpacity = 1.0,
        fillColor = ~pal(Sttlmnt),
        highlightOptions = highlightOptions(
          weight = 2,
          color = "red",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label =lias$Name,
        popup = ~paste("<strong>Area Type:</strong>",Are_Typ,
                       "<br>",
                       "<strong>Piped Water On Plot:</strong>",P_D__W_,"%",
                       "<br>",
                       "<strong>Water Source On Plot:</strong>",P_D__P_,"%",
                       "<br>",
                       "<strong>Flash Toilets:</strong>",P_D__F_,"%",
                       "<br>",
                       "<strong>Other Improved:</strong>",P_D__O_I,"%",
                       "<br>",
                       "<strong>Unimproved:</strong>",Pr_D__U,"%",
                       "<br>",
                       "<strong>Open Defecation:</strong>",P_D__O_D,"%",
                       "<br>",
                       "<strong>Population Per km2:</strong>",P_P_KM2
        )
        
      )%>%
      addLegend(title = "Settlement", position = "topleft",
                pal = pal, values = ~Sttlmnt, opacity = 1)
  })
  
  #observe function for legalized
  observe({
    #color mapping function for legalized
    pal<-colorFactor(rainbow(7),lias$Legalsd)
    
    leafletProxy("leaf",data=lglz()) %>%
      
      clearMarkers() %>%
      clearControls() %>%
      clearShapes()%>%
      addPolygons(
        weight = 1, smoothFactor = 0.5,
        opacity = 1.0, fillOpacity = 1.0,
        fillColor = ~pal(Legalsd),
        highlightOptions = highlightOptions(
          weight = 2,
          color = "red",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label =lias$Name,
        popup = ~paste("<strong>Area Type:</strong>",Are_Typ,
                       "<br>",
                       "<strong>Piped Water On Plot:</strong>",P_D__W_,"%",
                       "<br>",
                       "<strong>Water Source On Plot:</strong>",P_D__P_,"%",
                       "<br>",
                       "<strong>Flash Toilets:</strong>",P_D__F_,"%",
                       "<br>",
                       "<strong>Other Improved:</strong>",P_D__O_I,"%",
                       "<br>",
                       "<strong>Unimproved:</strong>",Pr_D__U,"%",
                       "<br>",
                       "<strong>Open Defecation:</strong>",P_D__O_D,"%",
                       "<br>",
                       "<strong>Population Per km2:</strong>",P_P_KM2
        )
        
      )%>%
      addLegend(title = "Legalized", position = "topleft",
                pal = pal, values = ~Legalsd, opacity = 1)
  })
  
  #observe function for water table dry
  observe({
    #color mapping function for wtd
    pal<-colorFactor(rainbow(7),lias$Wtr_T_D)
    
    leafletProxy("leaf",data=wtd()) %>%
      
      clearMarkers() %>%
      clearControls() %>%
      clearShapes()%>%
      addPolygons(
        weight = 1, smoothFactor = 0.5,
        opacity = 1.0, fillOpacity = 1.0,
        fillColor = ~pal(Wtr_T_D),
        highlightOptions = highlightOptions(
          weight = 2,
          color = "red",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label =lias$Name,
        popup = ~paste("<strong>Area Type:</strong>",Are_Typ,
                       "<br>",
                       "<strong>Piped Water On Plot:</strong>",P_D__W_,"%",
                       "<br>",
                       "<strong>Water Source On Plot:</strong>",P_D__P_,"%",
                       "<br>",
                       "<strong>Flash Toilets:</strong>",P_D__F_,"%",
                       "<br>",
                       "<strong>Other Improved:</strong>",P_D__O_I,"%",
                       "<br>",
                       "<strong>Unimproved:</strong>",Pr_D__U,"%",
                       "<br>",
                       "<strong>Open Defecation:</strong>",P_D__O_D,"%",
                       "<br>",
                       "<strong>Population Per km2:</strong>",P_P_KM2
        )
        
      )%>%
      addLegend(title = "Water Table Dry", position = "topleft",
                pal = pal, values = ~Wtr_T_D, opacity = 1)
  })
  
  #observe function for water table rainy
  observe({
    #color mapping function for wtr
    pal<-colorFactor(rainbow(7),lias$Wtr_T_R)
    
    leafletProxy("leaf",data=wtr()) %>%
      
      clearMarkers() %>%
      clearControls() %>%
      clearShapes()%>%
      addPolygons(
        weight = 1, smoothFactor = 0.5,
        opacity = 1.0, fillOpacity = 1.0,
        fillColor = ~pal(Wtr_T_R),
        highlightOptions = highlightOptions(
          weight = 2,
          color = "red",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label =lias$Name,
        popup = ~paste("<strong>Area Type:</strong>",Are_Typ,
                       "<br>",
                       "<strong>Piped Water On Plot:</strong>",P_D__W_,"%",
                       "<br>",
                       "<strong>Water Source On Plot:</strong>",P_D__P_,"%",
                       "<br>",
                       "<strong>Flash Toilets:</strong>",P_D__F_,"%",
                       "<br>",
                       "<strong>Other Improved:</strong>",P_D__O_I,"%",
                       "<br>",
                       "<strong>Unimproved:</strong>",Pr_D__U,"%",
                       "<br>",
                       "<strong>Open Defecation:</strong>",P_D__O_D,"%",
                       "<br>",
                       "<strong>Population Per km2:</strong>",P_P_KM2
        )
        
      )%>%
      addLegend(title = "Water Table Rainy", position = "topleft",
                pal = pal, values = ~Wtr_T_R, opacity = 1)
  })
  
  #observe function for flooding
  observe({
    #color mapping function for flooding
    pal<-colorFactor(rainbow(7),lias$Floodng)
    
    leafletProxy("leaf",data=fld()) %>%
      
      clearMarkers() %>%
      clearControls() %>%
      clearShapes()%>%
      addPolygons(
        weight = 1, smoothFactor = 0.5,
        opacity = 1.0, fillOpacity = 1.0,
        #data=fdata(),
        fillColor = ~pal(Floodng),
        highlightOptions = highlightOptions(
          weight = 2,
          color = "red",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label =lias$Name,
        popup = ~paste("<strong>Area Type:</strong>",Are_Typ,
                       "<br>",
                       "<strong>Piped Water On Plot:</strong>",P_D__W_,"%",
                       "<br>",
                       "<strong>Water Source On Plot:</strong>",P_D__P_,"%",
                       "<br>",
                       "<strong>Flash Toilets:</strong>",P_D__F_,"%",
                       "<br>",
                       "<strong>Other Improved:</strong>",P_D__O_I,"%",
                       "<br>",
                       "<strong>Unimproved:</strong>",Pr_D__U,"%",
                       "<br>",
                       "<strong>Open Defecation:</strong>",P_D__O_D,"%",
                       "<br>",
                       "<strong>Population Per km2:</strong>",P_P_KM2
        )
        
      )%>%
      addLegend(title = "Flooding", position = "topleft",
                pal = pal, values = ~Floodng, opacity = 1)
  })
  
  #observe function for soil
  observe({
    #color mapping function for soil
    pal<-colorFactor(rainbow(7),lias$Soil)
    
    leafletProxy("leaf",data=soil()) %>%
      
      clearMarkers() %>%
      clearControls() %>%
      clearShapes()%>%
      addPolygons(
        weight = 1, smoothFactor = 0.5,
        opacity = 1.0, fillOpacity = 1.0,
        fillColor = ~pal(Soil),
        highlightOptions = highlightOptions(
          weight = 2,
          color = "red",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label =lias$Name,
        popup = ~paste("<strong>Area Type:</strong>",Are_Typ,
                       "<br>",
                       "<strong>Piped Water On Plot:</strong>",P_D__W_,"%",
                       "<br>",
                       "<strong>Water Source On Plot:</strong>",P_D__P_,"%",
                       "<br>",
                       "<strong>Flash Toilets:</strong>",P_D__F_,"%",
                       "<br>",
                       "<strong>Other Improved:</strong>",P_D__O_I,"%",
                       "<br>",
                       "<strong>Unimproved:</strong>",Pr_D__U,"%",
                       "<br>",
                       "<strong>Open Defecation:</strong>",P_D__O_D,"%",
                       "<br>",
                       "<strong>Population Per km2:</strong>",P_P_KM2
        )
        
      )%>%
      addLegend(title = "Soil", position = "topleft",
                pal = pal, values = ~Soil, opacity = 1)
  })
  
  #observe function for soil self supporting
  observe({
    #color mapping function for sss
    pal<-colorFactor(rainbow(7),lias$Sl_Sl_S)
    
    leafletProxy("leaf",data=sss()) %>%
      
      clearMarkers() %>%
      clearControls() %>%
      clearShapes()%>%
      addPolygons(
        weight = 1, smoothFactor = 0.5,
        opacity = 1.0, fillOpacity = 1.0,
        #data=fdata(),
        fillColor = ~pal(Sl_Sl_S),
        highlightOptions = highlightOptions(
          weight = 2,
          color = "red",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label =lias$Name,
        popup = ~paste("<strong>Area Type:</strong>",Are_Typ,
                       "<br>",
                       "<strong>Piped Water On Plot:</strong>",P_D__W_,"%",
                       "<br>",
                       "<strong>Water Source On Plot:</strong>",P_D__P_,"%",
                       "<br>",
                       "<strong>Flash Toilets:</strong>",P_D__F_,"%",
                       "<br>",
                       "<strong>Other Improved:</strong>",P_D__O_I,"%",
                       "<br>",
                       "<strong>Unimproved:</strong>",Pr_D__U,"%",
                       "<br>",
                       "<strong>Open Defecation:</strong>",P_D__O_D,"%",
                       "<br>",
                       "<strong>Population Per km2:</strong>",P_P_KM2
        )
        
      )%>%
      addLegend(title = "Soil Self Supporting", position = "topleft",
                pal = pal, values = ~Sl_Sl_S, opacity = 1)
  })
  
  #observe function for area location
  observe({
    #color mapping function for area location
    pal<-colorFactor(rainbow(7),lias$Ar_Lctn)
    
    leafletProxy("leaf",data=lctn()) %>%
      
      clearMarkers() %>%
      clearControls() %>%
      clearShapes()%>%
      addPolygons(
        weight = 1, smoothFactor = 0.5,
        opacity = 1.0, fillOpacity = 1.0,
        fillColor = ~pal(Ar_Lctn),
        highlightOptions = highlightOptions(
          weight = 2,
          color = "red",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label =lias$Name,
        popup = ~paste("<strong>Area Type:</strong>",Are_Typ,
                       "<br>",
                       "<strong>Piped Water On Plot:</strong>",P_D__W_,"%",
                       "<br>",
                       "<strong>Water Source On Plot:</strong>",P_D__P_,"%",
                       "<br>",
                       "<strong>Flash Toilets:</strong>",P_D__F_,"%",
                       "<br>",
                       "<strong>Other Improved:</strong>",P_D__O_I,"%",
                       "<br>",
                       "<strong>Unimproved:</strong>",Pr_D__U,"%",
                       "<br>",
                       "<strong>Open Defecation:</strong>",P_D__O_D,"%",
                       "<br>",
                       "<strong>Population Per km2:</strong>",P_P_KM2
        )
        
      )%>%
      addLegend(title = "Area Location", position = "topleft",
                pal = pal, values = ~Ar_Lctn, opacity = 1)
  })
  
  #observe function for area topology
  observe({
    #color mapping function for area topology
    pal<-colorFactor(rainbow(7),lias$Ar_Tplg)
    
    leafletProxy("leaf",data=tplgy()) %>%
      
      clearMarkers() %>%
      clearControls() %>%
      clearShapes()%>%
      addPolygons(
        weight = 1, smoothFactor = 0.5,
        opacity = 1.0, fillOpacity = 1.0,
        #data=fdata(),
        fillColor = ~pal(Ar_Tplg),
        highlightOptions = highlightOptions(
          weight = 2,
          color = "red",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label =lias$Name,
        popup = ~paste("<strong>Area Type:</strong>",Are_Typ,
                       "<br>",
                       "<strong>Piped Water On Plot:</strong>",P_D__W_,"%",
                       "<br>",
                       "<strong>Water Source On Plot:</strong>",P_D__P_,"%",
                       "<br>",
                       "<strong>Flash Toilets:</strong>",P_D__F_,"%",
                       "<br>",
                       "<strong>Other Improved:</strong>",P_D__O_I,"%",
                       "<br>",
                       "<strong>Unimproved:</strong>",Pr_D__U,"%",
                       "<br>",
                       "<strong>Open Defecation:</strong>",P_D__O_D,"%",
                       "<br>",
                       "<strong>Population Per km2:</strong>",P_P_KM2
        )
        
      )%>%
      addLegend(title = "Area Topology", position = "topleft",
                pal = pal, values = ~Ar_Tplg, opacity = 1)
  })
  
  #observe function for population per km2
  observe({
    
    #color mapping function
    #pal1<-colorNumeric(palette = "magma",mp$PpDnsty)
    pal1 <- colorBin("plasma",lias$P_P_KM2, 5, pretty = TRUE)
    
    leafletProxy("leaf",data=ppd()) %>%
      
      # clearMarkers() %>%
      clearControls() %>%
      clearShapes()%>%
      addPolygons(
        weight = 1, smoothFactor = 0.5,
        opacity = 1.0, fillOpacity = 1.0,
        fillColor = ~pal1(P_P_KM2),
        highlightOptions = highlightOptions(
          weight = 2,
          color = "red",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label =lias$Name,
        popup = ~paste("<strong>Area Type:</strong>",Are_Typ,
                       "<br>",
                       "<strong>Piped Water On Plot:</strong>",P_D__W_,"%",
                       "<br>",
                       "<strong>Water Source On Plot:</strong>",P_D__P_,"%",
                       "<br>",
                       "<strong>Flash Toilets:</strong>",P_D__F_,"%",
                       "<br>",
                       "<strong>Other Improved:</strong>",P_D__O_I,"%",
                       "<br>",
                       "<strong>Unimproved:</strong>",Pr_D__U,"%",
                       "<br>",
                       "<strong>Open Defecation:</strong>",P_D__O_D,"%",
                       "<br>",
                       "<strong>Population Per km2:</strong>",P_P_KM2
        )
        
      )%>%
      addLegend(title = "Population Per km2", position = "topleft",
                pal = pal1, values = ~P_P_KM2, opacity = 1)
    
  })
  
  #observe function for sanitation type
  observe({
    #color mapping function
    #pal1<-colorNumeric(palette = "magma",mp$PpDnsty)
    pal1 <- colorBin("plasma", lias$P_D__F_, 5, pretty = TRUE)
    pal2<-colorBin("plasma", lias$P_D__O_I, 5, pretty = TRUE)
    pal3 <- colorBin("plasma", lias$Pr_D__U, 5, pretty = TRUE)
    pal4<-colorBin("plasma", lias$P_D__O_D, 5, pretty = TRUE)
    
    
    
    #leafletproxy function
    md<-leafletProxy("leaf",data=st()) %>% clearControls() %>% clearShapes()
    
    # clearMarkers() %>%
    #nakuru lias if function
    if("1" %in% input$san){
      md %>%
        
        #Base map
        #Add default OpenStreetMap map tiles
        addTiles()%>%
        #addProviderTiles("Esri.NatGeoWorldMap")%>%  
        #addProviderTiles("CartoDB.Positron")%>%
        
        #Overlay lias map
        addPolygons(
          
          fillColor = "blue",
          weight = 1, smoothFactor = 0.5,
          opacity = 1.0, fillOpacity = 1.0,
          highlightOptions = highlightOptions(
            weight = 2,
            color = "red",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label =lias$Name,
          popup = ~paste("<strong>Area Type:</strong>",Are_Typ,
                         "<br>",
                         "<strong>Piped Water On Plot:</strong>",P_D__W_,"%",
                         "<br>",
                         "<strong>Water Source On Plot:</strong>",P_D__P_,"%",
                         "<br>",
                         "<strong>Flash Toilets:</strong>",P_D__F_,"%",
                         "<br>",
                         "<strong>Other Improved:</strong>",P_D__O_I,"%",
                         "<br>",
                         "<strong>Unimproved:</strong>",Pr_D__U,"%",
                         "<br>",
                         "<strong>Open Defecation:</strong>",P_D__O_D,"%",
                         "<br>",
                         "<strong>Population Per km2:</strong>",P_P_KM2
          )
          
        ) 
    }
    
    #flash toilets if function
    if("2" %in% input$san){
      md%>%
        addPolygons(
          weight = 1, smoothFactor = 0.5,
          opacity = 1.0, fillOpacity = 1.0,
          fillColor = ~pal1(P_D__F_),
          highlightOptions = highlightOptions(
            weight = 2,
            color = "red",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label =lias$Name,
          popup = ~paste("<strong>Area Type:</strong>",Are_Typ,
                         "<br>",
                         "<strong>Piped Water On Plot:</strong>",P_D__W_,"%",
                         "<br>",
                         "<strong>Water Source On Plot:</strong>",P_D__P_,"%",
                         "<br>",
                         "<strong>Flash Toilets:</strong>",P_D__F_,"%",
                         "<br>",
                         "<strong>Other Improved:</strong>",P_D__O_I,"%",
                         "<br>",
                         "<strong>Unimproved:</strong>",Pr_D__U,"%",
                         "<br>",
                         "<strong>Open Defecation:</strong>",P_D__O_D,"%",
                         "<br>",
                         "<strong>Population Per km2:</strong>",P_P_KM2
          )
          
        )%>%
        addLegend(title = "Flush Toilets (%):", position = "topleft",
                  pal = pal1, values = ~P_D__F_, opacity = 1)
    }
    
    #otherimproved if function
    if("3" %in% input$san){
      md%>%
        addPolygons(
          weight = 1, smoothFactor = 0.5,
          opacity = 1.0, fillOpacity = 1.0,
          fillColor = ~pal2(P_D__O_I),
          highlightOptions = highlightOptions(
            weight = 2,
            color = "red",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label =lias$Name,
          popup = ~paste("<strong>Area Type:</strong>",Are_Typ,
                         "<br>",
                         "<strong>Piped Water On Plot:</strong>",P_D__W_,"%",
                         "<br>",
                         "<strong>Water Source On Plot:</strong>",P_D__P_,"%",
                         "<br>",
                         "<strong>Flash Toilets:</strong>",P_D__F_,"%",
                         "<br>",
                         "<strong>Other Improved:</strong>",P_D__O_I,"%",
                         "<br>",
                         "<strong>Unimproved:</strong>",Pr_D__U,"%",
                         "<br>",
                         "<strong>Open Defecation:</strong>",P_D__O_D,"%",
                         "<br>",
                         "<strong>Population Per km2:</strong>",P_P_KM2
          )
          
        )%>%
        addLegend(title = "Other Improved (%):", position = "topleft",
                  pal = pal2, values = ~P_D__O_I, opacity = 1)
    }
    
    #unimproved if function
    if("4" %in% input$san){
      md%>%
        addPolygons(
          weight = 1, smoothFactor = 0.5,
          opacity = 1.0, fillOpacity = 1.0,
          fillColor = ~pal3(Pr_D__U),
          highlightOptions = highlightOptions(
            weight = 2,
            color = "red",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label =lias$Name,
          popup = ~paste("<strong>Area Type:</strong>",Are_Typ,
                         "<br>",
                         "<strong>Piped Water On Plot:</strong>",P_D__W_,"%",
                         "<br>",
                         "<strong>Water Source On Plot:</strong>",P_D__P_,"%",
                         "<br>",
                         "<strong>Flash Toilets:</strong>",P_D__F_,"%",
                         "<br>",
                         "<strong>Other Improved:</strong>",P_D__O_I,"%",
                         "<br>",
                         "<strong>Unimproved:</strong>",Pr_D__U,"%",
                         "<br>",
                         "<strong>Open Defecation:</strong>",P_D__O_D,"%",
                         "<br>",
                         "<strong>Population Per km2:</strong>",P_P_KM2
          )
          
        )%>%
        addLegend(title = "UnImproved (%):", position = "topleft",
                  pal = pal3, values = ~Pr_D__U, opacity = 1)
    }
    
    #open defecation if function
    if("5" %in% input$san){
      md%>%
        addPolygons(
          weight = 1, smoothFactor = 0.5,
          opacity = 1.0, fillOpacity = 1.0,
          fillColor = ~pal4(P_D__O_D),
          highlightOptions = highlightOptions(
            weight = 2,
            color = "red",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label =lias$Name,
          popup = ~paste("<strong>Area Type:</strong>",Are_Typ,
                         "<br>",
                         "<strong>Piped Water On Plot:</strong>",P_D__W_,"%",
                         "<br>",
                         "<strong>Water Source On Plot:</strong>",P_D__P_,"%",
                         "<br>",
                         "<strong>Flash Toilets:</strong>",P_D__F_,"%",
                         "<br>",
                         "<strong>Other Improved:</strong>",P_D__O_I,"%",
                         "<br>",
                         "<strong>Unimproved:</strong>",Pr_D__U,"%",
                         "<br>",
                         "<strong>Open Defecation:</strong>",P_D__O_D,"%",
                         "<br>",
                         "<strong>Population Per km2:</strong>",P_P_KM2
          )
          
        )%>%
        addLegend(title = "Open Defecation (%):", position = "topleft",
                  pal = pal4, values = ~P_D__O_D, opacity = 1)
    }
    
    
  })
  
  
  #observe function for water access type
  observe({
    #color mapping function
    #pal1<-colorNumeric(palette = "magma",mp$PpDnsty)
    pal1 <- colorBin("plasma", lias$P_D__P_, 5, pretty = TRUE)
    pal2<-colorBin("plasma", lias$P_D__W_, 5, pretty = TRUE)
    
    md<-leafletProxy("leaf",data=wat()) %>% clearControls() %>% clearShapes()
    
    # clearMarkers() %>%
    if("1" %in% input$pw){
      md %>%
        
        
        
        #Base map
        #Add default OpenStreetMap map tiles
        addTiles()%>%
        #addProviderTiles("Esri.NatGeoWorldMap")%>%  
        #addProviderTiles("CartoDB.Positron")%>%
        
        #Overlay lias map
        addPolygons(
          
          fillColor = "blue",
          weight = 1, smoothFactor = 0.5,
          opacity = 1.0, fillOpacity = 1.0,
          highlightOptions = highlightOptions(
            weight = 2,
            color = "red",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label =lias$Name,
          popup = ~paste("<strong>Area Type:</strong>",Are_Typ,
                         "<br>",
                         "<strong>Piped Water On Plot:</strong>",P_D__W_,"%",
                         "<br>",
                         "<strong>Water Source On Plot:</strong>",P_D__P_,"%",
                         "<br>",
                         "<strong>Flash Toilets:</strong>",P_D__F_,"%",
                         "<br>",
                         "<strong>Other Improved:</strong>",P_D__O_I,"%",
                         "<br>",
                         "<strong>Unimproved:</strong>",Pr_D__U,"%",
                         "<br>",
                         "<strong>Open Defecation:</strong>",P_D__O_D,"%",
                         "<br>",
                         "<strong>Population Per km2:</strong>",P_P_KM2
          )
          
        ) 
    }
    
    #piped water on plot if function
    if("2" %in% input$pw){
      md%>%
        addPolygons(
          weight = 1, smoothFactor = 0.5,
          opacity = 1.0, fillOpacity = 1.0,
          fillColor = ~pal1(P_D__P_),
          highlightOptions = highlightOptions(
            weight = 2,
            color = "red",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label =lias$Name,
          popup = ~paste("<strong>Area Type:</strong>",Are_Typ,
                         "<br>",
                         "<strong>Piped Water On Plot:</strong>",P_D__W_,"%",
                         "<br>",
                         "<strong>Water Source On Plot:</strong>",P_D__P_,"%",
                         "<br>",
                         "<strong>Flash Toilets:</strong>",P_D__F_,"%",
                         "<br>",
                         "<strong>Other Improved:</strong>",P_D__O_I,"%",
                         "<br>",
                         "<strong>Unimproved:</strong>",Pr_D__U,"%",
                         "<br>",
                         "<strong>Open Defecation:</strong>",P_D__O_D,"%",
                         "<br>",
                         "<strong>Population Per km2:</strong>",P_P_KM2
          )
          
        )%>%
        addLegend(title = "Piped Water On Plot(%):", position = "topleft",
                  pal = pal1, values = ~P_D__P_, opacity = 1)
    }
    
    #water source on plot if function
    if("3" %in% input$pw){
      md%>%
        addPolygons(
          weight = 1, smoothFactor = 0.5,
          opacity = 1.0, fillOpacity = 1.0,
          fillColor = ~pal2(P_D__W_),
          highlightOptions = highlightOptions(
            weight = 2,
            color = "red",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label =lias$Name,
          popup = ~paste("<strong>Area Type:</strong>",Are_Typ,
                         "<br>",
                         "<strong>Piped Water On Plot:</strong>",P_D__W_,"%",
                         "<br>",
                         "<strong>Water Source On Plot:</strong>",P_D__P_,"%",
                         "<br>",
                         "<strong>Flash Toilets:</strong>",P_D__F_,"%",
                         "<br>",
                         "<strong>Other Improved:</strong>",P_D__O_I,"%",
                         "<br>",
                         "<strong>Unimproved:</strong>",Pr_D__U,"%",
                         "<br>",
                         "<strong>Open Defecation:</strong>",P_D__O_D,"%",
                         "<br>",
                         "<strong>Population Per km2:</strong>",P_P_KM2
          )
          
        )%>%
        addLegend(title = "WaterSource On Plot(%):", position = "topleft",
                  pal = pal2, values = ~P_D__W_, opacity = 1)
    }
    
    #Unknown Areas overlay map function
    if("1" %in% input$unknown1){
      md%>%
        addPolygons(
          data = unknown,
          weight = 1, smoothFactor = 0.5,
          opacity = 1.0, fillOpacity = 1.0,
          fillColor = "white",
          highlightOptions = highlightOptions(
            weight = 2,
            color = "black",
            fillOpacity = 0.7,
            bringToFront = FALSE
          ),
          label =lias$Name,
          popup = ~paste("<strong>Area Type:</strong>",Are_Typ,
                         "<br>",
                         "<strong>Piped Water On Plot:</strong>",P_D__W_,"%",
                         "<br>",
                         "<strong>Water Source On Plot:</strong>",P_D__P_,"%",
                         "<br>",
                         "<strong>Flash Toilets:</strong>",P_D__F_,"%",
                         "<br>",
                         "<strong>Other Improved:</strong>",P_D__O_I,"%",
                         "<br>",
                         "<strong>Unimproved:</strong>",Pr_D__U,"%",
                         "<br>",
                         "<strong>Open Defecation:</strong>",P_D__O_D,"%",
                         "<br>",
                         "<strong>Population Per km2:</strong>",P_P_KM2
          )
          
        )
    }
    
  })
}

# Run the application 
shinyApp(ui, server)

