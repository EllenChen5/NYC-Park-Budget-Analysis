library(shiny)
library(shinydashboard)
library(RSocrata)
library(nycgeo)
library(leaflet)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(sf)
library(tigris)
library(maptools)
library(ggmap)
library(httr)
library(scales)
library(rgdal)
library(raster)
library(vroom)
library(sf)



#Read in Zip Code shapefile and borough boundaries(zip code shapefile had wrong crs)
modzcta <- st_read("./ZIP_CODE_040114/ZIP_CODE_040114.shp") 
borough_boundaries <- st_read("./Borough Boundaries/geo_export_6e283d0e-8b75-4b77-bc5a-4eceef12730b.shp")
modified_zip <- st_transform(modzcta,crs(borough_boundaries))

# usd equals a dollar formatting function for making fund amount into dollar for pop up label
usd <- dollar_format(prefix = "$")

#Capital Tracker data
cap_track <- read_csv("./Capital_Projects_Tracker.csv")

#filter for funding that starts with '$' since those have actual amounts and convert column into int
cap_track_fil <- cap_track %>%
  filter(substr(TotalFunding,1,1) == "$")
cap_track_fil$TotalFunding <- gsub(",","",as.character(cap_track_fil$TotalFunding))
cap_track_fil$TotalFunding <- as.numeric(gsub("\\$","",cap_track_fil$TotalFunding))

#filter wrong lon/lat points that lands on africa
cap_track_fil <- cap_track_fil[cap_track_fil$Latitude != "0",]
cap_track_fil <- cap_track_fil[cap_track_fil$Longitude != "0",]

#Get average funding since some projects have their funding distributed over several parks
matcher_df <- aggregate(cap_track_fil[, 21], list(cap_track_fil$TrackerID),mean)
matcher_df <- rename(matcher_df, TrackerID = Group.1)
matcher_df2 <- cap_track_fil %>%
  count(TrackerID)
joined_matcher <- merge(matcher_df,matcher_df2,by = "TrackerID")
joined_matcher$avg_fund <- joined_matcher$TotalFunding/joined_matcher$n
avg_cap_track <- merge(cap_track_fil,joined_matcher[, c("TrackerID","avg_fund")],by = "TrackerID")

#Bins for the funding range, color palette is based on this as well
avg_cap_track$fund_range <- cut(avg_cap_track$avg_fund,
                                breaks = c(30000,100000,500000,1000000,10000000,100000000,150000000),
                                right = FALSE,
                                labels = c("$30,000-$100,000","$100,000-$500,000","$500,000-$1,000,000","$1,000,000-$10,000,000","$10,000,000-$100,000,000","> $100 million"))

#Read in NYC Neighborhood JSON file
tracker_spdf <- avg_cap_track
r <- GET('http://data.beta.nyc//dataset/0ff93d2d-90ba-457c-9f7e-39e47bf2ac5f/resource/35dd04fb-81b3-479b-a074-a27a37888ce7/download/d085e2f8d0b54d4590b1e7d1f35594c1pediacitiesnycneighborhoods.geojson')
nyc_neighborhoods <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)

#Project long/lat cords over neighborhood shapes, and add location specifics to tracker df
coordinates(tracker_spdf) <- ~Longitude + Latitude
proj4string(tracker_spdf) <- proj4string(nyc_neighborhoods)
matches <- over(tracker_spdf, nyc_neighborhoods)
avg_cap_track <- cbind(avg_cap_track, matches)

#Groups by neighborhood and sums total
funds_by_neighborhood <- avg_cap_track %>%
  group_by(neighborhood) %>%
  summarize(funding=sum(avg_fund),
            parks_n=n())
map_data <- geo_join(nyc_neighborhoods, funds_by_neighborhood, "neighborhood", "neighborhood")

#Read in OMB Budget file matched with fms id file for coordinates
tracker_2 <- read_csv("./Join_Tracker.csv")

#Remove projects with wrong lat/longs
tracker_2 <- tracker_2[tracker_2$Latitude != "0",]
tracker_2 <- tracker_2[tracker_2$Longitude != "0",]

#Get combined total
tracker_2$combined_actual <- tracker_2$CITY_PLAN_TOTAL + tracker_2$NONCITY_PLAN_TOTAL

#Project long/lat cords over neighborhood shapes, and add location specifics to tracker df
budget_watch <- tracker_2
coordinates(budget_watch) <- ~Longitude + Latitude
proj4string(budget_watch) <- proj4string(nyc_neighborhoods)
matched_up <- over(budget_watch, nyc_neighborhoods)
tracker_2 <- cbind(tracker_2, matched_up)

tracker_2$fund_range <- cut(tracker_2$combined_actual,
                            breaks = c(50,500,1000,2000,5000,10000,30000,90000),
                            right = FALSE,
                            labels = c("$50-$500","$500-$1,000","$1,000-$2,000","$2,000-$5,000","$5,000-$10,000","$10,000-$30,000","> $30 Thousand"))

#Group by neighborhood and sum funds
budget_by_neighborhood <- tracker_2 %>%
  group_by(neighborhood) %>%
  summarize(b_funding=sum(combined_actual))
budget_map_data <- geo_join(nyc_neighborhoods, budget_by_neighborhood, "neighborhood", "neighborhood")

#Zip code demographic data
#Read income by zip and merge with zip shapefile
income_zip <- read_csv("./income_by_zip.csv")
income_zip <- rename(income_zip,ZIPCODE = Zipcode)
income_zip_join <- merge(modified_zip,income_zip, by = "ZIPCODE")

income_zip_join$AGI <- income_zip_join$AGI * 1000
income_zip_join$Tot_income <- income_zip_join$Tot_income * 1000
income_zip_join$mean_agi <- income_zip_join$mean_agi * 1000
income_zip_join$mean_total <- income_zip_join$mean_total * 1000

income_zip_join$fund_range_inc <- cut(income_zip_join$mean_total,
                                  breaks = c(20000,40000,75000,95000,120000,160000,250000,350000,500000,1000000),
                                  right = FALSE,
                                  labels = c("$20,000-$40,000","$40,000-$75,000","$75,000-$95,000","$95,000-$120,000","$120,000-$160,000","$160,000-$250,000","$250,000-$350,000","$350,000-$500,000","> $500 Thousand"))

#Palletes and Labels for Capital Map

label_tracker <- sprintf(
  "<strong>%s</strong><br/> Capital Spending =  %s <br/> Liason =  %s",
  avg_cap_track$Title,usd(avg_cap_track$avg_fund),avg_cap_track$ProjectLiaison) %>%
  lapply(htmltools::HTML)

pal = colorFactor(palette = "PuRd", domain = avg_cap_track$fund_range)

label_tracker_nta <- sprintf(
  "<strong>%s</strong><br/> Capital Funding = %s",
  map_data$neighborhood,usd(map_data$funding)) %>%
  lapply(htmltools::HTML)

pallei <- colorBin(palette = "YlGn",
                   domain = range(map_data@data$funding,na.rm=T),
                   bins = 7 )

#Palletes and Labels for Budget Map

budget_label <- sprintf(
  "<strong>%s</strong><br/> Budget Funding = %s",
  tracker_2$PROJECT_DESCR,usd(tracker_2$combined_actual)) %>%
  lapply(htmltools::HTML)

pallte <- colorFactor(palette = "PuRd", domain = tracker_2$fund_range)

label_budget_nta <- sprintf(
  "<strong>%s</strong><br/> Budget Funding = %s",
  budget_map_data$neighborhood,usd(budget_map_data$b_funding)) %>%
  lapply(htmltools::HTML)

pallei_2 <- colorBin(palette = "YlGn",
                     domain = range(budget_map_data@data$b_funding,na.rm=T),
                     bins = 7)

#Palletes and Labels for Main Map

income_zip_label <- sprintf(
  "<strong>%s</strong><br/> Mean Income = %s <br/> Mean AGI = %s",
  income_zip_join$ZIPCODE,usd(income_zip_join$mean_total),usd(income_zip_join$mean_agi)) %>%
  lapply(htmltools::HTML)

pop_zip_label <- sprintf(
  "<strong>%s</strong><br/> Population = %s ",
  income_zip_join$ZIPCODE,comma(income_zip_join$POPULATION)) %>%
  lapply(htmltools::HTML)

pallett_income_zip <- colorFactor(palette = "OrRd", domain = income_zip_join$fund_range_inc)


pallett_pop_zip <- colorBin(palette = "Purples",
                            domain = range(income_zip_join$POPULATION,rm.na=T),
                            bins = 8,
                            pretty = TRUE)

# Define UI for application 
ui <- shinyUI(
  dashboardPage(skin = "green",
    dashboardHeader(title = "NYC Parks and Rec",titleWidth = 500),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Parks and Area Demographics", tabName = "Demo"),
        menuItem("Neighborhood Park Funding", tabName = "Parks")
        )
      ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "Parks",
                tabBox(
                  title = "Neighborhood Funding",
                  height = 750, width = 1000,
                  tabPanel("OMB Budget", leafletOutput("budget_map", height =625,width = 1000)),
                  tabPanel("Capital Projects", leafletOutput("capital_map", height =625,width = 1000))
                )),
        tabItem(tabName = "Demo",
                box(title = "Zip Code Demographics VS Park Projects", background = "yellow",
                    solidHeader = TRUE,
                    height = 750, width = 1000,
                    leafletOutput("main", height =625,width = 1000))
      )
    )
) )
)


# Define server logic 
server <- shinyServer(function(input,output,session){
  
  output$capital_map <- renderLeaflet({
    leaflet(map_data) %>%
      addProviderTiles(provider = providers$CartoDB.Positron) %>% 
      addPolygons(fillColor = ~pallei(funding),
                  weight = 2,
                  opacity = 1,
                  color = "black",
                  dashArray = "3",
                  fillOpacity = 1,
                  label = label_tracker_nta,
                  group = "Neighborhoods",
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = FALSE)
      ) %>%
      addCircleMarkers(data = avg_cap_track,
                       lat = ~Latitude,
                       lng = ~Longitude,
                       fillOpacity = 0.8,
                       radius = 8,
                       stroke = TRUE,
                       color = "black",
                       weight = 2,
                       fillColor = ~pal(fund_range),
                       popup = label_tracker,
                       group = "Capital Projects"
      ) %>%
      addLayersControl(
        baseGroups = c("NYC Map","Neighborhoods"),
        overlayGroups = c("Capital Projects"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      addLegend(position = "bottomright",pal = pallei,
                values = range(map_data@data$funding,na.rm=T),
                title = "Capital Funding",
                opacity = 1)
  })
  
  output$main <- renderLeaflet({
    leaflet(income_zip_join) %>%
      addProviderTiles(provider = providers$CartoDB.Positron) %>% 
      addPolygons(fillColor = ~pallett_pop_zip(income_zip_join$POPULATION),
                  weight = 2,
                  opacity = 1,
                  color = "black",
                  dashArray = "3",
                  fillOpacity = 1,
                  label = pop_zip_label,
                  group = "Population",
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = FALSE)
      ) %>%
      addPolygons(fillColor = ~pallett_income_zip(income_zip_join$fund_range_inc),
                  weight = 2,
                  opacity = 1,
                  color = "black",
                  dashArray = "3",
                  fillOpacity = 1,
                  label = income_zip_label,
                  group = "Median Income",
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = FALSE)
      ) %>%
      addCircleMarkers(
        data = avg_cap_track,
        lat = ~Latitude,
        lng = ~Longitude,
        fillOpacity = 0.4,
        radius = 6,
        stroke = TRUE,
        color = "black",
        weight = .75,
        popup = label_tracker,
        group = "Capital Tracker"
      ) %>%
      addCircleMarkers(data = tracker_2,
                       lat = ~Latitude,
                       lng = ~Longitude,
                       fillOpacity = 0.4,
                       radius = 6,
                       stroke = FALSE,
                       color = "olivedrab",
                       weight = .75,
                       popup = budget_label,
                       group = "Budget Tracker"
      ) %>%
      addLayersControl(
        baseGroups = c("NYC Map","Population","Median Income"),
        overlayGroups = c("Capital Tracker","Budget Tracker"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  output$budget_map <- renderLeaflet({
    leaflet(budget_map_data) %>%
      addProviderTiles(provider = providers$CartoDB.Positron) %>% 
      addPolygons(fillColor = ~pallei_2(b_funding),
                  weight = 2,
                  opacity = 1,
                  color = "black",
                  dashArray = "3",
                  fillOpacity = 1,
                  label = label_budget_nta,
                  group = "Neighborhoods",
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = FALSE)
      ) %>%
      addCircleMarkers(data = tracker_2,
                       lat = ~Latitude,
                       lng = ~Longitude,
                       fillOpacity = 0.7,
                       radius = 7,
                       stroke = TRUE,
                       color = "black",
                       weight = 2,
                       fillColor = ~pallte(fund_range),
                       popup = budget_label,
                       group = "Budget Tracker"
      ) %>%
      addLayersControl(
        baseGroups = c("NYC Map","Neighborhoods"),
        overlayGroups = c("Budget Tracker"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      addLegend(position = "bottomright",pal = pallei_2,
                values = range(budget_map_data@data$b_funding,na.rm=T),
                title = "Budget Spending",
                opacity = 1)
  })
  
}
  )

shinyApp(ui,server)
