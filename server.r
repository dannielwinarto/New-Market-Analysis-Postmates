# setwd("C:/Users/dan_9/Desktop/Jumpman23/Jumpman23")
rm(list = ls())
# dev.off()

library(shiny)
library(data.table)
library(dplyr)
library(lubridate)
library(leaflet)
library(geosphere)
library(plotly)
options(shiny.sanitize.errors = FALSE)

data = fread("analyze_me.csv", 
             colClasses = c(rep("character",7), "integer", rep("character",2), rep("double",4),   rep("character",4)))
# reformating the date format
date_col_1 = c("when_the_delivery_started", "when_the_Jumpman_arrived_at_pickup", "when_the_Jumpman_left_pickup", "when_the_Jumpman_arrived_at_dropoff")
date_col_2 = c("how_long_it_took_to_order")
# str(data)
data[, (date_col_1) := lapply(.SD, ymd_hms), .SDcols = (date_col_1)]

duration_to_order_in_mins = function(x){
  date_form = hms(x)
  tot_hours = as.numeric(hour(date_form))
  tot_mins = as.numeric(minute(date_form))
  tot_secs = as.numeric(second(date_form))
  ret = round(tot_hours*60 + tot_mins + tot_secs/60,1)
  return(ret)
}
data[, "how_long_it_took_to_order_in_minutes"  := lapply(.SD,  duration_to_order_in_mins), .SDcols = (date_col_2)]

data[, c("duration_deliveryStarted_and_JumpmanArrivedatStore_minutes",
         "duration_jumpmanatStore_minutes",
         "duration_store_to_customer_minutes",
         "duration_customerWaiting_minutes") := 
       list(round(as.numeric(when_the_Jumpman_arrived_at_pickup - when_the_delivery_started, units = "mins"),1),
            round(as.numeric(when_the_Jumpman_left_pickup - when_the_Jumpman_arrived_at_pickup, units = "mins"),1),
            round(as.numeric(when_the_Jumpman_arrived_at_dropoff-when_the_Jumpman_left_pickup,  units = "mins"),1),
            round(as.numeric(when_the_Jumpman_arrived_at_dropoff-when_the_delivery_started, units = "mins"),1))]

data[hour(data[,when_the_delivery_started]) >= 2 & hour(data[,when_the_delivery_started]) <= 6  , delivery_time := "early morning (2AM to 6AM)"]
data[hour(data[,when_the_delivery_started]) >= 7 & hour(data[,when_the_delivery_started]) <= 11  , delivery_time := "morning (7AM to 11AM)"]
data[hour(data[,when_the_delivery_started]) >= 12 & hour(data[,when_the_delivery_started]) <= 16  , delivery_time := "around noon (12PM to 4PM)"]
data[hour(data[,when_the_delivery_started]) >= 17 & hour(data[,when_the_delivery_started]) <= 20  , delivery_time := "evening (5PM to 8PM)"]
data[hour(data[,when_the_delivery_started]) >= 21 & hour(data[,when_the_delivery_started]) <= 1  , delivery_time := "late night (9PM to 1AM)"]

data[, delivery_day := wday(data$when_the_delivery_started, label = T,abbr = T)]
data[, distance_store_to_dropoff_miles := round(distVincentyEllipsoid( data[,c("pickup_lon","pickup_lat")], data[,c("dropoff_lon","dropoff_lat")])/1609.34,1) ]

simplified_data = data[,c(1:9,11:15,19:26)]

# head(data,2)
# write.csv(simplified_data, file = "simplified_data_by_Danniel.csv")

time_day_freq_table = table(data[,delivery_day], hour(data[, when_the_delivery_started]))
vehicle_freq = data[,.N,by = "vehicle_type"]
data[,store_to_customer_speed_mph := distance_store_to_dropoff_miles/duration_store_to_customer_minutes*60]

vehicle_median_speed = data[,lapply(.SD,median, na.rm = T),.SDcols = "store_to_customer_speed_mph", by =  "vehicle_type"]
vehicle_median_speed = merge(vehicle_median_speed,vehicle_freq)
setnames(vehicle_median_speed, colnames(vehicle_median_speed), c("vehicle", "median_speed_mph", "frequency"))

fav_place = data[,.N, by = pickup_place][order(-N)][1:5]
fave_place_cat = data[,.N, by = place_category][order(-N)][1:5]
fave_item_cat = data[,.N, by = item_category_name][order(-N)][1:5]

cust_wait_times_med = data[,lapply(.SD, median, na.rm = T),.SDcols = "duration_customerWaiting_minutes",by = "delivery_time"]
setnames(cust_wait_times_med,names(cust_wait_times_med), c("delivery_time", "median_customer_waiting_time_minutes"))

cust_wait_times_max = data[,lapply(.SD, max, na.rm = T),.SDcols = "duration_customerWaiting_minutes",by = "delivery_time"]
setnames(cust_wait_times_max,names(cust_wait_times_max), c("delivery_time", "max_customer_waiting_time_minutes"))

cust_wait_times = merge(cust_wait_times_med,cust_wait_times_max )

function(input, output){
  output$FinalTable = renderDataTable({ 
    simplified_data
  })
  
  output$heatmap_time_day = renderPlotly({ 
    plot_ly(z = time_day_freq_table, x = colnames(time_day_freq_table), y = rownames(time_day_freq_table),
            type = "heatmap", 
            colors = colorRamp(c( "white","brown")) ) %>% 
      layout( title = "Heatmap Jumpman23 frequency by time and day",
              xaxis = list(title = "time of the day", autotick = FALSE),
              yaxis = list(title = "Day of the week"))
    })
  
  output$vehicle_med_speed = renderTable(vehicle_median_speed)
  
  output$fav_pl = renderTable(fav_place)
  
  output$fav_pl_cat = renderTable(fave_place_cat)
  
  output$fav_it_cat = renderTable(fave_item_cat)
  
  output$cust_wait = renderTable(cust_wait_times)
  
  
  groupSelectionChoice = reactive({
    switch(input$FilterBy,
           "Filter by DAY only" = sort(unique(data[,delivery_day])),
           "Filter by delivery TIME only" = sort(unique(data[,delivery_time])),
           "Filter by BOTH" = sort(unique(data[,delivery_day])))
  })
  
  output$groupingUI = renderUI({
    if (is.null(input$FilterBy)){return()}
    switch(input$FilterBy,
           "Filter by DAY only" = selectInput( inputId = "day_xx",
                                     label = "Please select the day you interested",
                                     choices = groupSelectionChoice()),
           "Filter by delivery TIME only" = selectInput( inputId = "hour_xx",
                                    label = "Please select the hour/time you interested",
                                    choices = groupSelectionChoice()),
           "Filter by BOTH" = selectInput( inputId = "day_xx_both",
                                     label = "First, select the day you interested",
                                     choices = groupSelectionChoice())
    )
  })
  output$time_ui_add = renderUI({
    if (is.null(input$FilterBy)){return()}
    selectInput( inputId = "hour_xx_both",
                 label = "Then, select the hour/time you interested",
                 choices = sort(unique(data[,delivery_time])))
  })
  
  data_filtered = reactive({
    if (is.null(input$FilterBy)){return()}
    if (is.null(input$hour_xx_both)){return()}
    if(input$FilterBy == "Filter by DAY only"){
      temp = data[delivery_day == input$day_xx,]
      return(temp)
    }
    if(input$FilterBy == "Filter by delivery TIME only"){
      temp = data[delivery_time == input$hour_xx,]
      return(temp)
    }
    if(input$FilterBy == "Filter by BOTH"){
      temp = data[delivery_day == input$day_xx_both & delivery_time == input$hour_xx_both ,]
      return(temp)
    }
  })
  
  output$test = renderDataTable({ 
    data_filtered()
  })
  
  output$map_store_customer <- renderLeaflet({
    leaflet(data = data_filtered()) %>%
      addTiles() %>%
      addCircles(lng = ~pickup_lon, lat = ~pickup_lat, popup = ~as.character(paste("store name:", pickup_place)), label = "store", color = "blue", radius = 25) %>%
      addCircles(lng = ~dropoff_lon, lat = ~dropoff_lat, popup = ~as.character(paste("item ordered: ", item_category_name)),label = "customer" ,color = "green", radius = 25)
  })
  
  output$map_cust_waiting_time <- renderLeaflet({
    leaflet(data = data_filtered()) %>%
      addTiles() %>%
      addCircleMarkers(lng = ~dropoff_lon, lat = ~dropoff_lat, popup = ~as.character(paste("waiting for ", duration_customerWaiting_minutes, "minutes")), label = ~customer_id, 
                       color = "green", radius = ~duration_customerWaiting_minutes/15) 
  })
  
  output$map_store_jumpmanAtStore <- renderLeaflet({
    leaflet(data = data_filtered()) %>%
      addTiles() %>%
      addCircleMarkers(lng = ~pickup_lon, lat = ~pickup_lat, popup = ~as.character(paste("duration to order ", how_long_it_took_to_order_in_minutes, "minutes")), label = ~delivery_id, 
                       color = "blue", radius = ~how_long_it_took_to_order_in_minutes/15) 
  })
  
  output$data_modified = renderDataTable(data)
}
