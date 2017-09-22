library(shiny)
library(plotly)
library(data.table)
library(leaflet)
options(shiny.sanitize.errors = FALSE)
simplified_data = fread("simplified_data_by_Danniel.csv")

fluidPage(
  titlePanel("Jumpman23 analysis by Danniel Winarto"), 
  br(),
  h3("1. Heatmap frequency between day vs time of delivery"),
  plotlyOutput("heatmap_time_day"),
  h4("Explanation: The heatmap above illustrates the frequency of users based on the darkness of the color. The darkest regions of the map indicate the peak hours of delivery. From this heatmap, we can clearly see that in general, 
     there are two peaks of order frequency: the first peak is around 11:00 to 13:00 (noon) and the second peak is around 18:00 to 20:00 (evening).
     However, the evening peak is considerably more frequent relatively to the noon peak. From this observation we can make an educated guess and confirmation 
     that most of the items that people ordered are foods, just based on the time when they ordered. Another interesting fact is that we can see
     the frequency order time progressively increases into the later hours of the day, as the week goes on. For example:
     on Sunday (1st day of the week), the order time has a peak hour at 18:00 but as the week progressed, it gradually moved to 19:00. One possible conclusion we can make from this information is that
     that people eat their meals earlier on the weekends and later on the weekdays (potentially due to work schedules). "),
  br(),
  h3("2. Vehicle Analysis"),
  h4("In this analysis, we would like to know determine the best method of transportation in NYC for item delivery. From our dataset we are measuring the best method by observing the highest MEDIAN speed(in mph). The reason we used a median instead of mean is because of the data integrity issues such as data gaps in our data sets would skew the averages. (E.g. we found that some of the vehicle speeds are calculated at unrealistic speeds of 100 mph and 0 MPH).
      The way we calculate speed is by dividing the distance and delivery time. Distance is measured  by using latitude and longitude data points between store and customer dropoff locations. Time needed measured between the courier left the store and arrived at dropoff. 
      We also calculated the total frequency for each vehicle."),
  tableOutput("vehicle_med_speed"),
  h4("Recommendation: According to our analysis, more sophisticated vehicles such as a car/motorcycle does not really improve delivery times in NYC. In fact, 
     a scooter has the highest median speed according to our statistical analysis. We would recommend to incorporate these findings in order to improve the accuracy of estimated 
     time of delivery."),
  h4("Data integrity issue: As indicated earlier, we found that there is potential data integrity issue in the timestamp, this issue caused the speed calculation of 
     some vehicle to be as fast as 100 mph and as low as 0 mph, which is unrealistic"),
  br(),
  h3("3. Descriptive Statistics"),
  h4("This section illustrates the application's the most popular restaurants, types of restaurants, items ordered, and also customer waiting times"),
  column(4,tableOutput("fav_pl")),
  column(4,tableOutput("fav_pl_cat")),
  column(4,tableOutput("fav_it_cat")),
  tableOutput("cust_wait"),
  h4("Analysis: The tables above show the top 5 most popular of each respective category. Note*: Some of the categories have missing values due to data integrity issues. In addition
     of top 5, we provided the median and max of customer waiting times grouped by delivery time, it averages around 40 minutes. However, some customers wait for more than 1.5 hour
     which can be a deal breaker for some."),
  h4("Recommendation: By looking at this statistic, we can see that American fast foods have the biggest traction for food delivery. Thus, with this information we 
     may could potentially propose deal/agreement with these restaurants to have incentivized discounts/promotions for Jumpman23 users to boost user growth. Improving waiting times
     also recommended, especially since some customers wait for more than 1.5 hour."),
  h4("Data Integrity Issue: Missing data values above are also part of data integrity issue, especially when the missing data can heavily influence the end statistics"),
  br(),
  h3("4. Geospatial analysis"),
  h4("In this section, we created a UI friendly map dashboard that allows the user to observe the the geolocation. The user can select the desired
     day of the week, time of the day, or both in order to visualize desired portion of dataset."),
  br(),
  h4("The first map allows the users to visualize the stores vs customers ratio, alongside where they are located in NYC."),
  h4("The second map plots only the customers, it allows the users to visualize identify which customers wait the longer and which aren't(based on the size of the circle), 
     alongside where they are located in NYC. Also it provides the customer ID for further investigation when needed"),
  h4("The third map similar to second map, but it plots the stores where the size of the circle represent how long it took to order the item"),
  titlePanel("Grouping Selection"),
  selectInput( inputId = "FilterBy",
               label = "Please select Filtering System",
               choices = c("Filter by DAY only", "Filter by delivery TIME only", "Filter by BOTH")),
  uiOutput("groupingUI"),
  conditionalPanel(
    condition = "input.FilterBy == 'Filter by BOTH'",
    uiOutput('time_ui_add')
  ),
  br(),
  h4("This first geo spatial plot consist of two color:"),
  h4("Blue dot = store --> when we click at specific dot, it will pop the store name."),
  h4("Green dot = customer/dropoff location --> when we click at specific dot, it will pop-up the item ordered."),
  leafletOutput("map_store_customer", width = "85%", height = 700),
  br(),
  h4("The second geo spatial plot plot only on customer/dropoff location:"),
  h4("The size of the circle represents the waiting time of a customer, the larger the size, the longer waiting time."), 
  h4("When we hover at the circle, it will show the customerID."),
  h4("When we click at the circle, it will specifically pop the waiting time."),
  leafletOutput("map_cust_waiting_time", width = "85%", height = 700),
  br(),
  h4("The last geo spatial plot plot only on store:"),
  h4("The size of the circle represents how long to place an order , the larger the size, the longer the time."), 
  h4("When we hover at the circle, it will show the storeID."),
  h4("When we click at the circle, it will specifically pop how long to place an order."),
  leafletOutput("map_store_jumpmanAtStore", width = "85%", height = 700),
  h4("The benefit of having dynamic geospatial analysis in this analysis is that it allows the user see the distribution of Jumpman23's customers and 
     stores in NYC. On top of that it allows the user to visually and inspect which customers has longer waiting times, visually see his/her location, and track
     their customer ID for further investigation. The same thing applied for the vendors/stores, we can clearly see which stores are relatively slower, and investigate the
     root cause of the problems."),
  br(),
  br(),
  h3("5. Further data integrity issues"),
  h4("We believe most of data integrity issues located at the time-stamp column, which is the relation between when_the_delivery_started, when_the_Jumpman_arrived_at_pickup,
      when_the_Jumpman_left_pickup, and when_the_Jumpman_arrived_at_dropoff. We performed the calculation in order to find duration between these chronological events,
      and the results are some of the observation has negative values, which do not make sense. Please see the table below, where we attach our modified table, where
      we created additional features based on existing features, Please take a look on these new additional variables where the values are impossibly low or even negative."),
  dataTableOutput("data_modified"),
  h3("6. conclusion"),
  h4("How is New York doing? So far, NYC has higher areas of profitability and growth for Jumpman23 business. Directing attention to improving waiting time of customers 
     can be pivoting factor in the growth of this app. according to
     our second geo-spatial map where we plotted the customer, the size of green circle represents lenght of waiting time, we observed that some customers wait for longer 
     than 1.5 hour and this can be a turn off for some customers. Having ability to reduce the waiting time, either reducing the jumpmen's travel time, or minimizing 
     how long to place an order in a store, will improve the customer satisfaction tremendously, and ultimately  will improve the growth of Jumpman23 platform. We should
     also expect to hire more courier in the near future during our peak hours (mainly 6-8PM). We may want approach several american chain restaurant to create specific strategy
     in order to improve the growth of customer, or maybe develop a system that reduce the waiting time of the customer. Lastly, we should readjust our estimated delivery time 
     of our courier if we havent had, given interesting findings that in NYC the type of vehicles doesn't really affect the speed of delivery, in order to improve the 
     accuracy our estimation time of delivery."),
  h1("Thank you for the opportunity to work on this exciting project"),
  h1("Danniel Winarto")
)


  