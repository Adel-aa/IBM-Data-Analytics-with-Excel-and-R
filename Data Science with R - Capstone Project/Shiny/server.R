# Install and import required libraries
require(shiny)
require(ggplot2)
require(leaflet)
require(tidyverse)
require(httr)
require(scales)
# Import model_prediction R which contains methods to call OpenWeather API
# and make predictions
source("model_prediction.R")


test_weather_data_generation<-function(){
  #Test generate_city_weather_bike_data() function
  city_weather_bike_df<-generate_city_weather_bike_data()
  stopifnot(length(city_weather_bike_df)>0)
  print(head(city_weather_bike_df))
  return(city_weather_bike_df)
}

# Create a RShiny server
shinyServer(function(input, output){  
  
  observeEvent (input$city_dropdown,{
    if(input$city_dropdown == "All"){
      output$city_bike_map = renderLeaflet ({
        leaflet() %>% addTiles() %>% 
          addMarkers(label = cities_today_status$CITY_ASCII,
                     lng = cities_today_status$LNG, 
                     lat = cities_today_status$LAT, 
                     popup = cities_today_status$LABEL,
                     options = popupOptions(closeButton = FALSE)) %>% 
          addCircleMarkers(lng = cities_today_status$LNG,
                           lat = cities_today_status$LAT,
                           color = cities_today_status$COLOR, 
                           radius = cities_today_status$CIRCLE )
      })
    }else{
      selected_city <- reactive({ cities_today_status %>% 
          filter(CITY_ASCII==input$city_dropdown) }) 
      
      selected_city_5_day <- reactive({city_weather_bike_df %>% 
          filter(CITY_ASCII == input$city_dropdown)})
      
      output$city_bike_map <- renderLeaflet ({
        leaflet() %>% addTiles() %>% 
          setView(lng = selected_city()$LNG, 
                  lat = selected_city()$LAT, 
                  zoom=15) %>% 
          addMarkers(lng = selected_city()$LNG, 
                     lat = selected_city()$LAT, 
                     popup = selected_city()$DETAILED_LABEL)
      })
      output$temp_line <- renderPlot({
        ggplot(selected_city_5_day(),aes(x = FORECASTDATETIME,y = TEMPERATURE)) +
          geom_line(color = "brown") +
          geom_point(color = "red") +
          geom_text(aes(label = TEMPERATURE), size = 3) +
          labs(title = paste("Temperature forcast of next 5 days in",input$city_dropdown)) +
          xlab('Date (3hrs interval)') +
          ylab("Temperature in C")})
      
      output$bike_line <- renderPlot({
        ggplot(selected_city_5_day(),aes(x = FORECASTDATETIME, y = BIKE_PREDICTION)) +
          geom_line(color = "blue") +
          geom_point(color = "blue") +
          geom_text(aes(label = BIKE_PREDICTION),size=3) +
          labs(title = paste("Bike prediction of next 5 days in",input$city_dropdown)) +
          xlab('Date (3hrs interval)') +
          ylab("Bike No.")})
      
      output$bike_date_output <- renderText({paste0("Time=", as_datetime(input$plot_click$x),
                                                    "\nBike Count Prediction=", input$plot_click$y) })
      
      output$humidity_pred_chart=renderPlot({
        ggplot(selected_city_5_day(),aes(x=HUMIDITY,y=BIKE_PREDICTION))+
          geom_smooth(method = lm, formula = y ~ poly(x, 4),color = "green")+
          geom_point(color = "brown")+
          labs(title = paste("Relationship between Humidity & Bike prediction in", input$city_dropdown)) +
          xlab('Humidity') +
          ylab("Bike No.")})
      
    }})
  
})
