
library(shiny)
library(bs4Dash)
library(ggridges)
library(tidyverse)
library(shinydashboard)
library(recipes)
library(DT)
library(dplyr)
library(tidyverse)
library(gapminder)


#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("shiny")
#install.packages("DataExplorer")


library(dplyr)
library(ggplot2)
library(shiny)
library(DataExplorer)


getwd()
setwd("C:/Users/Luis/Downloads")
getwd()
datos<-read.csv("hotel_bookings.csv",header=TRUE,sep=",")
datos1<-read.csv("hotel1.csv",header=TRUE,sep=",")
datos2<-read.csv("hotel2table.csv",header=TRUE,sep=",")
datos4<-read.csv("hotel4.csv",header=TRUE,sep=",")
datos3<-read.csv("hotel3.csv",header=TRUE,sep=",")


attach(datos)
attach(datos1)
attach(datos2)
attach(datos3)
attach(datos4)

names(datos)
names(datos1)
names(datos2)
names(datos3)
names(datos4)

#hist(arrival_date_week_number)
#hist(days_in_waiting_list)
#hist(is_canceled)
#hist(previous_cancellations)

#view(datos)


View(datos1)
View(datos2)
View(datos3)
View(datos4)


#create_report(datos)
#view(meal)





dash1<-ggplot(datos1, aes(x = Month, y = Frec, color = reservation_status.hotel, group = reservation_status.hotel)) +
  geom_line(lwd = 1.0) +
  geom_point(size = 1.3) +
  scale_color_manual(values = c("red", "gray", "blue1", "#217832", "gray", "gray")) +
  ggtitle("Number of bookings by reservation status and hotel") +facet_wrap(~ reservation_status.hotel)+
  scale_x_discrete(limits = c("Nov-14", "Dec-14", "Jan-15", "Feb-15", "Mar-15", "Apr-15", "May-15", "Jun-15", "Jul-15", "Aug-15", "Sep-15", "Oct-15", "Nov-15", "Dec-15", "Jan-16", "Feb-16", "Mar-16", "Apr-16", "May-16", "Jun-16", "Jul-16", "Aug-16", "Sep-16", "Oct-16", "Nov-16", "Dec-16", "Jan-17", "Feb-17", "Mar-17", "Apr-17", "May-17", "Jun-17", "Jul-17", "Aug-17", "Sep-17"))

dash1


anal1<-ggplot(datos1, aes(x = Month, y = Frec, color = reservation_status.hotel, group = reservation_status.hotel)) +
  geom_line(lwd = 1.0) +
  geom_point(size = 1.3) +
  scale_color_manual(values = c("gray", "gray", "blue1", "#217832", "gray", "gray")) +
  ggtitle("Number of bookings by reservation status and hotel") +
  scale_x_discrete(limits = c("Nov-14", "Dec-14", "Jan-15", "Feb-15", "Mar-15", "Apr-15", "May-15", "Jun-15", "Jul-15", "Aug-15", "Sep-15", "Oct-15", "Nov-15", "Dec-15", "Jan-16", "Feb-16", "Mar-16", "Apr-16", "May-16", "Jun-16", "Jul-16", "Aug-16", "Sep-16", "Oct-16", "Nov-16", "Dec-16", "Jan-17", "Feb-17", "Mar-17", "Apr-17", "May-17", "Jun-17", "Jul-17", "Aug-17", "Sep-17"))

anal1

dash2 <- ggplot(datos2, aes(x = Day, y = Frec)) +
  geom_bar(stat = "identity", fill = "lightblue", colour = "black")+
  geom_text(aes(label = Frec), vjust = 1.5, colour = "black")+
  ggtitle("Days of stay in the hotel") +
  scale_x_discrete(limits = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
dash2



anal2 <- ggplot(datos2, aes(x = Day, y = Frec)) +
  geom_bar(stat = "identity", fill = "lightblue", colour = "black") +
  ggtitle("Days of stay in the hotel") +
  scale_x_discrete(limits = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")) +
  scale_y_continuous(limit = c(0, 70000))
anal2






dash3<-ggplot(datos3, aes(x = Days.between.booking.and.arrival, y = Frec, size = Frec ,color = Days.between.booking.and.arrival, group = Days.between.booking.and.arrival)) +
  geom_point()  +
  ggtitle("Frecuency of Days between booking and arrival")

dash3


anal3<-ggplot(datos3, aes(x = Days.between.booking.and.arrival, y = Frec, size = Frec ,color = Days.between.booking.and.arrival, group = Days.between.booking.and.arrival)) +
  geom_point()  +
  ggtitle("Frecuency of Days between booking and arrival")

anal3


dash4<-ggplot(datos4, aes(x = week, y = Frec, color = reservation_status.hotel, group = reservation_status.hotel)) +
  geom_line(lwd = 1.0) +
  geom_point(size = 1.3) +
  ggtitle("Booking demand per week") +
  scale_color_manual(values = c("red", "gray", "blue1", "#217832", "gray", "gray")) 


dash4

anal4<-ggplot(datos4, aes(x = week, y = Frec, color = reservation_status.hotel, group = reservation_status.hotel)) +
  geom_line(lwd = 1.0) +
  geom_point(size = 1.3) +
  ggtitle("Booking demand per week") +
  scale_color_manual(values = c("red", "gray", "blue1", "#217832", "gray", "gray")) 


anal4






ui <- dashboardPage(
  header = dashboardHeader(title="Hotel Flix",
                           tags$li(class ="dropdown",tags$a(href="https://www.linkedin.com/school/ie-business-school/posts/?feedView=all",icon("linkedin"),"Linkedn",target="blank")
                           ), tags$li(class ="dropdown",tags$a(href="https://public.tableau.com/app/profile/luis.carre.o/viz/HotelFlix/Dashboard1?publish=yes",icon("bar-chart"),"Tableau",target="blank")
                           ), tags$li(class ="dropdown",tags$a(href="https://www.youtube.com/user/IEBusinessSchool",icon("youtube"),"Youtube",target="blank")
                           ), tags$li(class ="dropdown",tags$a(href="https://events.ie.edu/es/all-events/evento/data-visualization-challenge-with-bayer",icon("laptop"),"IE University",target="blank")
                           )),
  sidebar=dashboardSidebar(
    sidebarMenu(id ="sidebarMenuid",
                menuItem( "Home", tabName = "Home", icon = icon("home")),
                menuItem("Dashboard",tabName = "dashboard",icon = icon("dashboard")),
                menuItem("Analysis",tabName = "Analysis",icon = icon("bar-chart")),
                menuItem("Data",tabName = "Data",icon = icon("database")),
                menuItem("Map",tabName = "Map",icon = icon("map"))

    )
  ),
  
  body=dashboardBody(
    tabItems(
      tabItem(tabName = "Home",
              tabBox(id= "t1" , width = 12,
                     tabPanel("About",icon = icon("address-card"),fluidRow(
                       column(width = 8,tags$img(src ="flexflex.jpg",width = 6,height= 3),
                              tags$br(),
                              tags$a("Photo FLix Hotel"),align ="center"),
                       column(width = 4, tags$br(),
                              tags$p("HotelFlix, a leading figure in data-driven
boutique hotel experiences digitalizing
the entire booking process, is in search
of your strategic expertise.

As this innovative company navigates
the competitive market, your insights
and guidance are crucial to enhancing
its market position. This initiative offers
you the unique opportunity to work
alongside industry leaders, applying

your knowledge, expertise, and data-
driven strategic thinking to address

HotelFlix's challenges.Your participation is more than just 
attending; it's a pivotal role in shaping 
HotelFlix's future and revolutionising 
the boutique hotel booking landscape 
across Europe.

Embark on an engaging data 
visualization challenge designed to 
unleash your technical prowess and 
sharpen your strategic thinking in this 
data-driven opportunity.")  
                              
                       ) )) ,
                     
                     
                     tabPanel("Data",icon = icon("table"),dataTableOutput("dataT")),
                     tabPanel("structure",icon = icon("uncharted"),verbatimTextOutput("structure")),
                     tabPanel("Summary",icon = icon("chart-pie"),verbatimTextOutput("summary")),
                     tabPanel("Task1",icon = icon("address-card"), "HotelFlix, a leading figure in data-driven
boutique hotel experiences digitalizing
the entire booking process, is in search
of your strategic expertise.

As this innovative company navigates
the competitive market, your insights
and guidance are crucial to enhancing
its market position. This initiative offers
you the unique opportunity to work
alongside industry leaders, applying

your knowledge, expertise, and data-
driven strategic thinking to address

HotelFlix's challenges.Your participation is more than just 
attending; it's a pivotal role in shaping 
HotelFlix's future and revolutionising 
the boutique hotel booking landscape 
across Europe.

Embark on an engaging data 
visualization challenge designed to 
unleash your technical prowess and 
sharpen your strategic thinking in this 
data-driven opportunity.") ,
                     tabPanel("Task2",icon = icon("address-card"), "Use the data to see the different
more repeated days of stay in
the hotel in a barplot. Don't
differentiate between weekends
and week days. Here you can
decide how many packages we
need and for how many days.
Don't be afraid, maybe if you sum
one day more to these just
created packages you attract
more people."),
                     tabPanel("Task3",icon = icon("address-card"), "Use the data to know the number
of days that elapsed between
the booking date and the arrival
data for the cancelled booking.
With this information, try to
create different periods with
different penalties that make
sense to you. I.e.:365-100 days 25% penalty , 365-100 days 25% penalty , 99-30 days 50% penalty , 29-3 days 75% penalty and 0-2 days 90% penalty. There is no perfect solution here,
it is about thinking about
grouping the cancellations
based on the days from booking.
Justify your response."),
                     tabPanel("Task4",icon = icon("address-card"), "Using the number of days
between booking and arrival date,
calculate the most demanded
weeks, show it in a plot and make
a prediction about the next most
demanded weeks. Justify your
response.")
              )
      ),
      tabItem(tabName = "dashboard", tabBox(id= "t2" , width = 12,
                                            tabPanel("Dashboard",icon = icon("bar-chart-o"),    fluidRow(
                                              box(title = "1.Number of booking",status = "primary", solidHeader = TRUE,collapsible = TRUE,plotOutput("dash1", height = 340)),
                                              box(title = "2.Days of stay in the hotel",status = "primary", solidHeader = TRUE,collapsible = TRUE,plotOutput("dash2", height = 340)),
                                              box(title = "3.Days that elapsed between the booking date and the arrival date",status = "primary", solidHeader = TRUE,collapsible = TRUE,plotOutput("dash3", height = 340)),
                                              box(title = "4.The most demanded weeks",status = "primary", solidHeader = TRUE,collapsible = TRUE,plotOutput("dash4", height = 340))
                                              
                                              
                                              
                                            )),
      )),
      
      
      tabItem(tabName = "Data", tabBox(id= "t3" , width = 12,
                                       tabPanel("Tables1",icon = icon("database"),dataTableOutput("dataT1")),
                                       tabPanel("Tables2",icon = icon("database")),
                                       tabPanel("Tables3",icon = icon("database")),
                                       tabPanel("Tables4",icon = icon("database")),
                                       
                                       
      )),
      
      
      
      tabItem(tabName = "Analysis",tabBox(id= "t4" , width = 12,
                                          tabPanel("Analysis",icon = icon("address-card"),   fluidRow(
                                            box(title = "1.Number of booking",status = "primary",solidHeader = TRUE,plotOutput("anal1", height = 135)),
                                            box(title = "1.Number of booking",status = "warning",solidHeader = TRUE,height = 200,"A seasonality can be observed in the reservations of the City and Resort hotels that have already checked out. During the months of January to March and October to December, a low season is experienced, while from April to June and July to September, a high season is experienced. Furthermore, this seasonality seems to be more pronounced in the city's hotels.
"),
                                            
                                            box(title = "2.Days of stay in the hotel",status = "primary",solidHeader = TRUE,plotOutput("anal2", height = 135)),
                                            box(title = "2.Days of stay in the hotel",status = "warning",solidHeader = TRUE,height = 200,"We calculate the number of guests staying daily.
With customer arrival data and the number of nights they stay and check out.
It can be seen that from Monday to Wednesday the demand is slightly lower than the other days. In order to take better advantage of the hotel's installed capacity, we could create a discount package from Monday to Wednesday and other packages that include weekends along with Mondays. , Mars and Wednesday."),
                                            
                                            box(title = "3.Days that elapsed between the booking date and the arrival date",status = "primary",solidHeader = TRUE,plotOutput("anal3", height = 135)),
                                            box(title = "3.Days that elapsed between the booking date and the arrival date",status = "warning",solidHeader = TRUE,height = 200,"Of the canceled registrations, we analyze the frequencies according to the differences in booking days and arrivals. Weighting those with the highest frequency in the graph.
And thus be able to suggest 4 periods with different penalties.

1.From 0 to 7 days, 85% penalty.
2.From 8 to 30 days, 75% penalty.
3.From 31 to 120 days, 50% penalty.
4.From 121 days or more, 25% penalty.

In order to improve the indicator, a period of 30 to 60 days could be created with a higher % penalty, reaching 60% because 25% of cancellations have these characteristics."),
                                            
                                            
                                            box(title = "4.The most demanded weeks",status = "primary",solidHeader = TRUE,plotOutput("anal4", height = 135)),
                                            box(title = "4.The most demanded weeks",status = "warning",solidHeader = TRUE,height = 200,"
Observing the annual seasonality of demand in most cases in the hotel reservation states we can forecast until the end of the year 2017 and in the cases that are flat we forecast the demand as an average. Thus resulting in the disaggregated demand that finally gives us It will allow you to have a forecast of demand.")  
                                          )
                                          
                                          
                                          
                                          )),
      ),
      
      
      
      
      tabItem(tabName = "Map",tabBox(id= "t5" , width = 12,
                                     tabPanel("Map",icon = icon("map"))
      ))
      
      
    )
  )
)

server <- function(input, output) {
  
  
  output$structure <-renderPrint(
    
    str(datos)
    
  )
  
  output$summary <-renderPrint(
    
    summary(datos)
    
  )
  
  output$dataT <-renderDataTable(
    
    datos)
  
  output$dash1 <-renderPlot(
    
    dash1)
  
  output$anal1 <-renderPlot(
    
    anal1)
  
  output$dash2 <-renderPlot(
    
    dash2)
  
  output$anal2 <-renderPlot(
    
    anal2)
  
  output$dash3 <-renderPlot(
    
    dash3)
  
  output$anal3 <-renderPlot(
    
    anal3)
  
  output$dash4 <-renderPlot(
    
    dash4)
  
  output$anal4 <-renderPlot(
    
    anal4)
  
  
  output$dataT1 <-renderDataTable(
    
    datos1)
  
  
}

shinyApp(ui,server)






