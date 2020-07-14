#A nice trick: Keeping tabs
#Creating tab boxes
#Lining up in columns
#col based layouts require specs for width. We  have to specify the width of the col and 
#the width of the box in that col.
library(shinydashboard)
library(moments)

ui<-dashboardPage(
    dashboardHeader(title = "Uniform Distribution"),
    dashboardSidebar(),
    dashboardBody(
        fluidRow(
            column(width=6,
                   box(
                       title = "Select a Number",
                       solidHeader = TRUE,
                       background = "yellow",
                       status="warning",
                       width = NULL,
                       height = 312,
                       sliderInput(inputId = "number",label = "Sample",value = 500, min = 25, max = 1000),
                       sliderInput(inputId = "binwidth",label = "Bin Width",value = .05,min = .025,max = 2)),
                   box(title = "Histogram",
                       solidHeader=TRUE,
                       background = "light-blue",
                       status="primary",
                       width = NULL,
                       plotOutput("hist", height = 250))
            ),
            column(width = 6,
                   tabBox(
                       title = "Central Tendancy",
                       id = "tabs1",height = 120,width = NULL,
                       tabPanel("Mean",h2(textOutput("meantext")),width=NULL),
                       tabPanel("Median",h3(textOutput("mediantext")),width=NULL),
                       tabPanel("Mode",h2(textOutput("modetext")),width=NULL)),
                   tabBox(title = "Dispersion",
                          id="tabs2",height = 120,width = NULL,
                          tabPanel("Variance",h2(textOutput("vartext")),width=NULL),
                          tabPanel("Standard Deviation",h2(textOutput("sdtext")),width=NULL),
                          tabPanel("Range",h2(textOutput("rangetext")),width=NULL)),
                   tabBox(title = "Appearance",
                          id="tabs3",height = 120,width = NULL,
                          tabPanel("Skewness",h2(textOutput("skewnesstext")),width=NULL),
                          tabPanel("Kurtosis",h2(textOutput("kurtosistext")),width=NULL)
                   ),
                   tabBox(title = "Summary",
                          id="tabs4",height = 120,width = NULL,
                          tabPanel("About",h2(textOutput("About")),width=NULL)),
            )
        )
    )
)
server<-function(input,output){
    #Creating data that the hist will use
    histdata<-reactive({runif(input$number,min = 0,max = 1)})
    
    output$hist<-renderPlot({hist(histdata(),xlab = "Value",col = "green",main = paste(input$number," random values between o and 1 with binwidth = ",input$binwidth))})
    
    output$meantext<-renderText({paste("Mean = ",round(mean(histdata()),3))})
    
    output$mediantext<-renderText({paste("Medidan = ",round(median(histdata()),3))})
    
    output$modetext<-renderText({paste("Mode = ",round(mode(histdata()),3))})
    
    output$sdtext<-renderText({paste("Standard Deviation = ",round(sd(histdata()),3))})
    
    output$vartext<-renderText({paste("Variance = ",round(var(histdata()),3))})
    
    output$rangext<-renderText({paste("Range = ",round(max(histdata())-min(histdata()),3))})
    
    output$skewnesstext<-renderText({paste("Skewness = ",round(skewness(histdata()),3))})
    
    output$kurtosistext<-renderText({paste("Kurtosis = ",round(kurtosis(histdata()),3))})
    
    aboutdata<-reactive({paste(" I am Gilbert. I am still learning R.\n I regret one thing. I am slow. \n I'll try my best.")})
    
    output$About<-renderText({paste(aboutdata())})
}
shinyApp(ui,server)
