#install.packages("shiny")
library(shiny)
library(ggplot2)
library(sqldf)
library(RSQLite)
library(rmarkdown)
d<-read.csv("Global Superstore Orders Dataset.csv", header = TRUE)
names(d)[names(d) == "Shipping.Cost"] <- "ShippingCost"
names(d)[names(d) == "Sub.Category"] <- "SubCategory"

dsum2<-aggregate.data.frame(d$Quantity,by=list(d$Category,d$Order.Year),FUN = "sum")
names(dsum2) = c("Category","Order.Year","Sum.Quantity")
dsum2.Furniture<- dsum2[dsum2$Category == "Furniture",]
dsum2.OfficeSupplies<- dsum2[dsum2$Category == "Office Supplies",]
dsum2.Technology<- dsum2[dsum2$Category == "Technology",]

dsum3<-aggregate.data.frame(d$Sales,by=list(d$Category,d$Order.Year),FUN = "mean")
names(dsum3) = c("Category","Order.Year","Sales")
dsum3.Furniture<- dsum3[dsum3$Category == "Furniture",]
dsum3.OfficeSupplies<- dsum3[dsum3$Category == "Office Supplies",]
dsum3.Technology<- dsum3[dsum3$Category == "Technology",]

dsum4<-aggregate.data.frame(d$Profit,by=list(d$Category,d$Order.Year),FUN = "mean")
names(dsum4) = c("Category","Order.Year","Profit")
dsum4.Furniture<- dsum4[dsum4$Category == "Furniture",]
dsum4.OfficeSupplies<- dsum4[dsum4$Category == "Office Supplies",]
dsum4.Technology<- dsum4[dsum4$Category == "Technology",]

dsum5<-aggregate.data.frame(d$ShippingCost,by=list(d$Category,d$Order.Year),FUN = "mean")
names(dsum5) = c("Category","Order.Year","ShippingCost")
dsum5.Furniture<- dsum5[dsum5$Category == "Furniture",]
dsum5.OfficeSupplies<- dsum5[dsum5$Category == "Office Supplies",]
dsum5.Technology<- dsum5[dsum5$Category == "Technology",]

#Shiny Example 1: Histogram where you control the number of bins
ui<-shinyUI(fluidPage(

        titlePanel("Financial Analysis of Superstore"),
    mainPanel(
      tabsetPanel(type = "tabs",
        tabPanel("Summary",
                 includeHTML("include.html"),
                 includeMarkdown("include.md")         ),
        tabPanel("Scatter Plot",
                 sidebarLayout(
                 sidebarPanel(
                   #implementing radio buttons
                   radioButtons("x", "Select the categories ",
                                list("Sum of Quantities"='a', "Average Sales"='b', "Average Profits"='c', "Average Shipping Cost"='d')),
          
                   
                   
                 ),
                 mainPanel( 
                 plotOutput("distPlot1")
                 )
                )
              ),
        tabPanel("Barplot",  selectInput("selected_bar", label = "Select Metric for Bar Plot:",
                                         choices = c("Sum of Order" =1,
                                                     "Average Sales" = 2, # Choices
                                                     "Average Profit" = 3,
                                                     "Average Shipping_Cost" = 4),
                                         selected = 1),
                 plotOutput(outputId = "distPlot"))
      )
      
    )
)
)

    
#writing server function
server<-shinyServer(function(input, output) {
  
  #referring output distPlot in ui.r as output$distPlot
  output$distPlot1 <- renderPlot({
    
    
    #referring input x in ui.r as input$x
    if(input$x=='a'){
      
      xrange <- range(dsum2$Order.Year) 
      yrange <- range(dsum2$Sum.Quantity) 
      
      #plot the range first
      plot(xrange, 
           yrange,
           xlim = xrange,
           xlab="Year",
           ylab="Sum of Quantity",
           xaxt="n",
           main = "Sum of Quantity Ordered by Category by Year") 
      #modify axes
      axis(1, labels = as.character(dsum2$Order.Year), at = as.numeric(dsum2$Order.Year))
      #add point markers
      points(dsum2.Furniture$Order.Year, dsum2.Furniture$Sum.Quantity)
      points(dsum2.OfficeSupplies$Order.Year, dsum2.OfficeSupplies$Sum.Quantity)
      points(dsum2.Technology$Order.Year, dsum2.Technology$Sum.Quantity)
      #add lines
      lines(dsum2.Furniture$Order.Year, dsum2.Furniture$Sum.Quantity,col = "red")
      lines(dsum2.OfficeSupplies$Order.Year, dsum2.OfficeSupplies$Sum.Quantity,col = "blue")
      lines(dsum2.Technology$Order.Year, dsum2.Technology$Sum.Quantity,col = "green")
      legend("right", legend=c("Furniture", "Office Supplies", "Technology"),
             col=c("red", "blue", "green"), lty=1:1)
      
    }
    
    if(input$x=='b'){
      xrange <- range(dsum3$Order.Year) 
      yrange <- range(dsum3$Sales) 
      
      #plot the range first
      plot(xrange, 
           yrange, 
           xlab="Year",
           ylab="Average Sales",
           xaxt="n",
           main = "Average Sales by Category by Year") 
      #modify axes
      axis(1, labels = as.character(dsum3$Order.Year), at = as.numeric(dsum3$Order.Year))
      #add point markers
      points(dsum3.Furniture$Order.Year, dsum3.Furniture$Sales)
      points(dsum3.OfficeSupplies$Order.Year, dsum3.OfficeSupplies$Sales)
      points(dsum3.Technology$Order.Year, dsum3.Technology$Sales)
      #add lines
      lines(dsum3.Furniture$Order.Year, dsum3.Furniture$Sales,col = "red")
      lines(dsum3.OfficeSupplies$Order.Year, dsum3.OfficeSupplies$Sales,col = "blue")
      lines(dsum3.Technology$Order.Year, dsum3.Technology$Sales,col = "green")
      legend("right", legend=c("Furniture", "Office Supplies", "Technology"),
             col=c("red", "blue", "green"), lty=1:1)
    }
    
    if(input$x=='c'){
      xrange <- range(dsum4$Order.Year) 
      yrange <- range(dsum4$Profit) 
      
      #plot the range first
      plot(xrange, 
           yrange, 
           xlab="Year",
           ylab="Average Profit",
           xaxt="n",
           main = "Average Profits by Category by Year") 
      #modify axes
      axis(1, labels = as.character(dsum4$Order.Year), at = as.numeric(dsum4$Order.Year))
      #add point markers
      points(dsum4.Furniture$Order.Year, dsum4.Furniture$Profit)
      points(dsum4.OfficeSupplies$Order.Year, dsum4.OfficeSupplies$Profit)
      points(dsum4.Technology$Order.Year, dsum4.Technology$Profit)
      #add lines
      lines(dsum4.Furniture$Order.Year, dsum4.Furniture$Profit,col = "red")
      lines(dsum4.OfficeSupplies$Order.Year, dsum4.OfficeSupplies$Profit,col = "blue")
      lines(dsum4.Technology$Order.Year, dsum4.Technology$Profit,col = "green")
      legend("right", legend=c("Furniture", "Office Supplies", "Technology"),
             col=c("red", "blue", "green"), lty=1:1)
    }
    
    if(input$x =='d')
    {
      
      xrange <- range(dsum5$Order.Year) 
      yrange <- range(dsum5$ShippingCost) 
      
      #plot the range first
      plot(xrange, 
           yrange, 
           xlab="Year",
           ylab="Average Shipping Cost",
           xaxt="n",
           main = "Average Shipping Cost by Category by Year") 
      #modify axes
      axis(1, labels = as.character(dsum5$Order.Year), at = as.numeric(dsum5$Order.Year))
      #add point markers
      points(dsum5.Furniture$Order.Year, dsum5.Furniture$ShippingCost)
      points(dsum5.OfficeSupplies$Order.Year, dsum5.OfficeSupplies$ShippingCost)
      points(dsum5.Technology$Order.Year, dsum5.Technology$ShippingCost)
      #add lines
      lines(dsum5.Furniture$Order.Year, dsum5.Furniture$ShippingCost,col = "red")
      lines(dsum5.OfficeSupplies$Order.Year, dsum5.OfficeSupplies$ShippingCost,col = "blue")
      lines(dsum5.Technology$Order.Year, dsum5.Technology$ShippingCost,col = "green")
      legend("right", legend=c("Furniture", "Office Supplies", "Technology"),
             col=c("red", "blue", "green"), lty=1:1)
    }
    
    
  })
  
  dataframe <- reactive({ 
    # Fetching Selected Plot
    plotNumber <- as.numeric(input$selected_bar)
    
    if (plotNumber==1) {
      sql="SELECT SubCategory, sum(Quantity) as metric FROM d GROUP BY SubCategory"
    }
    
    if (plotNumber==2) {
      sql="SELECT SubCategory,avg(Sales) as metric FROM d GROUP BY SubCategory"
    }
    # Downloads Vs Remixed
    if (plotNumber==3) {
      sql="SELECT SubCategory,avg(Profit) as metric FROM d GROUP BY SubCategory"
    }  
    if (plotNumber==4) {
      sql="SELECT SubCategory,avg(ShippingCost) as metric FROM d GROUP BY SubCategory"
    }
    sqldf(sql)  
  }
  )
  
  output$distPlot <- renderPlot({
    ggplot(dataframe(),mapping = aes(x=SubCategory,y=metric,fill = SubCategory))+
      geom_bar(stat = 'identity')+
      xlab("SubCategory")+
      theme(axis.text.x = element_text(angle = 90, hjust = TRUE))+
      ylab("Metric")+
      ggtitle("Bar Plot by Subcategory")
    
  })
})

shinyApp(ui, server)

