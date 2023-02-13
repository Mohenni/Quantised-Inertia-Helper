library(shiny)
library(ggplot2)

ui <- shinyUI(fluidPage(
  headerPanel("Quantised Inertia Helper"),
  
  # Hide errors
  tags$style(
    type = "text/css",
    ".shiny-output-error { visibility: hidden; }",
    ".shiny-output-error:before { visibility: hidden; }"
  ),
  
  tabsetPanel(
    tabPanel("Thrust calculator",
             
             fluidRow(
               column(
                 4,
                 wellPanel(
                   numericInput(
                     "V",
                     "Current voltage (in volts)",
                     min = 0,
                     value = 5000
                   ),
                   numericInput(
                     "d",
                     "Distance between capacitor plates (in meters)",
                     min = 0,
                     value = 1 * 10 ^ (-5)
                   ),
                   numericInput(
                     "A",
                     "Surface area of the capacitor (in meters squared)",
                     min = 0,
                     value = 0.0016
                   ),
                   numericInput(
                     "variance",
                     "Prediction error (in %)",
                     min = 0,
                     max = 99.99,
                     value = 20
                   ),
                   
                   wellPanel(
                     helpText("Graphical settings"),
                     
                     splitLayout(
                       cellWidths = c("33.33%", "33.33%","33.33%"),
                       numericInput(
                         "V_step",
                         "Voltage step",
                         min = 1,
                         max = 10000,
                         value = 1000
                       ),
                       numericInput(
                         "V_max",
                         "Max voltage",
                         min = 0,
                         value = NA
                       ),
                       numericInput(
                         "V_min",
                         "Min voltage",
                         min = 1,
                         value = NA
                       )
                       
                     ),
                     
                     splitLayout(
                       cellWidths = c("33.33%", "33.33%","33.33%"),
                       numericInput(
                         "T_max",
                         "Max thrust",
                         min = 0,
                         value = 2000
                       ),
                       numericInput(
                         "T_min",
                         "Min thrust",
                         min = 0,
                         value = 250
                       ),
                       selectInput("transformation",
                                 "Axis transformation",
                                 choices=c("log10","log2","identity"),
                                 selected = "identity"
                                 )
                       
                     )
                     
                   )
                   
                   
                 )
               )
               ,
               
               column(6, wellPanel(textOutput("thrust_result")),
                      wellPanel(
                        helpText("Plot with variation on voltage:"),
                        plotOutput("plot_voltage_vs_thrust")
                      ))
               
             )),
    
    tabPanel("Plate distance calculator",
             
             fluidRow(
               column(4, wellPanel(
                 numericInput(
                   "Weight",
                   "Total weight of the payload + capacitor (in grams)",
                   min = 0,
                   value = 250
                 ),
                 numericInput(
                   "SurfaceArea",
                   "Surface area of the capacitor (in meters squared)",
                   min = 0,
                   value = 0.0016
                 ),
                 numericInput(
                   "Current",
                   "Used electrical current in (in Amperes)",
                   min = 0,
                   value = 0.0001
                 )
                 
               ))
               ,
               
               column(6, wellPanel(textOutput("distance_result")),
                      wellPanel(
                        helpText(
                          "Plot with variation on the capacitor plates distance:"
                        ),
                        plotOutput("plot_distance_vs_weightloss")
                      ))
               
             ))
    
  ),
  hr(),
  print("Created & designed by M.Henni (2023)")
))

server <- function(input, output, session) {

    output$distance_result <- renderText({
      
      # the formula used is F = 0.00014 * I * A / d^2
      # Where F is in Newtons
      # I is the current in amperes
      # A is the surface of the capacitor (which should be greater or equal to the object)
      # d is the distance between the capacitor plates in meters
      
      capacitor_plates_distance = sqrt ((0.00014 * input$Current * input$SurfaceArea) / ( (input$Weight)/100 * 9.80665)) # 1kg = 9.80665 Newtons, introduced weight is in grams so / 100 x 10 => / 10
      
      output$plot_distance_vs_weightloss <- renderPlot({
        
        d_variation <- c(capacitor_plates_distance*seq(0.5,1,0.1),capacitor_plates_distance*seq(1.1,2,0.1))
        
        d_variation[d_variation < 0] <- NA
        
        Weight_variation <- input$Weight - (((0.00014 * input$Current * input$SurfaceArea)/(d_variation^2))/9.80665)*100 
        
        plot(x = d_variation*pi , y = Weight_variation, ylab = "Weight change (g)", xlab = "Distance between the capacitor plates (m)", type = "b", pch = 16)
        abline(h = 0,v = capacitor_plates_distance*pi, col = "red", lwd = 1)
        
      })
      
      
      paste("The theoretical minimal distance required between the plates of the capacitor for a positive lift is: ",round(capacitor_plates_distance*pi,9), " m.")
      
      })
    
    
    output$thrust_result <- renderText({
      
      # Example of thrust calculation:
      # V = 5000 is the voltage in volts
      # I = 10 * 10^(-6) is the current in amperes, but is not needed in the thrust since we multiply and divide by it.
      # P = V * I is the power in W
      # d = 1 * 10^(-5) is the distance between the capacitor plates in meters
      # F = 6 * 10^(-3) QI force in Newton
      # The thrust is calculated as = F/P in N per W, where F is the QI force (that depends on d)
      
      I = 1
      
      QI_FORCE <- (0.00014 * I * input$A) / (input$d^2)
      
      P = input$V * I # in W
      
      Thrust = (QI_FORCE / P) * 1000 # in N/kW
      
      Variance <- 1 - input$variance/100
      
      output$plot_voltage_vs_thrust <- renderPlot({
        
        V_variation <- c(input$V - seq(5000,0,-input$V_step), input$V + seq(0,5000,input$V_step))
        
        V_variation[V_variation <= 0] <- NA

        Thrust_variation <- QI_FORCE / (I * V_variation) * 1000
        
        Data <- data.frame(X = V_variation,Y = Thrust_variation, Y_min=Thrust_variation*(Variance), Y_max = Thrust_variation*(1/Variance))
        
        Data <- na.omit(Data)
        
        plot <- ggplot(Data, aes(x=X, y=Y)) + 
          geom_line(aes(y = Y_max), colour = "red",lwd=1.5) +
          geom_line(aes(y = Y_min), colour = "red",lwd=1.5) + 
          geom_point(size=3, shape=16) + 
          geom_line() +
          ylab("Thrust (N / kW)") + xlab("Voltage (V)") + 
          coord_cartesian(ylim = c(input$T_min,input$T_max)) + 
          coord_cartesian(xlim = c(input$V_min,input$V_max)) + 
          theme_minimal(base_size = 18) +
          scale_x_continuous(trans=input$transformation) + 
          scale_y_continuous(trans=input$transformation)

        plot
       
      })
      
      
      paste("The predicted thrust of the capacitor is: ", round(Thrust,3), " N / kW.",
            " Considering an estimation error of ",input$variance,"%, the actual thrust should fall within the following interval: ",
            "[",round(Thrust*(Variance),3),";",round(Thrust*(1/Variance),3),"]"," N / kW.")
      
    })
}

shinyApp(ui = ui, server = server)
