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
                   )),
                   
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
                       selectInput("transformation2",
                                 "Axis transformation",
                                 choices=c("log10","log2","identity"),
                                 selected = "identity"
                                 )
                       
                     )
                     
                   )
                   
                   
                 
               )
               ,
               
               column(6, wellPanel(textOutput("thrust_result")),
                      wellPanel(
                        helpText("Plot with variation on the voltage:"),
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
                 
               ),
               wellPanel(
                 helpText("Graphical settings"),
                 
                 splitLayout(
                   cellWidths = c("33.33%", "33.33%","33.33%"),
                   numericInput(
                     "d_step",
                     "Distance relative steps (in %)",
                     min = 1,
                     max = 100,
                     value = 10
                   ),
                   numericInput(
                     "d_max",
                     "Max distance",
                     min = 0,
                     value = NA
                   ),
                   numericInput(
                     "d_min",
                     "Min distance",
                     min = 0,
                     value = NA
                   )
                   
                 ),
                 
                 splitLayout(
                   cellWidths = c("33.33%", "33.33%","33.33%"),
                   numericInput(
                     "w_max",
                     "Max weight",
                     min = NA,
                     value = NA
                   ),
                   numericInput(
                     "w_min",
                     "Min weight",
                     min = NA,
                     value = NA
                   ),
                   selectInput("transformation1",
                               "X-Axis transformation",
                               choices=c("log10","log2","identity"),
                               selected = "identity"
                   )
                   
                 )
                 
               )
               
               
               
               
               )
               ,
               
               column(6, wellPanel(textOutput("distance_result")),
                      wellPanel(
                        helpText(
                          "Plot with variation on the capacitor plates distance:"
                        ),
                        plotOutput("plot_distance_vs_weightloss")
                      ))
               
             )),
             
             tabPanel("Voltage vs. Weight Change",
                      
                      fluidRow(
                        column(4, wellPanel(
                          numericInput(
                            "Weight2",
                            "Total weight of the payload + capacitor (in grams)",
                            min = 0,
                            value = 250
                          ),
                          numericInput(
                            "d2",
                            "Distance between capacitor plates (in meters)",
                            min = 0,
                            value = 1 * 10 ^ (-5)
                          ),
                          numericInput(
                            "SurfaceArea2",
                            "Surface area of the capacitor (in meters squared)",
                            min = 0,
                            value = 0.0016
                          ),
                          numericInput(
                            "Capacitance",
                            "Capacitance of the capacitor (in Farads)",
                            min = 0,
                            value = 1.0E-8
                          ),
                          splitLayout(
                            cellWidths = c("33.33%", "33.33%","33.33%"),
                            numericInput(
                              "V_step2",
                              "Voltage step",
                              min = 1,
                              max = 10000,
                              value = 1000
                            ),
                            numericInput(
                              "V_max2",
                              "Max voltage",
                              min = 0,
                              value = 5000
                            ),
                            numericInput(
                              "V_min2",
                              "Min voltage",
                              min = 1,
                              value = 0
                            )
                            
                          )
                          
                        ),
                        wellPanel(
                          helpText("Graphical settings"),
                          
                          
                          
                          splitLayout(
                            cellWidths = c("33.33%", "33.33%","33.33%"),
                            numericInput(
                              "w_max2",
                              "Max weight",
                              min = NA,
                              value = NA
                            ),
                            numericInput(
                              "w_min2",
                              "Min weight",
                              min = NA,
                              value = NA
                            )
                            
                          )
                          
                        )
                        
                        
                        
                        
                        )
                        ,
                        
                        column(6,
                               wellPanel(
                                 plotOutput("plot_voltage_vs_weightloss")
                               ))
                        
                      )             
             
             )
    
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
      
      capacitor_plates_distance = sqrt ((0.00014 * input$Current * input$SurfaceArea) / ( (input$Weight)/100 * 9.80665)) # 1kg = 9.80665 Newtons. Introduced weight is in grams.
      
      output$plot_distance_vs_weightloss <- renderPlot({
        
        d_variation <- c(capacitor_plates_distance*seq(0.5,1,input$d_step/100),capacitor_plates_distance*seq(1,2,input$d_step/100))
        
        d_variation[d_variation <= 0] <- NA
        
        Weight_variation <- input$Weight - (((0.00014 * input$Current * input$SurfaceArea)/(d_variation^2))/9.80665)*100 
        
        df1 <- data.frame(X = d_variation*pi,Y = Weight_variation)
        
        df1 <- na.omit(df1)
        
        plot1 <- ggplot(df1, aes(x=X, y=Y)) + 
          geom_point(size=3, shape=16) + 
          geom_line() +
          ylab("Weight change (g)") + xlab("Distance between the capacitor plates (m)") +
          scale_y_continuous(limits = c(input$w_min,input$w_max)) +
          coord_cartesian(xlim = c(input$d_min,input$d_max)) + 
          theme_minimal(base_size = 16) + 
          geom_line(aes(y = 0), colour = "red",lwd=1.5, linetype = 'dotted') +
          geom_line(aes(x = capacitor_plates_distance*pi), colour = "red",lwd=1.5, linetype = 'dotted') +
          scale_x_continuous(trans=input$transformation1)
        
        plot1
        
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
      # The thrust is calculated as = F/P in N per W, where F is the QI force (that mostly depends on d)
      
      I = 1
      
      QI_FORCE <- (0.00014 * I * input$A) / (input$d^2)
      
      P = input$V * I # in W
      
      Thrust = (QI_FORCE / P) * 1000 # in N/kW
      
      Variance <- 1 - input$variance/100
      
      output$plot_voltage_vs_thrust <- renderPlot({
        
        V_variation <- c(input$V - seq(5000,0,-input$V_step), input$V + seq(0,5000,input$V_step))
        
        V_variation[V_variation <= 0] <- NA

        Thrust_variation <- QI_FORCE / (I * V_variation) * 1000
        
        df2 <- data.frame(X = V_variation,Y = Thrust_variation, Y_min=Thrust_variation*(Variance), Y_max = Thrust_variation*(1/Variance))
        
        df2 <- na.omit(df2)
        
        plot2 <- ggplot(df2, aes(x=X, y=Y)) + 
          geom_line(aes(y = Y_max), colour = "red",lwd=1.5) +
          geom_line(aes(y = Y_min), colour = "red",lwd=1.5) + 
          geom_point(size=3, shape=16) + 
          geom_line() +
          ylab("Thrust (N / kW)") + xlab("Voltage (V)") + 
          coord_cartesian(ylim = c(input$T_min,input$T_max)) + 
          coord_cartesian(xlim = c(input$V_min,input$V_max)) + 
          theme_minimal(base_size = 16) +
          scale_x_continuous(trans=input$transformation2) + 
          scale_y_continuous(trans=input$transformation2)

        plot2
       
      })
      
      
      paste("The predicted thrust of the capacitor is: ", round(Thrust,3), " N / kW.",
            " Considering an estimation error of ",input$variance,"%, the actual thrust should fall within the following interval: ",
            "[",round(Thrust*(Variance),3),";",round(Thrust*(1/Variance),3),"]"," N / kW.")
      
    })
    
    
    
    output$plot_voltage_vs_weightloss <- renderPlot({
      
      # I  = C * dV/dt
      # where C is the capacitance in Farads
      # dV/dt is the change of voltage over time
      # and I is the current in Amperes
      
      dV_dt <- c(seq(input$V_min2,input$V_max2,input$V_step2))
      
      current = input$Capacitance * dV_dt
      
      Weight_variation2 <- input$Weight2 - (((0.00014 * current * input$SurfaceArea2)/((input$d2)^2))/9.80665)*100 
      
      df3 <- data.frame(X = dV_dt,Y = Weight_variation2)
      
      df3 <- na.omit(df3)
      
      plot3 <- ggplot(df3, aes(x=X, y=Y)) + 
        geom_point(size=3, shape=16) + 
        geom_line() +
        ylab("Weight change (g)") + xlab("Voltage (V)") +
        scale_y_continuous(limits = c(input$w_min2,input$w_max2)) +
        coord_cartesian(xlim = c(input$V_min2,input$V_max2)) + 
        theme_minimal(base_size = 16) + 
        geom_line(aes(y = input$Weight2), colour = "red",lwd=1.5, linetype = 'dotted')
      
      plot3
      
    })
}

shinyApp(ui = ui, server = server)
