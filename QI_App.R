library(shiny)

ui <- shinyUI(fluidPage(
  
  headerPanel("QI Helper"),
  
  # Hide errors
  tags$style(type="text/css",".shiny-output-error { visibility: hidden; }",".shiny-output-error:before { visibility: hidden; }"),
  
  tabsetPanel(
  
    tabPanel("Thrust calculator",
             
             fluidRow(column(4,wellPanel(
               
               numericInput("V",
                            "Current voltage (in volts)", min = 0, value = 5000),
               numericInput("d",
                            "Distance between capacitor plates (in meters)", min = 0, value = 1 * 10^(-5)),
               numericInput("A",
                            "Surface area of the capacitor (in meters squared)", min = 0, value = 0.0016)
               
             )
             
             )
             ,
             
             column(6,wellPanel(
               textOutput("thrust_result")),
               wellPanel(
                 helpText("Plot with variation on voltage:"),
                 plotOutput("plot_voltage_vs_thrust"))
             )
             
             )      
             
             ),
  
    tabPanel("Plate distance calculator",
             
             fluidRow(column(4,wellPanel(
               
               numericInput("Weight",
                            "Total weight of the payload + capacitor (in grams)", min = 0, value = 250),
               numericInput("SurfaceArea",
                            "Surface area of the capacitor (in meters squared)", min = 0, value = 0.0016),
               numericInput("Current",
                            "Used electrical current in (in Amperes)", min = 0, value = 0.0001)
               
             )
             
             )
             ,
             
             column(6,wellPanel(
               textOutput("distance_result")),
               wellPanel(
                 helpText("Plot with variation on the capacitor plates distance. The red line represents the minimal distance for the lift:"),
                 plotOutput("plot_distance_vs_weightloss")
               )
             )
             
             )
             
             )

  
                      
 
  ),hr(),
  print("Created & designed by M.Henni (2023)"))
)

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
        
        plot(x = d_variation , y = Weight_variation, ylab = "Weight change (g)", xlab = "Distance between the capacitor plates (m)", type = "b", pch = 16)
        abline(v = capacitor_plates_distance, col = "red", lwd = 1)
        
      })
      
      
      paste("The theoretical minimal distance required between the plates of the capacitor for a positive lift is: ",round(capacitor_plates_distance,9), " meters")
      
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
      
      output$plot_voltage_vs_thrust <- renderPlot({
        
        V_variation <- c(input$V-5000,input$V-4000,input$V-3000,input$V-2000,input$V-1000,input$V,input$V+1000,input$V+2000,input$V+3000,input$V+4000,input$V+5000)
        
        V_variation[V_variation < 0] <- NA
        
        Thrust_variation <- QI_FORCE / (I * V_variation) * 1000
        
        plot(x = V_variation , y = Thrust_variation, ylab = "Thrust (N / kW)", xlab = "Voltage (V)", type = "b", pch = 16)
        
      })
      
      
      paste("The predicted thrust of the capacitor is: ", round(Thrust,3), " N / kW")
      
    })
}

shinyApp(ui = ui, server = server)
