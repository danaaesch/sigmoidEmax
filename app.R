# app.R
# POC for MCPMod: interactive sigmoidal Emax (Hill) model visualizer
# Dana: this app keeps ED50 constrained to the selected dose range and
# enforces Emax > E0, h > 0. Use the sidebar to set parameters.

library(shiny)
library(ggplot2)

sigEmax <- function(d, E0, ED50, Emax, h) {
  E0 + Emax * d^h / (ED50^h + d^h)
}

ui <- fluidPage(
  titlePanel("POC: Sigmoidal Emax (Hill) Model"),
  sidebarLayout(
    sidebarPanel(
      # 1) Dose range selection (global range 1..50)
      sliderInput(
        "dose_range", label = "Dose range (D)", min = 1, max = 50,
        value = c(1, 50), step = 1
      ),
      
      # 2) ED50 must lie within selected dose range; min/max update reactively
      sliderInput(
        "ed50", label = "ED50 (must lie within dose range)",
        min = 1, max = 50, value = 25, step = 0.1
      ),
      
      # 3) Baseline, Maximum effect increment, and Hill coefficient
      numericInput("E0",  label = "E0 (baseline)", value = 0),
      numericInput("Emax", label = "Emax (increment above E0; must exceed E0)", value = 1),
      numericInput("h",    label = "h (Hill coefficient; > 0)", value = 1, min = 0.01, step = 0.01),
      
      # Optional resolution control
      sliderInput("npoints", label = "Curve resolution (points)", min = 50, max = 1000, value = 300, step = 50),
      
      actionButton("update", "Update", class = "btn-primary")
    ),
    mainPanel(
      plotOutput("curve", height = 450),
      br(),
      verbatimTextOutput("info")
    )
  )
)

server <- function(input, output, session) {
  # Keep ED50 slider constrained to the chosen dose range
  observeEvent(input$dose_range, {
    rng <- sort(input$dose_range)
    # If current ED50 is outside the new range, clamp it inside
    new_ed50 <- min(max(input$ed50, rng[1]), rng[2])
    updateSliderInput(session, "ed50", min = rng[1], max = rng[2], value = new_ed50)
  }, ignoreInit = TRUE)
  
  # Construct dose vector from the selected range
  d_vec <- reactive({
    rng <- sort(input$dose_range)
    seq(rng[1], rng[2], length.out = input$npoints)
  })
  
  # Compute the model response for the current parameters.
  # Use bindEvent so we only recompute when inputs actually change.
  model_df <- reactive({
    validate(
      need(input$h > 0, "h must be > 0"),
      need(input$Emax > input$E0, "Emax must be greater than E0 (increment above baseline)")
    )
    d <- d_vec()
    y <- sigEmax(d = d, E0 = input$E0, ED50 = input$ed50, Emax = input$Emax, h = input$h)
    data.frame(d = d, y = y)
  }) |> bindEvent(input$update, ignoreInit = TRUE)
  
  output$curve <- renderPlot({
    df <- model_df()
    ggplot(df, aes(x = d, y = y)) +
      geom_line(linewidth = 1) +
      labs(x = "Dose (D)", y = "Response (sigmoidal Emax)",
           title = "Sigmoidal Emax: y = E0 + Emax * D^h / (ED50^h + D^h)") +
      theme_minimal(base_size = 14)
  }) |> bindEvent(model_df())
  
  output$info <- renderText({
    paste0(
      "Current parameters:\n",
      "Dose range: [", paste(round(sort(input$dose_range), 3), collapse = ", "), "]\n",
      "ED50: ", round(input$ed50, 4), "\n",
      "E0: ", round(input$E0, 4), ", Emax: ", round(input$Emax, 4), ", h: ", round(input$h, 4)
    )
  }) |> bindEvent(model_df())
}

shinyApp(ui, server)
