library(shiny)
library(brms)
#library(cmdstanr)
library(rstan)
library(tidyverse)
library(posterior)
library(HDInterval)
library(matrixStats)

model_skeleton <- readRDS("model_skeleton.rds")

ui <- fluidPage(
  titlePanel("Sequential Bayesian Updating"),
  
  tabsetPanel(
    # --- MAIN APP TAB ---
    tabPanel("App",
             br(),
             fluidRow(
               # Panel 1: Priors and Stopping Criteria (Left)
               column(width = 3,
                      wellPanel(
                        h4("Priors"),
                        helpText("Set these before adding the first observation, or leave blank."),
                        numericInput("prior_mean", "Expected Mean:", value = NA, step = 0.1),
                        numericInput("prior_var", "Expected Variance:", value = NA, step = 0.1),
                        
                        hr(),
                        h4("Stopping Criteria"),
                        numericInput("ci_level", "Credible Interval (%):", value = 95, min = 50, max = 99.9, step = 0.1),
                        numericInput("target_width", 
                                     HTML("Target CrI Width: <br><small><i>in units of your variable</i></small>"), 
                                     value = 10, min = 0.01, step = 0.1)
                      )
               ),
               
               # Panel 2: Data Input and Summary (Middle)
               column(width = 3,
                      wellPanel(
                        numericInput("new_obs", "New Observation:", value = NULL, step = 0.1),
                        actionButton("add_obs", "Add Observation", class = "btn-primary"),
                        
                        hr(),
                        h4("Current Data"),
                        div(style = "max-height: 200px; overflow-y: auto;", uiOutput("data_list")),
                        br(),
                        actionButton("reset", "Reset All Data", class = "btn-danger"),
                        
                        hr(),
                        h4("Data Summary"),
                        verbatimTextOutput("data_summary")
                      )
               ),
               
               # Panel 3: Visualisations and Output (Right)
               column(width = 6,
                      plotOutput("posterior_plot", height = "350px"),
                      h4("Target Status"),
                      verbatimTextOutput("status"),
                      h4("Posterior Summary"),
                      verbatimTextOutput("posterior_summary"),
                      plotOutput("width_plot", height = "250px"),
                      h4("Probability of a value occurring"),
                      numericInput("prob_value", "Enter a value", value = NA, step = 0.1),
                      verbatimTextOutput("prob_output")
               )
             )
    ),
    
    # --- USER GUIDE TAB ---
    tabPanel("User Guide",
             fluidRow(
               column(width = 8, offset = 2,
                      br(),
                      wellPanel(
                        h2("How to Use This Application"),
                        p("This application performs Sequential Bayesian Updating. As you collect and enter data one observation at a time, the model updates its beliefs (the posterior distribution) about the underlying data."),
                        
                        hr(),
                        h4("1. Setting Up (Optional)"),
                        p(strong("Priors:"), " Before adding any data, you can set your prior beliefs about the Expected Mean and Variance. If you leave these blank, the model will use weak, data-driven priors based on your first observation."),
                        p(strong("Stopping Criteria:"), " Set your target Credible Interval (CrI) percentage and the desired width. The app will track if your current posterior distribution is narrow enough to meet this target."),
                        
                        hr(),
                        h4("2. Adding Data"),
                        p("Enter a single numerical value into the ", strong("New Observation"), " box and click ", strong("Add Observation"), "."),
                        hr(),
                        h4("3. Interpreting Results"),
                        tags$ul(
                          tags$li(strong("Target Status:"), " Tells you if your current Credible Interval has shrunk below your target width. This is useful for deciding when to stop collecting data."),
                          tags$li(strong("Posterior Plot:"), " Shows the 95th percentile distribution of the posterior predictions. The red dashed lines represent the bounds of your credible interval."),
                          tags$li(strong("Width Progression Plot:"), " Tracks how the width of your credible interval changes as you add more observations. The red dashed line represents your target width."),
                          tags$li(strong("Probability Calculator:"), " Allows you to query the posterior distribution. For example, enter '5' to find out the probability that the true value is greater than or equal to 5.")
                        )
                      )
               )
             )
    )
  )
)

server <- function(input, output, session) {
  
  # store all dynamic variables
  rv <- reactiveValues(
    observations      = numeric(0), # obs entered by user
    model             = NULL,
    posterior_samples = NULL,
    ci_widths         = numeric(0),
    n_obs             = numeric(0)
  )
  
  # update model everytime new obs is added
  update_model <- function() {
    
    # catch empty states
    if (length(rv$observations) == 0 || any(is.na(rv$observations))) {
      rv$model             <- NULL
      rv$posterior_samples <- NULL
      rv$ci_widths         <- numeric(0)
      rv$n_obs             <- numeric(0)
      return()
    }
    
    # Determine if this is the initial model fit or a rapid update
    is_initial_fit <- is.null(rv$model) || length(rv$observations) == 1
    
    if (is_initial_fit) {
      # Show persistent notification for initial compilation/fitting
      notif_id <- showNotification("Fitting initial model...", duration = NULL, type = "message")
      # Ensure the persistent notification is removed when the function exits (success or error)
      on.exit(removeNotification(notif_id), add = TRUE)
    } else {
      # Show quick notification for subsequent updates
      showNotification("Updating model...", duration = 2, type = "message")
    }
    
    data_df <- data.frame(y = rv$observations)
    
    ## Prior Construction
    # obs_value <- rv$observations[1]
    # obs_scale <- abs(obs_value)
    # if (is.na(obs_scale) || obs_scale == 0) obs_scale <- 1
    #
    # # Default data-driven priors
    # prior_int_mean <- obs_value
    # prior_int_sd   <- obs_scale * 2
    # prior_sig_sd   <- 2.5
    #
    # # Override with user inputs if provided
    # if (!is.na(input$prior_mean)) {
    #   prior_int_mean <- input$prior_mean
    # }
    # if (!is.na(input$prior_var) && input$prior_var > 0) {
    #   prior_int_sd <- sqrt(input$prior_var)
    #   prior_sig_sd <- sqrt(input$prior_var) # Scale the variance prior as well
    # }
    # 
    # custom_priors <- c(
    #   set_prior(paste0("normal(", prior_int_mean, ",", prior_int_sd, ")"), class = "Intercept"),
    #   set_prior(paste0("student_t(3, 0, ", prior_sig_sd, ")"), class = "sigma")
    # )
    
    ## Model Fitting / Updating
    if (is_initial_fit) {
      # rv$model <- brm(
      #   brms::bf(y ~ 1),
      #   data    = data_df,
      #   family  = skew_normal(),
      #   prior   = custom_priors,
      #   chains  = 2, iter = 1000, warmup = 250, refresh = 0, silent = 2, seed = 123,
      #   backend = "rstan"
      rv$model <- update(
        model_skeleton, 
        newdata = data_df,
        chains  = 2, 
        iter    = 1000, 
        warmup  = 250, 
        refresh = 0, 
        silent  = 2, 
        seed    = 123,
        backend = "cmdstanr"
      )
    } else {
      rv$model <- update(rv$model, newdata = data_df, refresh = 0, silent = 2,
                         backend = "cmdstanr")
    }
    
    ## 95th Percentile Posterior Prediction
    # must generate large dummy dataset so posterior_predict generates 
    # a distribution of predictions to calculate the 95th percentile from
    large_newdata <- data.frame(dummy = 1:500) 
    
    post_pred <- posterior_predict(
      rv$model,
      newdata = large_newdata
    ) 
    
    rv$posterior_samples <- matrixStats::rowQuantiles(post_pred, probs = 0.95)
    
    ci_prob      <- input$ci_level / 100
    hdi_result   <- hdi(rv$posterior_samples, credMass = ci_prob)
    rv$ci_widths <- c(rv$ci_widths, hdi_result[2] - hdi_result[1])
    rv$n_obs     <- c(rv$n_obs, length(rv$observations))
  }
  
  ##
  
  observeEvent(input$add_obs, {
    req(input$new_obs)
    rv$observations <- c(rv$observations, input$new_obs)
    updateNumericInput(session, "new_obs", value = NA)
    update_model()
  })
  
  observeEvent(input$reset, {
    rv$observations <- numeric(0)
    update_model()
    showNotification("Data reset")
  })
  
  output$data_list <- renderUI({
    if (length(rv$observations) == 0) return(p("No data"))
    
    table_rows <- lapply(seq_along(rv$observations), function(i) {
      tags$tr(
        tags$td(i,                  style = "border: 1px solid black; padding: 4px;"),
        tags$td(rv$observations[i], style = "border: 1px solid black; padding: 4px;")
      )
    })
    
    tags$table(
      style = "border-collapse: collapse; width: 100%;",
      tags$thead(
        tags$tr(
          tags$th("#",     style = "border: 1px solid black; padding: 4px;"),
          tags$th("Value", style = "border: 1px solid black; padding: 4px;")
        )
      ),
      tags$tbody(table_rows),
      style = "border: 1px solid black;"
    )
  })
  
  output$data_summary <- renderText({
    if (length(rv$observations) == 0) return("No data")
    paste0("N: ", length(rv$observations), "\nMean: ", round(mean(rv$observations), 3))
  })
  
  output$posterior_plot <- renderPlot({
    req(rv$posterior_samples)
    ci_prob <- input$ci_level / 100
    ci      <- hdi(rv$posterior_samples, credMass = ci_prob)
    ggplot(data.frame(x = rv$posterior_samples), aes(x)) +
      geom_density(fill = "steelblue", alpha = 0.6) +
      geom_vline(xintercept = ci, color = "red", linetype = "dashed") +
      labs(title = "95th percentile posterior distribution", x = "Value") +
      theme_minimal()
  })
  
  output$status <- renderText({
    req(rv$posterior_samples)
    ci_prob         <- input$ci_level / 100
    hdi_res         <- hdi(rv$posterior_samples, credMass = ci_prob)
    current_width   <- hdi_res[2] - hdi_res[1]
    target_achieved <- current_width <= input$target_width
    paste0(
      "Current ", input$ci_level, "% CrI Width: ", round(current_width, 3), "\n",
      "Target Width: ", input$target_width, "\n",
      "Target Achieved: ", ifelse(target_achieved, "YES", "NO")
    )
  })
  
  output$posterior_summary <- renderText({
    req(rv$posterior_samples)
    ci <- hdi(rv$posterior_samples, credMass = input$ci_level / 100)
    paste0(
      "Mean: ", round(mean(rv$posterior_samples), 3), "\n",
      "CI: [", round(ci[1], 3), ", ", round(ci[2], 3), "]"
    )
  })
  
  output$width_plot <- renderPlot({
    req(length(rv$ci_widths) > 0)
    ggplot(data.frame(n = rv$n_obs, w = rv$ci_widths), aes(n, w)) +
      geom_line() +
      geom_point() +
      geom_hline(yintercept = input$target_width, color = "red", linetype = "dashed") +
      labs(title = "Width Progression", y = "CrI Width") +
      theme_minimal()
  })
  
  output$prob_output <- renderText({
    req(rv$posterior_samples, !is.na(input$prob_value))
    prob <- mean(rv$posterior_samples >= input$prob_value)
    paste0("P(val >= ", input$prob_value, ") = ", round(prob, 4))
  })
}

shinyApp(ui = ui, server = server)