######################## Build Shiny App ######################################
library(shiny)
library(shinydashboard)

ui <- shinyUI(
  dashboardPage(
    title = 'HRCC',
    header = dashboardHeader(
      titleWidth = '100%',
      title = span(
        #tags$img(src="C:/Users/frank/Documents/R_Projects/Shiny_models/www/test_22.jpg", width = '100%'),
        column(12, class="title-box",
               #tags$h1(class="primary-title", style='margin-top:10px;', 'Full Picture Research & Technologies Inc'),
               tags$h1(class="primary-title", style='margin-top:10px;', 'Cost-Effectiveness Analysis of DCY Therapy for the Treatment of Chronic Y-Mole Disease versus Standard of Care')
               )
      )
    ),
    #title = "Five-State Time-Dependent Cohort Simulation (PSA)"),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem("Model Structure ", tabName = "Model_structure_tab", icon = icon("chart-line")),
        menuItem("Model_Output", tabName = "Results_tab", icon = icon("circle"))
        
      )
    ),
    dashboardBody(
      tags$style(type="text/css", "
    /*    Move everything below the header */
        .content-wrapper {
           margin-top: 50px;
        }
        .content {
            padding-top: 60px;
        }
    /*    Format the title/subtitle text */
        .title-box {
            position: absolute;
            text-align: center;
            top: 50%;
            left: 50%;
            transform:translate(-50%, -50%);
        }
        @media (max-width: 590px) {
            .title-box {
                position: absolute;
                text-align: center;
                top: 10%;
                left: 10%;
                transform:translate(-5%, -5%);
            }
        }
        @media (max-width: 767px) {
            .primary-title {
                font-size: 1.1em;
            }
            .primary-subtitle {
                font-size: 1em;
            }
        }
    /*    Make the image taller */
        .main-header .logo {
            height: 125px;
        }
    /*    Override the default media-specific settings */
        @media (max-width: 5000px) {
            .main-header {
                padding: 0 0;
                position: relative;
            }
            .main-header .logo,
            .main-header .navbar {
                width: 100%;
                float: none;
            }
            .main-header .navbar {
                margin: 0;
            }
            .main-header .navbar-custom-menu {
                float: right;
            }
        }
    /*    Move the sidebar down */
        .main-sidebar {
            position: absolute;
        }
        .left-side, .main-sidebar {
            padding-top: 175px;
        }"
      ),
      
      tabItems(
        tabItem("Model_structure_tab",
                h2("Model Structure"),
                h3("Conceptual model representing patient transitions in the treatment and standard care groups"),
                box(imageOutput("Model_structure"), width = 8),
                
                
                br(),
                hr(),
                h4(strong("Model description")),
                p(style="text-align: justify; font-size = 25px",
                  "We constructed a hypothetical Markov model (or state-transition model)
                to calculate the incremental costs and benefits associated with DCY therapy
                and standard of care over a 20-year time horizon for chronic y-mole disease. A Markov model framework 
                allows us to model recurring outcomes over a long period of time. 
                Patients reside in mutually exclusive health states and make 
                transitions between those health states at discrete time intervals (model cycles)
                based on transition probabilities. Conventional Markov models use 
                fixed transition probabilities for the entire model time horizon, 
                whereas our model relaxed this assumption and incorporated 
                time-dependent transition probabilities. We assumed a model cycle length of one year."), 
                #em("demonstrating various use of shiny features"), 
                #"of various Shiny features. Go to",
                #a(href = "https://github.com/NabiilahArdini/Shiny-Box",
                #"Shiny-Box GitHub Page"),
                #"to find more details on the source code."),
                
                tags$b("This is a hypothetical markov model built by
              Full Picture Research & Technologies Inc. Please take a look at"), 
                tags$a(href = "https://fullpictureresearchandtechnologies.ca/our-services","our website"),
                tags$b("for the services we offer."),
                hr()),
        
        tabItem("Results_tab",
                fluidPage ( # creates empty page
                  # title of app
                  titlePanel("Interactive Results Section"),
                  # layout is a sidebar-layout
                  sidebarLayout(
                    sidebarPanel( # open sidebar panel
                      # input type numeric
                      numericInput(inputId = "trt_cost",
                                   label = "Cost of DCY therapy",
                                   value = 750,
                                   min = 0,
                                   max = 2000),
                      numericInput(inputId = "n_sim",
                                   label = "Probabilistic sensitivity analysis runs",
                                   value = 5000,
                                   min = 0,
                                   max = 10000),
                      # input type slider
                      sliderInput(inputId = "n_cycles",
                                  label = "Number of cycles",
                                  value = 10,
                                  min = 1,
                                  max = 20),
                      # action button runs model when pressed
                      actionButton(inputId = "run_simulation",
                                   label = "Run simulation")
                    ), # close sidebarPanel
                    # open main panel
                    mainPanel(
                      # heading (results table)
                      h3("Results table"),
                      # tableOutput id = icer_table, from server
                      tableOutput(outputId = "icer_table"),
                      # heading (Cost effectiveness plane)
                      h3("Cost-effectiveness plane"),
                      # plotOutput id = SO_CE_plane, from server
                      plotOutput(outputId = "SO_CE_plane"),
                      
                      # heading(Cost-effectiveness acceptability curve)
                      h3("Cost-effectiveness acceptability curve"),
                      plotOutput(outputId = "CEAC"),
                    ) # close mainpanel
                  ) # close side barlayout
                ) # close UI fluidpage
        )
        
      )
    )
  )
)
