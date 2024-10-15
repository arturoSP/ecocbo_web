#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      waiter::use_waiter(),
      titlePanel("ecocbo"),

      tabPanel(
        title = "Online Simulated Ecological Cost-Benefit Optimization (ecocbo-Online)",
        sidebarLayout(
          sidebarPanel(
            wellPanel(p(strong("0. Prepare the data")),
                      fileInput("file_cbo", "Pilot data", accept = ".csv")
            ),
            wellPanel(p(strong("1. prep_data")),
                      selectInput("type_cbo", "Nature of the data to be processed", data.type, selected = "counts"),
                      selectInput("Sest_cbo", "Method for estimating species richness", Sest.method),
                      sliderInput("cases_cbo", "Cases", 1, 100, 5),
                      sliderInput("sites_cbo", "Sites", 1, 1000, 10),
                      sliderInput("N_cbo", "N", 1, 1000, 1000),
                      sliderInput("m_cbo", "m", 2, 100, 4),
                      sliderInput("n_cbo", "n", 2, 100, 7),
                      sliderInput("k_cbo", "k", 2, 100, 4),
                      # sliderInput("alpha_cbo", "alpha", 0, 1, 0.05),
                      selectInput("method_cbo", "Dissimilarity", method, selected = "bray"),
                      selectInput("transf_cbo", "Transformation", transf),
                      radioButtons("dummy_cbo", "Is a dummy variable needed?", mult,
                                   selected = "FALSE", inline = TRUE),
                      radioButtons("parall_cbo", "Use parallel computing?", mult,
                                   selected = "TRUE", inline = TRUE),
                      radioButtons("model_cbo", "Model to use", multmodel,
                                   selected = "single.factor", inline = TRUE),
                      actionButton("prep_cbo", "GO prep_data")
            ),
            wellPanel(p(strong("2. scompvar & sim_cbo")),
                      radioButtons("pickcbo", "Type of optimization",
                                   choiceNames = pickcboNames,
                                   choiceValues = pickcboValues,
                                   selected = "budget", inline = TRUE),
                      numericInput("multSE_cbo", "Multivariate standard error",
                                   value = 0.20),
                      numericInput("ct_cbo", "Total cost", value = 20000),
                      numericInput("cj_cbo", "Sampling unit cost", value = 1200),
                      numericInput("ck_cbo", "Sample cost", value = 400),
                      actionButton("cbo_cbo", "GO sim_cbo")
            ),
#
#             wellPanel(p(strong("1. assempar")),
#                       selectInput("type_cbo", "Nature of the data to be processed", data.type, selected = "counts"),
#                       selectInput("Sest_cbo", "Method for estimating species richness", Sest.method),
#                       actionButton("parameters_cbo", "GO assempar")
#             ),
#             wellPanel(p(strong("2. simdata H0")),
#                       sliderInput("cases_cbo0", "Cases", 1, 100, 5),
#                       sliderInput("sites_cbo0", "Sites", 1, 1, 1),
#                       sliderInput("N_cbo0", "N", 1, 1000, 1000),
#                       actionButton("simul_cbo0", "GO simdata")
#             ),
#             wellPanel(p(strong("3. simdata Ha")),
#                       sliderInput("cases_cboa", "Cases", 1, 100, 5),
#                       sliderInput("sites_cboa", "Sites", 1, 1000, 10),
#                       sliderInput("N_cboa", "N", 1, 1000, 100),
#                       actionButton("simul_cboa", "GO simdata")
#             ),

            wellPanel(p(strong("3. sim_beta")),
                      # sliderInput("m_cbo", "m", 2, 100, 4),
                      # sliderInput("n_cbo", "n", 2, 100, 7),
                      # sliderInput("k_cbo", "k", 2, 100, 4),
                      sliderInput("alpha_cbo", "alpha", 0, 1, 0.05),
                      # selectInput("method_cbo", "Dissimilarity", method, selected = "bray"),
                      # selectInput("transf_cbo", "Transformation", transf),
                      # radioButtons("dummy_cbo", "Is a dummy variable needed?", mult,
                      #              selected = "FALSE", inline = TRUE),
                      # radioButtons("parall_cbo", "Use parallel computing?", mult,
                      #              selected = "TRUE", inline = TRUE),
                      actionButton("betae_cbo", "GO simbeta")
            ),
            wellPanel(p(strong("4. plot_power")),
                      radioButtons("nPlot_yes", "Pick n?", mult,
                                   selected = "TRUE", inline = TRUE),
                      sliderInput("mPlot_cbo", "m", 2, 100, 4),
                      sliderInput("nPlot_cbo", "n", 2, 100, 4),
                      radioButtons("pickPlot_cbo", "Type of plot",
                                   choiceNames = pickPlotNames,
                                   choiceValues = pickPlotValues,
                                   selected = "power", inline = TRUE),
                      actionButton("plot_cbo", "GO plot_power")
            )
          ),

          mainPanel(
            tabsetPanel(
              # tabPanel("H0",
              #          tableOutput("dataH0_Out")),
              # tabPanel("Ha",
              #          tableOutput("dataHa_Out")),
              # tabPanel("parH0",
              #          tableOutput("parH0_Out")),
              # tabPanel("parHa",
              #          tableOutput("parHa_Out")),
              # tabPanel("simH0",
              #          tableOutput("simH0_Out")),
              # tabPanel("simHa",
              #          tableOutput("simHa_Out")),
              tabPanel("dataprep",
                       withSpinner(tableOutput("prep_Out"), type = 1)),
              tabPanel("CompVar",
                       withSpinner(tableOutput("CV_Out"), type = 1)),
              tabPanel("CBO",
                       withSpinner(tableOutput("cbo_Out"), type = 1)),
              tabPanel("betaE",
                       withSpinner(tableOutput("power_Out"), type = 1)),
              tabPanel("plot",
                       withSpinner(plotOutput("plot_Out"), type = 1))
            )
          )
        )
      )


    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "ecocbo_web"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
