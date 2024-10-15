#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import SSP
#' @import sampling
#' @import vegan
#' @import waiter
#' @import dplyr
#' @import shinyWidgets
#' @import tidyr
#' @import ecocbo
#' @import shinycssloaders
#' @import gt
#'
#' @noRd
#'

library(SSP)
library(vegan)
library(sampling)
library(waiter)
library(dplyr)
library(shinyWidgets)
library(tidyr)
library(ecocbo)
library(shinycssloaders)
library(gt)

app_server <- function(input, output, session) {
  ## read data ecocbo ----
  data_cbo <- reactive({
    infile <- input$file_cbo
    read.csv(infile$datapath)
  })

  # result from prep_data ----
  simResults <- eventReactive(input$prep_cbo, {
    progress <- Progress$new(min = 1, max = input$n_cbo * input$m_cbo * input$k_cbo)
    on.exit(progress$close())
    progress$set(message = "calculating",
                 detail = "please wait...")
    for (i in seq_len(input$n_cbo * input$m_cbo * input$k_cbo)) {
      progress$inc(1)
    }

    prep_data(data = data_cbo(),
              type = input$type_cbo,
              Sest.method = input$Sest_cbo,
              cases = input$cases_cbo,
              N = input$N_cbo,
              sites = input$sites_cbo,
              n = input$n_cbo,
              m = input$m_cbo,
              k = input$k_cbo,
              transformation = input$transf_cbo,
              method = input$method_cbo,
              dummy = input$dummy_cbo,
              useParallel = input$parall_cbo,
              model = input$model_cbo)
  })

  #
  #
  # ## simmulate H0 and Ha ----
  # dataH0 <- reactive({
  #   temp <- data_cbo()
  #   temp$site <- "T0"
  #   return(temp)
  # })
  # dataHa <- reactive({
  #   data_cbo()
  # })
  #
  # ## assempar ecocbo ----
  # par_cbo <- eventReactive(input$parameters_cbo, {
  #   progress <- Progress$new(min = 1, max = 20)
  #   on.exit(progress$close())
  #   progress$set(message = "estimating parameters",
  #                detail = "please wait...")
  #   for(i in seq_len(20)){
  #     progress$inc(1)
  #   }
  #
  #   temp <- list(H0 = dataH0(), Ha = dataHa())
  #   lapply(temp, assempar, type = input$type_cbo, Sest.method = input$Sest_cbo)
  # })
  #
  # ## simdata ecocbo ----
  # sim_cbo0 <- eventReactive(input$simul_cbo0, {
  #   progress <- Progress$new(min = 1, max = input$cases_cbo0 * input$N_cbo0 * input$sites_cbo0)
  #   on.exit(progress$close())
  #   progress$set(message = "simulating dataH0",
  #                detail = "please wait...")
  #   for(i in seq_len(input$cases_cbo0 * input$N_cbo0 * input$sites_cbo0)){
  #     progress$inc(1)
  #   }
  #
  #   simdata(Par = par_cbo()$H0, cases = input$cases_cbo0,
  #           N = input$N_cbo0, sites = input$sites_cbo0)
  # })
  # sim_cboa <- eventReactive(input$simul_cboa, {
  #   progress <- Progress$new(min = 1, max = input$cases_cboa * input$N_cboa * input$sites_cboa)
  #   on.exit(progress$close())
  #   progress$set(message = "simulating dataHa",
  #                detail = "please wait...")
  #   for(i in seq_len(input$cases_cboa * input$N_cboa * input$sites_cboa)){
  #     progress$inc(1)
  #   }
  #
  #   simdata(Par = par_cbo()$Ha, cases = input$cases_cboa,
  #           N = input$N_cboa, sites = input$sites_cboa)
  # })

  ## simbeta ----
  # # update the possible values for n and m in the next step
  # observeEvent(sim_cboa(), {
  #   updateSliderInput(session, inputId = "m_cbo", max = input$sites_cboa)
  #   updateSliderInput(session, inputId = "n_cbo", max = input$N_cboa)
  # })

  beta_cbo <- eventReactive(input$betae_cbo, {
    # progress <- Progress$new(min = 1, max = input$n_cbo * input$m_cbo * input$k_cbo)
    # on.exit(progress$close())
    # progress$set(message = "calculating",
    #              detail = "please wait...")
    # for (i in seq_len(input$n_cbo * input$m_cbo * input$k_cbo)) {
    #   progress$inc(1)
    # }

    # sim_beta(simH0 = sim_cbo0(),
    #          simHa = sim_cboa(),
    #          n = input$n_cbo,
    #          m = input$m_cbo,
    #          k = input$k_cbo,
    #          alpha = input$alpha_cbo,
    #          transformation = input$transf_cbo,
    #          method = input$method_cbo,
    #          dummy = input$dummy_cbo,
    #          useParallel = input$parall_cbo)

    sim_beta(simResults(), input$alpha_cbo)
  })
  pwr <- reactive({
    req(beta_cbo())
    temp <- beta_cbo()$Power[,c(1:3)] %>%
      tidyr::pivot_wider(names_from = "n", values_from = "Power",
                         names_prefix = "n =") %>%
      mutate(m = paste0("m = ", m))
  })

  ## plot_power ----
  #update possible values for n and m in the next step
  observeEvent(beta_cbo(), {
    updateSliderInput(session, inputId = "nPlot_cbo", max = input$n_cbo)
    updateSliderInput(session, inputId = "mPlot_cbo", max = input$m_cbo)
  })

  nn <- eventReactive(input$plot_cbo, {
    if(input$nPlot_yes == "TRUE"){input$nPlot_cbo} else{NULL}
  })

  plot_cbo <- eventReactive(input$plot_cbo, {
    plot_power(data = beta_cbo(),
               n = nn(),
               m = input$mPlot_cbo,
               method = input$pickPlot_cbo)
  })

  ## scompvar ----
  CV_cbo <- eventReactive(input$cbo_cbo, {
    scompvar(data = simResults())
  })

  ## sim_cbo ----
  cbo_cbo <- eventReactive(input$cbo_cbo, {
    if(input$pickcbo == "budget"){
      sim_cbo(CV_cbo(), multSE = NULL,
              ct = input$ct_cbo,
              ck = input$ck_cbo,
              cj = input$cj_cbo)
    } else {
      sim_cbo(CV_cbo(), multSE = input$multSE_cbo,
              ct = NULL,
              ck = input$ck_cbo,
              cj = input$cj_cbo)
    }
  })

  # # outputs SSP ----
  # ## Summary output ----
  # output$summary <- renderDataTable({
  #
  #   sum.MSE() %>%
  #     mutate(mean = round(mean, 2), upper = round(upper, 2),
  #            lower = round(lower, 2), rel = round(rel, 2),
  #            der = round(der, 2))
  #
  # })
  #
  # output$download1 <- downloadHandler(
  #   filename = "summary_mySSP.csv",
  #   content = function(file) {
  #     write.csv(sum.MSE(), file)
  #   },
  #   contentType = "text/csv"
  # )
  #
  #
  # ## Quality output ----
  # output$quality <- render_gt({
  #   qua2 <- round(qua(), 2)
  #   colnames(qua2) <- c("S.mean", "S.sd", "D.mean", "D.sd", "MVDmin","MVDmax")
  #   qua2 %>%
  #     mutate("Data" = c("Pilot", "Simulated")) %>%
  #     select(c(7,1:6)) %>%
  #     gt () %>%
  #     tab_header(
  #       title = "Quality of simulation",
  #       subtitle = "Comparison of pilot data with simulated data") %>%
  #     tab_spanner(label = "Species", columns = matches("S.mean|S.sd")) %>%
  #     tab_spanner(label = "Simpson Diversity index", columns = matches("D.mean|D.sd")) %>%
  #     tab_spanner(label = "Total multivariate dispersion", columns = matches("MVDmin|MVDmax")) %>%
  #     cols_label(S.mean = "Mean", S.sd = "SD", "D.mean" = "Mean", "D.sd" = "SD", MVDmin = "Minima", MVDmax = "Maxima") %>%
  #     cols_align(align = "center") %>%
  #     tab_footnote(
  #       footnote = "Values per sample unit.",
  #       locations = cells_column_labels(columns = c(S.mean, S.sd, D.mean, D.sd))) %>%
  #     tab_footnote(
  #       footnote = "The range applies only to simulated data.",
  #       locations = cells_column_labels(columns = c(MVDmin, MVDmax)))
  #
  # })
  #
  # output$download2 <- downloadHandler(
  #   filename = "quality_mySSP.csv",
  #   content = function(file) {
  #     write.csv(qua(), file)
  #   },
  #   contentType = "text/csv"
  # )
  #
  # ## Plot output ----
  # output$plot <- renderPlot(plot())
  #
  # output$download3 <- downloadHandler(
  #   filename = function() {
  #     paste0("plot_mySSP.", input$var3)
  #   },
  #   content = function(file) {
  #     if(input$var3 == "png")
  #       png(file) # open the png device
  #     else
  #       pdf(file) # open the pdf device
  #     # plot(x=x(), y=y(), main = "iris dataset plot", xlab = xl(), ylab = yl()) # draw the plot
  #     print(plot()) # for GGPLOT
  #     dev.off()  # turn the device off
  #   }
  # )

  # outputs ecocbo ----
  # ## read data ecocbo ----
  # output$dataH0_Out <- renderTable(dataH0())
  # output$dataHa_Out <- renderTable(dataHa())
  #
  # ## assempar ecocbo ----
  # output$parH0_Out <- renderTable(par_cbo()$H0$par)
  # output$parHa_Out <- renderTable(par_cbo()$Ha$par)
  #
  # ## simdata ecocbo ----
  # output$simH0_Out <- renderTable(sim_cbo0()[[1]])
  # output$simHa_Out <- renderTable(sim_cboa()[[1]])

  ## prep_data ----
  output$prep_Out <- renderTable(simResults()$Results)

  ## sim_beta ----
  output$power_Out <- renderTable(pwr())

  ## plot_power ----
  output$plot_Out <- renderPlot(plot_cbo())

  ## scompvar ---
  output$CV_Out <- renderTable(CV_cbo())

  ## sim_cbo ----
  output$cbo_Out <- renderTable(cbo_cbo())

  }
