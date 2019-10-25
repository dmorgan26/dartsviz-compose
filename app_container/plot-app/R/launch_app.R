#' Launch dartzviz Shiny App
#'
#' @return Shiny App Instance
#' @export
#' @import shiny dplyr ggplot2
#' @importFrom tidyr gather
#'
#' @examples launch_app()
launch_app <- function() {

  ui <- fluidPage(
    br(),
    h1("Dartsviz - Analysis of 2017 PDC Darts Data"),
    br(),

    tabsetPanel(
      tabPanel(
        title = "Darts To Complete Leg",

        plotOutput('plot_tab1'),

        fluidRow(

          column(3,
                 selectInput('p1_input_tab1', 'Player 1', choices = levels(as.factor(darts_to_win_leg_dataset$name)), width = "100%"),
                 selectInput('p2_input_tab1', 'Player 2', choices = levels(as.factor(darts_to_win_leg_dataset$name)), width = "100%", selected = "Adrian Lewis")
          ),

          column(7,
                 tableOutput('t_test_table'),
                 h6('At the 95% confidence level, is there a difference between the means of the number of darts that these two players require to win a leg?'),
                 h4(textOutput('significance'))
          )
        )
      ),

      tabPanel(
        title = "Double Conversion",

        plotOutput('plot_tab2'),

        fluidRow(

          column(3,
                 selectInput('p1_input_tab2', 'Player 1', choices = levels(as.factor(double_conversion_dataset$name)), width = "100%"),
                 selectInput('p2_input_tab2', 'Player 2', choices = levels(as.factor(double_conversion_dataset$name)), width = "100%", selected = "Adrian Lewis")
          )
        )
      ),
      tabPanel(
        title = "Pressure Effect",

          br(),
          textOutput('tab3_header'),
          br(),

        plotOutput('plot_tab3'),

        fluidRow(

          column(3,
                 selectInput('p1_input_tab3', 'Select Player', choices = levels(as.factor(pressure_effect_dataset$name)), width = "100%")

          ),

          column(7,
                  br(),
                   textOutput('chisq_null_hypothesis'),
                   br(),
                   textOutput('chisq_alternative_hypothesis'),
                   br(),
                   textOutput('chisq_p_value'),
                   br(),
                   textOutput('chisq_result')

          )
        )
      )


    )
  )


  server <- function(input,output) {

    ## write reactive functions that generate the datasets

    leg_completion_data <- reactive({subset(darts_to_win_leg_dataset, name == input$p1_input_tab1 | name == input$p2_input_tab1)})

    t_test_vec1 <- reactive({darts_to_win_leg_dataset$darts_thrown[darts_to_win_leg_dataset$name == input$p1_input_tab1]})
    t_test_vec2 <- reactive({darts_to_win_leg_dataset$darts_thrown[darts_to_win_leg_dataset$name == input$p2_input_tab1]})

    t_test_result <- reactive({t.test(darts_to_win_leg_dataset$darts_thrown[darts_to_win_leg_dataset$name == input$p1_input_tab1]
                                      , darts_to_win_leg_dataset$darts_thrown[darts_to_win_leg_dataset$name == input$p2_input_tab1])})


    sample_standard_deviations <- reactive({c(sd(darts_to_win_leg_dataset$darts_thrown[darts_to_win_leg_dataset$name == input$p1_input_tab1])
                                              , sd(darts_to_win_leg_dataset$darts_thrown[darts_to_win_leg_dataset$name == input$p2_input_tab1]))})


    sampling_distribution_of_sample_means <- reactive({

      pressure_no_sample <- vector()
      pressure_yes_sample <- vector()

      for (i in 1:1000) {

        pressure_no_sample[i] <- mean(base::sample(x = pressure_effect_dataset[pressure_effect_dataset$name == input$p1_input_tab3 & pressure_effect_dataset$is_pressure == 0, 3, drop=TRUE]
                                                   ,size = 50
                                                   ,replace = TRUE))

        pressure_yes_sample[i] <- mean(base::sample(x = pressure_effect_dataset[pressure_effect_dataset$name == input$p1_input_tab3 & pressure_effect_dataset$is_pressure == 1, 3, drop = TRUE]
                                                    ,size = 50
                                                    ,replace=TRUE))

      }

      long_pressure_df <- data.frame(no = pressure_no_sample, yes = pressure_yes_sample) %>%
        gather(key = pressure_applied, value = sampled_mean_conversion) %>%
        group_by(pressure_applied, sampled_mean_conversion) %>%
        tally()


      long_pressure_df
    })

    chisq_test_result <- reactive({

        chisq_df <- sampling_distribution_of_sample_means()[sampling_distribution_of_sample_means()$pressure_applied == "no",] %>%
                      inner_join(sampling_distribution_of_sample_means()[sampling_distribution_of_sample_means()$pressure_applied == "yes",]
                                                                                                  ,by = "sampled_mean_conversion")

          chisq_df2 <- chisq_df[,c(3,5)]

          suppressWarnings({chisq.test(chisq_df2)$p.value})

                              })


    output$plot_tab1 <- renderPlot({

      mean_darts_thrown_df <- leg_completion_data() %>%
        group_by(name) %>%
        summarise(mean_darts = mean(darts_thrown))

      ggplot(leg_completion_data(), aes(x = darts_thrown, fill = name)) +
        ggtitle(paste(input$p1_input_tab1, "vs. ", input$p2_input_tab1, " - Head to Head Mean Darts Thrown Probability Density Function")) +
        geom_density(alpha = 0.5)  +
        coord_cartesian(xlim = c(min(leg_completion_data()$darts_thrown),max(leg_completion_data()$darts_thrown))) +
        scale_x_continuous(breaks = seq(min(leg_completion_data()$darts_thrown),max(leg_completion_data()$darts_thrown))) +
        geom_vline(data = mean_darts_thrown_df, aes(xintercept = mean_darts,  colour = name),
                   linetype = "dashed", size = 1) +
        xlab("Darts Thrown") +
        ylab("Density")


    })

    output$t_test_table <- renderTable({ table_data <- data.frame(Player = c(input$p1_input_tab1, input$p2_input_tab1),
                                                                  `Mean Darts Thrown` = t_test_result()$estimate
                                                                  ,`Sample Std. Dev.` = sample_standard_deviations())

    table_data
    })

    output$significance <- renderText({ifelse(t_test_result()$p.value <= 0.05, "Yes","No")})

    output$plot_tab2 <- renderPlot({

      doubles_boxplot_data <- subset(double_conversion_dataset, name == input$p1_input_tab2 | name == input$p2_input_tab2)

      ggplot(doubles_boxplot_data, aes(x = name, y = conversion, fill = name)) +
        ggtitle(paste(input$p1_input_tab2, "vs. ", input$p2_input_tab2, " - Head to Head Double Conversion")) +
        geom_boxplot() +
        coord_flip() +
        ylab("Conversion") +
        xlab("Player") +
        guides(fill=guide_legend(title="Player")) +
        guides(fill = guide_legend(reverse = TRUE)) +
        scale_y_continuous(labels = function(x) paste0(x,"%"))


    })

    output$plot_tab3 <- renderPlot({

      game_state_means <-  sampling_distribution_of_sample_means() %>%
        group_by(pressure_applied) %>%
        summarise(mean_conversion = mean(sampled_mean_conversion))


      ggplot(sampling_distribution_of_sample_means(), aes(x = sampled_mean_conversion, y = n ,fill=pressure_applied)) +
        ggtitle(paste(input$p1_input_tab3, " - Sampling Distribution of Sample Means - Double Conversion by Game State")) +
        theme(plot.title = element_text(size = 18)) +
        geom_bar(stat = "identity", position = position_dodge(), alpha = 0.5)  +
        geom_vline(data = game_state_means, aes(xintercept = mean_conversion,  colour = pressure_applied),
                   linetype = "dashed", size = 1) +
        scale_x_continuous(labels = function(x) paste0(x*100,"%")) +
        xlab("Sampled Mean Conversion") +
        ylab("# of Samples")

    })

    output$tab3_header <- renderText({"Hypothesis - When a player has a shot to win a leg, their opponent being sat on a score that they are very likely to win with changes the throwing players conversion rate"})
    output$chisq_null_hypothesis <- renderText({paste0("H0 - Adding pressure has no effect on ", input$p1_input_tab3, "'s  double conversion")})
    output$chisq_alternative_hypothesis <- renderText({paste0("H1 - Adding pressure does effect ", input$p1_input_tab3, "'s double conversion")})
    output$chisq_p_value <- renderText({paste0("p-value of Pearson's Chi-Squared Test is ", round(chisq_test_result(),2))})
    output$chisq_result <- renderText({ifelse(chisq_test_result() <= 0.05, "Null is rejected", "Null is accepted")})

  }

  shinyApp(ui = ui, server = server)

}

#
# load_data_to_global_envir()
# create_darts_to_complete_leg_dataset()
# create_double_success_per_match_dataset()
# create_pressure_effect_dataset()
# launch_app()
