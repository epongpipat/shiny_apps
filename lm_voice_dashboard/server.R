shinyServer(function(input, output) {
  
  # app section ===========================================================================
  
  # load carData
  dataset <- carData::Salaries

  # rename variables so that they are human readable
  colnames(dataset) <- c("rank", "discipline", "years since phd", "years of service", "sex", "salary")

  # list of variables in Salaries Dataset
  variables_list <- colnames(dataset)

  # list of contrast coding schemes
  contrasts_list <- c("dummy", "deviant", "effects")
  
  # data wrangling
  dataset <- dataset %>%
    mutate(discipline = ifelse(discipline == "A", "theoretical", "applied")) %>%
    mutate(
      discipline = as.factor(discipline),
      rank = as.factor(rank),
      sex = as.factor(sex)
    )

  # dataset table summary
  output$dataset_variables_table <- renderTable({
    as.data.frame(rbind(
      c("Variable" = "rank", "Variable Type" = "Categorical", "Description" = "Professor's rank of either assistant professor, associate professor, or professor"),
      c("Variable" = "discipline", "Variable Type" = "Categorical", "Description" = "Type of department the professor works in, either applied or theoretical"),
      c("Variable" = "yrs.since.phd", "Variable Type" = "Continuous", "Description" = "Number of years since the professor has obtained their PhD"),
      c("Variable" = "yrs.service", "Variable Type" = "Continuous", "Description" = "Number of years the professor has served the department and/or university"),
      c("Variable" = "sex", "Variable Type" = "Categorical", "Description" = "Professor's sex of either male or female"),
      c("Variable" = "salary", "Variable Type" = "Continuous", "Description" = "Professor's nine-month salary (USD)")
    ))
  })

  # variable manipulation
  voice_variables <- reactive({
    DV <<- tolower(input$DV)
    IV <<- tolower(input$IV)
    contrast <<- input$contrast

    if (is.factor(dataset[[IV]]) && contrast %in% c("deviant", "effects")) {
      contrasts(dataset[[IV]]) <<- contr.treatment(length(levels(dataset[[IV]])))
    } else if (is.factor(dataset[[IV]])) {
      contrasts(dataset[[IV]]) <<- contr.sum(length(levels(dataset[[IV]])))
    }
  })

  # error
  voice_error <- reactive({
    voice_variables()
    if (!(DV %in% variables_list) || !(IV %in% variables_list)) {
      error <<- T
      error_message <<- paste(DV, "is not valid. Must be from the list of varaibles.")
    } else if (!(IV %in% variables_list)) {
      error <<- T
      error_message <<- paste(IV, "is not valid. Must be from the list of varaibles.")
    } else if (is.factor(dataset[[DV]])) {
      error <<- T
      error_message <<- paste(DV, "is not valid. Try using a continuous rather than categorical variable.")
    } else if (is.factor(dataset[[IV]]) && !(contrast %in% contrasts_list)) {
      error <<- T
      error_message <<- paste(contrast, "is not valid. Must be from the list of contrasts.")
    } else {
      error <<- F
    }
  })

  # dv_value_box
  output$DV <- renderValueBox({
    voice_variables()
    if (!(DV %in% variables_list)) {
      valueBox(
        paste(DV, "is not a valid option. Must be from the list of variables."),
        value = "DV", icon = icon("exclamation-circle"),
        color = "red"
      )
    } else if (is.factor(dataset[[DV]])) {
      valueBox(
        paste(DV, "is not a valid option. Try a continuous variable."),
        value = "DV", icon = icon("exclamation-circle"),
        color = "red"
      )
    } else {
      valueBox(
        DV,
        value = "DV", icon = icon("check-circle"),
        color = "light-blue"
      )
    }
  })
  
  # iv_value_box
  output$IV <- renderValueBox({
    voice_variables()
    if (IV %in% variables_list) {
      valueBox(
        IV,
        value = "IV", icon = icon("check-circle"),
        color = "light-blue"
      )
    } else {
      valueBox(
        paste(IV, "is not a valid option. Must be from the list of variables."),
        value = "IV", icon = icon("exclamation-circle"),
        color = "red"
      )
    }
  })
  
  # iv_categorical_coding_scheme_value_box
  output$IV_coding_scheme <- renderValueBox({
    voice_variables()
    if (!(IV %in% variables_list) || !is.factor(dataset[[IV]])) {
      valueBox(
        "N/A",
        value = "Contrast",
        color = "teal"
      )
    } else if (!(contrast %in% contrasts_list)) {
      valueBox(
        paste(contrast, "is not valid. Try again."),
        value = "Contrast", icon = icon("exclamation-circle"),
        color = "red"
      )
    } else {
      if (contrast %in% c("deviant", "effects")) {
        contrast <- "deviant (effects)"
      }
      valueBox(
        contrast,
        value = "Contrast", icon = icon("check-circle"),
        color = "light-blue"
      )
    }
  })
  
  # analysis
  voice_lm <- reactive({
    voice_error()
    if (error == F) {
      model_formula <<- eval(as.formula(paste0("`", DV, "` ~ `", IV, "`")))
      model <<- lm(model_formula, dataset)
    }
  })

  # anova source table
  output$anova_source_table <- renderTable({
    voice_lm()
    if (error == F) {
      anova_source_table <- as.data.frame(Anova(model, type = 3))
      anova_source_table <- cbind(Term = rownames(anova_source_table), anova_source_table)
      print(anova_source_table)
    }
  })

  # coefficients_summary table
  output$summary_table <- renderTable({
    voice_lm()
    if (error == F) {
      coefficients_table <- tidy(model)
      colnames(coefficients_table) <- c("Term", "Estimate", "Std. Error", "t value", "Pr(>|t|)")
      print(coefficients_table)
    }
  })

  # figure
  voice_figure <- reactive({
    voice_error()
    if (error == F) {

      # categorical IV
      if (is.factor(dataset[[IV]])) {

        # descriptive stats/build 95% CI
        datasetSummary <- dataset %>%
          group_by(IV = eval(as.name(IV))) %>%
          summarize(
            mean = mean(eval(as.name(DV))),
            sd = sd(eval(as.name(DV))),
            n = n(),
            df = n - 1,
            tcrit = abs(qt(0.05 / 2, df)),
            SEM = sd / sqrt(n),
            ME = tcrit * SEM
          )

        # dot plot with 95% CI
        plot <- ggplot(datasetSummary, aes(x = IV, y = mean, group = 1)) +
          geom_pointrange(aes(ymin = mean - ME, ymax = mean + ME)) +
          expand_limits(x = 0, y = 0) +
          theme_classic() +
          labs(x = IV, y = DV, caption = "Note: Bars represent 95% CI")
      } else {

        # continuous IV
        plot <- ggplot(mapping = aes(x = dataset[[IV]], y = dataset[[DV]])) +
          geom_point() +
          geom_smooth(method = "lm", color = "black") +
          theme_classic() +
          labs(x = IV, y = DV, caption = "Note: Gray band represents the 95% CI")
      }

      # modify scale if money (dollar) or continuous (comma)
      if (dataset[[DV]] == "salary") {
        plot <- plot + scale_y_continuous(labels = scales::dollar)
      } else if (!(is.factor(dataset[[DV]]))) {
        plot <- plot + scale_y_continuous(labels = scales::comma)
      }

      if (dataset[[IV]] == "salary") {
        plot <- plot + scale_x_continuous(labels = scales::dollar)
      } else if (!(is.factor(dataset[[IV]]))) {
        plot <- plot + scale_x_continuous(labels = scales::comma)
      }

      print(plot)
    }
  })

  # plot
  output$visualization <- renderPlot({
    voice_figure()
  })
  
  # code ==================================================================================
  output$ui_file <- renderText({
    read_file("ui.R")
  })
  
  output$server_file <- renderText({
    read_file("server.R")
  })
  
  output$javascript_file <- renderText({
    read_file("init.js")
  })
  
})
