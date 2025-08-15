anova_one_way_Server <- function(id, source_data) {
  moduleServer(id, function(input, output, session) {

    observeEvent(input$info_button_one_way_anova, {
      show_alert(
        session = session,
        title   = "",
        text    = tags$div(
          tags$h3("Information",
                  style = "color: #0076C8; font-weight: bold; text-align: center"),
          tags$br(),
          tags$br(),
          style = "text-align: justify;
        margin-left:  auto;
        margin-right: auto;",
          tags$b('One-way ANOVA', style = "font-weight: bold"),
          ' is the statistical analysis to compare the variation of means of two
          or more populations. The application has the ability to analyze structured
          databases according to the download example, providing a box plot and
          hypothesis tests to apply the ANOVA. The information can be loaded in
          xlsx or csv format, specifying the reference sheet and selecting the
          column containing the response variable.',
          tags$br(), tags$br(),
          'Note: You can download the sample database to test the app.',
          tags$br(),
        ),
        html  = TRUE,
        width = "55%"
      )
    })

    graph_type <- eventReactive(input$analyze, {
      box_plot(
        .data      = source_data(),
        x          = input$name_axis_x,
        y          = input$name_axis_y,
        .n_col     = input$n_col,
        .order_by  = input$id_order_fun,
        .order     = input$id_order,
        title_name = input$title_name
      )
    })

    observeEvent(input$id_order_fun, {
      if (input$id_order_fun == 'Default') {
        var <- 'empty'
      } else {var <- 'order'}
      updateTabsetPanel(session, 'tabset_order', selected = var)
    })

    empty_message <- eventReactive(input$analyze, {
      validate(need(input$inferential_test, "Select at least one test"))
    })

    output$empty <- renderTable({
      req(input$tabset == "Statistical analysis")
      empty_message()
    })

    analysis_type <- eventReactive(input$analyze, {
      req(input$inferential_test)
      vec_test <- input$inferential_test
      list_tests <- list(
        Normality        = shapiro_test(source_data(), input$n_col),
        Homoscedasticity = bartlett_test(source_data(), input$n_col),
        ANOVA            = anova_test(source_data(), input$n_col),
        Tukey            = anova_test(source_data(), input$n_col)
      )
      return(list_tests[vec_test])
    })

    render_table <- function(var_input) {
      output[[var_input]] <- renderTable({
        req(input$tabset == "Statistical analysis")
        analysis_type()[[var_input]]
      }, rownames = TRUE, na = '')
    }

    observeEvent(analysis_type(), {
      map(names(analysis_type()), render_table)
    })

    output$plot <- renderPlotly({
      if(input$tabset == "Box plot")
        graph_type()
    })

  })
}

function(input, output, session) {
  source_data <- reactive({
    read_xlsx('www/experimentacion.xlsx')
  })
  anova_one_way_Server('id', source_data)
}
