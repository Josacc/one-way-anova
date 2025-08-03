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

    analysis_type <- eventReactive(input$analyze, {
      switch(
        input$selection ,
        Normality        = shapiro(source_data(), input$n_col) ,
        Homoscedasticity = bar(source_data(), input$n_col) ,
        ANOVA            = an(source_data(), input$n_col) ,
        all              = analisis(source_data(), input$n_col)
      )
    })

    output$plot <- renderPlotly({
      if(input$tabset == "Box plot")
        graph_type()
    })

    output$t_anova <- renderPrint({
      if(input$tabset == "Statistical analysis")
        analysis_type()
    })

  })
}


function(input, output, session) {
  source_data <- reactive({
    read_xlsx('www/experimentacion.xlsx')
  })
  anova_one_way_Server('id', source_data)
}
