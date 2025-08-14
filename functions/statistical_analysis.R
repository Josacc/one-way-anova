shapiro_test <- function(data, n_col){
  st <- data[[1]] %>%
    unique() %>%
    sapply(function(x)(data %>% filter(data[[1]] == x))[[n_col]] %>% shapiro.test())

  st <- as.data.frame(st) %>% .[c(1, 2), ]
  return(st)
}

bartlett_test <- function(data, n_col){
  bt <- bartlett.test(data[[n_col]] ~ data[[1]])
  bt <- tibble(`Bartlett's K-squared` = bt$statistic, `p-value` = bt$p.value)
  return(bt)
}

anova_test <- function(data, x = 'Treatments',  n_col){
  at <- aov(data[[n_col]] ~ data[[1]], data = data)
  at <- summary(at)[[1]] %>%
    `row.names<-`(c(x, 'Residuals'))
  return(at)
}

ui <- fluidPage(
  tableOutput('table_anova'),
  tableOutput('table_bar'),
  tableOutput('table_shapiro')
)
server <- function(input, output, session) {
  output$table_anova <- renderTable({
    anova_test(d, n_col = 2)
  }, rownames = TRUE, na = "" )
  output$table_bar <- renderTable({
    bartlett_test(d, 2)
  })
  output$table_shapiro <- renderTable({
    shapiro_test(d, 2)
  }, rownames = TRUE, digits = 4, na = "" )
}

shinyApp(ui, server)
