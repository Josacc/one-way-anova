box_plot <- function(.data, .n_col, x = "", y = "", .order_by = 'Default', .order = FALSE, title_name = NULL) {

  if (str_length(x) == 0) {x <- 'Treatments'}
  if (str_length(y) == 0) {y <- 'Variable'}

  names(.data)[1] <- x
  names(.data)[.n_col] <- y
  vars <- c(x, y)

  .data <- .data %>% select(all_of(vars))

  if (!identical(.order_by, 'Default')) {
    .order_by <- str_to_lower(.order_by)
    order_fun <- match.fun(.order_by)
    .data <- .data %>%
      mutate("{x}" := fct_reorder(.data[[x]], .data[[y]], .fun = order_fun, .na_rm = TRUE, .desc = .order))
  }

  gg <- ggplot(.data, aes(x = .data[[x]], y = .data[[y]])) +
    geom_boxplot(aes(color = .data[[x]]), na.rm = T) +
    ggtitle(title_name %||% "") +
    labs(x = NULL, y = y) +
    theme_minimal()

  gg <- gg %>%
    ggplotly() %>%
    layout(legend = list(orientation = 'h', y = -0.1, title = list(text = paste0('<b>', x, '</b>'))))

  return(gg)
}
