shapiro_test <- function(data, n_col){
  st <- data[[1]] %>%
    unique() %>%
    sapply(function(x)(data %>% filter(data[[1]] == x))[[n_col]] %>% shapiro.test())
  st <- as.data.frame(st) %>% .[c(1, 2), ]
  return(st)
}

bartlett_test <- function(data, n_col, x = 'Treatments'){
  bt <- bartlett.test(data[[n_col]] ~ data[[1]])
  bt <- as.data.frame(list(bt$statistic, bt$parameter, bt$p.value), row.names =  x) %>%
    `names<-`(c("Bartlett's K-squared", 'Df', 'p.value'))
  return(bt)
}

anova_test <- function(data, n_col, x = 'Treatments'){
  at <- aov(data[[n_col]] ~ data[[1]], data = data)
  at <- summary(at)[[1]] %>%
    `row.names<-`(c(x, 'Residuals'))
  return(at)
}
