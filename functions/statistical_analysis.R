shapiro <- function(d, n_col){
  d[[1]] %>%
    unique() %>%
    sapply(function(x)(d %>% filter(d[[1]] == x))[[n_col]] %>% shapiro.test())
}

bar <- function(d, n_col){
  bartlett.test(d[[n_col]] ~ d[[1]])
}

an <- function(d, n_col){
  print("ANOVA")
  aov(d[[n_col]] ~ d[[1]]) %>%
    summary()
}

analisis <- function(d, n_col){

  print(shapiro(d , n_col))
  print(bar(d , n_col))
  print(an(d , n_col))

}
