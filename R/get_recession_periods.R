# Set up recession bars
get_recession_periods <- function(data) {
  recession_periods <- data %>%
    filter(recession == 1) %>%
    group_by(group = cumsum(c(1, diff(as.numeric(date)) > 31))) %>%
    summarise(start = min(date), end = max(date)) %>%
    ungroup() %>%
    select(-group)

  return(recession_periods)
}
