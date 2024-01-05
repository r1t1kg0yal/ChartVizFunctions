# Combo bet handler for return plots
combo_bet <- function(...){
  args <- list(...)
  n <- length(args) / 2

  # Split the arguments into variables and sizes
  variables <- args[1:n]
  sizes <- args[(n + 1):(2 * n)]

  # Create a list of bets
  bets <- lapply(1:n, function(i) list(var = variables[[i]], size = sizes[[i]]))

  return(bets)
}
