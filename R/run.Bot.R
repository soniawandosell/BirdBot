#' Function to run the Bot. Mantain the computer on while the bot should be runing.
#'
#' @param x A 'BirdBotSettings' object.
#' @examples
#' run.Bot(x=Settings)
#'
#'@export

run.Bot <- function(x=Settings){
  x$Configuration$updater$start_polling()
}
