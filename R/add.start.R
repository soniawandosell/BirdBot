#' Add a start command to the bot. Adding a start command is recommended.
#'
#' @param x A 'BirdBotSettings' object.
#' @param mss.welcome First message shown.
#' @param mss.commands Second message shown.
#' @return A 'BirdBotSettings' object.
#' @examples
#' Settings <- add.start(Settings, mss.welcome= "Welcome to the starlings data collection bot.", mss.commands="There are some commands you should learn:")
#'
#'@export
add.start <- function(x=Settings,
                      mss.welcome= "Welcome to the starlings data collection bot. \ud83d\ude0a",
                      mss.commands="There are some commands you should learn:"){
  start.f <- function(bot=x$Configuration$bot,update){
    for(i in 1:length(x$Interaction$Commands)){
      if(names(x$Interaction$Commands)[i]!="start"&
         names(x$Interaction$Commands)[i]!="help"&names(x$Interaction$Commands)[i]!="allow"){
        ANSWERS <- append(ANSWERS, paste0("\"/",names(x$Interaction$Commands)[i],
                                          "\", ",x$Interaction$Descriptions[i]))
      }
    }
    x$Configuration$bot$sendMessage(chat_id = update$message$chat$id, text = mss.welcome)
    x$Configuration$bot$sendMessage(chat_id = update$message$chat$id, text = mss.commands)
  }
  x$Interaction$Commands[["start"]]<- start.f
  x$Interaction$Descriptions[["start"]]<- "command to start the bot"

  help.f <- function(bot=x$Configuration$bot,update){
    ANSWERS <- list()
    for(i in 1:length(x$Interaction$Commands)){
      if(names(x$Interaction$Commands)[i]!="start"&
         names(x$Interaction$Commands)[i]!="help"&names(x$Interaction$Commands)[i]!="allow"){
        ANSWERS <- append(ANSWERS, paste0("\"/",names(x$Interaction$Commands)[i],
                                          "\", ",x$Interaction$Descriptions[i]))
      }
    }
    for(i in 1:length(ANSWERS)){
      x$Configuration$bot$sendMessage(chat_id = update$message$chat$id, text = ANSWERS[[i]])
    }
  }

  x$Interaction$Commands[["help"]]<- help.f
  x$Interaction$Descriptions[["help"]]<- "list of commands"

  x$Configuration$updater <- x$Configuration$updater +
    CommandHandler("help", x$Interaction$Commands[["help"]])
  x$Configuration$updater <- x$Configuration$updater +
    CommandHandler("start", x$Interaction$Commands[["start"]])
  return(x)
}
