#' Add a download command to the bot.
#'
#' @param x A 'BirdBotSettings' object.
#' @param command Command to type on telegram.
#' @param description A description for the command.
#' @param db.name Name of the database.
#' @return A 'BirdBotSettings' object.
#' @examples
#' Settings <- add.start.f(Settings, command="download", description="download the database.csv file", db.name ="database")
#'
#'@export
add.download <- function(x=Settings,
                         command="download",
                         description="download the database.csv file",
                         db.name ="database"){
  download.f <- function(bot=x$Configuration$bot,update){
    if (length(update$message$text > 0L)){
      if(update$message$from$id %in% x$Users$Allowed){

        bot$sendDocument(chat_id = update$message$chat$id,
                         document = x$Databases$Directory[db.name],
                         filename = x$Databases$Directory[db.name])

      }else{
        bot$sendMessage(chat_id = update$message$chat$id,
                        text = x$Messages$mss.deny)
        bot$sendMessage(chat_id = update$message$chat$id,
                        text = x$Messages$mss.request)
      }

    }
  }

  x$Interaction$Commands[[command]]<- download.f
  x$Interaction$Descriptions[[command]]<- description

  x$Configuration$updater <- x$Configuration$updater +
    CommandHandler(command, x$Interaction$Commands[[command]])
  return(x)
}
