#' Add a legend command to the bot.
#'
#' @param x A 'BirdBotSettings' object.
#' @param command Command to type on telegram.
#' @param description A description for the command.
#' @param mss Message shown on the chat.
#' @param team Filter the team from the tasks shown.
#' @return A 'BirdBotSettings' object.
#' @examples
#' Settings <- add.legend(Settings, command="legend", description="get the list of tasks", mss= "The tasks descriptions are:", team="none")
#'
#'@export
add.legend <- function(x=Settings,
                       command="legend",
                       description="get the list of tasks",
                       mss= "The tasks descriptions are:",
                       team="none"){
  legend.f <- function(bot=x$Configuration$bot,update){
    if (length(update$message$text > 0L)){
        if(update$message$from$id %in% x$Users$Allowed){
          x$Content$Tasks.df <- read_excel(x$Configuration$file,sheet = "Tasks")
          if(team=="none"){
            legend <- x$Content$Tasks.df
          }else{
            if(team%in%x$Content$Tasks.df$team){
              legend <- subset(x$Content$Tasks.df,team==team)
            }else{
              legend <- x$Content$Tasks.df
            }
          }
          answer <- character()
          for(r in 1:(nrow(legend))){
            descriptions <- ifelse(is.na(legend[r,"description"]), "",
                                   paste0(": ",legend[r,"description"]))
            answer <-  paste0(answer, legend[r,"name"],descriptions," \n")
          }
          x$Configuration$bot$sendMessage(chat_id = update$message$chat$id, text = mss)
          x$Configuration$bot$sendMessage(chat_id = update$message$chat$id, text = answer)
        }else{
          bot$sendMessage(chat_id = update$message$chat$id,
                          text = x$Messages$mss.deny)
          bot$sendMessage(chat_id = update$message$chat$id,
                          text = x$Messages$mss.request)
        }

    }
  }

  x$Interaction$Commands[[command]]<- legend.f
  x$Interaction$Descriptions[[command]]<- description

  x$Configuration$updater <- x$Configuration$updater +
    CommandHandler(command, x$Interaction$Commands[[command]])
  return(x)
}
