#' Add a newuser registration command to the bot.
#'
#' @param x A 'BirdBotSettings' object.
#' @param command Command to type on telegram.
#' @param description A description for the command.
#' @param mss.requested "You have already requested for access."
#' @param mss.allowed "You are already allowed to start using the bot!"
#' @param mss.sent "Your request has been sent!"
#' @return A 'BirdBotSettings' object.
#' @examples
#' Settings <- add.legend(x=Settings, command="newuser", description= "Command to request for access.", mss.requested="You have already requested for access.", mss.allowed="You are already allowed to start using the bot!", mss.sent="Your request has been sent!")
#'
#'@export
add.newuser <- function(x=Settings,
                        command="newuser",
                        description= "Command to request for access.",
                        mss.requested="You have already requested for access.",
                        mss.allowed="You are already allowed to start using the bot!",
                        mss.sent="... \n Your request has been sent!"){
  newuser.f <- function(bot=x$Configuration$bot,update){
    id <- update$message$from$id
    name <- update$message$from$first_name
    if(id %in% x$Users$To_allow){
      bot$sendMessage(chat_id = id, text = mss.requested)
    }else{
      if(id %in% x$Users$Allowed){
        bot$sendMessage(chat_id = id,
                        text = mss.allowed)
      }else{
        if(length(x$Users$Admin)>0){
          for(i in 1:length(x$Users$Admin)){
            bot$sendMessage(chat_id = x$Users$Admin[i],
                            text = paste0(id," - ",name, " HAS REQUESTED ACCESS"))
          }
        }
        if(nrow(x$Users$Users)==0){
          new.row <- c(id, "admin",name,name,randomColor(1))
        }else{
          new.row <- c(id, "to.allow",name,name,randomColor(1))
        }
        x$Users$Users <- rbind(x$Users$Users, new.row)
        Settings.wb <- wb_load(x$Configuration$file)
        writeData(Settings.wb, sheet = "Users", x$Users$Users, colNames = T)
        wb_save(Settings.wb,x$Configuration$file ,overwrite = T)
        bot$sendMessage(chat_id = update$message$chat$id, text = mss.sent)
        x$Users$Allowed <- subset(x$Users$Users,
                                  tolower(permit) =="user"|tolower(permit) =="admin")$chat.id
        x$Users$Admin <- subset(x$Users$Users,
                                tolower(permit) =="admin")$chat.id
        x$Users$To_allow <- subset(x$Users$Users,
                                   tolower(permit) =="to.allow")$chat.id
      }
    }
  }
  x$Interaction$Commands[[command]]<- newuser.f
  x$Interaction$Descriptions[[command]]<- description

  allow.f <- function(bot=x$Configuration$bot,update, id){
    if (length(id > 0L)){
      if(update$message$from$username %in% x$Users$Admin){
        if(id%in%x$Users$To_allow){
          bot$sendMessage(chat_id = update$message$from$id,
                          text = paste0(id, " NOW HAS ACCESS"))
          x$Users$Users[which(x$Users$Users$chat.id==id),
                        "permit"] <- "user"
          Settings.wb <- wb_load(x$Configuration$file)
          writeData(Settings.wb, sheet = "Users", x$Users$Users, colNames = T)
          wb_save(Settings.wb,x$Configuration$file ,overwrite = T)
          x$Users$Allowed <- subset(x$Users$Users,
                                    tolower(permit) =="user"|tolower(permit) =="admin")$chat.id
          x$Users$Admin <- subset(x$Users$Users,
                                  tolower(permit) =="admin")$chat.id
          x$Users$To_allow <- subset(x$Users$Users,
                                     tolower(permit) =="to.allow")$chat.id
        }else{
          bot$sendMessage(chat_id = update$message$from$id,
                          text = paste0(id, " has not requested access"))
        }
      }
    }
  }
  x$Interaction$Commands[["allow"]]<- allow.f
  x$Interaction$Descriptions[["allow"]]<- "command to allow other users"

  x$Configuration$updater <- x$Configuration$updater +
    CommandHandler(command, x$Interaction$Commands[[command]])

  x$Configuration$updater <- x$Configuration$updater +
    CommandHandler("allow", x$Interaction$Commands[["allow"]])

  return(x)
}
