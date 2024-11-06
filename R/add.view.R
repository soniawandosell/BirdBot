#' Add a view command to the bot.
#'
#' @param x A 'BirdBotSettings' object.
#' @param command Command to type on telegram. .
#' @param db.name Name of the database. Should be previously integred to Settings with the @backref set.database function.
#' @param description A description for the command.
#' @param show what to show on the table, could be 'c("Tasks","Status","Info")'.
#' @param firstday Number of days added or substracted to current date in which the table should start.
#' @param lastday Number of days added or substracted to current date in which the table should end.
#' @param caption Caption for the image.
#' @param mss.empty Message when there is no data.
#' @return A 'BirdBotSettings' object.
#' @examples
#' Settings <- add.view(Settings, command="viewweek", db.name ="database", description="view last week status of a nestbox", show="Status", firstday= -7, lastday= 0, caption="Last week status of nestbox ", mss.empty="No data on previous week for this nest.")
#'
#'@export
add.view <- function(x=Settings,
                     command="view",
                     db.name ="database",
                     description="view current days of a nestbox",
                     show=c("Tasks","Status","Info"),
                     firstday= -5,
                     lastday= 1,
                     caption="Last days of nestbox ",
                     mss.empty="No data on previous week for this nest."){
  if(!db.name%in%names(x$Databases$Data)){
    stop("Database should be one of the ones included in Settings.")
  }
  if(!is.numeric(today)){
    stop("Today should be a numeric value of days added (or substracted) to todays date.")
  }
  view.f <- function(bot=x$Configuration$bot,update,nest.number){
    if (length(nest.number > 0L)){
      if(nest.number %in% x$Locations$Nests$nests.id){
        if(update$message$from$id %in% x$Users$Allowed){
          date.time <- as.POSIXct(update$message$date, origin = "1970-01-01")
          dates.2.view <- seq(date.time+lubridate::days(firstday),
                              date.time+lubridate::days(lastday),1)%>%
            format(., "%Y-%m-%dd") %>% gsub(".$", "",. )
          date <- gsub(".$", "", format(date.time, "%Y-%m-%dd"))
          time <- format(date.time, "%H:%M")
          dir <- x$Databases$Directory[db.name]
          data <- read.csv(dir)%>%
            mutate(Date=as.Date(Date,format=x$Configuration$date.format))%>%as.data.frame()
          autor <- x$Users[which(x$Users$id==update$message$from$id),"name"]%>%as.character()
          data.2.view <- data%>%filter(,Nests == nest.number,
                                       as.character(Date) %in% c(dates.2.view))%>%
            arrange(Date)%>%mutate(Date = format(Date, "%d %b"))

          if(nrow(data.2.view)>0){
            img.name <- paste0(nest.number,".",command,".png")
            data.2.view[is.na(data.2.view)] <- ""
            data.2.plot <- as.matrix(select(data.2.view,Date, all_of(show)))%>%
              t()%>%as.data.frame()%>%tableGrob(., rows = rownames(.), cols = NULL)
            data.2.plot$grobs[data.2.plot$layout$name == "rowhead-bg"] <- lapply(
              data.2.plot$grobs[data.2.plot$layout$name == "rowhead-bg"],
              function(g) {g$gp <- gpar(fill = "gray90", col="white"); g})
            df_plot <- ggplot() +theme_minimal() + annotation_custom(data.2.plot) +
              theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))
            ggsave(img.name, plot = df_plot, width = 6, height = 4, dpi = 300)
            bot$sendPhoto(chat_id = update$message$chat$id, photo = img.name,
                          caption = paste0(caption, nest.number))
            file.remove(img.name)
          }else{
            bot$sendMessage(chat_id = update$message$chat$id, text = mss.empty)

          }
        }else{
          bot$sendMessage(chat_id = update$message$chat$id,
                          text = x$Messages$mss.deny)
          bot$sendMessage(chat_id = update$message$chat$id,
                          text = x$Messages$mss.request)
        }
      }else{
        bot$sendMessage(chat_id = update$message$chat$id, text = x$Messages$mss.wrong.nest)
      }
    }else{
      bot$sendMessage(chat_id = update$message$chat$id,
                      text =  x$Messages$mss.no.nest)
    }
  }
  x$Commands[[command]]<- view.f
  x$Commands.description[[command]]<- description

  x$Configuration$updater <- x$Configuration$updater +
    CommandHandler(command, x$Interaction$Commands[[command]], pass_args = TRUE)
  return(x)
}
