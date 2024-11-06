#' Add a map command to the bot.
#'
#' @param x A 'BirdBotSettings' object.
#' @param command Command to type on telegram..
#' @param db.name Name of the database. Should be previously integred to Settings with the @backref set.database function.
#' @param description A description for the command.
#' @param type Define the type of map. Could be one of the followings: "Tasks", "Status", "User", or the name from a column of the database.
#' @param team Filter map content by a team. Culd be NA or a team from the Settings file.
#' @param today Number of days added or substracted to current date.
#' @param title Title for the map, if NA, there is no title."today" or "tomorrow" will be substituted by real date.
#' @param completed Could be "done", to show just done stuff, "to-do" or "all".
#' @param caption Caption for the map.
#' @param mss.empty Message for when there is no content to show on the map.
#' @return A 'BirdBotSettings' object.
#' @examples
#' Settings <- add.map(Settings, command="donetoday", db.name ="database", description="map of tasks done today", type="User", team=NA, today= 0, title=NA, completed="done", caption="Tasks done today", mss.empty="No tasks done today")
#'
#' Settings <- add.map(Settings, command="tomorrowmap", db.name ="database", description="map of tasks to do tomorrow", type="Tasks", team=NA, today= 1, title="Tasks today", completed="to-do", caption="Tasks to do tomorrow", mss.empty="No tasks for tomorrow")
#'
#' Settings <- add.map(Settings, command="statusmap", db.name ="database", description="map of nestboxes status yesterday", type="Status", team=NA, today= -1, title=NA, completed="all", caption="Status of the nestboxes yesterday", mss.empty="No status registered")
#'
#'@export
add.map<- function(x=Settings,
                   command="tasksmap",
                   db.name ="database",
                   description="map of tasks done and to do today",
                   type="User",
                   team=NA,
                   today= 0,
                   title="Tasks today",
                   completed="done",
                   caption="Tasks to do and done today",
                   mss.empty="No tasks for today"){
  if(!db.name%in%names(x$Databases$Data)){
    stop("Database should be one of the ones included in Settings.")
  }
  if(!is.numeric(today)){
    stop("Today should be a numeric value of days added (or substracted) to todays date.")
  }

  map.f <- function(bot= x$Configuration$bot, update){
    if(length(update$message$text)> 0L){
      if(update$message$from$id %in% x$Users$Allowed){

        date.time <- as.POSIXct(update$message$date, origin = "1970-01-01") + lubridate::days(today)
        date <- gsub(".$", "", format(date.time, "%Y-%m-%dd"))
        time <- format(date.time, "%H:%M")
        dir <- x$Databases$Directory[db.name]
        data <- read.csv(dir)%>%
          mutate(Date=as.Date(Date,format=x$Configuration$date.format))%>%as.data.frame()
        autor <- x$Users[which(x$Users$id==update$message$from$id),"name"]%>%as.character()
        title <- sub("today|tomorrow",(as.Date(date)%>% format("%d %b")),title)

        mapdata_to_show <- data%>%select(Nests,Date,all_of(type),Info,Hour)%>%filter(Date==date)%>%
          rename(Column=all_of(type))
        if(completed=="done"){
          mapdata_to_show <- mapdata_to_show%>%filter(!is.na(Info)&!is.na(Column))
        }
        if(completed=="to-do"){
          mapdata_to_show <- mapdata_to_show%>%filter(is.na(Info)&!is.na(Column))
        }
        if(completed=="all"){
          mapdata_to_show <- mapdata_to_show%>%filter(!is.na(Column))
        }
        if(!completed%in%c("done","to-do","all")){
          stop("Parameter completed should be one of done, to-do or all.")
        }
        if(!is.na(team)){
          teams <- unique(x$Content$Tasks.df$type)[!unique(x$Content$Tasks.df$type)%in%c("Task","Status")]
          if(team%in%teams){
            team.tasks <- (x$Content$Tasks.df%>%filter(type==team))[,"name"]%>%unique()
            mapdata_to_show <- mapdata_to_show%>%filter(Task%in%team.tasks)
          }else{
              stop("Parameter team should be one of the included on Tasks dataframe on Settings excel file.")
            }
        }
        mapdata <- x$Locations$Nests

        if(nrow(mapdata_to_show)>0){
          r.color <- randomColor((length(unique(mapdata_to_show$Column))))
          for (i in 1:nrow(mapdata_to_show)){
            coord_caja<-filter(mapdata,mapdata$nests.id==mapdata_to_show$Nests[i])
            mapdata_to_show$x[i]<-coord_caja$x
            mapdata_to_show$y[i]<-coord_caja$y
            if(type%in%c("Status","Tasks")){
              mapdata_to_show$color[i]<- x$Content$Tasks.df$color[which(x$Content$Tasks.df$name==mapdata_to_show$Column[i])]
            }else{
              if(type=="User"){
                mapdata_to_show$color[i]<- x$Users$Users$color[which(x$Users$Users$name==mapdata_to_show$Column[i])]
              }else{
                mapdata_to_show$color[i]<- r.color[which(unique(mapdata_to_show$Column)==mapdata_to_show$Column[i])]
              }
            }
          }

          segments.df <- x$Locations$Segments
          labels.df <- x$Locations$Labels

          m <- ggplot() + geom_point(data = mapdata_to_show[mapdata_to_show$Column != '', ],
                                     aes(x, y, col = Column), shape = 21, size = 5, stroke = 1.5) +
            scale_color_manual(values = setNames(mapdata_to_show$color, mapdata_to_show$Column)) +
            geom_text(data = mapdata, aes(x, y, label = nests.id), size = 2.5)+
            labs(color = type)
          if(type=="User"){
           mx.hour <- mapdata_to_show %>% group_by(Column) %>% filter(Hour == max(Hour)) %>% ungroup()
            m <- m + geom_point(data = mx.hour[mx.hour$Column != '', ],
                                aes(x, y, col = Column), shape = 21, size = 5,fill= mx.hour$color, alpha = .25)
          }
          n <- m + geom_segment(data= segments.df,aes(x = x1, y = y1, xend = x2, yend = y2),
                                color=segments.df$color,linewidth=segments.df$size)
          p <- n + theme(legend.position = c(0.9,0.85))+
            annotate("text", x=labels.df$x, y=labels.df$y, label = labels.df$label,
                     color=labels.df$color, size=labels.df$size)+
            theme(axis.title.x=element_blank(),axis.text.x=element_blank(),
                  axis.ticks.x=element_blank())+
            theme(axis.title.y=element_blank(),axis.text.y=element_blank(),
                  axis.ticks.y=element_blank())+
            theme(panel.background = element_rect(fill = "grey95",
                                                  colour = "grey95",
                                                  linewidth = 0.5, linetype = "solid"),
                  panel.grid.major = element_line(linewidth = 0, linetype = 'solid',
                                                  colour = "grey95"),
                  panel.grid.minor = element_line(linewidth = 0, linetype = 'solid',
                                                  colour = "grey95"))
          if(!is.na(title)){
            p <- p + ggtitle(title)
          }
          ggsave(paste0("./data/",command,".png"),units="in", width = 10, height = 6)

          bot$sendPhoto(chat_id = update$message$chat$id, photo = paste0(command,".png"),
                        caption = caption)
          file.remove(paste0(command,".png"))
        }else{
          bot$sendMessage(chat_id = update$message$chat$id,
                          text = mss.empty)
          #bot$sendPhoto(chat_id = update$message$chat$id, photo = "empty.map.png",
           #             caption = "Empty map")
        }
      }else{
        bot$sendMessage(chat_id = update$message$chat$id,
                        text = x$Messages$mss.deny)
        bot$sendMessage(chat_id = update$message$chat$id,
                        text = x$Messages$mss.request)
      }
    }
  }
  x$Commands[[command]]<- map.f
  x$Commands.description[[command]]<- description

  x$Configuration$updater <- x$Configuration$updater +
    CommandHandler(command, x$Interaction$Commands[[command]], pass_args = TRUE)
  return(x)
}
