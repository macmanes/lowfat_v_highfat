#########################bring_in_temps_pregnancy_rep1

bring_in_temps_pregnancy_rep1_old <- function(data_file)
{
  data <- paste(path,data_file,sep="")
  rawtemps <- read_csv(data, skip_empty_rows=TRUE,
                       col_types = cols(date = col_date(format = "%m/%d/%Y"),
                                        AntennaID = col_integer(), 
                                        body_temp = col_double(),
                                        time = col_time(format = "%H:%M:%S")), na = "")
  
  rawtemps <- rawtemps %>% 
    mutate(date, date = as.Date(date, format = "%Y-%m-%f", tz="EST")) %>%
    unite("DateTime", date:time, remove = FALSE, sep =  " ") %>%
    mutate(DateTime = as.POSIXlt(DateTime, tz="EST"))

  
  target <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)
  tempscages <- rawtemps %>% filter(AntennaID %in% target)
  
  return(rawtemps)
}

###############################################new function

bring_in_temps_pregnancy_rep1 <- function(data_file)
{
  data <- paste(path,data_file,sep="")
  rawtemps <- read_xlsx(data, 
                         sheet = "Downloaded Tag IDs",
                         col_types = c("text", "text", "numeric", "text", "numeric"))
  colnames(rawtemps) <- c("date", "time","AntennaID", "TagID", "body_temp")
  rawtemps$AntennaID <- as.integer(rawtemps$AntennaID)
  rawtemps$date <- as.Date(rawtemps$date, format = "%m/%d/%Y")

  rawtemps <- rawtemps %>% 
    mutate(date, date = as.Date(date, format = "%Y-%m-%f", tz="EST")) %>%
    unite("DateTime", date:time, remove = FALSE, sep =  " ") %>%
    mutate(DateTime = as.POSIXlt(DateTime, tz="EST"))
  
  return(rawtemps)
}

