#' ######## Configuracion inicial: 0 ########

#' @details setting opciones generales .Rprofile
suppressMessages({
  rm(list=ls());
  if(!is.null(
    dev.list())
  ) dev.off(); 
  graphics.off();
  options(stringsAsFactors = F, 
          "scipen" = 100, 
          "digits" = 4, 
          warn = -1)
  
  #' @details load\install packages
  pkgload <- function(packages){
    suppressWarnings({
      installed_packages <- packages %in% rownames(utils::installed.packages())
      if (any(installed_packages == FALSE)) {
        utils::install.packages(packages[!installed_packages])
      };
      invisible((lapply(packages, library, character.only = TRUE)))
    })
  }
  packages <- c('purrr',"dplyr","tidyr",
                "readr", "readxl", "data.table", 
                "lubridate","stringr", "stringi", 
                "magrittr", "fs")
  pkgload(packages)
  
  #' @details equivalente a `paste0(x,y)`
  `%+%` <- function(x,y) return(paste0(x,y))
  
  #' @details equivalente a negacion de `%in%`
  `%notin%` <- Negate(`%in%`)
  
})
cat("\014\n")

#' @details Definir Path
re <- function(x) {
  path_ <- './data/'
  re1 <- "^(./data/Datos_Pensiones/)" %+% 
    "(PB|PT|PvT|PvB)\\d{0,1}.*\\.csv$"
  re2 <- "^(./data/Logs/\\d{2}-\\d{2})/" %+% 
    "Participant \\d{1,2}_Participant \\d{1,2}_Analysis \\d{1}_video_\\d{8}_\\d{6}" %+% 
    "_state\\.(txt)$" 
  regex_path <- c(re1,re2)
  return(regex_path[x])
}
path_webA <- "/Users/mauro/Desktop/Cess/Pensiones/2023/emov-main/data/Datos_Pensiones/Pension+Web+A+-Lab_January+23,+2023_09.16.xlsx"
path_webB <- "/Users/mauro/Desktop/Cess/Pensiones/2023/emov-main/data/Pension Web B Privada (with Effort)_January 23, 2023_13.21.csv"
path_pension <- fs::dir_ls(path = './data/', regexp = re(1), ignore.case = TRUE, recurse = TRUE, type = "file")
path_video <- fs::dir_ls(path = './data/', regexp = re(2), ignore.case = TRUE, recurse = TRUE, type = "file")


## versión anterior ##
format_output <- function(data){
  data <- data[grepl('LAB',data$UID),]
  data['PC'] <- as.integer(stringr::str_sub(data$UID, -3, -2))
  unique_UID <- sdiff(data$UID[duplicated(data$UID)], unique(data$UID))
  data <- filter(data, UID %in% unique_UID)
  
  rm_cols <- c('Status', 
               'IPAddress', 'Progress', 'seconds)', 
               'Finished', 'ResponseId', 'RecipientLastName', 
               'RecipientFirstName', 'RecipientEmail', 
               'ExternalReference', 'LocationLatitude', 
               'LocationLongitude', 'DistributionChannel',
               'Duration (in seconds)', 'UserLanguage', 
               'Q526_Browser', 'Q526_Version', 
               'Q526_Operating System', 'Q526_Resolution', 
               'survey', 'QRead','show-up','useridn',
               'sesion','source', 'FL_210_DO', 'FL_229_DO', 
               'FL_292_DO', 'FL_307_DO',
               'HSist_9_TEXT', 'Q493', 'Longevidad',
               'name1','name2','name3','name4','Pn1','pn1',
               'name5','name6','name7','name8',
               'verb1','verb2','verb3','noun1',
               'dueño','contratado','un','herido','tetra',
               'lo','solo','ciego','mismo','declarado',
               'el','hn1','noun','dueña','contratada',
               'una','herida','la','sola','ciega',
               'misma','declarada','ella',
               'Tram_jubilacion','EpistemicC_2', 
               'PlanJubi_6_TEXT','Consent_visual')
  drop_ <- names(data) %in% rm_cols
  data <- data[!drop_]
  
  events <- c('ev1i','ev1f','ev2i','ev2f','ev3i','ev3f')
  keep_ <- c("UID","time",'PC',events)
  
  keep <-names(data) #TMP
  names_ <- unique(c(keep_, keep))
  data <- data[names(data) %in% names_]
  data <- as.data.frame(data)
  return(data)
}
## Fin versión anterior ##


#' ######## Obtener Pares/Natch 1 ########
#' 
#' @details load_webA y load_webB Carga la base de datos WEB-A/WEB-B solo 
#' con variables de tiempo objetivas y UID. load_video 
#' hace lo mismo para los de video. El objetivo es 
#' obtener los pares/match de UID video\web.
#' GET_LINES y VIDEO_UID, son fuciones auxiliares
#' para la carga de datos de video.
#' GET_PAIR obtiene los pares entre las bases de datos 
#' load_webA & load_video en base a la coincidencia 
#' de Hora y PC, filtrando por diferencias de tiempo 
#' y casos raros listados en path_drop para revisar
#' 

load_webA <- function(){  
  suppressMessages({
    vars_ <- colnames(readxl::read_excel(path_webA))
    data <- readxl::read_excel(path_webA,skip=1, na = c("", "NA", "<NA>"))
    colnames(data) <- vars_; vars_ <- colnames(data)
  })

  data <- as.data.frame(data)
  data <- data[!is.na(data$UID), ]
  data <- data[grepl("SCESS-LAB", data$UID), ]
  data$PC <- as.integer(stringr::str_sub(data$UID, -3, -2))
  data$Date <- stringr::str_sub(data$RecordedDate, 1, 10)
  data$Time <- stringr::str_sub(data$time_consent, 1, 8)
  data$DateTime <- data$Date %+% ' ' %+% data$Time
  data$Flag <- data$Date %+% '-' %+% data$PC
  data$time_cotizacion <- stringr::str_sub(data$time_cotizacion, 1, 8)
  data$time_consent <- stringr::str_sub(data$time_consent, 1, 8)
  data <- data %>% select(UID, Date, Time, PC, Flag, DateTime, time_cotizacion, time_consent) %>% arrange(DateTime)
  data <- as.data.frame(data)
  return(data)
}

#' ######## Data WEB-A, PENSION, WEB-B  load: 2 ########

READ_CSV <- function(filename){
  suppressMessages({
    data <- readr::read_csv(filename, col_types = readr::cols(.default = "c"))
    data <-  data[-c(1, 2), ]
    data <- as.data.frame(data)
  })
  return(data)
}

load_webB <- function(){
  data <- purrr::map_df(path_webB, READ_CSV)
  colnames(data)[colnames(data) == "Duration (in seconds)"] ="Duration"
  data <- data[!is.na(data$UID), ]
  data <- data[grepl("SCESS-LAB", data$UID), ]
  data <- filter(data, UID %in% names(table(data$UID)[table(data$UID)==1]))
  data <- as.data.frame(data)
  data$time_ingreso2 <- stringr::str_sub(data$time_ingreso2, 1, 8)
  data$EndDate <- as.POSIXct(data$EndDate, format = "%Y-%m-%d %H:%M:%S")
  data$salida <- data$EndDate + lubridate::hours(4)
  data$salida <- format(data$salida, "%H:%M:%S")
  keep_cols <- c('UID','time_ingreso2', 'salida')
  drop_ <- names(data) %in% keep_cols
  data <- data[drop_]
  data <- as.data.frame(data)
  return(data)
}

#' @details Load data WIX
#' 
load_pension <- function(){
  suppressMessages({
    data <- path_pension %>% purrr::map_df(
      ~readr::read_csv(., col_types = readr::cols(.default = "c")))
  })
  data <- as.data.frame(data)
  colnames(data)[colnames(data) == "userid"] ="UID"
  data <- data[!is.na(data$UID), ]
  data <- data[grepl("SCESS-LAB", data$UID), ]
  data <- filter(data, UID %in% names(table(data$UID)[table(data$UID)==1]));
  
  data <- data %>% tidyr::unite(inicio_wix, matches("^inicio_wix"), na.rm = TRUE, remove = TRUE)
  data <- data %>% tidyr::unite(yn_contestar, matches("^(y|n)_contestar"), na.rm = TRUE, remove = TRUE, sep =' ; ')
  data$inicio_wix <- stringr::str_sub(data$inicio_wix, 1, 19)
  data$yn_contestar <- stringr::str_sub(data$yn_contestar, 1, 19)
  data$inicio_wix <- stringr::str_sub(data$inicio_wix, -8, -1)
  data$yn_contestar <- stringr::str_sub(data$yn_contestar, -8, -1)
  
  data[data == ""]<-NA
  data <- data[!is.na(data$inicio_wix), ]
  data <- data[!is.na(data$yn_contestar), ]
  keep_cols <- c('UID', 'inicio_wix', 'yn_contestar')
  drop_ <- names(data) %in% keep_cols
  data <- data[drop_]
  data <- as.data.frame(data)
  return(data)
}

############ versión anterior ############
read_data <- function(fname){
  options(digits.secs=3)
  fname_uid <- fname
  fname_data <- sub("_state","_detailed", fname)
  
  data <- as.data.frame(data.table::fread(fname_data, sep='\t', skip=10))
  colnames(data)[colnames(data) == "Video Time"] ="VideoTime"
  data$HMS <- lubridate::hms(data$VideoTime)
  data$Valence <- as.numeric(data$Valence)
  data$Arousal <- as.numeric(data$Arousal)
  data$DeltaTime <- 0.147
  data$CumTime <- cumsum(data$DeltaTime)
  data$Secs <- as.integer(data$CumTime)
  data<-aggregate(cbind(data$Arousal, data$Valence), list(data$Secs), mean)
  colnames(data) <- c('Secs', 'Arousal','Valence')
  keep_ <- names(data) %in% c('Secs','Valence','Arousal')
  data <- data[keep_]
  
  uid_data <- read_uid(fname_uid)
  data$UID <- uid_data$UID
  data$UIDz<- uid_data$UID
  data$PC <- uid_data$PC
  data$time <- uid_data$time
  
  data <- as.data.frame(data)
  return(data)
}


read_uid <- function(filename) {
  basename <- tools::file_path_sans_ext(basename(filename))
  raw <- stringi::stri_read_raw(filename)
  top_enc <- stringi::stri_enc_detect(raw)[[1]][1, 1]
  lines <- stringi::stri_read_lines(filename, encoding = top_enc)
  filename <- lines[stringr::str_detect(lines, "Filename")]
  uid_line <- gsub(".mp4", "", gsub(".*PC", "", filename))
  return(uid_line)
}

############ FIN versión anterior ############

GET_LINES <- function(filename) {
  basename <- tools::file_path_sans_ext(basename(filename))
  raw <- stringi::stri_read_raw(filename)
  top_enc <- stringi::stri_enc_detect(raw)[[1]][1, 1]
  lines <- stringi::stri_read_lines(filename, encoding = top_enc)
  filename <- lines[stringr::str_detect(lines, "Filename")]
  uid_line <- gsub(".mp4", "", gsub(".*PC", "", filename))
  return(uid_line)
}

VIDEO_UID <- function(filename) {
  uid_line <- GET_LINES(filename); 
  v <- unlist(strsplit(uid_line,'-'))
  pc <- v[1]
  dateinv_ <- v[c(2:4)]
  date_ <- rev(dateinv_)
  time_ <- head(c(v[-c(1:4)], tail(v, n=1)), n=3)
  
  date <- paste(date_, collapse = '-')
  time <- paste(time_, collapse = ':')
  
  datetime <- date %+% ' ' %+% time
  flag <- date %+% '-' %+% pc
  uid <- paste(c(dateinv_, time_, pc), collapse = '')
  
  data <- data.frame(
    UID = uid, 
    PC = pc, 
    Date = date, 
    Time = time, 
    DateTime = datetime, 
    Flag = flag
  )
  data$Path <- sub("_state","_detailed", filename)
  return(data)
}

load_video <- function(){
  suppressWarnings({
    data <- purrr::map_df(path_video, VIDEO_UID)
    data <- as.data.frame(data)
  })
  return(data)
}

GET_PAIR <- function() {
  VALID_FLAG <- unique(load_video()$Flag)
  data_w <- load_webA() %>% filter(Flag %in% VALID_FLAG) %>% arrange(Flag) %>% select(UID,Flag,DateTime)
  data_v <- load_video() %>% select(UID,Flag,DateTime,PC, Path)
  data_1 <- merge(data_w,data_v,by='Flag') %>% arrange(PC)
  colnames(data_1) <- gsub('.x$', 'Web',colnames(data_1))
  colnames(data_1) <- gsub('.y$', 'Video',colnames(data_1))
  AllUIDVideo <- data_1$UIDVideo
  PAIR_1 <- names(table(AllUIDVideo)[table(AllUIDVideo)==1])
  PAIR_2 <- names(table(AllUIDVideo)[table(AllUIDVideo)==2])
  PAIR_3 <- names(table(AllUIDVideo)[table(AllUIDVideo)==3])
  tmp <- data_1
  tmp <- tmp %>% mutate(P1 = ifelse(UIDVideo %in% PAIR_1,1, 0)) 
  tmp <- tmp %>% mutate(P2 = ifelse(UIDVideo %in% PAIR_2,2, 0)) 
  tmp <- tmp %>% mutate(P3 = ifelse(UIDVideo %in% PAIR_3,3, 0)) 
  tmp <- tmp %>% mutate(PAIRS = P1+P2+P3) %>% select(-c(P1,P2,P3)) 
  DROP_ <- c('241120221548521', '241120221639391','161120221702551') 
  tmp <- tmp %>% filter(UIDVideo %notin% DROP_) %>% arrange(Flag)
  data_3 <- tmp %>% filter(PAIRS==3) %>% arrange(Flag)
  data_3$Delta <- abs(as.integer(difftime(data_3$DateTimeWeb, data_3$DateTimeVideo, units="sec")))
  data_3 <- data_3 %>% filter(Delta<400)
  data_3 <- data_3 %>% select(UIDWeb, UIDVideo, Path)
  data_2 <- tmp %>% filter(PAIRS==2) %>% arrange(Flag)
  data_2$Delta <- abs(as.integer(difftime(data_2$DateTimeWeb, data_2$DateTimeVideo, units="sec")))
  data_2 <- data_2 %>% filter(Delta<700)
  data_2 <- data_2 %>% select(UIDWeb, UIDVideo, Path)
  data_1 <- data_1 %>% filter(UIDVideo %in% PAIR_1) %>% arrange(Flag)
  data_1 <- data_1[data_1$UIDVideo !="301120221528451",]
  data_1 <- data_1 %>% select(UIDWeb, UIDVideo, Path)
  data_12 <- rbind(data_1,data_2)
  data <- rbind(data_12,data_3)
  return(data)
}

#' @details Casos con ERROR
path_drop <- c(
  './data/Logs/17-11/Participant 1_Participant 1_Analysis 1_video_20221121_034345_detailed.txt', 
  './data/Logs/15-11/Participant 5_Participant 5_Analysis 1_video_20221120_185015_detailed.txt', 
  './data/Logs/11-11/Participant 3_Participant 3_Analysis 1_video_20221118_130100_detailed.txt', 
  './data/Logs/15-11/Participant 11_Participant 11_Analysis 1_video_20221120_185023_detailed.txt',
  './data/Logs/29-11/Participant 6_Participant 6_Analysis 1_video_20230112_155846_detailed.txt',
  './data/Logs/30-11/Participant 9_Participant 9_Analysis 1_video_20230112_214215_detailed.txt',
  './data/Logs/16-11/Participant 4_Participant 4_Analysis 1_video_20221120_233831_detailed.txt') 
filter_path <- GET_PAIR() %>% filter(Path %notin% path_drop) %>% select(Path)
path_pair <- filter_path$Path

############ Versión anterior ############

flag <- function(data){
  data$flag<-stringr::str_sub(data$time, 1, 10) %+%'-PC'%+%data$PC
  return(data)
}

match_all <- function(x,y){
  xz <- merge(x,z,by='flag')
  colnames(xz) <- gsub('.x$', 'x',colnames(xz))
  colnames(xz) <- gsub('.y$', 'z',colnames(xz))
  
  xz$dtime<- as.numeric(difftime(xz$timez, xz$timex, units="hours"))
  w <- select(xz,flag,UIDx,UIDz,timex,timez,dtime)
  
  mdf <- data.frame(flag=character(),
                    UIDx=character(),UIDz=character(),
                    timex=character(),timez=character(),dtime=character(),
                    bin=integer())
  
  idls <- unique(w$UIDx); n <- len(idls);
  
  for (j in 1:3){
    count<-1
    for (i in seq(1,n)){
      nw <- w[w$UIDx==idls[i],]
      if (nrow(nw)==j){
        nw$bin <- j
        mdf<-rbind(mdf,nw)
        count<-count+1
      }
    }
  }
  
  return(mdf)
}
############ FIN versión anterior ############

#' ######## Video Data Manipulation 3 ########
#' 
#' @details Remplaza y normaliza los valores no numericos
#' en las variables de series de tiempo, y los define cono NA.
STR_REPLACE <- function(data) {
  STR_TARGET <- c("FIT_FAILED|FIND_FAILED")
  data <- data %>% mutate(AROUSAL = stringr::str_replace(Arousal, STR_TARGET, "NA"))
  data <- data %>% mutate(VALENCE = stringr::str_replace(Valence, STR_TARGET, "NA"))
  suppressWarnings(
    {
      data$AROUSAL <-  as.numeric(data$AROUSAL)
      data$VALENCE <-  as.numeric(data$VALENCE) 
    }
  )
  return(data)
}

#' @details Coerciona todas las variables a tipo caracter para 
#' normalizar series de tiempo con formato datetime
TO_STRING <- function(data) {
  suppressWarnings({
    data <- data %>% mutate(across(everything(), as.character))
  })
  return(data)
}

#' @details Obtiene un valor entero en una cadena de caracteres
GET_INT <- function(str_var, a, b) {
  num <- as.integer(stringr::str_sub(str_var, a, b))
  return(num)
}

#' @details Redefine/indexa Video Time para obtener una
#'  secuencia ordenada con tasa de sample cada 1 sec.
GET_TIMEEVENT <- function(data) {
  data$COUNT <- GET_INT(data$TIMESTAMP, -2, -1) + 
    GET_INT(data$TIMESTAMP, -5, -4)*60 + 
    GET_INT(data$TIMESTAMP, -8, -7)*60*60
  VDATE <- unique(data$DateTime)
  VDATE <- stringr::str_sub(VDATE, 1, 10)
  data$TIMEEVENT <- VDATE %+% ' ' %+% data$TIMESTAMP
  data$TIMEEVENT <- lubridate::ymd_hms(data$TIMEEVENT)
  data$DateTime <- lubridate::ymd_hms(data$DateTime)
  data <- data %>% mutate(TIMEEVENT = DateTime + COUNT)
  return(data)
}

#' @details NA_INTERPOL interpola de forma lineal 
#' un vector  de serie temoporal por aproximacion.
NA_INTERPOL <- function(x,...) {
  data <- x
  if (!is.null(dim(data)[2]) && dim(data)[2] > 1) {
    for (i in 1:dim(data)[2]) {
      if (!anyNA(data[, i])) {next}
      data[, i] <- NA_INTERPOL(data[, i], Inf)
    }
    return(data)
  }
  else {
    missindx <- is.na(data)
    if (!anyNA(data)) {return(x)}
    if (sum(!missindx) < 2) {stop("error")}
    if (!is.null(dim(data)[2]) && !dim(data)[2] == 1) {stop("error")}
    if (!is.null(dim(data)[2])) {data <- data[, 1]}
    if (!is.numeric(data)) {stop("error")}
    n <- length(data)
    allindx <- 1:n
    indx <- allindx[!missindx]
    data_vec <- as.vector(data)
    if (methods::hasArg(rule)) {
      interp <- stats::approx(indx, data_vec[indx], 1:n, ...)$y}
    else {interp <- stats::approx(indx, data_vec[indx], 1:n, rule = 2, ...)$y}
    data[missindx] <- interp[missindx]
    if (is.finite(Inf) && Inf >= 0) {
      rlencoding <- rle(is.na(x))
      rlencoding$values[rlencoding$lengths <= Inf] <- FALSE
      en <- inverse.rle(rlencoding)
      data[en == TRUE] <- NA
    }
    if (!is.null(dim(x)[2])) {x[, 1] <- data
    return(x)
    }
    return(data)
  }
}

#' @details Lee y edita un archivo de log video.txt,
#' filtrandom normalizando, interpolando el archivo 
#' de video inicial no estructurado. Retorna un archivo 
#' de video denso para un sujeto con nobre de archivo filename. 
READV <- function(filename){
  video <- VIDEO_UID(filename)
  options(digits.secs=3)
  fname_data <- sub("_state","_detailed", filename)
  data <- as.data.frame(data.table::fread(fname_data, sep='\t', skip=10))
  colnames(data)[colnames(data) == "Video Time"] = "VideoTime"
  data$TIMESTAMP <- as.character(stringr::str_sub(data$VideoTime, 1, 8))
  data['INDEX'] <- as.character(1:nrow(data))
  data$DateTime <- video$DateTime
  data <- TO_STRING(data)
  data <- STR_REPLACE(data)
  data <- GET_TIMEEVENT(data)
  data$AROUSALx <- data$AROUSAL
  data$VALENCEx <- data$VALENCE
  data$AROUSAL <- NA_INTERPOL(data$AROUSAL)
  data$VALENCE <- NA_INTERPOL(data$VALENCE)
  keep_ <- c("TIMEEVENT", "INDEX", "AROUSAL","AROUSALx", "VALENCE", "VALENCEx","DateTime")
  data <- data %>% select(!!!rlang::syms(keep_))
  data$DateTime <- video$DateTime
  data$UID <- video$UID
  data$PC <- video$PC
  return(data)
}

#' ######## Obtencion de Eventos: 4 ########
#' 
#' @details GET_INTERVAL retorna una sequencia de Timestamps
#' para el intervalo t1, t1, e.g t1=15:28:45
GET_INTERVAL <- function(t1,t2) {
  yrx <- "2010-01-01 "
  interval_event <- stringr::str_sub(
    seq(
      from=as.POSIXct(yrx %+% t1, tz="UTC"), 
      to=as.POSIXct(yrx %+% t2, tz="UTC"),
      by="sec")
    , -8, -1)
  return(interval_event)
}

#' @details WEB_EVENT retorna las marcas temporales 
#' en df WEB-A con identificador con match 
#' en video/Web-A para agregarlos a la serie temporal de video.
#' TO-DO: add WEB-B con merge(x,y,by='UID')
WEB_EVENT <- function() { 
  data_pair_weba <- merge(
    GET_PAIR() %>% mutate(UID=UIDWeb) %>% select(UID,UIDWeb, UIDVideo), 
    load_webA(),by='UID')
  data <- merge(data_pair_weba, load_pension(),by='UID')
  data <- data %>% mutate(UID=UIDVideo)
  return(data)
}

#' @details EVENT retorna la series de tiempo solo para los intervalos 
#' definidos a partir de las variables de marcas de tiempos temporales 
#' en los dataset WEB-A y PESIONES de un sujeto
#' 
#' 
EVENT <- function(filename) {
  video <- READV(filename)
  data <- video
  data <- aggregate(
    list(VALENCE=data$VALENCE,AROUSAL=data$AROUSAL,
         VALENCEx=data$VALENCEx,AROUSALx=data$AROUSALx),
    by = list(TIMEEVENT=data$TIMEEVENT), 
    FUN=mean, na.rm=TRUE, na.action=NULL
  )
  data$UID <- unique(video$UID)
  data$DateTime <- unique(video$DateTime)
  data$PC <- unique(video$PC)
  data$VideoEvent <- stringr::str_sub(data$TIMEEVENT, -8, -1)
  videoUID <- unique(video$UID)
  ev <- WEB_EVENT() %>% filter(UIDVideo == videoUID)
  data$UIDWeb <- ev$UIDWeb
  a1 <- ev$time_consent
  a2 <- ev$time_cotizacion
  p1 <- ev$inicio_wix
  p2 <- ev$yn_contestar
  INT_EVENT_A <- GET_INTERVAL(a1,a2)
  INT_EVENT_P <- GET_INTERVAL(p1,p2)
  data <- data %>% mutate(BinA = ifelse(VideoEvent %in% INT_EVENT_A,1, 0))
  data <- data %>% mutate(BinP = ifelse(VideoEvent %in% INT_EVENT_P,2, 0))
  data <- data %>% mutate(Bin = BinA+BinP) %>% select (-c(BinA, BinP))
  data[data==0] <- NA
  data <- data %>% tidyr::drop_na()
  data$Bin <- as.factor(data$Bin)
  return(data)
}

#' @details SUM retorna el promedio de arousal y valence (con y sin interpolacion) 
#' para los bines temporales agrupados
SUM <- function(filename) {
  data <- EVENT(filename)
  UIDWeb <- unique(data$UIDWeb)
  UIDVideo <- unique(data$UID)
  data_mean <- aggregate(
    list(Valence=data$VALENCE,Arousal=data$AROUSAL,
         ValenceX=data$VALENCEx,ArousalX=data$AROUSALx),
    by = list(Bin=data$Bin), 
    FUN=function(x) c(
      mean = mean(x, na.rm=TRUE, na.action=NULL)
    )
  )
  data_mean$UIDVideo <- UIDVideo
  data_mean$UIDWeb <- UIDWeb
  data <- as.data.frame(data_mean)
  return(data)
}

transformar_a_wide <- function(dataset) {
  
  # Obtener tratamiento y perfil desde la columna "UIDWeb"
  dataset$treatment <- sapply(strsplit(dataset$UIDWeb, "-"), "[[", 3)
  dataset$perfil <- sapply(strsplit(dataset$UIDWeb, "-"), "[[", 5)
  
  # Realizar la transformación
  dataset_wide <- dataset %>%
    pivot_wider(names_from = Bin,
                values_from = c(Valence, Arousal, ValenceX, ArousalX),
                names_sep = "")
  
  return(dataset_wide)
}

calcular_diferencias <- function(dataset) {
  # Calcular las diferencias entre las columnas correspondientes
  dataset$Diff_Valence <- dataset$Valence1 - dataset$Valence2
  dataset$Diff_Arousal <- dataset$Arousal1 - dataset$Arousal2
  dataset$Diff_ValenceX <- dataset$ValenceX1 - dataset$ValenceX2
  dataset$Diff_ArousalX <- dataset$ArousalX1 - dataset$ArousalX2
  
  return(dataset)
}

calcular_diferencias2 <- function(dataset) {
  # Calcular las diferencias entre las columnas correspondientes
  dataset$Diff2_Valence <- dataset$Valence2 - dataset$Valence1
  dataset$Diff2_Arousal <- dataset$Arousal2 - dataset$Arousal1
  dataset$Diff2_ValenceX <- dataset$ValenceX2 - dataset$ValenceX1
  dataset$Diff2_ArousalX <- dataset$ArousalX2 - dataset$ArousalX1
  
  return(dataset)
}

agregar_columnas <- function(dataset) {
  # Agregar la columna sitio
  dataset$sitio <- ifelse(dataset$treatment %in% c(1, 4), "Perfil",
                          ifelse(dataset$treatment %in% c(2, 5), "Video",
                                 ifelse(dataset$treatment %in% c(3, 6), "VideoPerfil",
                                        ifelse(dataset$treatment %in% c(7, 8), "Baseline", NA))))
  
  # Agregar la columna video
  dataset$video <- ifelse(dataset$treatment %in% c(2, 3, 5, 6), 1, 0)
  
  # Agregar la columna t_perfil
  dataset$t_perfil <- ifelse(dataset$treatment %in% c(1, 3, 4, 6), "perfil", "producto")
  
  # Agregar la columna t_video
  dataset$t_video <- ifelse(dataset$sitio %in% c("Video", "VideoPerfil"), "video", "texto")
  
  return(dataset)
}

# Dividir la cadena ID utilizando el carácter "-"
#segmento <- strsplit(UIDWeb, "-")[[1]]

# Obtener el tratamiento y el perfil
#treatment <- segmento[3]
#perfil <- segmento[5]




cat("\014\n")