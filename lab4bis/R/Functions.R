library(httr)
library(xlsx)
library(roxygen2)


#' @title Functions.
#' @description Uses a GET-verb to connect to val.se and downloads a xls-file, which is
#' converted and read by a read.xlsx-command into the actual data-file we need. From this
#' file it extracts the things we need. The shiny app then reads this by calling the 
#' package Lab5, which only consists of this data.
#' @param No params
#' @author Maxime Bonneau, Niclas Lovsjö
#'
#'


q<-GET("http://www.val.se/val/val2014/statistik/2014_riksdagsval_per_kommun.xls")
kommun<-content(q,"raw")
writeBin(kommun,"kommun.xlsx")
num <- rep("numeric", length(kommun)-4)
chara <- rep("character", 4)
nam <- c(chara, num)
theData <- read.xlsx("kommun.xlsx",sheetIndex = 1,
                     header = TRUE, colClasses = nam, 
                     startRow = 3, encoding = "UTF-8",
                     stringsAsFactors=FALSE)

names(theData)[3]<-"LAAN"

lan1 <- unlist(lapply(1:25, as.character))
kom1 <- c("1", "2", "3", "4", "5", "6", "7", "9", "10", "12", "13", "14", 
         "15", "17", "18", "19", "20", "21", "22", "23", "25", "26", "27", 
         "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38", 
         "39", "40", "41", "42", "43", "44", "45", "46", "47", "52", "56", 
         "57", "60", "61", "62", "63", "64", "65", "66", "67", "70", "71", 
         "72", "73", "75", "76", "77", "78", "80", "81", "82", "83", "84", 
         "85", "86", "87", "88", "89", "90", "91", "92", "93", "94", "95",
         "96", "97", "98", "99")
laan1 <- c("","Blekinge län", "Dalarnas län", "Gävleborgs län", "Gotlands län", 
         "Hallands län", "Jämtlands län", "Jönköpings län", "Kalmar län", 
         "Kronobergs län", "Norrbottens län", "Örebro län", 
         "Östergötlands län", "Skåne län", "Södermanlands län", 
         "Stockholms län", "Uppsala län", "Värmlands län", 
         "Västerbottens län", "Västernorrlands län",  "Västmanlands län", 
         "Västra Götalands län")
kommun1 <- c("", "Ale", "Alingsås", "Älmhult", "Älvdalen", "Alvesta", 
            "Älvkarleby", "Älvsbyn", "Åmål", "Aneby", "Ånge", "Ängelholm", 
            "Arboga", "Åre", "Årjäng", "Arjeplog", "Arvidsjaur", "Arvika", 
            "Åsele", "Askersund", "Åstorp", "Åtvidaberg", "Avesta", 
            "Båstad", "Bengtsfors", "Berg", "Bjurholm", "Bjuv", "Boden", 
            "Bollebygd", "Bollnäs", "Borås", "Borgholm", "Borlänge", 
            "Botkyrka", "Boxholm", "Bräcke", "Bromölla", "Burlöv", 
            "Dals-Ed", "Danderyd", "Degerfors", "Dorotea", "Eda", "Ekerö",
            "Eksjö", "Emmaboda", "Enköping", "Eskilstuna", "Eslöv", 
            "Essunga", "Fagersta", "Falkenberg", "Falköping", "Falun", 
            "Färgelanda", "Filipstad", "Finspång", "Flen", "Forshaga", 
            "Gagnef", "Gällivare", "Gävle", "Gislaved", "Gnesta", "Gnosjö", 
            "Göteborg", "Götene", "Gotland", "Grästorp", "Grums", 
            "Gullspång", "Habo", "Håbo", "Hagfors", "Hällefors", 
            "Hallsberg", "Hallstahammar", "Halmstad", "Hammarö", "Haninge", 
            "Haparanda", "Härjedalen", "Härnösand", "Härryda", 
            "Hässleholm", "Heby", "Hedemora", "Helsingborg", "Herrljunga", 
            "Hjo", "Hofors", "Höganäs", "Högsby", "Höör", "Hörby", 
            "Huddinge", "Hudiksvall", "Hultsfred", "Hylte", "Järfälla", 
            "Jokkmokk", "Jönköping", "Kalix", "Kalmar", "Karlsborg", 
            "Karlshamn", "Karlskoga", "Karlskrona", "Karlstad", 
            "Katrineholm", "Kävlinge", "Kil", "Kinda", "Kiruna", "Klippan", 
            "Knivsta", "Köping", "Kramfors", "Kristianstad", 
            "Kristinehamn", "Krokom", "Kumla", "Kungälv", "Kungsbacka", 
            "Kungsör", "Laholm", "Landskrona", "Laxå", "Lekeberg", 
            "Leksand", "Lerum", "Lessebo", "Lidingö", "Lidköping", 
            "Lilla Edet", "Lindesberg", "Linköping", "Ljungby", "Ljusdal", 
            "Ljusnarsberg", "Lomma", "Ludvika", "Luleå", "Lund", 
            "Lycksele", "Lysekil", "Malå", "Malmö", "Malung-Sälen", 
            "Mariestad", "Mark", "Markaryd", "Mellerud", "Mjölby", 
            "Mölndal", "Mönsterås", "Mora", "Mörbylånga", "Motala", 
            "Mullsjö", "Munkedal", "Munkfors", "Nacka", "Nässjö", "Nora", 
            "Norberg", "Nordanstig", "Nordmaling", "Norrköping", 
            "Norrtälje", "Norsjö", "Nybro", "Nyköping", "Nykvarn", 
            "Nynäshamn", "Ockelbo", "Öckerö", "Ödeshög", "Olofström",
            "Örebro", "Örkelljunga", "Örnsköldsvik", "Orsa", "Orust",
            "Osby", "Oskarshamn", "Österåker", "Östersund", "Östhammar",
            "Östra Göinge", "Ovanåker", "Överkalix", "Övertorneå",
            "Oxelösund", "Pajala", "Partille", "Perstorp", "Piteå",
            "Ragunda", "Rättvik", "Robertsfors", "Ronneby", "Säffle",
            "Sala", "Salem", "Sandviken", "Säter", "Sävsjö", "Sigtuna",
            "Simrishamn", "Sjöbo", "Skara", "Skellefteå", "
            Skinnskatteberg", "Skövde", "Skurup", "Smedjebacken", 
            "Söderhamn", "Söderköping", "Södertälje", "Sollefteå", 
            "Sollentuna", "Solna", "Sölvesborg", "Sorsele", "Sotenäs",
            "Staffanstorp", "Stenungsund", "Stockholm", "Storfors",
            "Storuman", "Strängnäs", "Strömstad", "Strömsund", 
            "Sundbyberg", "Sundsvall", "Sunne", "Surahammar", "Svalöv",
            "Svedala", "Svenljunga", "Täby", "Tanum", "Tibro", 
            "Tidaholm", "Tierp", "Timrå", "Tingsryd", "Tjörn", 
            "Tomelilla", "Töreboda", "Torsås", "Torsby", "Tranås", 
            "Tranemo", "Trelleborg", "Trollhättan", "Trosa", "Tyresö",
            "Uddevalla", "Ulricehamn", "Umeå", "Upplands-Bro", 
            "Upplands Väsby", "Uppsala", "Uppvidinge", "Vadstena", 
            "Vaggeryd", "Valdemarsvik", "Vallentuna", "Vänersborg",
            "Vännäs", "Vansbro", "Vara", "Varberg", "Vårgårda", "Värmdö", 
            "Värnamo", "Västerås", "Västervik", "Vaxholm", "Växjö", 
            "Vellinge", "Vetlanda", "Vilhelmina", "Vimmerby", "Vindeln", 
            "Vingåker", "Ydre", "Ystad")
party1 <- c("M", "C", "FP", "KD", "S", "V", "MP", "SD", 
            "FI", "OVR", "BL", "OG")

para <- function(lan = NULL, kom = NULL, laan = NULL, 
                      kommun = NULL, party = NULL){
  if(!is.null(lan)){stopifnot(lan %in% lan1)} 
  if(!is.null(kom)){stopifnot(kom %in% kom1)}
  if(!is.null(laan)){stopifnot(laan %in% laan1)}
  if(!is.null(kommun)){stopifnot(kommun %in% kommun1)}
  if(!is.null(party)){stopifnot(party %in% party1)}
  result <- list(c("lan : ", lan), c("kom : ", kom), c("län : ", laan), 
                 c("kommun : ", kommun), c("party : ", party))
  return(result)
}


lan_func<-function(lan){
        kommun_vect<-c(theData$KOMMUN[theData$LAAN==lan])
        return(kommun_vect)
}
