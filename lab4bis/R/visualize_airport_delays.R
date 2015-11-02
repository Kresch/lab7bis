#' 
#' @name visualize_airport_delays
#' @title visualize_airport_delays
#' @author Niclas Lovsjö & Maxime Bonneau
#' @description Plots the mean delay of ﬂights for different airports by longitude and latitude using ggplot2
#' @param no parameters
#' @references https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf
#' https://cran.r-project.org/web/packages/nycflights13/nycflights13.pdf
#' 

# install.packages("nycflights13")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("ggmap")
# install.packages("rworldmap")



visualize_airport_delays <- function(){
        library(nycflights13)
        library(dplyr)
        library(ggplot2)
        library(ggmap)
        library(rworldmap)
        data("airports")
        data("flights")
        fli <- flights[is.na(flights$arr_delay)==FALSE,]
        mean_delay <- aggregate(fli$arr_delay,list(fli$dest),mean)
        names(mean_delay)<-c("faa","mean_delay")
        air <- airports[airports$faa%in%mean_delay$faa==TRUE,]
        joint <- dplyr::inner_join(air,mean_delay, by = "faa")
        joint$scaled<-"3"
        joint$scaled[joint$mean_delay<=5]<-"2"
        joint$scaled[joint$mean_delay<=0]<-"1"
        joint$sized <- "1"
        joint$sized[abs(joint$mean_delay)>5] <- "2"
        joint$sized[abs(joint$mean_delay)>10] <- "3"
        joint$sized[abs(joint$mean_delay)>15] <- "4"
        joint$sized[abs(joint$mean_delay)>20] <- "5"
        my_data <- dplyr::select(joint,lat,lon,mean_delay,scaled,sized)
        

        usa <- get_map(location = c(-161,20,-65,50),crop=FALSE,maptype = "terrain",zoom=3)

        m <- ggmap(usa) +
                geom_point(data=my_data, mapping = aes(x=lon, y=lat, color=scaled, size=sized)) +
                scale_colour_manual(values = c("1"="green","2"="darkorange1","3"="red"),
                                    label = c("<0","<5",">5"), name="Color of mean delay") +
                scale_size_manual(values = c("1"=3,"2"=3.5,"3"=4,"4"=4.5,"5"=5),
                                  labels = c("<5", "<10", "<15", "<20", ">20"), 
                                  name = "Absolute value of mean delay") +
                labs(x = "Longitude", y = "Latitude") +
                ggtitle("Mean delay of ﬂights from NYC for different airports by longitude and latitude") +
                theme_bw() +
                theme(panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      plot.title = element_text(size = rel(1.5), face = "bold", vjust = 1.5))
        m
}