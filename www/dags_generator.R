library(ggplot2)
devtools::install_github("malcolmbarrett/ggdag")
library(ggdag)
library(dplyr)

########################################################################
#                           figura 1a                                  
########################################################################


tidy_ggdag <- dagify(A ~ W,
                     Y ~ W) %>% tidy_dagitty()

tidy_ggdag

for(i in 1:dim(tidy_ggdag$data)[[1]]){
  flecha<-tidy_ggdag$data[i, ]
  
  if(flecha$name=="W"){
    tidy_ggdag$data[i, ]$x<-1
    tidy_ggdag$data[i, ]$y<-2
  }
  
  if(flecha$name=="Y"){
    tidy_ggdag$data[i, ]$x<-2
    tidy_ggdag$data[i, ]$y <- 0
  }
  
  if(flecha$name=="A"){
    tidy_ggdag$data[i, ]$x <- 0
    tidy_ggdag$data[i, ]$y <- 0
  }
  
}

for(i in 1:dim(tidy_ggdag$data)[[1]]) {
    if(is.na(tidy_ggdag$data[i, ]$xend) == TRUE) {primer_na = i
                                                break()}
}

for(i in 1:(primer_na - 1)){
  flecha<-tidy_ggdag$data[i, ]
  
  if(flecha$to=="W"){
      tidy_ggdag$data[i, ]$xend<-1
      tidy_ggdag$data[i, ]$yend<-2
  }
  
  if(flecha$to=="Y"){
    tidy_ggdag$data[i, ]$xend<-2
    tidy_ggdag$data[i, ]$yend <- 0
  }
  
  if(flecha$to=="A"){
    tidy_ggdag$data[i, ]$xend <- 0
    tidy_ggdag$data[i, ]$yend <- 0
  }
  
}

g1a <- ggplot(tidy_ggdag$data, aes(x = x, y = y, xend = xend, yend = yend)) +
        geom_dag_edges(aes(),edge_colour = "black") +
        geom_dag_node() +
        ggtitle("Figure 1A") +
        geom_dag_text(col = "white") +
        theme_dag() + 
        theme(panel.background = element_rect(fill = "transparent", colour = NA),
              panel.grid.minor = element_blank(), 
              panel.grid.major = element_blank())

########################################################################
#                           figura 1b                                  
########################################################################


tidy_ggdag <- dagify(W ~ A,
                     Y ~ W) %>% tidy_dagitty()

tidy_ggdag

for(i in 1:dim(tidy_ggdag$data)[[1]]){
    flecha <- tidy_ggdag$data[i, ]
    
    if(flecha$name == "W"){
        tidy_ggdag$data[i, ]$x <- 1
        tidy_ggdag$data[i, ]$y <- 2
    }
    
    if(flecha$name == "Y"){
        tidy_ggdag$data[i, ]$x <- 2
        tidy_ggdag$data[i, ]$y <- 0
    }
    
    if(flecha$name == "A"){
        tidy_ggdag$data[i, ]$x <- 0
        tidy_ggdag$data[i, ]$y <- 0
    }
    
}

for(i in 1:dim(tidy_ggdag$data)[[1]]) {
    if(is.na(tidy_ggdag$data[i, ]$xend) == TRUE) {primer_na = i
    break()}
}

for(i in 1:(primer_na - 1)){
    flecha<-tidy_ggdag$data[i, ]
    
    if(flecha$to == "W"){
        tidy_ggdag$data[i, ]$xend <- 1
        tidy_ggdag$data[i, ]$yend <- 2
    }
    
    if(flecha$to == "Y"){
        tidy_ggdag$data[i, ]$xend <- 2
        tidy_ggdag$data[i, ]$yend <- 0
    }
    
    if(flecha$to == "A"){
        tidy_ggdag$data[i, ]$xend <- 0
        tidy_ggdag$data[i, ]$yend <- 0
    }
    
}

g1b <- ggplot(tidy_ggdag$data, aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_edges(aes(),edge_colour = "black") +
    geom_dag_node() +
    ggtitle("Figure 1B") +
    geom_dag_text(col = "white") +
    theme_dag() + 
    theme(panel.background = element_rect(fill = "transparent", colour = NA),
          panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank())

########################################################################
#                           figura 1c                                  
########################################################################


tidy_ggdag <- dagify(W ~ A,
                     W ~ Y) %>% tidy_dagitty()

tidy_ggdag

for(i in 1:dim(tidy_ggdag$data)[[1]]){
    flecha<-tidy_ggdag$data[i, ]
    
    if(flecha$name == "W"){
        tidy_ggdag$data[i, ]$x <- 1
        tidy_ggdag$data[i, ]$y <- 2
    }
    
    if(flecha$name == "Y"){
        tidy_ggdag$data[i, ]$x <- 2
        tidy_ggdag$data[i, ]$y <- 0
    }
    
    if(flecha$name == "A"){
        tidy_ggdag$data[i, ]$x <- 0
        tidy_ggdag$data[i, ]$y <- 0
    }
    
}

for(i in 1:dim(tidy_ggdag$data)[[1]]) {
    if(is.na(tidy_ggdag$data[i, ]$xend) == TRUE) {primer_na = i
    break()}
}

for(i in 1:(primer_na - 1)){
    flecha<-tidy_ggdag$data[i, ]
    
    if(flecha$to == "W"){
        tidy_ggdag$data[i, ]$xend <- 1
        tidy_ggdag$data[i, ]$yend <- 2
    }
    
    if(flecha$to == "Y"){
        tidy_ggdag$data[i, ]$xend <- 2
        tidy_ggdag$data[i, ]$yend <- 0
    }
    
    if(flecha$to == "A"){
        tidy_ggdag$data[i, ]$xend <- 0
        tidy_ggdag$data[i, ]$yend <- 0
    }
    
}

g1c <- ggplot(tidy_ggdag$data, aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_dag_edges(aes(),edge_colour = "black") +
    geom_dag_node() +
    ggtitle("Figure 1C") +
    geom_dag_text(col = "white") +
    theme_dag() + 
    theme(panel.background = element_rect(fill = "transparent", colour = NA),
          panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank())

library(gridExtra)

setwd("C:\\Users\\dredondo\\Dropbox\\COLLIDER\\ColliderApp\\www")
svg(filename = "figure1.svg", 
    width = 18, 
    height = 6,
    pointsize = 1)

grid.arrange(g1a, g1b, g1c, ncol = 3)

dev.off()

