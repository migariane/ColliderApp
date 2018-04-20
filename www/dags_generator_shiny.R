library(ggplot2)
#devtools::install_github("malcolmbarrett/ggdag")
library(ggdag)
library(dplyr)


########################################################################
#                           MODELO A                                   #
########################################################################

tidy_ggdag <- dagify(SBP ~ AGE)%>% tidy_dagitty()

tidy_ggdag

tidy_ggdag$data$x<-c(0,1)
tidy_ggdag$data$y<-c(0,0)

tidy_ggdag$data$xend<-c(1,NA)
tidy_ggdag$data$yend<-c(0,NA)

ggplot(tidy_ggdag$data,aes(x=x,y=y,xend=xend,yend=yend))+
  geom_dag_edges(aes(),edge_colour="black") +
  geom_dag_node() +
  geom_dag_text(col = "white") +
  theme_dag() + 
  theme(panel.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank())

setwd("C:\\Users\\dredondo\\Desktop\\dredondo\\Shiny\\CollideR\\www")
svg(filename="graficoA.svg", 
    width=7.5, 
    height=6,
    pointsize=1)

ggplot(tidy_ggdag$data,aes(x=x,y=y,xend=xend,yend=yend))+
  geom_dag_edges(aes(),edge_colour="black") +
  geom_dag_node() +
  geom_dag_text(col = "white") +
  theme_dag() + 
  theme(panel.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank())
dev.off()


########################################################################
#                           MODELO B                                   #
########################################################################

tidy_ggdag <- dagify(SBP ~ SOD + AGE,
                     SOD ~ AGE)%>% tidy_dagitty()

tidy_ggdag

for(i in 1:dim(tidy_ggdag$data)[[1]]) {   flecha<-tidy_ggdag$data[i, ]
for(i in 1:4){   flecha<-tidy_ggdag$data[i,]
  
  if(flecha$name=="AGE"){
    tidy_ggdag$data[i,]$x<-1
    tidy_ggdag$data[i,]$y<-2
  }
  
  if(flecha$name=="SBP"){
    tidy_ggdag$data[i,]$x<-2
    tidy_ggdag$data[i,]$y<-0
  }
  
  if(flecha$name=="SOD"){
    tidy_ggdag$data[i,]$x<-0
    tidy_ggdag$data[i,]$y<-0
  }
  
}
}

for(i in 1:3){
  flecha<-tidy_ggdag$data[i,]
  
  if(flecha$to=="SBP"){
    tidy_ggdag$data[i,]$xend<-2
    tidy_ggdag$data[i,]$yend<-0
  }
  
  if(flecha$to=="SOD"){
    tidy_ggdag$data[i,]$xend<-0
    tidy_ggdag$data[i,]$yend<-0
  }
  
}

ggplot(tidy_ggdag$data,aes(x=x,y=y,xend=xend,yend=yend))+
  geom_dag_edges(aes(),edge_colour="black") +
  geom_dag_node() +
  geom_dag_text(col = "white") +
  theme_dag() + 
  theme(panel.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank())


setwd("C:\\Users\\dredondo\\Desktop\\dredondo\\Shiny\\CollideR\\www")
svg(filename="graficoB.svg", 
    width=7.5, 
    height=6,
    pointsize=1)

ggplot(tidy_ggdag$data,aes(x=x,y=y,xend=xend,yend=yend))+
  geom_dag_edges(aes(),edge_colour="black") +
  geom_dag_node() +
  geom_dag_text(col = "white") +
  theme_dag() + 
  theme(panel.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank())
dev.off()


########################################################################
#                           MODELO C                                   #
########################################################################

tidy_ggdag <- dagify(PRO ~ SBP + AGE + SOD,
                     SBP ~ SOD + AGE,
                     SOD ~ AGE) %>% tidy_dagitty()

tidy_ggdag

for(i in 1:dim(tidy_ggdag$data)[[1]]){
  
    flecha <- tidy_ggdag$data[i,]
  
  if(flecha$name=="AGE"){
    tidy_ggdag$data[i,]$x <- 1
    tidy_ggdag$data[i,]$y <- 2
  }
  if(flecha$name=="SBP"){
    tidy_ggdag$data[i,]$x <- 2
    tidy_ggdag$data[i,]$y <- 0
  }
  if(flecha$name=="SOD"){
    tidy_ggdag$data[i,]$x <- 0
    tidy_ggdag$data[i,]$y <- 0
  }
  if(flecha$name=="PRO"){
    tidy_ggdag$data[i,]$x <- 1
    tidy_ggdag$data[i,]$y <- 1
  }
}
for(i in 1:dim(tidy_ggdag$data)[[1]]) {
    if(is.na(tidy_ggdag$data[i, ]$xend) == TRUE) {primer_na = i
    break()}
}
for(i in 1:(primer_na - 1)){
  flecha <- tidy_ggdag$data[i,]
  if(flecha$to=="PRO"){
    tidy_ggdag$data[i,]$xend <- 1
    tidy_ggdag$data[i,]$yend <- 1
  }
  if(flecha$to=="SBP"){
    tidy_ggdag$data[i,]$xend <- 2
    tidy_ggdag$data[i,]$yend <- 0
  }
  if(flecha$to=="SOD"){
    tidy_ggdag$data[i,]$xend <- 0
    tidy_ggdag$data[i,]$yend <- 0
  }
}

ggplot(tidy_ggdag$data,aes(x=x,y=y,xend=xend,yend=yend))+
  geom_dag_edges(aes(),edge_colour="black") +
  geom_dag_node() +
  geom_dag_text(col = "white") +
  theme_dag() + 
  theme(panel.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank())

setwd("~/Dropbox/COLLIDER/EpiCollider/wwww")
svg(file="graficoC.svg", 
    width=7.5, 
    height=6,
    pointsize=1)

ggplot(tidy_ggdag$data,aes(x=x,y=y,xend=xend,yend=yend))+
    geom_dag_edges(aes(),edge_colour="black") +
    geom_dag_node() +
    geom_dag_text(col = "white") +
    theme_dag() + 
    theme(panel.background = element_rect(fill = "transparent",colour = NA),
          panel.grid.minor = element_blank(), 
          panel.grid.major = element_blank())
dev.off()

#####

setwd("~/Dropbox/COLLIDER/Article/LaTeX")
pdf(file="Figure3.pdf", 
    width=7.5, 
    height=6,
    pointsize=1)

ggplot(tidy_ggdag$data,aes(x=x,y=y,xend=xend,yend=yend))+
  geom_dag_edges(aes(),edge_colour="black") +
  geom_dag_node() +
  geom_dag_text(col = "white") +
  theme_dag() + 
  theme(panel.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank())
dev.off()

