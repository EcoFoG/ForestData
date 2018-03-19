library(dplyr)
library("ggrepel")
library("ggplot2")
forestData.map <- function(data, X = "X", Y = "Y", IdTree = "idTree", Measures = F, Status = F, title = "Map", label_size = 2){
  # browser()
  carre <- data %>%
    select_(X, Y, IdTree)
  if (Measures == F) {
    carre$circonf <- 1
  } else {
    carre$circonf <- data[[Measures]]
  }
  if (Status == F) {
    carre$code_vivant <- 1
  } else {
    carre$code_vivant <- data[[Status]]
  }
  return(graphCarre(carre, title = title, text_size = text_size))
}

graphCarre <- function(carre, title, text_size = 3, repel = TRUE) {

  carre$code_vivant[carre$recrute == TRUE] <- 2
  carre$code_vivant <- factor(carre$code_vivant)


  if (repel == TRUE) {
    legende_texte <- geom_text_repel(size=text_size, fontface=ifelse(carre$n_arbre>= 1000,"bold","plain"), force = 0.3)
  } else {
    legende_texte <- geom_text_repel(size=text_size, fontface=ifelse(carre$n_arbre>= 1000,"bold","plain"), force = 0.001)
  }

  graph <- ggplot(carre, aes(x = X, y = Y, label=n_arbre)) +
    coord_fixed(ratio = 1) +
    geom_point(aes(x=X, y=Y, shape=code_vivant, size=circonf*0.1)) +
    legende_texte +
    scale_shape_manual(values=c(3,16,17), name=" ", label=c("Vivant","Mort","Recrute"), breaks=c(1,0,2)) +
    scale_size_continuous(range = c(1, 5), name = "Vivant") +
    theme_bw() +
    ggtitle(title) +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank()
    )

  return(graph)
}
data <- DataGuyafor %>%
  filter(n_parcelle == "1", n_carre=="1", campagne =="2016")
forestData.map(data, IdTree = "n_arbre", Measures = "circonf", Status = "code_vivant")
