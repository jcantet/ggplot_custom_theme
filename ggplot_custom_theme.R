# packages ====
library(ggplot2)
library(dplyr)



# Dataset ====
diamonds <- diamonds
diamonds_sub <- diamonds %>% sample_n(5000)

# Graphiqur type 1 : nuage de points
(g1 <- 
  ggplot(diamonds_sub,aes(x = carat, y = price, color = clarity))+
  geom_point()+
  labs(title ="Prix en fonction des carats", subtitle = "Jeu de données : Diamonds",caption = "@j_cantet"))


# Graphique type 2 : nuage de points avec facette
(g2 <- 
  ggplot(diamonds_sub, aes(x=carat, y = price, color = clarity))+
  geom_point(show.legend = FALSE)+
  facet_wrap(~clarity)+
  labs(title ="Pour chaque niveau de clareté, prix en fonction des carats", subtitle = "Jeu de données : Diamonds",caption = "@j_cantet")) 


# Graphique type 3 : barplot
(g3 <- 
  ggplot(diamonds_sub, aes(x = cut))+
  geom_bar(aes(fill = ..count..))+
  labs(title ="Nombre de diamants pour chaque coupe", subtitle = "Jeu de données : Diamonds",caption = "@j_cantet")) 



# Thème perso
theme_jcantet <-function(){
  theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"),
        plot.background = element_rect(fill = "#FEFBF8"),
        # Paneau principal
        panel.border = element_rect(colour = NA, fill = NA),
        panel.background = element_rect(fill = "#FEFBF8"),
        # Grilles
        panel.grid.major.x = element_line(colour = "steelblue2", linetype = 3, size = 0.1),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y =  element_line(colour = "steelblue2", linetype = 3, size = 0.1),
        panel.grid.minor.y = element_blank(),
        # Facette
        strip.background = element_rect(fill = "#736364"),
        strip.text = element_text(color = "white", face = "bold", size = 9),
        # Modification du texte
        axis.text = element_text(colour = "steelblue4", face = "italic", size = 10),
        axis.text.y = element_text(vjust = 0.35, margin = margin(r = 5)),
        axis.title = element_text(colour = "steelblue4", face = "bold", size = 12),
        axis.title.x = element_text(vjust = 0, hjust = 0.98),
        axis.title.y = element_text(vjust = 2, hjust = 0.98),
        axis.ticks = element_line(colour = "steelblue4"),
        plot.title = element_text(size = 16, face = 'bold', hjust = 0, vjust = 1),
        plot.subtitle = element_text(size = 13, face = 'italic'),
        plot.caption = element_text(size = 9, face = "italic", vjust = -1),
        legend.title = element_text(size = 11, face = "bold"))
}



g3 +
  theme_jcantet()


g2 +
  theme_jcantet()

g1 +
  theme_jcantet()
