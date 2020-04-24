# Objectif : tester un thème ggplot, et des palettes de couleurs personnalisées pour utiliser dans mes graphiques

# packages ====
library(ggplot2)
library(dplyr)
library(jcan)


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
  theme_jcantet()+
  scale_color_jcan(palette = "principale", discrete = TRUE, reverse = TRUE)

g1 +
  theme_jcantet()





# Palette custom ====

# Set de couleurs 
jcan_colors <- c(
  `bleu`  = "#004385",
  `bleu clair` = "#3F88C5",
  `rouge` = "#CD342F",
  `vert` = "#078339",
  `vert clair` = "#5FAD41",
  `orange` = "#FF8C42",
  `bleu gris` = "#4F5D75",
  `jaune` = "#FFBA08",
  `violet` = "#7F2982",
  `blanc` = "#EDF4F9",
  `bleu tres clair` = "#DFEBF5",
  `rouge tres clair` = "#F7DDDC",
  `vert tres clair` = "#D6EADE",
  `violet tres clair` = "#EADBEA",
  `violet grad` = "#432371",
  `orange grad` = "#FAAE7B")


#' Fonction pour extraire mes couleurs comme des hex codes
#'
#' @param ... Character names of jcan_colors 
#'
jcan_cols <- function(...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (jcan_colors)
  
  jcan_colors[cols]
}
jcan_cols()


# Palette
jcan_palettes <- list(
  `principale` = jcan_cols("bleu clair", "rouge","vert","orange","violet","jaune","bleu gris","vert clair"),
  `progressive bleue` = jcan_cols("bleu clair", "blanc"),
  `progressive bleue2` = jcan_cols("bleu clair", "bleu tres clair"),
  `progressive rouge` = jcan_cols("rouge", "blanc"),
  `progressive rouge2` = jcan_cols("rouge", "rouge tres clair"),
  `progressive vert` = jcan_cols("vert","blanc"),
  `progressive vert2` = jcan_cols("vert","vert tres clair"),
  `progressive violet` = jcan_cols("violet","white"),
  `progressive violet2` = jcan_cols("violet","violet tres clair"),
  `VioletOrange` = jcan_cols("violet grad", "orange grad"),
  `divergente` = jcan_cols("bleu clair" ,"blanc","rouge")
  )


#' Fonction pour interpoler une palette custom
#'
#' @param palette Character name of palette in jcan_palettes
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette() comme l'inversion de la palette
#'
jcan_pal <- function(palette = "principale", reverse = FALSE, ...) {
  pal <- jcan_palettes[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  colorRampPalette(pal, ...)
}






#' Color scale constructor for jcan colors
#'
#' @param palette Character name of palette in jcan_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_color_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_color_jcan <- function(palette = "principale", discrete = TRUE, reverse = FALSE, ...) {
  pal <- jcan_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("jcan_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

#' Fill scale constructor for drsimonj colors
#'
#' @param palette Character name of palette in jcan_palettes
#' @param discrete Boolean indicating whether color aesthetic is discrete or not
#' @param reverse Boolean indicating whether the palette should be reversed
#' @param ... Additional arguments passed to discrete_scale() or
#'            scale_fill_gradientn(), used respectively when discrete is TRUE or FALSE
#'
scale_fill_jcan <- function(palette = "principale", discrete = TRUE, reverse = FALSE, ...) {
  pal <- jcan_pal(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("fill", paste0("jcan_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}



g3 +
  theme_jcantet()+
  scale_fill_jcan(palette = "progressive rouge", discrete = FALSE, reverse = TRUE)

g3 +
  theme_jcantet()+
  scale_fill_jcan(palette = "progressive rouge2", discrete = FALSE, reverse = TRUE)


g3 +
  theme_jcantet()+
  scale_fill_jcan(palette = "progressive bleue", discrete = FALSE, reverse = TRUE)

g3 +
  theme_jcantet()+
  scale_fill_jcan(palette = "progressive bleue2", discrete = FALSE, reverse = TRUE)


g3 +
  theme_jcantet()+
  scale_fill_jcan(palette = "progressive vert", discrete = FALSE, reverse = TRUE)

g3 +
  theme_jcantet()+
  scale_fill_jcan(palette = "progressive vert2", discrete = FALSE, reverse = TRUE)


g3 +
  theme_jcantet()+
  scale_fill_jcan(palette = "progressive violet", discrete = FALSE, reverse = TRUE)

g3 +
  theme_jcantet()+
  scale_fill_jcan(palette = "progressive violet2", discrete = FALSE, reverse = TRUE)

g3 +
  theme_jcantet()+
  scale_fill_jcan(palette = "VioletOrange", discrete = FALSE, reverse = TRUE)


g1+
  theme_jcantet()+
  scale_color_jcan(palette = "principale")




g2 +
  theme_jcantet()+
  scale_color_jcan(discrete = TRUE, palette = "principale")
