
#' Simple heatmap
#' 
#' Plot a simple heatmap without clustering or dendorgram
#' @param mat A matrix
#' @param col RColorBrewer color palette to use
#' @param rotate Rotate the matrix 90º before calling `image`
#' @param reverse_color Reverse the color scale
#' @param title Plot title
#' 
#' @importFrom RColorBrewer brewer.pal
#' 
#' @export
#' 
#' @examples
#' mat <- matrix(seq(100)) 
#' hmap(mat, title = "Example heatmap")
#' 
hmap <- function(mat, col = "RdYlBu", rotate = TRUE,
                 reverse_color = TRUE, title = "", ...){
    if(rotate) {
        m <- t(apply(mat, 2, rev))
    } else {
        m <- mat
    }
    if(reverse_color) {
        colorscale <- rev(colorRampPalette(brewer.pal(9, col))(100))
    } else{
        colorscale <- colorRampPalette(brewer.pal(9, col))(100)
    }
    image(m, col = colorscale, main = title,
          xaxt = 'n', yaxt = 'n', ...)
}

#' Define fonts
#' 
#' Define some custom font families
#' @param set Set the default font family to Helvetica. If FALSE, just loads the custom font families
#' 
#' @export
#' 
#' @examples
#' library(stuart)
#' define_fonts()
#' 
set_fonts <- function (set = TRUE) {
    quartzFonts(avenir = c("Avenir Book",
                           "Avenir Light",
                           "Avenir Book Oblique", 
                           "Avenir Black Oblique"),
                helvetica = c("Helvetica Neue Light", 
                              "Helvetica Neue",
                              "Helvetica Neue Light Italic", 
                              "Helvetica Neue Bold Italic"))
    if(set){ par(family = 'helvetica') }
}

#' Colour schemes
#' 
#' Vectors of different colours for use in figures
#' @param type Name of colour scheme to choose colours from
#' @return Vector of character strings of colours in hexadecimal
#' 
#' @export
#' 
stuart_colors <- function(type = 'three') {
    # categorical
    pairs <- c("#F98866", "#FF420E", "#80BD9E", "#89DA59")
    four <- c("#375E97", "#FB6542", "#FFBB00", "#3F681C")
    genotypes <- c("#DAECF3", "#FF404E","#1CA5B8", "#D3D3D3")
    three <- c("#225378", "#ACF0F2", "#EB7F00")
    mc <- c('#B4B464', '#6665AD', '#B29492')
    flat <- c("#9b59b6", "#3498db", "#e74c3c", "#34495e", "#2ecc71")
    
    # continuous
    darkblue <- c("#121C25", "#13314D", "#184169", "#2D587B", "#9CB1BF")
    
    switch(type,
           pairs=pairs,
           four=four,
           genotypes=genotypes,
           three=three,
           mc=mc,
           flat=flat,
           darkblue=darkblue)
}
