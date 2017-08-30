
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
#' @example
#' mat <- matrix(seq(100)) 
#' hmap(mat, title = "Example heatmap")
#' 
hmap <- function(mat, col = "RdYlBu", rotate = TRUE, reverse_color = TRUE, title = "", ...){
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
#' @example 
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