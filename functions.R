# Utility function to convert hex to rgba with alpha
hex_to_rgba <- function(hex, alpha = 1) {
    rgb <- col2rgb(hex)
    sprintf("rgba(%d, %d, %d, %.2f)", rgb[1], rgb[2], rgb[3], alpha)
}