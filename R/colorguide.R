#'Plot a simple guide to colors
#'
#'Plots a simple guide to colors with color blocks and names arranged
#'vertically
#'
#'
#'@param col A vector of colors
#'@param main Title
#'@param border Type of border around rectangles
#'@return A plot of colors and names
#'@author Chris Stubben
#'@keywords color
#'@examples
#'
#'op<-par(mfrow=c(2,2))
#'colorguide(palette(), "Palette colors")
#'## 657 built-in colors  
#'## RED 
#'reds<-grep("red", colors(), value=TRUE)
#'## sorted alphabetically
#'colorguide(reds, "Reds sorted alphabetically")
#'# GREEN
#'greens<-grep("green", colors(), value=TRUE)
#'RGBColors <- col2rgb(greens)
#'RGBOrder <- order( RGBColors[2,], RGBColors[3,], RGBColors[1,]  )
#'colorguide(greens[RGBOrder][1:30], "Greens sorted by RGB")
#'## light blues
#'colorguide(grep("light.*blue", colors(), value=TRUE) , "Light blues")
#'
#'## Functions
#'colorguide(rainbow(16, end=.7)  , "Rainbow colors")
#'colorguide(heat.colors(16)  , "Heat.colors")
#'
#'## colorRampPalette
#'jet.colors = colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
#'colorguide(rev(jet.colors(16)), "Jet colors from Matlab")
#'blue2red<-colorRampPalette(c('blue','lightyellow','red'))
#'colorguide(blue2red(16), "Blue to Red colors")
#'
#'par(op)
#'
#'
colorguide <- function(col, main="", border=FALSE)
{
  col<-rev(col)   ## reverse order of colors to print top to bottom
  op<-par(mar=c(1,1,1.5,1))
  plot(c(1,100),c(1,100), type = "n", axes = FALSE,  xlab="", ylab="" , main=main)
  x<-100/length(col)
  i <- seq(1,100,x)
  ## draw rectangles at left of plot
  rect(0, i, 20, x+i, col=col, border=border)
  mid<-diff(i)[1]/2
  text(20, i+mid, col, pos=4)
  par(op)
}

