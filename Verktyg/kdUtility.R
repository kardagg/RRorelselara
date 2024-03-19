#Ritar en linjal med valbart antal ticks, enhet mm

kdRitaLinjal <- function(nr_major_ticks, majorIntervall= 1, unit = "m", prefix = "",  bg_color = "white", tick_color = "black", 
                            aspectRatio = 10, fontSize = 0.8, 
                            majorTick = 0.2, midTick = 0.15, minorTick = 0.1) {
  # Calculate the total ticks
  nr_total_ticks <- nr_major_ticks * 10
  
  #Parameters
  width <- nr_total_ticks/aspectRatio
  tickWidth <- 1
  
  # Remove margins, but store old values
  old.par <-  par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i") 
  
  # Create an empty plot space with the specified background color
  plot(0, type = "n", xlim = c(0,  nr_total_ticks), ylim = c(-width/2, width/2), 
       xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "n", asp = 1,
       col = bg_color)
  rect(0, -width/2, nr_total_ticks, width/2, col = bg_color, border = bg_color)  # Set background color
  
  # Draw the tick marks
  for (i in 0:nr_total_ticks) {
    if (i %% 10 == 0) {  # Major ticks every cm
      segments(i, -width/2, i, -width/2+majorTick*width, lwd = tickWidth, col = tick_color)
      segments(i, width/2, i, width/2-majorTick*width, lwd = tickWidth, col = tick_color)
    } else if (i %% 5 == 0) {  # Medium ticks every half cm
      segments(i, -width/2, i, -width/2+midTick*width, lwd = tickWidth, col = tick_color)
      segments(i, width/2, i, width/2-midTick*width, lwd = tickWidth, col = tick_color)
    } else {  # Minor ticks every mm
      segments(i, -width/2, i, -width/2+minorTick*width, lwd = tickWidth, col = tick_color)
      segments(i, width/2, i, width/2-minorTick*width, lwd = tickWidth, col = tick_color)
    }
  }
  
  # Add numbers for major tick marks
  for (i in 1:(nr_major_ticks-1)) {
    text(i*10 , 0, labels = as.character(i*majorIntervall), cex = fontSize, col = tick_color)
  }
  
  text(nr_major_ticks*10-5 , 0, labels = paste(prefix,unit, sep=""), cex = fontSize, col = tick_color)
  
  par(old.par) # Restore old margins
}