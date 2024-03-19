# Creates graphics and biomechanical data for a sagittal body model

# Create a new environment for local variables and functions
kdEnvSB <- new.env()




# Function: Plot sagittal symmetric body  ------------------------------

kdDrawSagittalBodySymmetric <- function(angles, 
                                        mass = 70,
                                        length = 180,
                                        fixCenterOfMass = TRUE, 
                                        showCenterOfGravity= TRUE, 
                                        showGravityForce = TRUE,
                                        showMomentOfInertia = TRUE){
  
  # Variables -----------------------
  # Read from environment 
  bodySegmentsSagSymMassRatio <- kdEnvSB$bodySegmentsSagSymMassRatio
  bodySegmentsSagSymCM <- kdEnvSB$bodySegmentsSagSymCM
  jointPositionsSagSymStart <- kdEnvSB$jointPositionsSagSymStart
  jointTreeSagSym <- kdEnvSB$jointTreeSagSym
  bodySegmentsSagSym <- kdEnvSB$bodySegmentsSagSym
  
  # New variables
  jointPositionsNew <- jointPositionsSagSymStart
  bodySegmentsSagSymCMNew <- bodySegmentsSagSymCM
  
  anglesRad <- -angles*pi/180
  xLeft <- -60
  xRight <- 80
  yBottom <- -5
  yTop <- 220
  centerScreen <- c((xRight+xLeft)/2, (yTop+yBottom)/2)
  centerOfMass <-  0
  
  # Interior recursive function -----------------------------
  
  #Transverse the segment tree and update geometry
  recursiveSegmentGeometryUpdate <- function(indices){
    
    segmentIndex <- indices[[1]]
    
    # <<- to set the value in the parent function
    # update segment coordinates
    bodySegmentsSagSym[[segmentIndex]] <<- kdEnvSB$rotateAndTranslateCoords(
      bodySegmentsSagSym[[segmentIndex]],
      anglesRad[[segmentIndex]],
      center = jointPositionsSagSymStart[[segmentIndex]],
      translate = jointPositionsNew[[segmentIndex]]-jointPositionsSagSymStart[[segmentIndex]]
      )
    
    # add to center of mass
    bodySegmentsSagSymCMNew[[segmentIndex]] <<- kdEnvSB$rotateAndTranslateCoords(
      matrix( bodySegmentsSagSymCM[[segmentIndex]],
              ncol = 2, byrow = TRUE),
      anglesRad[[segmentIndex]],
      center = jointPositionsSagSymStart[[segmentIndex]],
      translate = jointPositionsNew[[segmentIndex]]-jointPositionsSagSymStart[[segmentIndex]]
    )
    centerOfMass <<- centerOfMass + bodySegmentsSagSymMassRatio[segmentIndex]*bodySegmentsSagSymCMNew[[segmentIndex]]
      
    
  if(length(indices)>1){
  
    for(i in 2:length(indices)){
      jointPositionsNew[[indices[[i]]]] <<- c(kdEnvSB$rotateAndTranslateCoords(
      matrix(jointPositionsSagSymStart[[indices[[i]]]],
             ncol = 2, byrow = TRUE),
      anglesRad[[segmentIndex]],
      center = jointPositionsSagSymStart[[segmentIndex]],
      translate = jointPositionsNew[[segmentIndex]]-jointPositionsSagSymStart[[segmentIndex]]))
    
    #recursive
    recursiveSegmentGeometryUpdate(jointTreeSagSym[[indices[[i]]]])
   }
  }
  }
  
 
  # Main program -------------------------
  recursiveSegmentGeometryUpdate(jointTreeSagSym[[1]])
  


  if(showMomentOfInertia){
    momentOfInertia <- 0

    for(i in 1:length(bodySegmentsSagSymMassRatio)){
      vectorToCenterOfMass <- bodySegmentsSagSymCMNew[[i]]-centerOfMass
    
      #Steiners sats
      momentOfInertia <- momentOfInertia + 
        mass*bodySegmentsSagSymMassRatio[[i]]*((length*kdEnvSB$relativeRadiusOfGyrationSagSym[[i]])^2 + 
        sum(vectorToCenterOfMass*vectorToCenterOfMass))
    }
    momentOfInertia <- momentOfInertia/10000 # from kg*cm^2 to kg*m^2
  }


  if(fixCenterOfMass) {
    translate <- centerScreen - centerOfMass
    for(i in 1:length(bodySegmentsSagSym)){
      bodySegmentsSagSym[[i]] <- sweep(bodySegmentsSagSym[[i]], 2, translate, "+") 
    }
  }
  
 
  # Graphics
  old.par <- par(mar = rep(0, 4)) # Remove margins, but store old values
  # par(bg = 'blue') # Sets background color
  plot.new()
  plot.window(xlim = c(xLeft, xRight), ylim = c(yBottom, yTop), asp = 1)
  for (i in c(1,4,5,2,3,6,7,8)) {
    polygon(bodySegmentsSagSym[[i]], col = "lightGray", border="black")
  }

 # print((centerScreen[2]-mass*centerScreen[2]/90))
  # Center of mass
  if(fixCenterOfMass){
    if(showCenterOfGravity)
        points(centerScreen[1], centerScreen[2],col="red", pch=19, lwd = 8)
    if(showGravityForce)
        arrows(centerScreen[1], centerScreen[2], 
               y1=centerScreen[2]-mass*centerScreen[2]/90, 
               col="red", length = .2, angle = 20, lwd = 3)
  }
  else{
    polygon(  matrix(c(-80, -10, 100, -10, 100, 0, -80, 0), 
                     ncol = 2, byrow = TRUE),col = "gray", border="gray")
    if(showCenterOfGravity)
        points(centerOfMass[1], centerOfMass[2],col="red", pch=19, lwd = 8)
    if(showGravityForce) 
       arrows(centerOfMass[1], centerOfMass[2], 
              y1=centerOfMass[2]-mass*centerOfMass[2]/90, 
              col="red", length = .2, angle = 20, lwd = 3)
  }
  
  if(showMomentOfInertia){
    textMomentOfInertia <- paste("Tröghetsmoment = ", format(momentOfInertia,digits=2, nsmall=1), "kg·m\ub2")
    text(xLeft+10, yTop-10, textMomentOfInertia, cex=1.65, pos=3, col="blue") 
  }

  par(old.par) # Restore old margins
  
# Not used graphics
#  polygon(matrix(c(xLeft,yBottom,xRight,yBottom,xRight,yTop,xLeft,yTop,xLeft,yBottom), dncol = 2, byrow = TRUE), border="red")
#  translate <- (centerScreen - centerOfMass)
# bluePoints <- t(matrix(unlist(bodySegmentsSagSymCMNew ),ncol = 2, byrow = TRUE))
# points(bluePoints[1,]+translate[1], bluePoints[2,]+translate[2],col="blue", pch=19)

}


# Function: Plot sagittal all segments body  ------------------------------

kdDrawSagittalBodyAllSegments <- function(angles, 
                                        mass = 70,
                                        length = 180,
                                        fixCenterOfMass = TRUE, 
                                        showCenterOfGravity= TRUE, 
                                        showGravityForce = TRUE,
                                        showMomentOfInertia = TRUE){
  
  # Variables -----------------------
  # Read from environment 
  bodySegmentsSagAllMassRatio <- kdEnvSB$bodySegmentsSagAllMassRatio
  bodySegmentsSagAllCM <- kdEnvSB$bodySegmentsSagAllCM
  jointPositionsSagAllStart <- kdEnvSB$jointPositionsSagAllStart
  jointTreeSagAll <- kdEnvSB$jointTreeSagAll
  bodySegmentsSagAll <- kdEnvSB$bodySegmentsSagAll
  
  # New variables
  jointPositionsNew <- jointPositionsSagAllStart
  bodySegmentsSagAllCMNew <- bodySegmentsSagAllCM
  
  anglesRad <- -angles*pi/180
  xLeft <- -60
  xRight <- 80
  yBottom <- -5
  yTop <- 220
  centerScreen <- c((xRight+xLeft)/2, (yTop+yBottom)/2)
  centerOfMass <-  0
  
  # Interior recursive function -----------------------------
  
  #Transverse the segment tree and update geometry
  recursiveSegmentGeometryUpdate <- function(indices){
    
    segmentIndex <- indices[[1]]
    
    # <<- to set the value in the parent function
    # update segment coordinates
    bodySegmentsSagAll[[segmentIndex]] <<- kdEnvSB$rotateAndTranslateCoords(
      bodySegmentsSagAll[[segmentIndex]],
      anglesRad[[segmentIndex]],
      center = jointPositionsSagAllStart[[segmentIndex]],
      translate = jointPositionsNew[[segmentIndex]]-jointPositionsSagAllStart[[segmentIndex]]
    )
    
    # add to center of mass
    bodySegmentsSagAllCMNew[[segmentIndex]] <<- kdEnvSB$rotateAndTranslateCoords(
      matrix( bodySegmentsSagAllCM[[segmentIndex]],
              ncol = 2, byrow = TRUE),
      anglesRad[[segmentIndex]],
      center = jointPositionsSagAllStart[[segmentIndex]],
      translate = jointPositionsNew[[segmentIndex]]-jointPositionsSagAllStart[[segmentIndex]]
    )
    centerOfMass <<- centerOfMass + bodySegmentsSagAllMassRatio[segmentIndex]*bodySegmentsSagAllCMNew[[segmentIndex]]
    
    
    if(length(indices)>1){
      
      for(i in 2:length(indices)){
        jointPositionsNew[[indices[[i]]]] <<- c(kdEnvSB$rotateAndTranslateCoords(
          matrix(jointPositionsSagAllStart[[indices[[i]]]],
                 ncol = 2, byrow = TRUE),
          anglesRad[[segmentIndex]],
          center = jointPositionsSagAllStart[[segmentIndex]],
          translate = jointPositionsNew[[segmentIndex]]-jointPositionsSagAllStart[[segmentIndex]]))
        
        #recursive
        recursiveSegmentGeometryUpdate(jointTreeSagAll[[indices[[i]]]])
      }
    }
  }
  
  
  # Main program -------------------------
  recursiveSegmentGeometryUpdate(jointTreeSagAll[[1]])
  
  
  
  if(showMomentOfInertia){
    momentOfInertia <- 0
    
    for(i in 1:length(bodySegmentsSagAllMassRatio)){
      vectorToCenterOfMass <- bodySegmentsSagAllCMNew[[i]]-centerOfMass
      
      #Steiners sats
      momentOfInertia <- momentOfInertia + 
        mass*bodySegmentsSagAllMassRatio[[i]]*((length*kdEnvSB$relativeRadiusOfGyrationSagAll[[i]])^2 + 
                                                 sum(vectorToCenterOfMass*vectorToCenterOfMass))
    }
    momentOfInertia <- momentOfInertia/10000 # from kg*cm^2 to kg*m^2
  }
  
  
  if(fixCenterOfMass) {
    translate <- centerScreen - centerOfMass
    for(i in 1:length(bodySegmentsSagAll)){
      bodySegmentsSagAll[[i]] <- sweep(bodySegmentsSagAll[[i]], 2, translate, "+") 
    }
  }
  
  # Graphics
  old.par <- par(mar = rep(0, 4)) # Remove margins, but store old values
  # par(bg = 'blue') # Sets background color
  plot.new()
  plot.window(xlim = c(xLeft, xRight), ylim = c(yBottom, yTop), asp = 1)
  for (i in c(12,13,11,9,10,1,4,5,2,3,6,7,8)) {
    polygon(bodySegmentsSagAll[[i]], col = "lightGray", border="black")
  }
  
  # print((centerScreen[2]-mass*centerScreen[2]/90))
  # Center of mass
  if(fixCenterOfMass){
    if(showCenterOfGravity)
      points(centerScreen[1], centerScreen[2],col="red", pch=19, lwd = 8)
    if(showGravityForce)
      arrows(centerScreen[1], centerScreen[2], 
             y1=centerScreen[2]-mass*centerScreen[2]/90, 
             col="red", length = .2, angle = 20, lwd = 3)
  }
  else{
    polygon(  matrix(c(-80, -10, 100, -10, 100, 0, -80, 0), 
                     ncol = 2, byrow = TRUE),col = "gray", border="gray")
    if(showCenterOfGravity)
      points(centerOfMass[1], centerOfMass[2],col="red", pch=19, lwd = 8)
    if(showGravityForce) 
      arrows(centerOfMass[1], centerOfMass[2], 
             y1=centerOfMass[2]-mass*centerOfMass[2]/90, 
             col="red", length = .2, angle = 20, lwd = 3)
  }
  
  if(showMomentOfInertia){
    textMomentOfInertia <- paste("Tröghetsmoment = ", format(momentOfInertia,digits=2, nsmall=1), "kg·m\ub2")
    text(xLeft+10, yTop-10, textMomentOfInertia, cex=1.65, pos=3, col="blue") 
  }
  
  par(old.par) # Restore old margins
  
  # Not used graphics
  #  polygon(matrix(c(xLeft,yBottom,xRight,yBottom,xRight,yTop,xLeft,yTop,xLeft,yBottom), dncol = 2, byrow = TRUE), border="red")
  #  translate <- (centerScreen - centerOfMass)
  # bluePoints <- t(matrix(unlist(bodySegmentsSagSymCMNew ),ncol = 2, byrow = TRUE))
  # points(bluePoints[1,]+translate[1], bluePoints[2,]+translate[2],col="blue", pch=19)
  
}


# Local functions --------------------------

# Rotate coordinates about center coordinate ant then translates
kdEnvSB$rotateAndTranslateCoords <- function(matrix, angle, center = c(0, 0), translate = c(0, 0)) {
  # Create the rotation matrix
  rotation_matrix <- matrix(c(cos(angle), -sin(angle),
                              sin(angle),  cos(angle)), nrow = 2, byrow = TRUE)
  
  # Adjust points by the center
  adjusted_points <- sweep(matrix, 2, center, "-")
  
  # Apply the rotation
  rotated_points <- t(rotation_matrix %*% t(adjusted_points))
  
  # Readjust the points to the original center
  rotated_points <- sweep(rotated_points, 2, center+translate, "+")
  
  return(rotated_points)
}


# Local varaiables Sagittal symmetrical man -----------------
# 1-foot, 2-shank, 3-thigh, 4-pelvis, 5-trunk, 5-head, 6-upperArm, 7-lowerArm

# Mass ratio for each segment (bilateral segments represent only one side)
kdEnvSB$bodySegmentsSagSymMassRatio <- c(2*0.015, 2*0.043, 2*0.103, 0.114, 0.407, 
                                   0.073, 2*0.026, 2*0.016)

# Center of mass fore each segment
kdEnvSB$bodySegmentsSagSymCM <- list(
  c(11.4, 5.),   # 1 = foot
  c(7.75, 31.5), # 2 = shank
  c(13.8, 73.), # 3 = thigh 
  c(13.8, 97.), # 4 = pelvis 
  c(17.45, 131.), # 5 = trunk 
  c(17.9, 169.), # 6 = head
  c(14.25, 136.5), # 7 = upperArm 
  c(16.5, 104.5) # 8 =  lowerArm
)


# joint positions in start position
kdEnvSB$jointPositionsSagSymStart <- list(
  c(27.5, 1),   # 1 = toe
  c(8.25, 9.5), # 2 = ancle 
  c(10.3, 49.5), # 3 = knee 
  c(15.2, 88.5), # 4 = hip 
  c(14.1, 104.5), # 5 = S1L5 
  c(14.1, 157.5), # 6 = neck
  c(14.4, 147.), # 7 = sholder 
  c(14.4, 120.5) # 8 =  elbow
)

# Joint index correspond to index of segment to turn
kdEnvSB$jointTreeSagSym <- list(c(1,2), # 1 = foot
                  c(2,3), # 2 = shank
                  c(3,4), # 3 = thigh
                  c(4,5), # 4 = pelvis 
                  c(5,6,7), # 5 = trunk 
                  c(6),   # 6 = head
                  c(7,8), # 7 = upperArm 
                  c(8))   # 8 =  lowerArm

kdEnvSB$relativeRadiusOfGyrationSagSym <- c(0.077, 0.074, 0.079, 0.05, 0.15, 0.062, 
                                      0.081, 0.078)

# Coordinatest for segment shapes
kdEnvSB$bodySegmentsSagSym <- list(
  # 1 = foot
  matrix(c(17.98, 0.31, 15.52, 0.5, 13.06, 0.69, 10.6, 
           0.87, 8., 0.83, 4.98, 0.42, 2.51, 1.01, 
           0.64, 3.42, 2.22, 6.51, 3.14, 8.88, 3.16, 
           10.05, 3.18, 11.21, 3.21, 12.37, 6.04, 12.4, 
           9.2, 12.74, 12.06, 11.95, 12.49, 11.23, 12.93, 
           10.51, 13.38, 9.79, 16.58, 8.11, 19.79, 6.44, 
           23., 4.77, 25.23, 4.22, 25.23, 4.22, 27.6, 
           3.38, 27.9, 2.99, 28.2, 2.59, 28.5, 2.2, 
           27.88, 0.73, 24.5, 0.17, 21.32, 0.31, 17.98, 
           0.31), 
         ncol = 2, byrow = TRUE),
  # 2 = shank
  matrix(c(7.21, 11.62, 5.86, 11.79, 4.51, 11.95, 3.17, 
           12.11, 2.99, 18.35, 2.45, 24.52, 2.12, 30.64, 
           1.84, 31.53, 1.56, 32.42, 1.29, 33.29, 1.29, 
           37.75, 2.77, 42.61, 4.7, 47.35, 6.38, 52.1, 
           11.53, 51.06, 11.53, 51.06, 15.43, 50.43, 15.4, 
           48.44, 15.38, 46.45, 15.36, 44.44, 14.8, 40.68, 
           14.25, 38.03, 13.97, 35.8, 13.68, 31.06, 12.82, 
           26.35, 12.02, 21.59, 11.74, 16.85, 11.74, 16.85, 
           11.67, 12.11, 8.47, 11.62, 8.47, 11.62, 7.21, 
           11.62), 
         ncol = 2, byrow = TRUE),
  # 3 = thigh
  matrix(c(14.43, 47.85, 11.78, 49.09, 9.41, 50.27, 7.04, 
           51.76, 6.59, 52.34, 6.15, 52.92, 5.71, 53.5, 
           5.54, 58.84, 4.56, 64.02, 3.77, 69.24, 3.77, 
           75.94, 4.38, 79.72, 4.97, 83.17, 5.99, 86.81, 
           7.29, 88.71, 8.59, 90.61, 9.9, 92.51, 11.3, 
           93.14, 13.76, 94.68, 15.26, 94.68, 18., 93.35, 
           20.74, 92.03, 23.49, 90.7, 23.6, 87.87, 23.71, 
           85.04, 23.83, 82.2, 23.19, 76.54, 22.63, 71.31, 
           21.05, 65.76, 19.35, 60.86, 18.35, 55.73, 16.58, 
           50.85, 16.02, 49.87, 15.46, 48.9, 14.91, 47.92, 
           14.75, 47.9, 14.59, 47.88, 14.43, 47.85), 
         ncol = 2, byrow = TRUE),
  # 4 = pelvis
  matrix(c(8.06, 85.11, 7.4, 85.21, 6.76, 85.3, 6.11, 
           85.39, 4.27, 87.23, 3.17, 89.48, 1.85, 91.59, 
           1.85, 94.22, 2.05, 96.46, 2.41, 98.98, 4.15, 
           102.9, 6.29, 106.61, 9.32, 109.64, 18.79, 109.64, 
           21.43, 109.09, 21.43, 109.09, 25.2, 107.97, 26.52, 
           107.06, 27.85, 106.16, 29.17, 105.25, 28.21, 101.17, 
           26.75, 97.64, 24.85, 93.82, 24.64, 92.58, 24.43, 
           91.32, 24.22, 90.07, 19.16, 88.42, 14.1, 86.77, 
           9.04, 85.11), 
         ncol = 2, byrow = TRUE),
  # 5 = trunk
  matrix(c(23.51, 103.76, 20.6, 103.93, 17.8, 104.21, 15.01, 
           104.6, 11.8, 105.72, 11.8, 105.72, 9.99, 106.54, 
           9.26, 107.45, 8.55, 108.36, 7.83, 109.26, 7.9, 
           111.17, 8.1, 112.92, 8.39, 114.7, 8.29, 116.19, 
           8.2, 117.67, 8.11, 119.16, 6.99, 121.58, 5.88, 
           123.99, 4.77, 126.4, 3.92, 130.58, 2.92, 134.61, 
           1.84, 138.66, 2.26, 142.43, 2.26, 142.43, 2.82, 
           145.08, 3.87, 148.5, 4.87, 151.93, 6.71, 154.97, 
           7.43, 155.88, 8.15, 156.79, 8.88, 157.68, 11.84, 
           157.61, 14.74, 157.34, 17.79, 156.99, 18.86, 156.6, 
           19.93, 156.21, 21., 155.81, 20.88, 154.93, 20.76, 
           154.05, 20.65, 153.16, 22.5, 148.52, 25.34, 144.19, 
           27.89, 139.78, 31.23, 130.31, 29.99, 120.51, 29.29, 
           110.66, 28.17, 106.75, 28.17, 106.75, 27.62, 105.23, 
           25.96, 104.29, 25.35, 103.76, 23.51, 103.76), 
         ncol = 2, byrow = TRUE),
  # 6 = head
  matrix(c(18.26, 152.95, 17.01, 153.5, 15.76, 154.06, 14.51, 
           154.62, 12.4, 155.15, 10.44, 155.6, 8.54, 156.48, 
           8.3, 160.85, 8.38, 165.04, 7.08, 169.13, 7.17, 
           170.8, 7.26, 172.46, 7.36, 174.13, 8.09, 175.38, 
           8.83, 176.63, 9.58, 177.87, 10.57, 178.69, 11.57, 
           179.5, 12.57, 180.31, 14.09, 180.45, 15.62, 180.59, 
           17.15, 180.73, 18.54, 180.36, 19.93, 179.99, 21.32, 
           179.61, 22.47, 178.88, 23.62, 178.14, 24.79, 177.39, 
           25.55, 175.98, 26.31, 174.57, 27.08, 173.16, 27.22, 
           171.68, 27.36, 170.2, 27.5, 168.71, 28.33, 167.01, 
           28.86, 166.25, 28.96, 164.27, 28.58, 164.11, 28.21, 
           163.95, 27.84, 163.78, 26.98, 162.33, 26.99, 160.09, 
           26.87, 158.36, 25.14, 157.42, 23.93, 157.33, 22.01, 
           157.04, 21.25, 155.64, 20.93, 154.49, 20.48, 152.88, 
           19.74, 152.91, 19., 152.93, 18.26, 152.95), 
         ncol = 2, byrow = TRUE),
  # 7 = upperArm
  matrix(c(13.16, 120.46, 12.18, 120.58, 11.2, 120.7, 10.23, 
           120.81, 9.67, 125.76, 9.34, 130.51, 8.49, 135.3, 
           8.44, 137.16, 8.4, 139.03, 8.36, 140.88, 7.8, 
           144.5, 7.8, 144.5, 7.66, 146.45, 7.94, 147.67, 
           8.22, 148.87, 8.49, 150.08, 9.14, 151.05, 9.79, 
           152.03, 10.44, 152.99, 13.07, 154.01, 14.73, 155.24, 
           17.69, 153.97, 19.91, 150.49, 19.91, 150.49, 21.87, 
           146.87, 21.73, 146.17, 21.59, 145.47, 21.45, 144.77, 
           18.68, 140.01, 19.32, 135.11, 19.23, 129.59, 18.12, 
           126.55, 18.15, 124.69, 18.39, 121.51, 17.94, 121.16, 
           17.5, 120.81, 17.06, 120.46), 
         ncol = 2, byrow = TRUE),
  # 8 = lowerArm
  matrix(c(16.34, 77.88, 15.96, 78.04, 15.59, 78.2, 15.23, 
           78.36, 14.31, 81.49, 14.34, 84.29, 14.18, 87.42, 
           13.49, 91.08, 13.89, 92.8, 14.88, 96.34, 14., 
           101.9, 12.03, 106.93, 10.42, 112.22, 10.44, 115.08, 
           10.46, 117.94, 10.49, 120.79, 13.58, 122.55, 15.52, 
           122.32, 19.13, 122.25, 19.34, 121.37, 19.55, 120.49, 
           19.76, 119.6, 19.62, 116.12, 19.62, 116.12, 19.62, 
           107.49, 20.13, 102.14, 20.52, 96.78, 20.74, 91.31, 
           21.01, 89.15, 21.27, 87.26, 21.01, 85.05, 19.56, 
           84.01, 19.77, 83.75, 19.48, 81.84, 19.13, 80.89, 
           18.78, 79.95, 18.43, 78.99, 16.62, 77.88, 16.62, 
           77.88, 16.34, 77.88), 
         ncol = 2, byrow = TRUE)
)

# Local varaiables Sagittal all segments man -----------------
# 1-footR, 2-shankR, 3-thighR,  4-pelvis, 5-trunk, 6-head, 7-upperArmR, 8-lowerArmR, 
# 9-thighL, 10-shankL, 11-footL, 12-upperArmL, 13-lowerArmL

# Mass ratio for each segment (bilateral segments represent only one side)
kdEnvSB$bodySegmentsSagAllMassRatio <- c(0.015, 0.043, 0.103, 0.114, 0.407, 
                                         0.073, 0.026, 0.016, 
                                         0.103, 0.043, 0.015,
                                               0.026, 0.016)

# Center of mass fore each segment
kdEnvSB$bodySegmentsSagAllCM <- list(
  c(11.4, 5.),   # 1 = footR
  c(7.75, 31.5), # 2 = shankR
  c(13.8, 73.), # 3 = thighR 
  c(13.8, 97.), # 4 = pelvis 
  c(17.45, 131.), # 5 = trunk 
  c(17.9, 169.), # 6 = head
  c(14.25, 136.5), # 7 = upperArmR 
  c(16.5, 104.5), # 8 =  lowerArmR
  c(13.8, 73.), # 9 = thighL 
  c(7.75, 31.5), # 10 = shankL
  c(11.4, 5.),   # 11 = footL
  c(14.25, 136.5), # 12 = upperArmL 
  c(16.5, 104.5) # 13 =  lowerArmL
)


# joint positions in start position
kdEnvSB$jointPositionsSagAllStart <- list(
  c(27.5, 1),   # 1 = toeR
  c(8.25, 9.5), # 2 = ancleR 
  c(10.3, 49.5), # 3 = kneeR 
  c(15.2, 88.5), # 4 = hipR 
  c(14.1, 104.5), # 5 = S1L5 
  c(14.1, 157.5), # 6 = neck
  c(14.4, 147.), # 7 = sholderR 
  c(14.4, 120.5), # 8 =  elbowR
  c(15.2, 88.5), # 9 = hipL
  c(10.3, 49.5), # 10 = kneeL 
  c(8.25, 9.5), # 11 = ancleL 
  c(14.4, 147.), # 12 = sholderL 
  c(14.4, 120.5) # 13 =  elbowL
)

# Joint index correspond to index of segment to turn
kdEnvSB$jointTreeSagAll <- list(c(1,2), # 1 = footR
                                c(2,3), # 2 = shankR
                                c(3,4), # 3 = thighR
                                c(4,5,9), # 4 = pelvis 
                                c(5,6,7,12), # 5 = trunk 
                                c(6),   # 6 = head
                                c(7,8), # 7 = upperArmR 
                                c(8),   # 8 =  lowerArmR
                                c(9,10), # 9 = thighL
                                c(10,11), # 10 = shankL
                                c(11), # 11 = footL
                                c(12,13), # 12 = upperArmL 
                                c(13)   # 13 =  lowerArmL
                                )

kdEnvSB$relativeRadiusOfGyrationSagAll <- c(0.077, 0.074, 0.079, 0.05, 0.15, 0.062, 
                                            0.081, 0.078, 
                                            0.077, 0.074, 0.079,
                                            0.081, 0.078)

# Coordinatest for segment shapes
kdEnvSB$bodySegmentsSagAll <- list(
  # 1 = footR
  matrix(c(17.98, 0.31, 15.52, 0.5, 13.06, 0.69, 10.6, 
           0.87, 8., 0.83, 4.98, 0.42, 2.51, 1.01, 
           0.64, 3.42, 2.22, 6.51, 3.14, 8.88, 3.16, 
           10.05, 3.18, 11.21, 3.21, 12.37, 6.04, 12.4, 
           9.2, 12.74, 12.06, 11.95, 12.49, 11.23, 12.93, 
           10.51, 13.38, 9.79, 16.58, 8.11, 19.79, 6.44, 
           23., 4.77, 25.23, 4.22, 25.23, 4.22, 27.6, 
           3.38, 27.9, 2.99, 28.2, 2.59, 28.5, 2.2, 
           27.88, 0.73, 24.5, 0.17, 21.32, 0.31, 17.98, 
           0.31), 
         ncol = 2, byrow = TRUE),
  # 2 = shankR
  matrix(c(7.21, 11.62, 5.86, 11.79, 4.51, 11.95, 3.17, 
           12.11, 2.99, 18.35, 2.45, 24.52, 2.12, 30.64, 
           1.84, 31.53, 1.56, 32.42, 1.29, 33.29, 1.29, 
           37.75, 2.77, 42.61, 4.7, 47.35, 6.38, 52.1, 
           11.53, 51.06, 11.53, 51.06, 15.43, 50.43, 15.4, 
           48.44, 15.38, 46.45, 15.36, 44.44, 14.8, 40.68, 
           14.25, 38.03, 13.97, 35.8, 13.68, 31.06, 12.82, 
           26.35, 12.02, 21.59, 11.74, 16.85, 11.74, 16.85, 
           11.67, 12.11, 8.47, 11.62, 8.47, 11.62, 7.21, 
           11.62), 
         ncol = 2, byrow = TRUE),
  # 3 = thighR
  matrix(c(14.43, 47.85, 11.78, 49.09, 9.41, 50.27, 7.04, 
           51.76, 6.59, 52.34, 6.15, 52.92, 5.71, 53.5, 
           5.54, 58.84, 4.56, 64.02, 3.77, 69.24, 3.77, 
           75.94, 4.38, 79.72, 4.97, 83.17, 5.99, 86.81, 
           7.29, 88.71, 8.59, 90.61, 9.9, 92.51, 11.3, 
           93.14, 13.76, 94.68, 15.26, 94.68, 18., 93.35, 
           20.74, 92.03, 23.49, 90.7, 23.6, 87.87, 23.71, 
           85.04, 23.83, 82.2, 23.19, 76.54, 22.63, 71.31, 
           21.05, 65.76, 19.35, 60.86, 18.35, 55.73, 16.58, 
           50.85, 16.02, 49.87, 15.46, 48.9, 14.91, 47.92, 
           14.75, 47.9, 14.59, 47.88, 14.43, 47.85), 
         ncol = 2, byrow = TRUE),
  # 4 = pelvis
  matrix(c(8.06, 85.11, 7.4, 85.21, 6.76, 85.3, 6.11, 
           85.39, 4.27, 87.23, 3.17, 89.48, 1.85, 91.59, 
           1.85, 94.22, 2.05, 96.46, 2.41, 98.98, 4.15, 
           102.9, 6.29, 106.61, 9.32, 109.64, 18.79, 109.64, 
           21.43, 109.09, 21.43, 109.09, 25.2, 107.97, 26.52, 
           107.06, 27.85, 106.16, 29.17, 105.25, 28.21, 101.17, 
           26.75, 97.64, 24.85, 93.82, 24.64, 92.58, 24.43, 
           91.32, 24.22, 90.07, 19.16, 88.42, 14.1, 86.77, 
           9.04, 85.11), 
         ncol = 2, byrow = TRUE),
  # 5 = trunk
  matrix(c(23.51, 103.76, 20.6, 103.93, 17.8, 104.21, 15.01, 
           104.6, 11.8, 105.72, 11.8, 105.72, 9.99, 106.54, 
           9.26, 107.45, 8.55, 108.36, 7.83, 109.26, 7.9, 
           111.17, 8.1, 112.92, 8.39, 114.7, 8.29, 116.19, 
           8.2, 117.67, 8.11, 119.16, 6.99, 121.58, 5.88, 
           123.99, 4.77, 126.4, 3.92, 130.58, 2.92, 134.61, 
           1.84, 138.66, 2.26, 142.43, 2.26, 142.43, 2.82, 
           145.08, 3.87, 148.5, 4.87, 151.93, 6.71, 154.97, 
           7.43, 155.88, 8.15, 156.79, 8.88, 157.68, 11.84, 
           157.61, 14.74, 157.34, 17.79, 156.99, 18.86, 156.6, 
           19.93, 156.21, 21., 155.81, 20.88, 154.93, 20.76, 
           154.05, 20.65, 153.16, 22.5, 148.52, 25.34, 144.19, 
           27.89, 139.78, 31.23, 130.31, 29.99, 120.51, 29.29, 
           110.66, 28.17, 106.75, 28.17, 106.75, 27.62, 105.23, 
           25.96, 104.29, 25.35, 103.76, 23.51, 103.76), 
         ncol = 2, byrow = TRUE),
  # 6 = head
  matrix(c(18.26, 152.95, 17.01, 153.5, 15.76, 154.06, 14.51, 
           154.62, 12.4, 155.15, 10.44, 155.6, 8.54, 156.48, 
           8.3, 160.85, 8.38, 165.04, 7.08, 169.13, 7.17, 
           170.8, 7.26, 172.46, 7.36, 174.13, 8.09, 175.38, 
           8.83, 176.63, 9.58, 177.87, 10.57, 178.69, 11.57, 
           179.5, 12.57, 180.31, 14.09, 180.45, 15.62, 180.59, 
           17.15, 180.73, 18.54, 180.36, 19.93, 179.99, 21.32, 
           179.61, 22.47, 178.88, 23.62, 178.14, 24.79, 177.39, 
           25.55, 175.98, 26.31, 174.57, 27.08, 173.16, 27.22, 
           171.68, 27.36, 170.2, 27.5, 168.71, 28.33, 167.01, 
           28.86, 166.25, 28.96, 164.27, 28.58, 164.11, 28.21, 
           163.95, 27.84, 163.78, 26.98, 162.33, 26.99, 160.09, 
           26.87, 158.36, 25.14, 157.42, 23.93, 157.33, 22.01, 
           157.04, 21.25, 155.64, 20.93, 154.49, 20.48, 152.88, 
           19.74, 152.91, 19., 152.93, 18.26, 152.95), 
         ncol = 2, byrow = TRUE),
  # 7 = upperArmR
  matrix(c(13.16, 120.46, 12.18, 120.58, 11.2, 120.7, 10.23, 
           120.81, 9.67, 125.76, 9.34, 130.51, 8.49, 135.3, 
           8.44, 137.16, 8.4, 139.03, 8.36, 140.88, 7.8, 
           144.5, 7.8, 144.5, 7.66, 146.45, 7.94, 147.67, 
           8.22, 148.87, 8.49, 150.08, 9.14, 151.05, 9.79, 
           152.03, 10.44, 152.99, 13.07, 154.01, 14.73, 155.24, 
           17.69, 153.97, 19.91, 150.49, 19.91, 150.49, 21.87, 
           146.87, 21.73, 146.17, 21.59, 145.47, 21.45, 144.77, 
           18.68, 140.01, 19.32, 135.11, 19.23, 129.59, 18.12, 
           126.55, 18.15, 124.69, 18.39, 121.51, 17.94, 121.16, 
           17.5, 120.81, 17.06, 120.46), 
         ncol = 2, byrow = TRUE),
  # 8 = lowerArmR
  matrix(c(16.34, 77.88, 15.96, 78.04, 15.59, 78.2, 15.23, 
           78.36, 14.31, 81.49, 14.34, 84.29, 14.18, 87.42, 
           13.49, 91.08, 13.89, 92.8, 14.88, 96.34, 14., 
           101.9, 12.03, 106.93, 10.42, 112.22, 10.44, 115.08, 
           10.46, 117.94, 10.49, 120.79, 13.58, 122.55, 15.52, 
           122.32, 19.13, 122.25, 19.34, 121.37, 19.55, 120.49, 
           19.76, 119.6, 19.62, 116.12, 19.62, 116.12, 19.62, 
           107.49, 20.13, 102.14, 20.52, 96.78, 20.74, 91.31, 
           21.01, 89.15, 21.27, 87.26, 21.01, 85.05, 19.56, 
           84.01, 19.77, 83.75, 19.48, 81.84, 19.13, 80.89, 
           18.78, 79.95, 18.43, 78.99, 16.62, 77.88, 16.62, 
           77.88, 16.34, 77.88), 
         ncol = 2, byrow = TRUE),
  # 9 = thighL
  matrix(c(14.43, 47.85, 11.78, 49.09, 9.41, 50.27, 7.04, 
           51.76, 6.59, 52.34, 6.15, 52.92, 5.71, 53.5, 
           5.54, 58.84, 4.56, 64.02, 3.77, 69.24, 3.77, 
           75.94, 4.38, 79.72, 4.97, 83.17, 5.99, 86.81, 
           7.29, 88.71, 8.59, 90.61, 9.9, 92.51, 11.3, 
           93.14, 13.76, 94.68, 15.26, 94.68, 18., 93.35, 
           20.74, 92.03, 23.49, 90.7, 23.6, 87.87, 23.71, 
           85.04, 23.83, 82.2, 23.19, 76.54, 22.63, 71.31, 
           21.05, 65.76, 19.35, 60.86, 18.35, 55.73, 16.58, 
           50.85, 16.02, 49.87, 15.46, 48.9, 14.91, 47.92, 
           14.75, 47.9, 14.59, 47.88, 14.43, 47.85), 
         ncol = 2, byrow = TRUE),
  # 10 = shankL
  matrix(c(7.21, 11.62, 5.86, 11.79, 4.51, 11.95, 3.17, 
           12.11, 2.99, 18.35, 2.45, 24.52, 2.12, 30.64, 
           1.84, 31.53, 1.56, 32.42, 1.29, 33.29, 1.29, 
           37.75, 2.77, 42.61, 4.7, 47.35, 6.38, 52.1, 
           11.53, 51.06, 11.53, 51.06, 15.43, 50.43, 15.4, 
           48.44, 15.38, 46.45, 15.36, 44.44, 14.8, 40.68, 
           14.25, 38.03, 13.97, 35.8, 13.68, 31.06, 12.82, 
           26.35, 12.02, 21.59, 11.74, 16.85, 11.74, 16.85, 
           11.67, 12.11, 8.47, 11.62, 8.47, 11.62, 7.21, 
           11.62), 
         ncol = 2, byrow = TRUE),
  # 11 = footL
  matrix(c(17.98, 0.31, 15.52, 0.5, 13.06, 0.69, 10.6, 
           0.87, 8., 0.83, 4.98, 0.42, 2.51, 1.01, 
           0.64, 3.42, 2.22, 6.51, 3.14, 8.88, 3.16, 
           10.05, 3.18, 11.21, 3.21, 12.37, 6.04, 12.4, 
           9.2, 12.74, 12.06, 11.95, 12.49, 11.23, 12.93, 
           10.51, 13.38, 9.79, 16.58, 8.11, 19.79, 6.44, 
           23., 4.77, 25.23, 4.22, 25.23, 4.22, 27.6, 
           3.38, 27.9, 2.99, 28.2, 2.59, 28.5, 2.2, 
           27.88, 0.73, 24.5, 0.17, 21.32, 0.31, 17.98, 
           0.31), 
         ncol = 2, byrow = TRUE),
  # 12 = upperArmL
  matrix(c(13.16, 120.46, 12.18, 120.58, 11.2, 120.7, 10.23, 
           120.81, 9.67, 125.76, 9.34, 130.51, 8.49, 135.3, 
           8.44, 137.16, 8.4, 139.03, 8.36, 140.88, 7.8, 
           144.5, 7.8, 144.5, 7.66, 146.45, 7.94, 147.67, 
           8.22, 148.87, 8.49, 150.08, 9.14, 151.05, 9.79, 
           152.03, 10.44, 152.99, 13.07, 154.01, 14.73, 155.24, 
           17.69, 153.97, 19.91, 150.49, 19.91, 150.49, 21.87, 
           146.87, 21.73, 146.17, 21.59, 145.47, 21.45, 144.77, 
           18.68, 140.01, 19.32, 135.11, 19.23, 129.59, 18.12, 
           126.55, 18.15, 124.69, 18.39, 121.51, 17.94, 121.16, 
           17.5, 120.81, 17.06, 120.46), 
         ncol = 2, byrow = TRUE),
  # 13 = lowerArmL
  matrix(c(16.34, 77.88, 15.96, 78.04, 15.59, 78.2, 15.23, 
           78.36, 14.31, 81.49, 14.34, 84.29, 14.18, 87.42, 
           13.49, 91.08, 13.89, 92.8, 14.88, 96.34, 14., 
           101.9, 12.03, 106.93, 10.42, 112.22, 10.44, 115.08, 
           10.46, 117.94, 10.49, 120.79, 13.58, 122.55, 15.52, 
           122.32, 19.13, 122.25, 19.34, 121.37, 19.55, 120.49, 
           19.76, 119.6, 19.62, 116.12, 19.62, 116.12, 19.62, 
           107.49, 20.13, 102.14, 20.52, 96.78, 20.74, 91.31, 
           21.01, 89.15, 21.27, 87.26, 21.01, 85.05, 19.56, 
           84.01, 19.77, 83.75, 19.48, 81.84, 19.13, 80.89, 
           18.78, 79.95, 18.43, 78.99, 16.62, 77.88, 16.62, 
           77.88, 16.34, 77.88), 
         ncol = 2, byrow = TRUE)
)

