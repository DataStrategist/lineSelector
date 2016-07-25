library(dplyr)
library(plotly)

Cont <- structure(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                    0, 0, 0, 0, 0, 0, 0, 1816, 2320, 1406, 2028, 1760, 1932, 1630, 
                    1835, 1873, 1474, 1671, 2073, 1347, 2131, 2038, 1969, 2036, 1602, 
                    1986, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                    0, 0, 0, 0, 0, 2311, 1947, 2094, 1947, 2441, 1775, 1461, 1260, 
                    1494, 2022, 1863, 1587, 2082, 1567, 1770, 2065, 1404, 1809, 1972, 
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                    0, 0, 0, 2314, 1595, 2065, 1870, 2178, 1410, 1994, 1979, 2111, 
                    1531, 1917, 1559, 2109, 1921, 1606, 1469, 1601, 1771, 1771), .Dim = c(19L, 
                                                                                       30L))
  Cont[Cont==0] <- NA
  
  ## Interpolate empty data ----------------
  ## First get real control values
  idx <- which(Cont > 0, arr.ind=TRUE)
  V <- Cont[idx]
  ControlValues <- data.frame(idx,V)
  
  ## Make data.frame of other values
  toFill <- which(Cont == 0, arr.ind=TRUE) %>% as.data.frame
  toFill$V <- 0
  
  ## And now figure out the weighted value of each point
  for (i in 1:nrow(toFill)){
    toFill[i,] -> CurrentPoint
    
    Xs <- (1/abs(CurrentPoint[,1] - ControlValues[,1])) 
    Xs[is.infinite(Xs)] <- 0
    Xs <- Xs/sum(Xs)/100
    
    Ys <- (1/abs(CurrentPoint[,2] - ControlValues[,2])) 
    Ys[is.infinite(Ys)] <- 0
    Ys <- Ys/sum(Ys)/100
    
    ControlValues1 <- data.frame(Xs,Ys)
    toFill[i,3] <- sum(rowMeans(ControlValues1) * ControlValues$V)*100
  }
  
  ## add back in the controls and reorder
  bind_rows(ControlValues,toFill) -> Both
  
  Both %>% arrange(row,col) -> Both
  
  
  NewCont <- matrix(Both$V,max(Both$row),max(Both$col),byrow = T)
  plot_ly(z=Cont, type="surface",showscale=FALSE) 
  
  
  library(akima)
  
  
  bicubic(x = 1:nrow(Cont) - 1,
          y = 1:nrow(Cont) - 1,
          z = Cont,
          x0 = toFill$row,
          y0 =toFill$col)
    
  NewCont <- interp(x=ControlValues$row,y = ControlValues$col,z = ControlValues$V,xo = 1:nrow(Cont),yo = 1:ncol(Cont))
  plot_ly(z=NewCont[[3]], type="surface",showscale=FALSE)
  
  plot_ly(z=NewCont[[3]], type="surface",showscale=FALSE,alpha=0.5) %>%
    add_trace(z=Cont, type="surface", showscale=FALSE, colors = terrain.colors(3) )
  