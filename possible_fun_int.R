# Function to find intersections
find_intersections <- function(df1, df2) {
  intersections <- data.frame()
  for (i in 1:(nrow(df1) - 1)) {
    for (j in 1:(nrow(df2) - 1)) {
      # Check if lines (x1, y1)-(x2, y2) and (x3, y3)-(x4, y4) intersect
      a1 <- df1$Prezzo[i+1] - df1$Prezzo[i]
      b1 <- df1$Quantita.sum[i] - df1$Quantita.sum[i+1]
      c1 <- a1 * df1$Quantita.sum[i] + b1 * df1$Prezzo[i]
      
      a2 <- df2$Prezzo[j+1] - df2$Prezzo[j]
      b2 <- df2$Quantita.sum[j] - df2$Quantita.sum[j+1]
      c2 <- a2 * df2$Quantita.sum[j] + b2 * df2$Prezzo[j]
      
      det <- a1 * b2 - a2 * b1
      if (det != 0) {
        x <- (b2 * c1 - b1 * c2) / det
        y <- (a1 * c2 - a2 * c1) / det
        # Check if intersection point (x, y) is within line segments
        if (x >= min(df1$Quantita.sum[i], df1$Quantita.sum[i+1]) && 
            x <= max(df1$Quantita.sum[i], df1$Quantita.sum[i+1]) && 
            x >= min(df2$Quantita.sum[j], df2$Quantita.sum[j+1]) && 
            x <= max(df2$Quantita.sum[j], df2$Quantita.sum[j+1])) {
          intersections <- rbind(intersections, data.frame(x = x, y = y))
        }
      }
    }
  }
  return(intersections)
}

find_intersections_prediction <- function(c.off, c.bid) {
  intersections <- data.frame()
  for (i in 1:(nrow(c.off) - 1)) {
    for (j in 1:(nrow(c.bid) - 1)) {
      # Check if lines (x1, y1)-(x2, y2) and (x3, y3)-(x4, y4) intersect
      a1 <- c.off[i+1] - c.off[i]
      b1 <- x.synt.off[i] - x.synt.bid[i+1]
      c1 <- a1 * x.synt.bid[i] + b1 * c.off[i]
      
      a2 <- c.bid[j+1] - c.bid[j]
      b2 <- x.synt.bid[j] - x.synt.bid[j+1]
      c2 <- a2 * x.synt.bid[j] + b2 * c.bid[j]
      
      det <- a1 * b2 - a2 * b1
      if (det != 0) {
        x <- (b2 * c1 - b1 * c2) / det
        y <- (a1 * c2 - a2 * c1) / det
        # Check if intersection point (x, y) is within line segments
        if (x >= min(x.synt.off[i], x.synt.off[i+1]) && 
            x <= max(x.synt.off[i], x.synt.off[i+1]) && 
            x >= min(x.synt.bid[j], x.synt.bid[j+1]) && 
            x <= max(x.synt.bid[j], x.synt.bid[j+1])) {
          intersections <- rbind(intersections, data.frame(x = x, y = y))
        }
      }
    }
  }
  return(intersections)
}

find_step_intersections <- function(df1, df2){
  fd.1 <- stepfun(df1$Quantita.sum[-1], df1$Prezzo)
  fd.2 <- stepfun(df2$Quantita.sum[-1], df2$Prezzo)
  
  ## Curve subtraction
  x.min <- max(min(df1$Quantita.sum), min(df2$Quantita.sum))
  x.max <- min(max(df1$Quantita.sum), max(df2$Quantita.sum))
  x_values <- seq(x.min, x.max, length.out = 10000)
  
  # Evaluate fd.off at x_values
  y_off <- fd.1(x_values)
  
  # Evaluate fd.bid at x_values
  y_bid <- fd.2(x_values)
  
  # Compute the difference between the two curves
  difference_curve <- abs(y_off - y_bid)
  
  x.minimizer <- x_values[which.min(difference_curve)]
  
  intersection <- data.frame(x = df2$Quantita.sum[which(df2$Quantita.sum>=x.minimizer)][1], y = fd.1(x.minimizer))
  return (intersection)
}

find_intersections_prediction <- function(c.off, c.bid) {
  intersections <- data.frame()
  for (i in 1:(nrow(c.off) - 1)) {
    for (j in 1:(nrow(c.bid) - 1)) {
      # Check if lines (x1, y1)-(x2, y2) and (x3, y3)-(x4, y4) intersect
      a1 <- c.off[i+1] - c.off[i]
      b1 <- x.synt.off[i] - x.synt.bid[i+1]
      c1 <- a1 * x.synt.bid[i] + b1 * c.off[i]
      
      a2 <- c.bid[j+1] - c.bid[j]
      b2 <- x.synt.bid[j] - x.synt.bid[j+1]
      c2 <- a2 * x.synt.bid[j] + b2 * c.bid[j]
      
      det <- a1 * b2 - a2 * b1
      if (det != 0) {
        x <- (b2 * c1 - b1 * c2) / det
        y <- (a1 * c2 - a2 * c1) / det
        # Check if intersection point (x, y) is within line segments
        if (x >= min(x.synt.off[i], x.synt.off[i+1]) && 
            x <= max(x.synt.off[i], x.synt.off[i+1]) && 
            x >= min(x.synt.bid[j], x.synt.bid[j+1]) && 
            x <= max(x.synt.bid[j], x.synt.bid[j+1])) {
          intersections <- rbind(intersections, data.frame(x = x, y = y))
        }
      }
    }
  }
  return(intersections)
}