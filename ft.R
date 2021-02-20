w <- rep(0, ncol(line_pts)+1) # Init weights (dim + bias)
weights <- list() # Store so can look at dynamics
i <- 0
while (any(xdt$y!=xdt$y_pred)) { 
  i <- i + 1
  rando <- round(runif(1, 1, nrow(xdt)), 0)
  obs <- data.frame(xdt[rando, ]) # Pick random observation 
  x_rand <- obs[grep("x", names(obs))]
  w <- w + as.numeric(x_i)*as.numeric(xdt$y[rando]) # Weight update
  weights[[i]] <- w
  # Update classification if misclassified 
  if (obs$y!=obs$y_pred) {
    xdt$y_pred[rando] <- sign(t(as.numeric(w)) %*% as.numeric(x_rand))
  } 
  print(xdt$y==xdt$y_pred)
}