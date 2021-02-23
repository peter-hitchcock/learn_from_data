RunLRExp <- function(n, line_pts=0) {
  ### Run linear regression experiment including generating random points, either #
  # inputting the line marker (E_out) or generating it #
  # n = no. data points to use each exp ###
  set.seed(runif(1, 1, 1000))
  # Generate line pts if don't already have them 
  #browser()
  if (!line_pts) line_pts <- DefLinePts()
  xdt <- GenXPointsAndY(line_pts, n) 
  p <- ggplot(xdt, aes(x1, x2, fill=as.factor(y))) + 
    geom_point(pch=21, size=6) + 
    geom_line(inherit.aes=FALSE, linetype="dashed", color="black", data=line_pts, size=2, aes(pt1, pt2)) + 
    ga + ap + scale_fill_manual(values=cf_vals) + lp 
  plots <- p
  # Matrix ops
  X <- as.matrix(xdt %>% select(contains("x")))
  y <- as.matrix(xdt %>% select("y"))
  X_cross <- solve(t(X) %*% X) %*% t(X) # X_cross (pseudo inverse) = (X^T X)^-1 * X^T
  w_lin <- X_cross %*% y
  y_hat <- X %*% w_lin
  # Classify pts and calculate error 
  y_class <- ifelse(y_hat < 0, -1, 1) 
  E_in <- 1 - length(which(y_class == y))/length(y)
  stuff_to_keep <- list("plots"=plots, "w_lin"=t(w_lin), "xdt"=xdt, "E_in"=E_in, "line_pts"=line_pts)
}
out_exp_res <- lapply(1:1000, function(i) RunLRExp(1000, line_pts[[i]]))