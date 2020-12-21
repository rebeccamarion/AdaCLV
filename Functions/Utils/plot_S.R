PlotS <- function(S, 
                  binary = F,
                  ylim = NULL, 
                  main = NULL, 
                  col.vals = NULL, 
                  lwd = 4,
                  cex.axis = NULL, 
                  cex.lab = NULL, 
                  cex.main = NULL, 
                  ylab = "", 
                  xlab = "",
                  yaxt = "n"){
  
  if (is.null(col.vals)){
    col.vals <- 1:ncol(S)
  }
  
  # Remove columns of S with NaN values
  wh.nan <- which(apply(S, 2, function(x) mean(is.nan(x))) > 0)
  if (length(wh.nan) > 0){
    S <- S[, -wh.nan]
  }
  
  if(is.null(ylim)){
    ylim <- range(S)
  }
  
  if(is.null(main)){
    main <- ""
  }
  
  
  #### Non-binary S ####
  if (binary == F){
    plot(S[, 1], 
         type = "l",
         ylab = ylab, 
         xlab = xlab,
         ylim = ylim,
         main = main,
         col = col.vals[1],
         lwd = lwd,
         cex.lab = cex.lab,
         cex.axis = cex.axis,
         cex.main = cex.main,
         yaxt = yaxt)
    if (ncol(S) > 1){
      sapply(2:ncol(S), function(x)
        lines(x = 1:nrow(S),
              y = S[, x], 
              col = col.vals[x],
              lwd = lwd))
    }
    
    
    abline(h = 0, col = "black", lwd = lwd)
    
    #### Binary S ####
  } else {
    
    col.index <- apply(S, 1, which.max) 
    wh.zero <- which(apply(S, 1, function(x) sum(abs(x))) == 0)
    cols <- col.vals[col.index]
    # Row j set to white if s_j = 0
    if (length(wh.zero)> 0){
      cols[wh.zero] <- "white"
    }
    
    plot(S[, 1], 
         type = "l",
         ylab = ylab, 
         xlab = xlab,
         ylim = c(0, 1),
         main = main,
         col = "white",
         lwd = lwd,
         cex.lab = cex.lab,
         cex.axis = cex.axis,
         cex.main = cex.main,
         yaxt = yaxt)
    
    rect(xleft = 1:nrow(S) - 1, 
         ybottom = 0, 
         xright = 1:nrow(S), 
         ytop = 1, 
         density = NULL, 
         border = NA, 
         col = cols)
  }
  
  
}