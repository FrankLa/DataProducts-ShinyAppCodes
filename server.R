library(shiny)
## ------------------------------------------------------------------------------------------

quartic <- function(A0=0, A1=0, A2=0, A3=0, A4=1) {
  # The function 
  fun <- function(x){
    A0 + A1*x + A2*x^2 + A3*x^3 + A4*x^4
  }
  
  # 1. Find all roots, real or complex -------------------------------------
  roots <- polyroot(c(A0, A1, A2, A3, A4))
  roots <- round(roots, 4) 
  
  # separate real roots & complex roots
  realRootIndex <- which(Im(roots)==0)
  cplxRootIndex <- which(Im(roots)!=0)
  
  realRoot <- as.numeric(roots[realRootIndex])
  cplxRoot <- roots[cplxRootIndex]
  
  
  # 2. Find all extrema ----------------------------------------------------
  
  # First derivative
  B <- (1:4) * c(A1, A2, A3, A4)
  
  FP <- function(X){
    B[1] + B[2]*X + B[3]*X^2 + B[4]*X^3
  }
  
  extrema <- polyroot(B)
  extrema <- round(extrema, 4)
  
  # separate real extrema
  realExtIndex <- which(Im(extrema)==0)
  # cplxExtIndex <- which(Im(extrema)!=0)
  
  realExt <- as.numeric(extrema[realExtIndex])
  realExt <- unique(realExt)
  # cplxExt <- extrema[cplxExtIndex]
  
  
  # Second derivative
  C <- (1:3) * B[2:4]
  
  FPP <- function(X){
    C[1] + C[2]*X + C[3]*X^2
  }
  
  # Find maxima and minima 
  
  indicator <- sapply(realExt, function(x){
    if(FPP(x) < 0){-1}  # maximum
    else if(FPP(x) > 0){1} # minimum
    else {  
      # 2nd derivative test is inconclusive. Hence, need to calculate signs of f'(x+/-e)
      e <- 1e-3
      if(FP(x-e) > 0 && FP(x+e) < 0){-1} # maximum
      else if(FP(x-e) < 0 && FP(x+e) > 0){1} # minimum
      else {0}
    }
  })
  Xmax <- realExt[which(indicator==-1)]
  Xmin <- realExt[which(indicator==1)]
  
  Ymax <- fun(Xmax)
  Ymin <- fun(Xmin)
  
  maxima <- data.frame(x=Xmax,y=Ymax)
  minima <- data.frame(x=Xmin,y=Ymin)
  
  # 3. Find inflection points -----------------------------------------------
  rootC <- polyroot(C)
  rootC <- round(rootC, 4)
  
  # separate real points
  realRootCIndex <- which(Im(rootC)==0)
  
  realRootC <- as.numeric(rootC[realRootCIndex])
  realRootC <- unique(realRootC)
  
  # Third derivative
  D <- (1:2) * C[2:3]
  
  FPPP <- function(X){
    D[1] + D[2]*X
  }
  
  # for quartic functions, inflections are points where f''(x)=0 and f'''(x) != 0
  indicator2 <- sapply(realRootC, function(x){
    if(FPPP(x) != 0){1}  # inflection 
    else {0} # not inflection
  })
  
  Xinfl <- realRootC[which(indicator2==1)]
  Yinfl <- fun(Xinfl)
  
  inflection <- data.frame(x=Xinfl,y=Yinfl)
  
  # Return
  results <- list(fun=fun, realRoot=realRoot, complexRoot=cplxRoot, 
                  minima=minima, maxima=maxima, inflection=inflection)
  results
}

## -----------------------------------------------------------------------------------

eqnFormula <- function(A0,A1,A2,A3,A4){
  if(A0 == 0 && A1 == 0 && A2 ==0 && A3 == 0 && A4 ==0) {
    text <- '$$f(x) \\equiv 0$$'
  } else {
    if (A0 > 0) t0 <- sprintf("+%.2f",A0)
    else if (A0 < 0) t0 <- sprintf("-%.2f",-A0)
    else t0 <- ''
    
    if (A1 > 0) t1 <- sprintf("+%.2f x",A1)
    else if (A1 < 0) t1 <- sprintf("-%.2f x",-A1)
    else t1 <- ''
    
    if (A2 > 0) t2 <- sprintf("+%.2f x^2",A2)
    else if (A2 < 0) t2 <- sprintf("-%.2f x^2",-A2)
    else t2 <- ''
    
    if (A3 > 0) t3 <- sprintf("+%.2f x^3",A3)
    else if (A3 < 0) t3 <- sprintf("-%.2f x^3",-A3)
    else t3 <- ''
    
    if (A4 > 0) t4 <- sprintf("%.2f x^4",A4)
    else if (A4 < 0) t4 <- sprintf("-%.2f x^4",-A4)
    else t4 <- ''
    
    text <- paste0('$$f(x) =',t4,t3,t2,t1,t0,'= 0$$')
  }
  text
}


## ------------------------------------------------------------------------------------

shinyServer(function(input, output) {
  
  eqnText <- reactive({
    eqnFormula(input$A0, input$A1, input$A2, input$A3, input$A4)
  })
  
  output$eqn <- renderUI({
    withMathJax(
      eqnText()
    ) 
  })
  
  tempList <- reactive({
    quartic(input$A0, input$A1, input$A2, input$A3, input$A4)
  })
  
  output$realRoot <- renderText({
    tmp <- tempList()
    if(length(tmp$realRoot) > 0) tmp$realRoot
    else "none"
  })
  
  output$cplxRoot <- renderText({
    tmp <- tempList()
    if(length(tmp$complexRoot) > 0) tmp$complexRoot
    else "none"
  })
  
  output$minimum <- renderTable({
    tmp <- tempList()
    if(nrow(tmp$minima) > 0) tmp$minima
    else as.matrix("none")
  }, digits=4)
  
  output$maxima <- renderTable({
    tmp <- tempList()
    if(nrow(tmp$maxima) > 0) tmp$maxima
    else as.matrix("none")
  }, digits=4)
  
  output$inflection <- renderTable({
    tmp <- tempList()
    if(nrow(tmp$inflection) > 0) tmp$inflection
    else as.matrix("none")
  }, digits=4)
  
  output$graph <- renderPlot({
    
    tmp <- tempList()
    
    Xmax <- tmp$maxima[,1]
    Xmin <- tmp$minima[,1]
    Xinfl <- tmp$inflection[,1]
    
    Ymax <- tmp$maxima[,2]
    Ymin <- tmp$minima[,2]
    Yinfl <- tmp$inflection[,2]
    
    allX <- c(tmp$realRoot, Xinfl, Xmax, Xmin)
    upper <- max(allX)
    lower <- min(allX)
    range <- upper - lower
    
    Xfrom <- lower-min(1,0.15*range)
    Xto <- upper+min(1,0.15*range)
    
    if(Xto - Xfrom == 0){
      Xto <- 1
      Xfrom <- -1
    }
    
    fun <- tmp$fun
    
    curve(fun, from = Xfrom, to = Xto, n = 501, ylab = 'f(x)')
    points(tmp$realRoot, rep(0,length(tmp$realRoot)), pch = 20)
    points(Xmax, Ymax, pch = 2, col = "red", cex = 1)
    points(Xmin, Ymin, pch = 6, col = "blue", cex = 1)
    points(Xinfl, Yinfl, pch = 8, col = "green", cex = 1)
    abline(h=0, v=0, lty=2)
    
    legend("bottomleft", legend = c("Root", "Maximum", "Minimum", "Inflection"), 
           bty = "n", lwd = 1, cex = 1, col = c("black", "red", "blue", "green"), 
           lty = c(NA, NA, NA, NA), pch = c(20, 2, 6, 8))
  },
  height = 560,
  width = 560
  )
  
  
  output$complexPlot <- renderPlot({
    tmp <- tempList()
    
    x <- c(tmp$realRoot, tmp$complexRoot)
    plot(x, col = "red", pch = 20)
    abline(h=0, v=0)
    segments(Re(x), Im(x), rep(0,length(x)), rep(0,length(x)), lty = 2)
  },
  height = 560,
  width = 560)
})
