ppnorm <- function(x, xbreaks=NULL, xlab=""){
  ipos <- (rank(x)-0.5)/length(x)
  df <- data.frame(x=x, y=ipos)
  params <- MASS::fitdistr(x, "normal")
  df_line <- data.frame(x=seq(min(x), max(x), length.out=100), y=pnorm(seq(min(x), max(x), length.out=100), mean=params$estimate[1], sd=params$estimate[2]))
  p <- ggplot(df, aes(x=x, y=y)) + geom_point() + scale_y_continuous(transform="probit", breaks=c(0.01, 0.01, 0.05, 0.2, 0.5, 0.8, 0.95, 0.99, 0.999)) + 
    coord_cartesian(ylim=c(0.005, 0.9995)) + geom_line(inherit.aes = FALSE, aes(x=x, y=y), data=df_line, color="royalblue") +
    xlab(xlab) + ylab("Proporción acumulada")
  if(!is.null(xbreaks)){
    p <- p + scale_x_continuous(breaks=xbreaks)
  }
  ggplotly(p)
}
