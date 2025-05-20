library(packcircles)

day1 <- grouping |> 
  filter(day == 1)

day2 <- grouping |> 
  filter(day == 8)

day3 <- grouping |> 
  filter(day == 9)

day4 <- grouping |> 
  filter(day == 10)

# Adding radii column
datasets <- list(day1, day2, day3, day4)
datasets_with_radii <- lapply(datasets, function(data) {
  data$radius <- data$n * 7
  return(data)
})

#Plot 1.
day1 <- datasets_with_radii[[1]]
day1$x <- 1:dim(day1)[1]/12
day1$y <- runif(length(day1$x), -30, 30)
day1_coords <- circleRepelLayout(day1[,c("x", "y", "radius")],
                                 sizetype="radius",
                                 xlim=c(-50,50), ylim=c(-40,40))
par(bg="#e8e8e8")
plot(day1_coords$layout$x, day1_coords$layout$y, 
     type="n", asp=1, xlab="", ylab="", axes=FALSE)
symbols(day1_coords$layout$x, day1_coords$layout$y, 
        circles=day1_coords$layout$radius, lwd=.3,
        inches=FALSE, add=TRUE)
text(verbs_coords$layout$x[1:5], verbs_coords$layout$y[1:5], verbs$verb[1:5], cex=.75)

