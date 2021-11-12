gb1 <- ggplot_build(p1)
gb2 <- ggplot_build(p2)
n1 <- length(gb1$panel$ranges[[1]]$y.labels)
n2 <- length(gb2$panel$ranges[[1]]$y.labels)
gA <- ggplot_gtable(gb1)
gB <- ggplot_gtable(gb2)
g <- gtable:::rbind_gtable(gA, gB, "last")
panels <- g$layout$t[grep("panel", g$layout$name)]
g$heights[panels] <- list(unit(n1, "null"), unit(n2*5,"null")) # change 5 to other int
grid.newpage()
grid.draw(g)