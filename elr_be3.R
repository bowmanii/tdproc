library(hydrorecipes)
library(data.table)

x <- (1:10000) / (2*pi*60)
y <- sin(x)
y2 <- y * 0.3
plot(y~x, type = 'l')

dat <- data.table(datetime = x,y,y2)
hydrorecipes::be_visual(dat, dep = "y2", ind = "y", time = "x", inverse = FALSE)