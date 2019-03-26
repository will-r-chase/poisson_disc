

#fast poisson disc sampling algorithm 
#Robert Birdson https://www.cs.ubc.ca/~rbridson/docs/bridson-siggraph07-poissondisk.pdf
#R implementation by William Chase (@W_R_Chase)


#gonna use for loops, might be slow, can update later (lolz yeah right)
#trying with grid as list
#this solution works
#but check that we're actually getting the "end columns"
#also figure out the warnings
#and maybe add back the check to see if the sample point is already in our grid


rad <- 5 #radius for min distance between points
k <- 30 #limit of samples to choose before rejection

#2d background grid
#make 1d vector with index is each cell in background grid
#im using a list for the grid
#also need an active list for "Active" points

#calc dist between 2 points
#can we remove sqrt? would make it faster
eu_dist <- function(a, b) {
  sqrt((a["x"] - b["x"])^2 + (a["y"] - b["y"])^2)
}

grid <- list() #background grid
active <- list() #active list
ordered <- list() #for plotting points
w <- rad/sqrt(2) #cell size is r/sqrt(n) 

#define width and height of canvas
width <- 100
height <- 100

#define # of cols and rows in array
cols <- floor(width/w)
rows <- floor(height/w)

#fill background grid with NA
for(i in 1:(cols*rows)) {
  grid[[i]] <- NA
}

#pick a random point to initialize
x <- runif(1, 0, width)
y <- runif(1, 0, height)
i <- floor(x / w) + 1 #call i the column index
j <- floor(y / w) + 1 #j is row index
pos_init <- c(x = x, y = y)

grid[[(i + j * cols)-cols]] <- pos_init #find the grid index and insert the initial point
active <- c(active, list(pos_init)) #insert initial point into active list

#loop until no more active points
while(length(active) > 0) {
  #choose a random index from the active list
  rand_index <- floor(runif(1, 1, length(active)))
  pos <- active[[rand_index]]
  found <- FALSE
  #print(paste0("total:", total))
  
  for(n in 1:k) {
    #choose a random point between r-2r around the active point
    a <- runif(1, 0, 2*pi)
    m <- runif(1, rad, 2*rad)
    sample <- c(x = m*cos(a), y = m*sin(a)) + pos
    
    #print(paste0("n:", n))
    
    #get col/row index of sample point
    col <- floor(sample["x"]/w) + 1
    row <- floor(sample["y"]/w) + 1
    
    #print(paste0("col:", col))
    #print(paste0("row:", row))
  
    if(col > 1 & row > 1 & col < cols & row < rows) {
      ok <- TRUE
      #check each neighboring square to see if empty
      for(i in -1:1) {
        for(j in -1:1) {
          index <- ((col + i) + (row + j) * cols) - cols
          #print(paste0("index:", index))
          neighbor <- grid[[index]] 
          #if not empty, calc dist b/w sample and point in neighbor
          if(!is.na(neighbor)) {
            d <- eu_dist(sample, neighbor)
            #if they're too close, we wont keep the new point
            if(d < rad) {
              ok <- FALSE
            }
          }
        }
      }
      #print(paste0("ok:", ok))
      #if they're acceptable distance, we keep the sample point and add to active
      if(ok) {
        found <- TRUE
        grid[[(col + row * cols) - cols]] <- sample
        active <- c(active, list(sample))
        ordered <- c(ordered, list(sample))
        break()
      }
    }
  }
  #remove the old active point
  if(!found) {
    active[[rand_index]] <- NULL
  }
}

df <- as.data.frame(Reduce(rbind, ordered))



#test if it worked
library(ggplot2)
ggplot() +
  geom_point(data = df, aes(x = x, y = y), color = "red") +
  lims(x = c(0, width), y = c(0, height)) +
  theme_minimal()

ggsave("poisson_disc_test1.png", device = "png", type = "cairo")
