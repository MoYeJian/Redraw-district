# Name: Haiyan Tian
# Course: 44-149 Scientific Computing
# Project 03
# Due Date: April 20
# Brief: Modifying Algorithms and Population Clustering
# By submitting this, I pledge that the code in this file was written by the author indicated above,
#  and that all assistance from outside sources was correctly attributed in comments.  Additionally, I
#  agree to abide by the rules expressed in the CSIS Academic Honesty Policy

# creation of problem solving
census <- read.csv('us_census.csv')
contiguous <- census[!census$state %in% c('AK', 'HI', 'PR'), ]
plot(contiguous$longitude,
     contiguous$latitude,
     type = 'p',
     col = contiguous$state)

N <- 12

chosen_counties <- sample(1:nrow(contiguous), N)


centers <- matrix(0, nrow = N, ncol = 2)

centers[, 1] = contiguous$latitude[chosen_counties]
centers[, 2] = contiguous$longitude[chosen_counties]

#print(centers)


centers_df = contiguous[chosen_counties, 3:4]
#print(contiguous[1,])
#print(contiguous[,1])

deltax <- contiguous[1, 'latitude'] - centers[1, 1]
deltay <- contiguous[1, 'longtitude'] - centers[1, 1]

dist_sq <- function(county, center) {
  #print(county)
  deltax <- county['latitude'] - center[1]
  deltay <- county['longitude'] - center[2]
  deltax ^ 2 + deltay ^ 2
}

belongs_to <- rep(0, nrow(contiguous))

for (i in 1:3) {
  for (county in 1:nrow(contiguous)) {
    closest_center <- 1
    closest_distance <- dist_sq(contiguous[1, ], centers[1, ])
    
    
    
    for (cluster in 2:N) {
      d <- dist_sq(contiguous[county, ], centers[cluster, ])
      if (d < closest_distance) {
        closest_center <- cluster
        closest_distance <- d
      }
    }
    belongs_to[county] <- closest_center
  }
  #print(closest_center)
  
  
  for (i in 1:nrow(centers)) {
    clust_of_interest <- contiguous[belongs_to == i, ]
    total_pop <- sum(clust_of_interest$population)
    new_latitude <-
      sum(clust_of_interest$latitude * clust_of_interest$population) / total_pop
    new_longtitude <-
      sum(clust_of_interest$longitude * clust_of_interest$population) / total_pop
    centers[i, 1] <- new_latitude
    centers[i, 2] <- new_longtitude
  }
  plot(contiguous$longitude,
       contiguous$latitude,
       type = 'p',
       col = belongs_to)
}