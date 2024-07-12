library(imager)
library(tidyverse)
library(rvest)

#b
flip = function(a) {
  mat = as.array(a[,,1,])
  dims= dim(mat)
  rev = array(0,dim = dims)
  
  for(i in 1:dims[1]) {
    for(j in 1:dims[2]){
      rev[dims[1]-i, j, ] = mat[i,j,]
    }
  }
  return(as.cimg(rev))
  
}

#c
d = data.frame(ships)

ggplot(data=d, aes(x=service, y=incidents, color=type)) + 
  geom_point(size=2, shape=19) +
  labs(title="Number of Services by Ship Type", x="services", y="incidents")

accidents_summary <- d %>% 
  group_by(type) %>% 
  summarise(total_accidents = sum(incidents))

ggplot(data = accidents_summary, aes(x = type, y = total_accidents)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Total Accidents by Ship Type", x = "Ship Type", y = "Total Accidents") +
  theme_minimal()
#d

html= read_html("https://stats.stackexchange.com/questions?tab=Votes")
title = html%>%html_elements(".s-post-summary--content-title a")%>%html_text()
#views = html%>%html_elements(".s-user-card--rep span")%>%html_text()
ans = html%>%html_elements(".s-post-summary--stats-item-number")%>%html_text()
votes = ans[seq_along(ans) %%3 == 1]
nans = ans[seq_along(ans) %%3 == 2]

#e
tablet = function(n = 100) {
  days = 0  # Initialize days counter
  
  while (n > 0) {
    # Randomly pull one tablet from the bottle
    pulled_tablet = sample(c("whole", "half"), 1, prob = c(num_whole_tablets, 0.5))
    
    if (pulled_tablet == "half") {
      # If pulled tablet is half, we stop the simulation
      days = days + 1
      break
    } else {
      # If pulled tablet is whole, cut it in half and put back in the bottle
      n = n - 1
    }
    
    # Increment days counter
    days = days + 1
  }
  
  return(days)
}
d = {}
for(i in 1:1000){
  d[i]=tablet()
}
mean(d)
views = ans[seq_along(ans) %%3 == 0]
data=data.frame(Title=title, Views=views,Answers=nans,Votes=votes)

