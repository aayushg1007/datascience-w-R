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

#d

html= read_html("https://stats.stackexchange.com/questions?tab=Votes")
title = html%>%html_elements(".s-post-summary--content-title a")%>%html_text()
#views = html%>%html_elements(".s-user-card--rep span")%>%html_text()
ans = html%>%html_elements(".s-post-summary--stats-item-number")%>%html_text()
votes = ans[seq_along(ans) %%3 == 1]
nans = ans[seq_along(ans) %%3 == 2]
views = ans[seq_along(ans) %%3 == 0]
data=data.frame(Title=title, Views=views,Answers=nans,Votes=votes)

