library(tidyverse)
library(rvest)


#a
html <- read_html("https://www.moneyworks4me.com/best-index/nse-stocks/top-nifty50-companies-list/")
tab = html%>%html_table()
data=data.frame(tab[1])
data=data[,-1]

#b1
html2=read_html("https://www.moneyworks4me.com/indianstocks/large-cap/fmcg/consumer-food/britannia-inds/company-info")
x=html2%>%html_table()
data2=data.frame(x[1])
data2=data2[-c(1:5),-c(12:14)]
temp=data.frame(x[3])
temp=temp[-1,-c(12,13)]
data2 <- data2 %>% 
  rename(e=1,Mar13 = 2,Mar14 = 3,Mar15 = 4,Mar16 = 5,Mar17 = 6,Mar18 = 7,Mar19 = 8,Mar20 = 9,Mar21 = 10,Mar22 = 11 )
temp <- temp %>% 
  rename(e=1,Mar13 = 2,Mar14 = 3,Mar15 = 4,Mar16 = 5,Mar17 = 6,Mar18 = 7,Mar19 = 8,Mar20 = 9,Mar21 = 10,Mar22 = 11 )
data2=rbind(data2,temp)
row.names(data2) <- c(1:14)

#b2
html02=read_html("https://www.moneyworks4me.com/indianstocks/large-cap/it-ites/it-software/tech-mahindra/company-info")
x=html02%>%html_table()
data02=data.frame(x[1])
data02=data02[-c(1:5),-c(12:14)]
temp=data.frame(x[3])
temp=temp[-1,-c(12,13)]
data02 <- data02 %>% 
  rename(e=1,Mar13 = 2,Mar14 = 3,Mar15 = 4,Mar16 = 5,Mar17 = 6,Mar18 = 7,Mar19 = 8,Mar20 = 9,Mar21 = 10,Mar22 = 11 )
temp <- temp %>% 
  rename(e=1,Mar13 = 2,Mar14 = 3,Mar15 = 4,Mar16 = 5,Mar17 = 6,Mar18 = 7,Mar19 = 8,Mar20 = 9,Mar21 = 10,Mar22 = 11 )
data02=rbind(data02,temp)
row.names(data02) <- c(1:14)

#b3
html12=read_html("https://www.moneyworks4me.com/indianstocks/large-cap/bfsi/insurance/sbi-life-insuran/company-info")
x=html12%>%html_table()
data12=data.frame(x[1])
data12=data12[-c(1:5),-c(12:14)]
temp=data.frame(x[3])
temp=temp[-1,-c(12,13)]
data12 <- data12 %>% 
  rename(e=1,Mar13 = 2,Mar14 = 3,Mar15 = 4,Mar16 = 5,Mar17 = 6,Mar18 = 7,Mar19 = 8,Mar20 = 9,Mar21 = 10,Mar22 = 11 )
temp <- temp %>% 
  rename(e=1,Mar13 = 2,Mar14 = 3,Mar15 = 4,Mar16 = 5,Mar17 = 6,Mar18 = 7,Mar19 = 8,Mar20 = 9,Mar21 = 10,Mar22 = 11 )
data12=rbind(data12,temp)
row.names(data12) <- c(1:14)

#b4
html22=read_html("https://www.moneyworks4me.com/indianstocks/large-cap/consumer-durables/diamond-jewellery/titan-co/company-info")
x=html22%>%html_table()
data22=data.frame(x[1])
data22=data12[-c(1:5),-c(12:14)]
temp=data.frame(x[3])
temp=temp[-1,-c(12,13)]
data22 <- data22 %>% 
  rename(e=1,Mar13 = 2,Mar14 = 3,Mar15 = 4,Mar16 = 5,Mar17 = 6,Mar18 = 7,Mar19 = 8,Mar20 = 9,Mar21 = 10,Mar22 = 11 )
temp <- temp %>% 
  rename(e=1,Mar13 = 2,Mar14 = 3,Mar15 = 4,Mar16 = 5,Mar17 = 6,Mar18 = 7,Mar19 = 8,Mar20 = 9,Mar21 = 10,Mar22 = 11 )
data22=rbind(data22,temp)
row.names(data22) <- c(1:14)

#b5
html32=read_html("https://www.moneyworks4me.com/indianstocks/large-cap/oil-gas/refineries/reliance-industries/company-info")
x=html32%>%html_table()
data32=data.frame(x[1])
data32=data32[-c(1:5),-c(12:14)]
temp=data.frame(x[3])
temp=temp[-1,-c(12,13)]
data32 <- data32 %>% 
  rename(e=1,Mar13 = 2,Mar14 = 3,Mar15 = 4,Mar16 = 5,Mar17 = 6,Mar18 = 7,Mar19 = 8,Mar20 = 9,Mar21 = 10,Mar22 = 11 )
temp <- temp %>% 
  rename(e=1,Mar13 = 2,Mar14 = 3,Mar15 = 4,Mar16 = 5,Mar17 = 6,Mar18 = 7,Mar19 = 8,Mar20 = 9,Mar21 = 10,Mar22 = 11 )
data32=rbind(data32,temp)
row.names(data32) <- c(1:14)

#c1
tennis <- function(p)
{
  a=rbinom(n=5,size=1,prob=p)
  m=0
  w=0
  l=0
  for(i in a){
    if(w>=3) {return(m)}
    if(l>=3) {return(m)}
    if(i==1){
      w=w+1
    }
    else{
      l=l+1
    }
    m=m+1
    
  }
  return(m)
}

#c2
matches = {}
for(i in 1:1000) 
{
  matches[i] <- tennis(0.7)
}
ans <- mean(matches)

#d
MontyHallold = function(){
  chooseP=sample(x=1:3,size = 1)
  if(chooseP==1) {
    chooseP=sample(x=c(1,3), size=1)
  }
  if(chooseP==2) {
    chooseP=sample(x=c(2,3), size=1)
  }
  else {
    Monty=sample(x=c(1,2), size=1)
    if(Monty==1){
      chooseP=sample(x=c(2,3), size=1)
    }
    else{
      chooseP=sample(x=c(1,3), size=1)
    }
  }
  if(chooseP==3){
    return(1)
  }
  else {
    return(0)
  }
}

MontyHall = function(){
  chooseP=sample(x=1:3,size = 1)
  if(chooseP==1) {
    return(1)
  }
  if(chooseP==2) {
    return(1)
  }
  else {
    return(0)
  }
  if(chooseP==3){
    return(1)
  }
  else {
    return(0)
  }
}

results = {}
for(i in 1:1000) 
{
  results[i] <- MontyHall()
}
ans <- sum(results)/1000


#e
html3 = read_html("https://editorial.rottentomatoes.com/guide/best-netflix-movies-to-watch-right-now/")
z=html3%>%html_table()
rank=html3%>%html_elements(".countdown-index")%>%html_text()
mname=html3%>%html_elements(".article_movie_title a")%>%html_text()
tsc=html3%>%html_elements(".tMeterScore")%>%html_text()
yr=html3%>%html_elements(".start-year")%>%html_text()

data3=data.frame(Ranking=rank, Movie=mname,Tomato_Score=tsc,Year=yr)



  