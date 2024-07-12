library(ggplot2)
#1

dni <- data.frame(iris)

ggplot(dni, aes(x = Sepal.Length, y = Petal.Length, )) +
  geom_point(aes(shape = Species, col=Species))+
  labs(caption = "Sepal Length and Petal Length ranges are unique to distinct species")

#2

tx = data.frame(txhousing)
tx=tx[complete.cases(tx),]
ggplot(tx, aes(x = year, y = sales, size=volume)) +
  geom_point(aes(col=city))


#3

d = read.csv("titanic.csv")
surv=(d$Survived==1)
d[surv,2]="Survived"
died = (d$Survived==0)
d[died,2]="Died"
final_Plot=ggplot(d,aes(x= Fare, y= Survived)) +
  geom_boxplot(aes(col=Sex))+
  labs(title = "Fare vs Survival", subtitle = "Irrespective of Sex, rich people survived", y="")
final_Plot
