# Some intro R exercises

3 + 4
x<-c(1,1,2)
3x
3*x
x^3
t(x)%*%x
cbind(x,x)
rbind(x,x)
x*x
x+as.factor(x)

y<-matrix(runif(500^2),nrow=500)
dim(y)
library(Matrix)
y[y<0.8]<-0
y[y>0.8]<-1
Y<-Matrix(y)

rt<-proc.time()
C<-y%*%y%*%y
proc.time()-rt

rt<-proc.time()
C<-Y%*%Y%*%Y
proc.time()-rt


# Cars fun
head(cars)
is.data.frame(cars)
summary(cars$dist)
table(cars$speed)
cars$speed[cars$speed<10]<-"Slow"
rm(cars)
head(cars)
dim(cars)
sp<-numeric(50)
sp[cars$speed<10]<-"Slow"
sp[cars$speed>=10 & cars$speed<20]<-"Medium"
sp[cars$speed>=20]<-"Fast"
cars$speed<-sp
head(cars)
head(as_tibble(cars))
cars %>% group_by(speed) %>% summarize(Mean_dist=mean(dist))


# Estimate the expected value of a truncated normal distribution
# truncated between 0 and 1
mu=1
sigma=2
alpha=(0-mu)/sigma
beta=(1-mu)/sigma
x<-rnorm(10000,mu,sigma)
w<-(x>0)&(x<1)
ind<-which(w==TRUE)
mean(x[ind])

# true value
mu+(dnorm(alpha)-dnorm(beta))/(pnorm(beta)-pnorm(alpha))*sigma


# compute the sum of n^p

int.power<-function(n,p){
  # n is nonnegative integers
  # p is a (possibly negative) integer
  # compute the sum of x^p for x=1,2,...,n
  return(sum(c(1:n)^p))
}

sapply(1:10,function(x) int.power(x,1))
sapply(1:10,function(x) int.power(x,2))
sqrt(int.power(100,-2)*6)
sqrt(int.power(1000,-2)*6)
sqrt(int.power(10000,-2)*6)

int.power(5.4444,2.4)
int.power(5,2.4)
1:5.4444


library(gapminder)
head(gapminder)
table(gapminder$year)
gap2007<-gapminder %>% filter(year==2007) 
head(gap2007)
gap2007 %>% group_by(continent) %>% summarize(lifeExp=median(lifeExp))

library(socviz)
dim(gss_sm)
gss<-na.omit(gss_sm)
dim(gss)
names(gss)
gss %>% group_by(bigregion, religion) %>% summarize(n=length(id)) %>%
  ungroup %>% group_by(bigregion) %>% 
  mutate(proportion = n / sum(n))



# Fun with mushrooms

mushroom <- read.csv("~/Documents/R/Notes 705/mushroom.csv", header=FALSE)
mushroom <- as_tibble(mushroom)
head(mushroom)

mushroom <- mushroom %>% mutate_if(is.character,as.factor)
head(mushroom)
colnames(mushroom) <- c("edibility", "cap_shape", "cap_surface", 
                        "cap-color", "bruises", "odor", 
                        "gill-attachement", "gill-spacing", "gill-size", 
                        "gill-color", "stalk-shape", "stalk-root", 
                        "stalk-surface-above-ring", "stalk-surface-below-ring", 
                        "stalk-color-above-ring", 
                        "stalk-color-below-ring", "veil-type", "veil-color", 
                        "ring-number", "ring-type", "spore-print-color", 
                        "population", "habitat")

head(mushroom)

table(mushroom$edibility)
table(mushroom$edibility,mushroom$odor)


ggplot(data=mushroom, aes(fill=edibility,x=odor)) + geom_bar(position="dodge")+
 scale_fill_discrete(name = "Edibility", labels = c("Edible", "Poisonous"))+
  scale_x_discrete(labels=c("Almond", "Creosote", "Foul", "Anise", "Musty", "None",
                            "Pungent", "Spicy", "Fishy"))+
  xlab("Odor") + 
  ylab("Count") 
