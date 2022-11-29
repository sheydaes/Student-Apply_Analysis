####Q1

#a.
library(ggplot2)
UniversityAdmissions <- read_csv("C:/Users/sheyd/Downloads/UniversityAdmissions.csv")
data=data.frame(value=UniversityAdmissions$`TOEFL Score`)
p <- ggplot(data, aes(x=value)) + geom_histogram(binwidth = 2)

ggplot(data, aes(x=value)) +                 
  geom_histogram(binwidth = 2, aes(y = stat(density))) +
  geom_density(col = "red")

#b.
ggqqplot(UniversityAdmissions$`TOEFL Score`)

#c.
skewness(UniversityAdmissions$`TOEFL Score`)

#d.
plot(UniversityAdmissions$`TOEFL Score`, UniversityAdmissions$`Serial No.`, 
     xlab="TOEFL SCORE", 
     ylab="STUDENTS", 
     pch=4)

#e.
mean(UniversityAdmissions$`TOEFL Score`)
median(UniversityAdmissions$`TOEFL Score`)
var(UniversityAdmissions$`TOEFL Score`)
sd(UniversityAdmissions$`TOEFL Score`)

#f.
ggplot(data, aes(x=UniversityAdmissions$`TOEFL Score`)) + 
geom_histogram(binwidth = 2, aes(y=..density..), colour="black", fill="white")+
geom_density(alpha=.2,fill="#FF6666") + 
geom_vline(aes(xintercept=mean(UniversityAdmissions$`TOEFL Score`)),color="blue",linetype="dashed",size=1) + 
geom_vline(aes(xintercept=median(UniversityAdmissions$`TOEFL Score`)), color="red", linetype="dashed", size=1)

#g.


#h.
boxplot(UniversityAdmissions$`TOEFL Score`,
        main = "TOEFL SCORE",
        col = "orange",
        border = "brown",
        horizontal = TRUE,
        notch = TRUE
)
b <- boxplot(UniversityAdmissions$`TOEFL Score`)
IQR(UniversityAdmissions$`TOEFL Score`)
q1<-quantile(UniversityAdmissions$`TOEFL Score`, 0.25)
q2<-quantile(UniversityAdmissions$`TOEFL Score`, 0.5)
q3<-quantile(UniversityAdmissions$`TOEFL Score`, 0.75)



###Q2
library(plyr)


#a.
factor(UniversityAdmissions$`University Rating`)
w=table(UniversityAdmissions$`University Rating`)
t=as.data.frame(w)
names(t)[1]='rank'
p=t[,2]/500*100


#b.
t$percent<-p[1:5]
c <- ggplot(t, aes(x=`rank`, y=`percent`, fill=`rank`)) +
  +     geom_bar(stat="identity")+theme_minimal()
c + scale_fill_brewer(palette="Dark2")

#c.
newT <- t[order(t$Freq),]
c <- ggplot(newT, aes(x=`rank`, y=`Freq`, fill=`rank`))
  +     geom_bar(stat="identity")+theme_minimal()
c + scale_fill_brewer(palette="Dark2") +  coord_flip()
#d.
v <- ggplot(t, aes(x=rank, y=freq)) + 
  geom_violin()


###Q3

library("ggpubr")

##a.
q <- ggplot(UniversityAdmissions, aes(x=`TOEFL Score`, y=`GRE Score`)) + geom_point()

##b.
res2 <-cor.test(UniversityAdmissions$`GRE Score`, UniversityAdmissions$`TOEFL Score`,  method = "pearson")
res2

##c.
res2$p.value

##d.
##

##e.
##


##f.
ggplot(UniversityAdmissions, aes(x=`TOEFL Score`, y=`GRE Score`, shape=`University Rating`, color=`University Rating`)) + geom_point()


##g.
library(hexbin)
ggplot(UniversityAdmissions, aes(x=`GRE Score`, y=`TOEFL Score`) ) +
  geom_hex(bins = 22) +
  scale_fill_continuous(type = "viridis") +
  theme_bw()

##h.
ggplot(UniversityAdmissions, aes(x=`GRE Score`, y=`TOEFL Score`) ) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", colour="white")

##Q4

##a.
library(GGally)
numeric_data <- data.frame(GRE=UniversityAdmissions$`GRE Score`, Toefl=UniversityAdmissions$`TOEFL Score`, Rank=UniversityAdmissions$`University Rating`, chance= UniversityAdmissions$`Chance of Admit`)
ggpairs(numeric_data, title="correlogram plot", ggplot2::aes(color="blue"), upper = list(continuous = ggally_density, combo = ggally_box_no_facet))


##b.
melted_cormat <- melt(UniversityAdmissions)
head(melted_cormat)
get_upper_tri <- function(UniversityAdmissions){
  UniversityAdmissions[lower.tri(UniversityAdmissions)]<- NA
  return(UniversityAdmissions)
}
get_lower_tri<-function(UniversityAdmissions){
  UniversityAdmissions[upper.tri(UniversityAdmissions)] <- NA
  return(UniversityAdmissions)
}

upper_tri <- get_upper_tri(UniversityAdmissions)
lower_tri <- get_lower_tri(UniversityAdmissions)
library(reshape2)
melted_cormat <- melt(upper_tri, na.rm = TRUE)
library(ggplot2)
ggplot(data = melted_cormat, aes( melted_cormat$variable,melted_cormat$`University Rating`, fill = `value`))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

##c.
library("scatterplot3d")
scatterplot3d(UniversityAdmissions[,4:6], angle = 55,color="steelblue")


##Q5

##a.
frame1<-data.frame(rank = UniversityAdmissions$`University Rating`, research = UniversityAdmissions$Research)
addmargins(table(frame1))


##b.
ggplot(data = frame1, aes(fill = rank, x=research)) +
  geom_bar(position="dodge") +
  geom_text(stat = "count", aes(label = ..count..), position = position_dodge(width = 0.9)) +
  labs(x="research", y = "count")

##c.
ggplot(frame1, aes(fill = rank, x = research)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count.., y = ..count..), position = position_stack(vjust = 0.5)) +
  labs(x="research", y = "count")

##d.
library("ggmosaic")
ggplot(data = frame1) +
  geom_mosaic(aes(x = product(research) , fill=rank)) 

##Q6

##a
library(Rmisc)
CI(UniversityAdmissions$`TOEFL Score`, ci=0.95)

##b.
##

##c.
qsample <- sample(UniversityAdmissions$`TOEFL Score`, 100)
qframe <- data.frame(TOEFL = qsample)
ggplot(qframe, aes(x = qsample)) + 
  geom_histogram(binwidth = 5, fill="blue", color="grey", position="identity", alpha=.1) + 
  labs(title="Sample histogram") +
  geom_vline(aes(xintercept=mean(qsample), color = "mean"), show.legend = TRUE, linetype="dashed") +
  geom_vline(aes(xintercept=106.6576, color = "left"), show.legend = TRUE, linetype="dashed") +
  geom_vline(aes(xintercept=107.7264, color = "right"), show.legend = TRUE, linetype="dashed")


##d.
t.test(UniversityAdmissions$`TOEFL Score`, mu = 107.192, alternative = 'greater')


##e.
##

##f.
sd<-sd(UniversityAdmissions$`TOEFL Score`)
sem = sd/sqrt(500)
q = qnorm(0.5, mean=mu0, sd=sem)
error2<-pnorm(q, mean=mu, sd=sem, lower.tail=FALSE)


##g.
power<- 1- error2


##Q7

sample(UniversityAdmissions,25)



##Q8


##a.
LOR_mean <- mean(UniversityAdmissions$LOR)
CI.percentile(x, probs = c(0.025, 0.975), expand = TRUE, ...)

##b.
sample<-sample(UniversityAdmissions$LOR,20)
CI.bootstrapT(sample, probs = c(0.025, 0.975))

#Q9

library(tidyverse)
library(palmerpenguins)
dat <- UniversityAdmissions %>%
  select(`University Rating`, `Chance of Admit`)
ggplot(dat) +
  aes(x =`University Rating`, y = `Chance of Admit`, color = `University Rating`) +
  geom_jitter() +
  theme(legend.position = "none")

one.way <- aov(UniversityAdmissions$`Chance of Admit` ~ UniversityAdmissions$`University Rating`, data = UniversityAdmissions)
plot(one.way)


