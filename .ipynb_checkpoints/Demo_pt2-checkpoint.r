
library(ggplot2)
library(psych)
library(dplyr)

bfi_data<-bfi #load bfi dataset
head(bfi_data) #view first few cases of dataset

describe (bfi_data) #look at descriptives for each variable

scree(bfi_data[,1:25])

bfi_fa <- fa(bfi_data[1:25],
nfactors = 5,
fm="pa",
max.iter = 100,
rotate = "oblimin")

fa.diagram(bfi_fa)

print(bfi_fa$loadings, cutoff=0, digits=3)

keys.list <-
list(agree=c("-A1","A2","A3","A4","A5"), conscientious=c("C1","C2","C3","-C4","-C5"),
extraversion=c("-E1","-E2","E3","E4","E5"), neuroticism=c("N1","N2","N3","N4","N5"),
openness = c("O1","-O2","O3","O4","-O5")) 


scores <- scoreItems(keys.list,bfi_data,min=1,max=6) #create scores & specify the minimum and maximum values
head(scores$scores) #view scores

scores

bfi_data <- cbind(bfi_data,as.data.frame(scores$scores))
head(bfi_data)

bfi_data %>%
select(gender, education, age, agree, conscientious, extraversion, neuroticism, openness) %>%
lowerCor()

summary(lm(conscientious~ age*factor(gender), data = bfi_data))

ggplot(aes(x=age, y=conscientious, color = factor(gender)),data=bfi_data) +
  geom_smooth(method="lm") +
  scale_color_discrete(name = "Gender", labels = c("Male","Female"))+ 
 labs( x = "Age", y = "Conscientiousness",
 title ="Relationship between Age and Conscientiousness by Gender")
