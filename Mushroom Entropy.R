#Mantar ile ilgili bir veri seti amaç mantar yenilebilir ve yenilemez olarak ayırma#
#Hedef(target) değişkenimiz edible(yenilebilir)#
#zehirli mantarlarda var#
#veri Analitiği ile mantarları zehirli ve ya yenilebilir olarak ayırıcaz#
#edible -> yenilebilir# 
#bruise ->yara,iz#
#odor -> kokusu#
#store_pr_color -> rengi#

library(readr)
a <- read_csv("mushroom_entropy.csv")
View(a)

table(a$edible)

prop.table(table(a$edible))

#sırayla entropy bul sonra ig hesapla --> doğru ayırmayı bul#

table(a$bruise)

table(a$odor)

table(a$store_pr_color)

#################################################

table(a$edible, a$bruise)

table(a$edible, a$odor)

table(a$edible, a$store_pr_color)
install.packages("entropy")
library(entropy)

#entropy root

ep <- entropy(table(a$edible),unit = "log2")#heterojen bir set#

#odor için entropy ve ig#

e_n <- entropy(table(a$edible[a$odor=="n"]),unit = "log2")

prop_n = sum(a$odor=="n")/length(a$odor)

ig_odor = ep-prop_n*e_n 

#bruise için entropy ve ig#

e_f <- entropy(table(a$edible[a$bruise=="FALSE"]),unit = "log2")

e_t <- entropy(table(a$edible[a$bruise=="TRUE"]),unit = "log2")

prop_t= sum (a$bruise=="TRUE")/length(a$bruise)

prop_f= sum (a$bruise=="FALSE")/length(a$bruise)

ig_bruise = ep-(prop_t*e_t+prop_f*e_f) 

#İG EN BÜYÜK OLAN HOMOJENLİĞE YAKLAŞTIRDIĞINDAN ODOR DEĞERLİ OLAN#

#Store_pr_color için entropy ve ig#

e1 <- entropy(table(a$edible[a$store_pr_color=="k"]),unit = "log2")

e2 <- entropy(table(a$edible[a$store_pr_color=="n"]),unit = "log2")

e3 <- entropy(table(a$edible[a$store_pr_color=="w"]),unit = "log2")

p1= sum (a$store_pr_color=="k")/length(a$store_pr_color)

p2= sum (a$store_pr_color=="n")/length(a$store_pr_color)

p3= sum (a$store_pr_color=="w")/length(a$store_pr_color)

ig_store = ep-(p1*e1+p2*e2+p3*e3) 

#İG EN BÜYÜK OLAN HOMOJENLİĞE YAKLAŞTIRDIĞINDAN ODOR DEĞERLİ OLAN#
#HOMOJENLİĞE EN YAKIN OLAN SEÇİLDİKTEN SONRA GRUPLA#
#ODOR = c,f,m,l zehirli yeme"#

#Odor ile belirlenen Segmentler#

segment1 <- subset(a, subset = odor %in% c("a","l","n"))

segment2 <- subset(a, subset = odor %in% c("c","f","m","p"))

table(segment1$edible)

table(segment2$edible)

#Entropy Segment 1# 

e_s1 <- entropy(table(segment1$edible),unit = "log2")

#bruise için entropy ve ig#

e_f <- entropy(table(segment1$edible[segment1$bruise=="FALSE"]),unit = "log2")

e_t <- entropy(table(segment1$edible[segment1$bruise=="TRUE"]),unit = "log2")

prop_t= sum (segment1$bruise=="TRUE")/length(segment1$bruise)

prop_f= sum (segment1$bruise=="FALSE")/length(segment1$bruise)

ig_bruise = e_s1-(prop_t*e_t+prop_f*e_f) 

#Store_pr_color için entropy ve ig#

e1 <- entropy(table(segment1$edible[segment1$store_pr_color=="k"]),unit = "log2")

e2 <- entropy(table(segment1$edible[segment1$store_pr_color=="n"]),unit = "log2")

e3 <- entropy(table(segment1$edible[segment1$store_pr_color=="w"]),unit = "log2")

p1= sum (segment1$store_pr_color=="k")/length(segment1$store_pr_color)

p2= sum (segment1$store_pr_color=="n")/length(segment1$store_pr_color)

p3= sum (segment1$store_pr_color=="w")/length(segment1$store_pr_color)

ig_store = e_s1-(p1*e1+p2*e2+p3*e3)

table(segment1$edible,segment1$store_pr_color)

install.packages("rpart")

install.packages("rpart.plot")

library(rpart)

library(rpart.plot)

tree <- rpart(edible~.,method="class",data=a)

rpart.plot(tree)

rpart.rules(tree, cover=T)

summary(tree)
