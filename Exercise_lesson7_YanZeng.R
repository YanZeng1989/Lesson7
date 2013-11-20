
###########Exercise of Lesson 7 Yan Zeng

sceneinfo$year<-format(sceneinfo$date,"%Y")

a<-sceneinfo$year==2000
which(a)
b<-sceneinfo$year==2005
which(b)
c<-sceneinfo$year==2010
c
which(c)
ndvi2000<-calc(tura[[which(a)]],fun = mean, na.rm = TRUE)
plot(ndvi2000)
ndvi2005<-calc(tura[[which(b)]],fun = mean, na.rm = TRUE)
plot(ndvi2005)
ndvi2010<-calc(tura[[which(c)]],fun = mean, na.rm = TRUE)
plot(ndvi2010)

Rasterchange<-brick(ndvi2000,ndvi2005,ndvi2010)
plot(Rasterchange)
plotRGB(x=Rasterchange,1,2,3,stretch="hist")

#####draw poly of decrease change
#change1<-drawPoly(sp=TRUE)#####Red color in composite image
load("change1.RData")
projection(change1) <- projection(tura)
Change1<-as.data.frame(extract(tura,change1))
Change1M<-colMeans(Change1, na.rm = T, dims = 1)
Change1 <- data.frame(date = sceneinfo$date,
                      sensor = sceneinfo$sensor,
                      ndvi = Change1M,
                      class="decrease00_05"
)
Change1<- Change1[which(Change1$sensor!="TM"), ]
#########draw poly of increase change
#change2<-drawPoly(sp=TRUE)###green in composite image
load("change2.RData")
projection(change2) <- projection(tura)
plot(change2, add=TRUE)
Change2<-as.data.frame(extract(tura,change2))
Change2M<-colMeans(Change2, na.rm = T, dims = 1)
Change2 <- data.frame(date = sceneinfo$date,
                      sensor = sceneinfo$sensor,
                      ndvi = Change2M,
                      class="increase00_05"
)
Change2<- Change2[which(Change2$sensor!="TM"), ]

##########increase 05-10

#change3<-drawPoly(sp=TRUE)####blue in image 
load("change3.RData")
projection(change3) <- projection(tura)
plot(change3, add=TRUE)
Change3<-as.data.frame(extract(tura,change3))
Change3M<-colMeans(Change3, na.rm = T, dims = 1)
Change3 <- data.frame(date = sceneinfo$date,
                      sensor = sceneinfo$sensor,
                      ndvi = Change3M,
                      class="increase05_10"
)

Change3<- Change3[which(Change3$sensor!="TM"), ]


#Increase from 05-10
#change4<-drawPoly(sp=TRUE)###green in image
load("change4.RData")
projection(change4) <- projection(tura)
plot(change4, add=TRUE)
Change4<-as.data.frame(extract(tura,change4))
Change4M<-colMeans(Change4, na.rm = T, dims = 1)
Change4 <- data.frame(date = sceneinfo$date,
                      sensor = sceneinfo$sensor,
                      ndvi = Change4M,
                      class="decrease05_10")

Change4<- Change4[which(Change4$sensor!="TM"), ]
#########Plot with two polygon
plotRGB(x=Rasterchange,1,2,3,stretch="hist")
plot(change1, add=TRUE)
plot(change2,add=TRUE)
plot(change3,add=TRUE)
plot(change4,add=TRUE)

Change<-rbind(Change1,Change2,Change3,Change4)

ggplot(data=Change, aes(x=date, y=ndvi)) +
  geom_point() +
  facet_wrap(~ class, nrow=4) +
  theme_bw()

