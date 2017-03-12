LON<-123.4836
LAP<-41.6401
c(LON,LAP)

w11<-41+37/60+38.24/60/60
j11<-123+29/60+00/60/60
w22<-41+47/60+29.38/60/60
j22<-123+30/60+25.56/60/60
c(w1,j1)
c(w2,j2)


FT<-footpoint(LON,LAP)
distm(c(LON,LAP),FT,fun=distHaversine)

distm(c(j1,w1),c(j2,w2),fun=distHaversine)

distm(c(LON,LAP),c(j11,w11),fun=distHaversine)

distm()




for (i in 1:10000){
  distancesTest<-c(distancesTest,distm(positionMatrix[i,1:2],positionMatrix[i,3:4],fun=distVincentyEllipsoid))
}
