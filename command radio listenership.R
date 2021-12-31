# communications science

setwd("D:/project/bu aini ilkom")
dat <- read.csv("commdata.csv",sep=";" ,header=T, na.strings = c(NA, ""))
str(dat)
summary(dat)

#descriptive
#persebaran data
windows()
hist(dat$age, main="Persebaran Usia Responden", col=5, ylab="Frekuensi",xlab="Usia",las=1, ylim=c(0,80),xlim=c(10,70))

# pengetahuan ttg radio
windows()
pct <- round(table(dat$RadioKnowing)/270*100,1)
lg <- c("tidak tahu","tahu       ")
pie(table(dat$RadioKnowing), labels=paste(lg), col=c(4,6), edges=50)
legend("topright",inset=c(0.005,0.01),leg=paste(lg," ", pct, "%" ),lwd=1,x.intersp=0.2,
       col=c(4,6),title="Pengetahuan ttg Radio",bg="ivory",cex=1,y.intersp=0.8, pch=15)
library(plotrix)
windows()
pie3D(table(dat$RadioKnowing), labels=paste(lg), col=c(4,6), theta=1, radius =1)
legend("topright",inset=c(0.005,0.01),leg=paste(lg," ", pct, "%" ),lwd=1,x.intersp=0.2,
       col=c(4,6),title="Pengetahuan ttg Radio",bg="ivory",cex=1,y.intersp=0.8, pch=15)

# terakhir mendengarkan radio
windows() 
par(las=1,mar=c(0,0.5,0.5,0.3),oma=c(3,3,3,0.8),tcl=0,mgp=c(1,0.1,0))
pct <- round(table(dat$LastTime)/270*100,1)
lg <- c("< 1 bulan","> 2 bulan", "> 1 tahun")
pie(table(dat$LastTime), labels=paste(lg), col=c(4,5,6), edges=50)
legend("topleft",inset=c(0.005,0.01),leg=paste(lg," ", pct, "%" ),lwd=1,x.intersp=0.2,
       col=c(4,5,6),title="Terakhir kali mendengarkan Radio",bg="ivory",cex=1,y.intersp=0.8, pch=15)
library(plotrix)
windows()
par(las=1,mar=c(0,0.5,0.5,0.3),oma=c(3,3,3,0.8),tcl=0,mgp=c(1,0.1,0))
pie3D(table(dat$LastTime), labels=paste(lg), col=c(4,5,6), theta=1, radius =1)
legend("topright",inset=c(-0.15,-0.15),leg=paste(lg," ", pct, "%" ),lwd=1,x.intersp=0.2,
       col=c(4,5,6),title="Terakhir kali mendengarkan Radio",bg="ivory",cex=1,y.intersp=0.8, pch=15)

# Radio Listenership
windows()
pct <- round(table(dat$willListenRadio.)/270*100,1)
lg <- c("tidak","ya")
pie(table(dat$willListenRadio.), labels=paste(lg), col=c(4,6), edges=50)
legend("topright",inset=c(0.005,0.01),leg=paste(lg," ", pct, "%" ),lwd=1,x.intersp=0.2,
       col=c(4,6),title="Radio Listenership",bg="ivory",cex=1,y.intersp=0.8, pch=15)
library(plotrix)
windows()
pie3D(table(dat$willListenRadio.), labels=paste(lg), col=c(4,6), theta=1, radius =1, explode = 0.2)
legend("topright",inset=c(0.005,0.01),leg=paste(lg," ", pct, "%" ),lwd=1,x.intersp=0.2,
       col=c(4,6),title="radio listenership",bg="ivory",cex=1,y.intersp=0.8, pch=15)

# percentage
prop.table(summary(dat))

# radio
library(ggplot2)
windows()
ggplot(dat, aes(fill=reason, y=age, x=willListenRadio., na.rm=T)) +
        geom_bar( position="dodge", stat="identity",na.rm=T, alpha=0.7)

tab1 <- t(prop.table(table(dat$Streaming))*100)
tab2 <- t(prop.table(table(dat$VideoStreaming))*100)
tab3 <- t(prop.table(table(dat$UtubePonRadio))*100)
tab4 <- t(prop.table(table(dat$sosmedRadio))*100)
tab <- rbind(tab1,tab2, tab3,tab4)
var <- c("radio streaming", "video streaming", "youtube streaming", "sosmed radio")
newtab <- as.data.frame(cbind(var,tab))
a <- newtab[,c(1:2)]
will <- rep(c("tidak","ya"), each=4)
b <- newtab[,c(1,3)]
colnames(a)[colnames(a)=="Tidak"] <- "freq"
colnames(b)[colnames(b)=="Ya"] <- "freq"
c<- rbind(a,b)
c<- cbind(c,will)
c$freq <- as.numeric(as.character(c$freq))
windows()
ggplot(c, aes(fill=var, x=will, y=freq, na.rm=T)) +
        geom_bar( position="dodge", stat="identity",na.rm=T, alpha=0.7)+
        labs(x="kemauan", y="percentase", fill="jenis radio")+
        ylim(0,100)

tab1 <- t(prop.table(table(dat$feelingRS))*100)
colnames(tab1) <- c("mudah","susah")
tab2 <- t(prop.table(table(dat$feelingVS))*100)
colnames(tab2) <- c("mudah","susah")
tab3 <- t(prop.table(table(dat$FeelingUtubeRadio))*100)
colnames(tab3) <- c("mudah","susah")
tab <- rbind(tab1,tab2, tab3)
var <- c("radio streaming", "video streaming", "youtube streaming")
newtab <- as.data.frame(cbind(var,tab))
a <- newtab[,c(1:2)]
will <- rep(c("mudah","susah"), each=3)
b <- newtab[,c(1,3)]
colnames(a)[colnames(a)=="mudah"] <- "freq"
colnames(b)[colnames(b)=="susah"] <- "freq"
c<- rbind(a,b)
c<- cbind(c,will)
c$freq <- round(as.numeric(as.character(c$freq), digit=2))
ggplot(c, aes(fill=var, x=will, y=freq, na.rm=T)) +
        geom_bar( position="dodge", stat="identity",na.rm=T, alpha=0.7)+
        labs(x="kemudahan", y="persentase", fill="jenis radio")+
        ylim(0,100)

df <- as.data.frame(round(prop.table(table(dat$LastTime))*100, digits=2))
levels(df$Var1) <- c("< sebulan", "> 2 bulan", "> setahun")
windows()
ggplot(df, aes(fill=Var1, y=Freq, x=Var1, na.rm=T)) +
        geom_bar( position="dodge", stat="identity",na.rm=T, alpha=0.7)+
        labs(title="Terakhir kali mendengarkan radio", x="waktu", y="persentase")+
        ylim(0,60)+
        geom_text(aes(label=Freq), hjust=1.6, color="white", size=3.5)+
        coord_flip()
windows()
ggplot(df, aes(fill=Var1, y=Freq, x="", na.rm=T)) +
        geom_bar( width=1 , stat="identity",na.rm=T, alpha=0.7)+
        coord_polar("y")+
        geom_text(aes(y = Freq + c(30,10, -10 ), 
                      label = percent(Freq/100)), size=5)+
        labs(title="", x="", y="", fill="Terkahir mendengarkan")



df <- as.data.frame(round(prop.table(table(dat$media))*100, digits=2))
levels(df$Var1) <- c("internet", "konvensional")
ggplot(df, aes(fill=Var1, y=Freq, x=Var1, na.rm=T)) +
        geom_bar( position="dodge", stat="identity",na.rm=T, alpha=0.7)+
        labs(title="", x="radio", y="persentase")+
        ylim(0,100)+
        geom_text(aes(label=Freq), hjust=1.6, color="white", size=3.5)+
        coord_flip()
windows()
ggplot(df, aes(fill=Var1, y=Freq, x=Var1, na.rm=T)) +
        geom_bar( position="dodge", stat="identity",na.rm=T, alpha=0.7)+
        labs(title="", x="radio", y="persentase")+
        coord_polar("y", start=0)

library(scales)
windows()
ggplot(df, aes(fill=Var1, y=Freq, x="", na.rm=T)) +
        geom_bar( width=1 , stat="identity",na.rm=T, alpha=0.7)+
        coord_polar("y")+
        geom_text(aes(y = Freq/2 - c(-85, cumsum(Freq)[-length(Freq)]), 
        label = percent(Freq/100)), size=5)+
        labs(title="", x="", y="", fill="Radio")
        


# UNIVARIATE
library(epiDisplay)
names(dat)
tableStack(data=dat, by=willListenRadio., vars=c(RadioKnowing, LastTime,Place,reason,
           media, Streaming, lastRS,feelingRS, VideoStreaming, feelingVS, UtubePonRadio,
           FeelingUtubeRadio, websiteRadio, feelingWR, sosmedRadio,whatSosmed, whatInfo,
           reasonSosmedRadio, sosmedRadioPreferTo),na.rm=T)

# LastTime,media,Streaming, feelingRS, VideoStreaming,UtubePonRadio,feelingWR, sosmedRadio,reasonSosmedRadio