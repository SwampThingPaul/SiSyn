# WRTDS -------------------------------------------------------------------
## WRTDS percent change files from KJ

pctchge=read.csv(paste0(data.path,"WRTDS/WRTDS_pctchange_map.csv"))
unique(pctchge$likeC)
range(pctchge$pConc,na.rm=T)

unique(pctchge$likeF)
range(pctchge$pFlux,na.rm=T)

unique(pctchge$LTER)
# Edits to fit site file
# subset(pctchge,SITE=="Toolik Inlet")
# pctchge[pctchge$SITE=='Toolik Inlet',]$LTER="ARC"
## Removed Toolik Inlet due to NAs
pctchge=subset(pctchge,SITE!="Toolik Inlet")

subset(pctchge,LTER=="NWT")
pctchge[pctchge$SITE=='ALBION',]$SITE="Albion"
pctchge[pctchge$SITE=='MARTINELLI',]$SITE="Martinelli"
pctchge[pctchge$SITE=='SADDLE STREAM 007',]$SITE="Saddle"

subset(pctchge,LTER=="GRO")
pctchge[pctchge$SITE=='Ob',]$SITE="Ob'"

subset(pctchge,LTER=="Sagehen")
pctchge[pctchge$SITE=='Sagehen',]$SITE="Sagehen Creek"



canal=spTransform(readOGR("C:/Julian_LaCie/_GISData/SFER_GIS_Geodatabase.gdb","SFWMD_Canals"),wgs84)

cols=wesanderson::wes_palette("Zissou1",length(unique(pctchge$LTER)),"continuous")
sites2=subset(sites,LTER%in%unique(pctchge$LTER))
sites2$Site.Stream=with(sites2@data,ifelse(LTER=="UMR",Unique.ID,Site.Stream))

sort.LTERs=sort.LTERs[sort.LTERs%in%unique(pctchge$LTER)]
sites2$LTER=factor(sites2$LTER,levels=sort.LTERs)

site.cols=cols[sites2$LTER]

bks=c(-50,-25,0,100,200,300) # seq(-50,300,10)
bks.int=findInterval(pctchge$pConc,bks)
pal=colorRampPalette(c("blue","grey","red"))
# cols2=adjustcolor(pal(length(bks)),0.75)
# pctchge$pConc.cols=cols2[bks.int]#viridis::magma(length(bks))[bks.int]

bks.int=findInterval(pctchge$pConc,bks)
bks.size=seq(1,4, along.with=bks)#length.out=length(bks))
pctchge$pConc.size=bks.size[bks.int]

# WBT.cols=viridis::inferno(7)
# plot(rep(1,7)~seq(0,1,along.with=WBT.cols),pch=21,bg=WBT.cols,cex=2)
# WBT=data.frame(WBT.txt=c("HLikely","VLikely","Likely","About","Unlikely","VUnlikely","HUnlikely"),
#               WBT.col=WBT.cols)
WBT=data.frame(likeC=c("highly","very","likely","",NA),
               WBT.col=c(pal(4)[1:3],pal(4)[4],pal(4)[4]))


# png(filename=paste0(plot.path,"WRTDS_PchangeMap.png"),width=8,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",oma=c(0.25,0.25,0.25,0.25),mar=c(0.1,0.1,0.1,0.1),xpd=F)
# layout(matrix(c(1:2,2:5,rep(7,6)),2,6,byrow=T))
layout(matrix(c(1:6,rep(7,6),8:13),3,6,byrow=T),heights=c(0.8,1,0.8))

for(i in 1:6){
  tmp=merge(subset(sites2,LTER==sort.LTERs[i]),
            subset(pctchge,LTER==sort.LTERs[i]),by.x="Site.Stream",by.y="SITE",all.x=T)
  if(sort.LTERs[i]=="GRO"){
    m=maps2sp(xlim=c(-180,180),ylim=c(60,120))
    gl = gridlines(m, easts = seq(-180,180,20))
    plot(spTransform(m,Npolar),lwd=0.01,col="grey90",border="grey",bg="skyblue")
    plot(spTransform(pol.clip(glac.dat,xlim=c(-180,180),ylim=c(60,120)),Npolar),col="white",border=NA,add=T)
    plot(spTransform(pol.clip(lake.dat,xlim=c(-180,180),ylim=c(60,120)),Npolar),col="lightblue",border="dodgerblue1",lwd=0.1,add=T)
    plot(spTransform(pol.clip(rivers.dat,xlim=c(-180,180),ylim=c(60,120)),Npolar),col="skyblue",add=T,lwd=0.8)
    plot(spTransform(ice.shelf,Npolar),col="skyblue",border="blue",add=T,lty=2)
    gl.polar = spTransform(gl, Npolar)
    lines(gl.polar,lty=2,lwd=0.25)
    # raster::text(spTransform(tmp,Npolar),"Site.Stream",pos=1,halo=T)
    plot(spTransform(tmp,Npolar),pch=21,bg=subset(tmp,is.na(pConc)==F)$pConc.cols,cex=1.5,lwd=0.1,add=T)
    mtext(side=1,adj=0,line=-1.25,sort.LTERs[i],cex=0.8)
    box(lwd=1)
    # }else if(sort.LTERs[i]=="MCM"){
    #   m=maps2sp(xlim=c(-180,180), ylim = c(-90,-70), clip = FALSE)
    #   plot(spTransform(m,Spolar),col="grey")
    #   plot(spTransform(tmp,Spolar),pch=21,bg=subset(tmp,is.na(pConc)==F)$pConc.cols,cex=1.25,add=T)
    #   mtext(side=3,adj=0,line=-1.25,sort.LTERs[i])
    # }
  }else{
    bbox.lims=bbox(gBuffer(spTransform(tmp,NAD83),width=0.06))
    plot(world,col="grey90",border="grey",ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.01,bg="skyblue")
    plot(glac.dat,col="white",border=NA,add=T)
    # if(sort.LTERs[i]!="MCM"){
    # plot(lake.dat,col="lightblue",border="dodgerblue1",add=T,lwd=0.75)
    # plot(rivers.dat,col="skyblue",add=T)
    # plot(ice.shelf,col="skyblue",border="blue",add=T,lty=2)
    # }
    plot(tmp,pch=21,bg=adjustcolor("grey",0.5),col=adjustcolor("white",0.5),lwd=0.1,cex=0.8,add=T)
    plot(subset(tmp,is.na(pConc)==F),pch=21,bg=subset(tmp,is.na(pConc)==F)$pConc.cols,cex=1.5,lwd=0.1,add=T)
    mtext(side=1,adj=0,line=-1.25,sort.LTERs[i],cex=0.8)
    box(lwd=1)
  }
}

bbox.lims=bbox(sites2)
plot(world,col="grey90",border="grey",ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.01)
plot(sites2,add=T,pch=21,bg=site.cols,col="white",lwd=0.1,cex=1.25)
box(lwd=1)
mapmisc::scaleBar(wgs84,"bottom",bty="n",cex=1,seg.len=4,outer=F)
legend("bottomleft",legend=sort.LTERs,
       pt.bg=cols,pch=21,lty=0,lwd=0.1,col="white",
       pt.cex=1.5,ncol=2,cex=0.75,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,
       title.adj=0,title = " LTER")

for(i in 7:11){
  tmp=merge(subset(sites2,LTER==sort.LTERs[i]),
            subset(pctchge,LTER==sort.LTERs[i]),by.x="Site.Stream",by.y="SITE",all.x=T)
  
  bbox.lims=bbox(gBuffer(spTransform(tmp,NAD83),width=0.06))
  plot(world,col="grey90",border="grey",ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.01,bg="skyblue")
  plot(glac.dat,col="white",border=NA,add=T)
  if(sort.LTERs[i]!="MCM"){
    plot(lake.dat,col="lightblue",border="dodgerblue1",add=T,lwd=0.75)
    plot(rivers.dat,col="skyblue",add=T)
    plot(ice.shelf,col="skyblue",border="blue",add=T,lty=2)
    # for SF
    plot(canal,col="skyblue",add=T)
    
  }
  plot(tmp,pch=21,bg=adjustcolor("grey",0.5),col=adjustcolor("white",0.5),lwd=0.1,cex=0.8,add=T)
  plot(subset(tmp,is.na(pConc)==F),pch=21,bg=subset(tmp,is.na(pConc)==F)$pConc.cols,cex=1.5,lwd=0.1,add=T)
  mtext(side=1,adj=0,line=-1.25,sort.LTERs[i],cex=0.8)
  box(lwd=1)
}

plot(0:1,0:1,ann=F,axes=F,type="n")
int.bks.vals=bks
labs=c(paste0("< ",int.bks.vals[2]),paste(int.bks.vals[2:9],int.bks.vals[3:10],sep=" - "),paste(paste0(">",int.bks.vals[11])))
n.bks=length(bks)-1
bx.val= seq(0.1,0.8,(0.8-0.1)/n.bks)
rect(0.15,bx.val[1:n.bks],0.25,bx.val[2:(n.bks+1)],col=rev(cols2),lty=0)
text(x=0.25, y = bx.val[2:(n.bks+1)]-c(mean(diff(bx.val[2:(n.bks+1)]))/2), labels = rev(labs),cex=0.85,adj=0,pos=4)
text(x=0.15,y=0.95,"Percent Change\nSi Concentration",adj=0,cex=1)
dev.off()



sort.LTERs2=sort.LTERs[!(sort.LTERs%in%c("LMP","Sagehen"))]
# png(filename=paste0(plot.path,"WRTDS_PchangeMap_Conc.png"),width=8,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",oma=c(0.25,0.25,0.25,0.25),mar=c(0.1,0.1,0.1,0.1),xpd=F)
# layout(matrix(c(1:2,2:5,rep(7,6)),2,6,byrow=T))
layout(matrix(c(1:4,rep(5,2),c(6,6),7:10),3,4,byrow=T),heights=c(0.8,1,0.8))

# ARC and GRO
tmp=merge(subset(sites2,LTER%in%sort.LTERs2[1:2]),
          subset(pctchge,LTER%in%sort.LTERs2[1:2]),by.x="Site.Stream",by.y="SITE",all.x=T)
tmp=merge(tmp,WBT,"likeC",all.x=T)
head(tmp@data)
m=maps2sp(xlim=c(-180,180),ylim=c(60,120))
gl = gridlines(m, easts = seq(-180,180,20))
plot(spTransform(m,Npolar),lwd=0.01,col="grey90",border="grey",bg="skyblue")
plot(spTransform(pol.clip(glac.dat,xlim=c(-180,180),ylim=c(60,120)),Npolar),col="white",border=NA,add=T)
plot(spTransform(pol.clip(lake.dat,xlim=c(-180,180),ylim=c(60,120)),Npolar),col="lightblue",border="dodgerblue1",lwd=0.1,add=T)
plot(spTransform(pol.clip(rivers.dat,xlim=c(-180,180),ylim=c(60,120)),Npolar),col="skyblue",add=T,lwd=0.8)
plot(spTransform(ice.shelf,Npolar),col="skyblue",border="blue",add=T,lty=2)
gl.polar = spTransform(gl, Npolar)
lines(gl.polar,lty=2,lwd=0.25)
plot(spTransform(subset(tmp,is.na(pConc)==F),Npolar),pch=21,
     bg=as.character(subset(tmp,is.na(pConc)==F)$WBT.col),
     cex=subset(tmp,is.na(pConc)==F)$pConc.size,lwd=0.1,add=T)
raster::text(spTransform(subset(tmp,is.na(pConc)==F),Npolar),"LTER.x",pos=2,halo=T,offset = 0.5,cex=0.75)
mtext(side=1,adj=0,line=-1.25," GRO & ARC",cex=0.8)
mapmisc::scaleBar(wgs84,"bottomright",bty="n",cex=0.8,seg.len=4,outer=F)
box(lwd=1)

for(i in 3:5){
  tmp=merge(subset(sites2,LTER==sort.LTERs2[i]),
            subset(pctchge,LTER==sort.LTERs2[i]),by.x="Site.Stream",by.y="SITE",all.x=T)
  tmp=merge(tmp,WBT,"likeC",all.x=T)
  
  bbox.lims=bbox(gBuffer(spTransform(tmp,NAD83),width=0.05))
  plot(world,col="grey90",border="grey",ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.01,bg="skyblue")
  plot(glac.dat,col="white",border=NA,add=T)
  plot(lake.dat,col="lightblue",border="dodgerblue1",add=T,lwd=0.75)
  plot(rivers.dat,col="skyblue",add=T)
  plot(ice.shelf,col="skyblue",border="blue",add=T,lty=2)
  plot(spTransform(subset(tmp,is.na(pConc)==F),NAD83),pch=21,
       bg=as.character(subset(tmp,is.na(pConc)==F)$WBT.col),
       cex=subset(tmp,is.na(pConc)==F)$pConc.size,lwd=0.1,add=T)
  mtext(side=1,adj=0,line=-1.25,sort.LTERs2[i],cex=0.8)
  box(lwd=1)
  mapmisc::scaleBar(wgs84,"bottomright",bty="n",cex=0.8,seg.len=4,outer=F)
}

# Overall 
bbox.lims=bbox(sites2)
plot(world,col="grey90",border="grey",ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.01)
plot(sites2,add=T,pch=21,bg=site.cols,col="white",lwd=0.1,cex=1.25)
box(lwd=1)
mapmisc::scaleBar(wgs84,"bottom",bty="n",cex=1,seg.len=4,outer=F)
legend("bottomleft",legend=sort.LTERs,
       pt.bg=cols,pch=21,lty=0,lwd=0.1,col="white",
       pt.cex=1.5,ncol=2,cex=0.75,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,
       title.adj=0,title = " LTER")

# Legend
plot(0:1,0:1,ann=F,axes=F,type="n")
int.bks.vals=bks
labs=c(paste0("< ",int.bks.vals[2]),paste(int.bks.vals[2:3],int.bks.vals[3:4],sep=" - "),paste(paste0(">",int.bks.vals[5])))
legend(0.25,0.5,legend=labs,
       pt.bg="grey",pch=21,lty=0,lwd=0.1,col="black",
       pt.cex=bks.size,ncol=1,cex=1,bty="n",y.intersp=1.5,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,
       title.adj=0,title = "% Change\nSi Concenctration")
legend(0.6,0.5,legend=c("Highly Likely","Very Likely","Likely","Unlikely"),
       pt.bg=pal(4),pch=21,lty=0,lwd=0.1,col="black",
       pt.cex=2,ncol=1,cex=1,bty="n",y.intersp=1.5,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,
       title.adj=0,title = "Likelihood of\nIncreasing Trend")

for(i in 6:10){
  tmp=merge(subset(sites2,LTER==sort.LTERs2[i]),
            subset(pctchge,LTER==sort.LTERs2[i]),by.x="Site.Stream",by.y="SITE",all.x=T)
  tmp=merge(tmp,WBT,"likeC",all.x=T)
  
  if(sort.LTERs2[i]=="NWT"){
    bbox.lims=bbox(gBuffer(spTransform(tmp,NAD83),width=0.01))}else{
      bbox.lims=bbox(gBuffer(spTransform(tmp,NAD83),width=0.05))
    }
  plot(world,col="grey90",border="grey",ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.01,bg="skyblue")
  plot(glac.dat,col="white",border=NA,add=T)
  plot(lake.dat,col="lightblue",border="dodgerblue1",add=T,lwd=0.75)
  plot(rivers.dat,col="skyblue",add=T)
  plot(ice.shelf,col="skyblue",border="blue",add=T,lty=2)
  plot(canal,col="skyblue",add=T) # for KRR
  plot(spTransform(subset(tmp,is.na(pConc)==F),NAD83),pch=21,
       bg=as.character(subset(tmp,is.na(pConc)==F)$WBT.col),
       cex=subset(tmp,is.na(pConc)==F)$pConc.size,lwd=0.1,add=T)
  mtext(side=1,adj=0,line=-1.25,sort.LTERs2[i],cex=0.8)
  box(lwd=1)
  mapmisc::scaleBar(wgs84,"bottomright",bty="n",cex=0.8,seg.len=4,outer=F)
}
dev.off()

# png(filename=paste0(plot.path,"WRTDS_PchangeMap_Conc_overall.png"),width=8,height=2.5,units="in",res=200,type="windows",bg="white")
par(family="serif",oma=c(0.25,0.25,0.25,0.25),mar=c(0.1,0.1,0.1,0.1),xpd=F)
# layout(matrix(c(1:2,2:5,rep(7,6)),2,6,byrow=T))
layout(matrix(1:2,1,2,byrow=T),widths = c(1.5,0.75))

tmp=merge(sites2,pctchge,by.x="Site.Stream",by.y="SITE",all.x=T)
tmp=merge(tmp,WBT,"likeC",all.x=T)

bbox.lims=bbox(sites2)
plot(world,col="grey90",border="grey",ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.01)
plot(spTransform(subset(tmp,is.na(pConc)==F),NAD83),pch=21,
     bg=adjustcolor(as.character(subset(tmp,is.na(pConc)==F)$WBT.col),0.25),
     cex=subset(tmp,is.na(pConc)==F)$pConc.size/2,lwd=0.1,add=T)
box(lwd=1)
mapmisc::scaleBar(wgs84,"bottom",bty="n",cex=0.8,seg.len=4,outer=F)

# Legend
plot(0:1,0:1,ann=F,axes=F,type="n")
int.bks.vals=bks
labs=c(paste0("< ",int.bks.vals[2]),paste(int.bks.vals[2:3],int.bks.vals[3:4],sep=" - "),paste(paste0(">",int.bks.vals[5])))
legend(0.25,0.5,legend=labs,
       pt.bg="grey",pch=21,lty=0,lwd=0.1,col="black",
       pt.cex=bks.size/2,ncol=1,cex=0.75,bty="n",y.intersp=1.5,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,
       title.adj=0,title = "% Change\nSi Concenctration")
legend(0.75,0.5,legend=c("Highly Likely","Very Likely","Likely","Unlikely"),
       pt.bg=pal(4),pch=21,lty=0,lwd=0.1,col="black",
       pt.cex=2,ncol=1,cex=0.75,bty="n",y.intersp=1.5,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,
       title.adj=0,title = "Likelihood of\nIncreasing Trend")
dev.off()

subset(pctchge,LTER=="ARC")

range(pctchge$pFlux)
bks=c(-40,-20,0,100,150,200) # seq(-50,300,10)
bks.int=findInterval(pctchge$pFlux,bks)
pal=colorRampPalette(c("blue","grey","red"))

bks.int=findInterval(pctchge$pFlux,bks)
bks.size=seq(1,4, along.with=bks)#length.out=length(bks))
pctchge$pFlux.size=bks.size[bks.int]

WBT=data.frame(likeF=c("highly","very","likely","",NA),
               WBT.col=c(pal(4)[1:3],pal(4)[4],pal(4)[4]))

# png(filename=paste0(plot.path,"WRTDS_PchangeMap_Flux.png"),width=8,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",oma=c(0.25,0.25,0.25,0.25),mar=c(0.1,0.1,0.1,0.1),xpd=F)
# layout(matrix(c(1:2,2:5,rep(7,6)),2,6,byrow=T))
layout(matrix(c(1:4,rep(5,2),c(6,6),7:10),3,4,byrow=T),heights=c(0.8,1,0.8))

# ARC and GRO
tmp=merge(subset(sites2,LTER%in%sort.LTERs2[1:2]),
          subset(pctchge,LTER%in%sort.LTERs2[1:2]),by.x="Site.Stream",by.y="SITE",all.x=T)
tmp=merge(tmp,WBT,"likeF",all.x=T)
head(tmp@data)
m=maps2sp(xlim=c(-180,180),ylim=c(60,120))
gl = gridlines(m, easts = seq(-180,180,20))
plot(spTransform(m,Npolar),lwd=0.01,col="grey90",border="grey",bg="skyblue")
plot(spTransform(pol.clip(glac.dat,xlim=c(-180,180),ylim=c(60,120)),Npolar),col="white",border=NA,add=T)
plot(spTransform(pol.clip(lake.dat,xlim=c(-180,180),ylim=c(60,120)),Npolar),col="lightblue",border="dodgerblue1",lwd=0.1,add=T)
plot(spTransform(pol.clip(rivers.dat,xlim=c(-180,180),ylim=c(60,120)),Npolar),col="skyblue",add=T,lwd=0.8)
plot(spTransform(ice.shelf,Npolar),col="skyblue",border="blue",add=T,lty=2)
gl.polar = spTransform(gl, Npolar)
lines(gl.polar,lty=2,lwd=0.25)
plot(spTransform(subset(tmp,is.na(pFlux)==F),Npolar),pch=21,
     bg=as.character(subset(tmp,is.na(pFlux)==F)$WBT.col),
     cex=subset(tmp,is.na(pConc)==F)$pFlux.size,lwd=0.1,add=T)
raster::text(spTransform(subset(tmp,is.na(pFlux)==F),Npolar),"LTER.x",pos=2,halo=T,offset = 0.5,cex=0.75)
mtext(side=1,adj=0,line=-1.25," GRO & ARC",cex=0.8)
mapmisc::scaleBar(wgs84,"bottomright",bty="n",cex=0.8,seg.len=4,outer=F)
box(lwd=1)

for(i in 3:5){
  tmp=merge(subset(sites2,LTER==sort.LTERs2[i]),
            subset(pctchge,LTER==sort.LTERs2[i]),by.x="Site.Stream",by.y="SITE",all.x=T)
  tmp=merge(tmp,WBT,"likeF",all.x=T)
  
  bbox.lims=bbox(gBuffer(spTransform(tmp,NAD83),width=0.05))
  plot(world,col="grey90",border="grey",ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.01,bg="skyblue")
  plot(glac.dat,col="white",border=NA,add=T)
  plot(lake.dat,col="lightblue",border="dodgerblue1",add=T,lwd=0.75)
  plot(rivers.dat,col="skyblue",add=T)
  plot(ice.shelf,col="skyblue",border="blue",add=T,lty=2)
  plot(spTransform(subset(tmp,is.na(pFlux)==F),NAD83),pch=21,
       bg=as.character(subset(tmp,is.na(pFlux)==F)$WBT.col),
       cex=subset(tmp,is.na(pFlux)==F)$pFlux.size,lwd=0.1,add=T)
  mtext(side=1,adj=0,line=-1.25,sort.LTERs2[i],cex=0.8)
  box(lwd=1)
  mapmisc::scaleBar(wgs84,"bottomright",bty="n",cex=0.8,seg.len=4,outer=F)
}

# Overall 
bbox.lims=bbox(sites2)
plot(world,col="grey90",border="grey",ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.01)
plot(sites2,add=T,pch=21,bg=site.cols,col="white",lwd=0.1,cex=1.25)
box(lwd=1)
mapmisc::scaleBar(wgs84,"bottom",bty="n",cex=1,seg.len=4,outer=F)
legend("bottomleft",legend=sort.LTERs,
       pt.bg=cols,pch=21,lty=0,lwd=0.1,col="white",
       pt.cex=1.5,ncol=2,cex=0.75,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,
       title.adj=0,title = " LTER")

# Legend
plot(0:1,0:1,ann=F,axes=F,type="n")
int.bks.vals=bks
labs=c(paste0("< ",int.bks.vals[2]),paste(int.bks.vals[2:3],int.bks.vals[3:4],sep=" - "),paste(paste0(">",int.bks.vals[5])))
legend(0.25,0.5,legend=labs,
       pt.bg="grey",pch=21,lty=0,lwd=0.1,col="black",
       pt.cex=bks.size,ncol=1,cex=1,bty="n",y.intersp=1.5,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,
       title.adj=0,title = "% Change\nSi Flux")
legend(0.6,0.5,legend=c("Highly Likely","Very Likely","Likely","Unlikely"),
       pt.bg=pal(4),pch=21,lty=0,lwd=0.1,col="black",
       pt.cex=2,ncol=1,cex=1,bty="n",y.intersp=1.5,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,
       title.adj=0,title = "Likelihood of\nIncreasing Trend")

for(i in 6:10){
  tmp=merge(subset(sites2,LTER==sort.LTERs2[i]),
            subset(pctchge,LTER==sort.LTERs2[i]),by.x="Site.Stream",by.y="SITE",all.x=T)
  tmp=merge(tmp,WBT,"likeF",all.x=T)
  
  if(sort.LTERs2[i]=="NWT"){
    bbox.lims=bbox(gBuffer(spTransform(tmp,NAD83),width=0.01))}else{
      bbox.lims=bbox(gBuffer(spTransform(tmp,NAD83),width=0.05))
    }
  plot(world,col="grey90",border="grey",ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.01,bg="skyblue")
  plot(glac.dat,col="white",border=NA,add=T)
  plot(lake.dat,col="lightblue",border="dodgerblue1",add=T,lwd=0.75)
  plot(rivers.dat,col="skyblue",add=T)
  plot(ice.shelf,col="skyblue",border="blue",add=T,lty=2)
  plot(canal,col="skyblue",add=T) # for KRR
  plot(spTransform(subset(tmp,is.na(pFlux)==F),NAD83),pch=21,
       bg=as.character(subset(tmp,is.na(pFlux)==F)$WBT.col),
       cex=subset(tmp,is.na(pFlux)==F)$pFlux.size,lwd=0.1,add=T)
  mtext(side=1,adj=0,line=-1.25,sort.LTERs2[i],cex=0.8)
  box(lwd=1)
  mapmisc::scaleBar(wgs84,"bottomright",bty="n",cex=0.8,seg.len=4,outer=F)
}
dev.off()

# png(filename=paste0(plot.path,"WRTDS_PchangeMap_Flux_overall.png"),width=8,height=2.5,units="in",res=200,type="windows",bg="white")
par(family="serif",oma=c(0.25,0.25,0.25,0.25),mar=c(0.1,0.1,0.1,0.1),xpd=F)
# layout(matrix(c(1:2,2:5,rep(7,6)),2,6,byrow=T))
layout(matrix(1:2,1,2,byrow=T),widths = c(1.5,0.75))

tmp=merge(sites2,pctchge,by.x="Site.Stream",by.y="SITE",all.x=T)
tmp=merge(tmp,WBT,"likeF",all.x=T)

bbox.lims=bbox(sites2)
plot(world,col="grey90",border="grey",ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.01)
plot(spTransform(subset(tmp,is.na(pFlux)==F),NAD83),pch=21,
     bg=adjustcolor(as.character(subset(tmp,is.na(pFlux)==F)$WBT.col),0.25),
     cex=subset(tmp,is.na(pFlux)==F)$pFlux.size/2,lwd=0.1,add=T)
box(lwd=1)
mapmisc::scaleBar(wgs84,"bottom",bty="n",cex=0.8,seg.len=4,outer=F)

# Legend
plot(0:1,0:1,ann=F,axes=F,type="n")
int.bks.vals=bks
labs=c(paste0("< ",int.bks.vals[2]),paste(int.bks.vals[2:3],int.bks.vals[3:4],sep=" - "),paste(paste0(">",int.bks.vals[5])))
legend(0.25,0.5,legend=labs,
       pt.bg="grey",pch=21,lty=0,lwd=0.1,col="black",
       pt.cex=bks.size/2,ncol=1,cex=0.75,bty="n",y.intersp=1.5,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,
       title.adj=0,title = "% Change\nSi Flux")
legend(0.75,0.5,legend=c("Highly Likely","Very Likely","Likely","Unlikely"),
       pt.bg=pal(4),pch=21,lty=0,lwd=0.1,col="black",
       pt.cex=2,ncol=1,cex=0.75,bty="n",y.intersp=1.5,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,
       title.adj=0,title = "Likelihood of\nIncreasing Trend")
dev.off()


## Experiment
# adding imagery? ---------------------------------------------------------
library(ceramic)
source("./DataInventory/src/0_ceramictoken.R")
public.token
Sys.setenv(MAPBOX_API_KEY=public.token)
# roi=raster::extent(subset(tmp,is.na(pConc)==F))
# im <- cc_location(roi,zoom=12)
# im=projectRaster(im,crs=wgs84)
# setValues(im,scales::rescale(values(im), c(0,255)))
# plotRGB(im)


imagery.zoom=c(7,NA,10,11,12,8,14,15,12,12,9)
i=11
tmp=merge(subset(sites2,LTER==sort.LTERs[i]),
          subset(pctchge,LTER==sort.LTERs[i]),by.x="Site.Stream",by.y="SITE",all.x=T)


roi=raster::extent(spTransform(gBuffer(spTransform(tmp,NAD83),width=0.4),wgs84))
im <- cc_location(roi,zoom=9)
im=projectRaster(im,crs=wgs84)
setValues(im,scales::rescale(values(im),to= c(0,255)))
plotRGB(im)

plot(subset(tmp,is.na(pConc)==F),pch=21,bg=subset(tmp,is.na(pConc)==F)$pConc.cols,col="white",cex=1.5,lwd=0.1,add=T)

# png(filename=paste0(plot.path,"WRTDS_PchangeMap2.png"),width=8,height=5,units="in",res=200,type="windows",bg="white")
par(family="serif",oma=c(0.25,0.25,0.25,0.25),mar=c(0.1,0.1,0.1,0.1),xpd=F)
# layout(matrix(c(1:2,2:5,rep(7,6)),2,6,byrow=T))
layout(matrix(c(1:5,rep(6,3),5,6,7:11),3,5,byrow=T),heights=c(0.8,1,0.8))

# ARC and GRO
tmp=merge(subset(sites2,LTER%in%sort.LTERs[1:2]),
          subset(pctchge,LTER%in%sort.LTERs[1:2]),by.x="Site.Stream",by.y="SITE",all.x=T)
tmp=merge(tmp,WBT,by.x="likeC",by.y="WBT.txt",all.x=T)
head(tmp@data)
m=maps2sp(xlim=c(-180,180),ylim=c(60,120))
gl = gridlines(m, easts = seq(-180,180,20))
plot(spTransform(m,Npolar),lwd=0.01,col="grey90",border="grey",bg="skyblue")
plot(spTransform(pol.clip(glac.dat,xlim=c(-180,180),ylim=c(60,120)),Npolar),col="white",border=NA,add=T)
plot(spTransform(pol.clip(lake.dat,xlim=c(-180,180),ylim=c(60,120)),Npolar),col="lightblue",border="dodgerblue1",lwd=0.1,add=T)
plot(spTransform(pol.clip(rivers.dat,xlim=c(-180,180),ylim=c(60,120)),Npolar),col="skyblue",add=T,lwd=0.8)
plot(spTransform(ice.shelf,Npolar),col="skyblue",border="blue",add=T,lty=2)
gl.polar = spTransform(gl, Npolar)
lines(gl.polar,lty=2,lwd=0.25)
plot(spTransform(subset(tmp,is.na(pConc)==F),Npolar),pch=21,
     bg=subset(tmp,is.na(pConc)==F)$WBT.col,
     cex=subset(tmp,is.na(pConc)==F)$pConc.size,lwd=0.1,add=T)
raster::text(spTransform(subset(tmp,is.na(pConc)==F),Npolar),"LTER.x",pos=2,halo=T)
mtext(side=1,adj=0,line=-1.25," GRO & ARC",cex=0.8)
mapmisc::scaleBar(wgs84,"bottom",bty="n",cex=1,seg.len=4,outer=F)
box(lwd=1)

i=3
tmp=merge(subset(sites2,LTER%in%sort.LTERs[i]),
          subset(pctchge,LTER%in%sort.LTERs[i]),by.x="Site.Stream",by.y="SITE",all.x=T)
tmp=merge(tmp,WBT,by.x="likeC",by.y="WBT.txt",all.x=T)

roi=raster::extent(spTransform(gBuffer(spTransform(tmp,NAD83),width=0.1),wgs84))
im <- cc_location(roi,zoom = 12, type = "mapbox.satellite")
im=projectRaster(im,crs=wgs84)
setValues(im,scales::rescale(values(im),c(0,255)))

ext.val=gBuffer(spTransform(tmp,NAD83),width=0.01)
plotRGB(crop(im,ext.val))
# plot(crop(rivers.dat,extent(im)),col="skyblue",add=T)
# plot(crop(canal,extent(im)),col="skyblue",add=T)
plot(subset(tmp,is.na(pConc)==F),
     pch=21,
     bg=subset(tmp,is.na(pConc)==F)$WBT.col,
     cex=subset(tmp,is.na(pConc)==F)$pConc.size,lwd=0.1,add=T)
mtext(side=3,line=-1.5,paste0(" ",sort.LTERs[i]),cex=0.8,col="white")
mapmisc::scaleBar(wgs84,"bottom",bty="n",cex=1,seg.len=4,outer=F,col="white")

i=4
tmp=merge(subset(sites2,LTER%in%sort.LTERs[i]),
          subset(pctchge,LTER%in%sort.LTERs[i]),by.x="Site.Stream",by.y="SITE",all.x=T)
tmp=merge(tmp,WBT,by.x="likeC",by.y="WBT.txt",all.x=T)

roi=raster::extent(spTransform(gBuffer(spTransform(tmp,NAD83),width=0.1),wgs84))
im <- cc_location(roi,zoom = 13, type = "mapbox.satellite")
im=projectRaster(im,crs=wgs84)
setValues(im,scales::rescale(values(im),c(0,255)))

ext.val=gBuffer(spTransform(tmp,NAD83),width=0.01)
plotRGB(crop(im,ext.val))
# plot(crop(rivers.dat,extent(im)),col="skyblue",add=T)
# plot(crop(canal,extent(im)),col="skyblue",add=T)
plot(subset(tmp,is.na(pConc)==F),
     pch=21,
     bg=subset(tmp,is.na(pConc)==F)$WBT.col,
     cex=subset(tmp,is.na(pConc)==F)$pConc.size,lwd=0.1,add=T)
mtext(side=3,line=-1.5,paste0(" ",sort.LTERs[i]),cex=0.8,col="white")
mapmisc::scaleBar(wgs84,"bottom",bty="n",cex=1,seg.len=4,outer=F,col="white")

i=5
tmp=merge(subset(sites2,LTER%in%sort.LTERs[i]),
          subset(pctchge,LTER%in%sort.LTERs[i]),by.x="Site.Stream",by.y="SITE",all.x=T)
tmp=merge(tmp,WBT,by.x="likeC",by.y="WBT.txt",all.x=T)

roi=raster::extent(spTransform(gBuffer(spTransform(tmp,NAD83),width=0.1),wgs84))
im <- cc_location(roi,zoom = 12, type = "mapbox.satellite")
im=projectRaster(im,crs=wgs84)
setValues(im,scales::rescale(values(im),c(0,255)))

ext.val=gBuffer(spTransform(tmp,NAD83),width=0.01)
plotRGB(crop(im,ext.val))
plot(crop(rivers.dat,extent(im)),col="skyblue",add=T)
plot(crop(canal,extent(im)),col="skyblue",add=T)
plot(subset(tmp,is.na(pConc)==F),
     pch=21,
     bg=subset(tmp,is.na(pConc)==F)$WBT.col,
     cex=subset(tmp,is.na(pConc)==F)$pConc.size,lwd=0.1,add=T)
mtext(side=3,line=-1.5,paste0(" ",sort.LTERs[i]),cex=0.8,col="white")
mapmisc::scaleBar(wgs84,"bottom",bty="n",cex=1,seg.len=4,outer=F,col="white")  


bbox.lims=bbox(sites2)
plot(world,col="grey90",border="grey",ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],lwd=0.01)
plot(sites2,add=T,pch=21,bg=site.cols,col="white",lwd=0.1,cex=1.25)
box(lwd=1)
mapmisc::scaleBar(wgs84,"bottom",bty="n",cex=1,seg.len=4,outer=F)
legend("bottomleft",legend=sort.LTERs,
       pt.bg=cols,pch=21,lty=0,lwd=0.1,col="white",
       pt.cex=1.5,ncol=2,cex=0.75,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=0.5,
       title.adj=0,title = " LTER")

i=11
imagery.zoom=c(7,NA,12,11,12,8,7,10,11,11,9)
tmp=merge(subset(sites2,LTER%in%sort.LTERs[i]),
          subset(pctchge,LTER%in%sort.LTERs[i]),by.x="Site.Stream",by.y="SITE",all.x=T)
tmp=merge(tmp,WBT,by.x="likeC",by.y="WBT.txt",all.x=T)
# tmp=spTransform(tmp,wgs84)



roi=raster::extent(spTransform(gBuffer(spTransform(tmp,NAD83),width=0.2),wgs84))
im <- cc_location(roi,buffer = 500000, type = "mapbox.satellite")
# im <- cc_elevation(roi)
im=projectRaster(im,crs=wgs84)
setValues(im,scales::rescale(values(im),c(0,255)))

ext.val=gBuffer(spTransform(tmp,NAD83),width=0.06)
plotRGB(crop(im,ext.val))
plot(crop(rivers.dat,extent(im)),col="skyblue",add=T)
plot(crop(canal,extent(im)),col="skyblue",add=T)
plot(subset(tmp,is.na(pConc)==F),
     pch=21,
     bg=subset(tmp,is.na(pConc)==F)$WBT.col,
     cex=subset(tmp,is.na(pConc)==F)$pConc.size,lwd=0.1,add=T)
mtext(side=1,line=-1.25,paste0(" ",sort.LTERs[i]),cex=0.8)


## trying another biome plot 
library(BIOMEplot)
plot_biome()
points(200,10)
