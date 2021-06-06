
library(tidyverse)

#install.packages("jpeg")
library(jpeg)
library(png)
#install.packages("imager")
library(imager)
#devtools::install_github("thomasp85/patchwork")
library(patchwork)
#remotes::install_github("dreamRs/colorscale")
library(colorscale)
sortunjpeg<-function(x,y,z,titre){
  jpeg(filename = paste0(titre,".jpg"), width=y, height = z, quality=100, units = "px",type="cairo")
  plot(x)
  dev.off()
}


FaisMoiUnColoriage<-function(urlimage,rangsimplif,nombrecouleurs,titre){
  
  #On importe l'image via jpeg et imager.
  URL1<-urlimage
  tableau1im <- load.image(URL1)
  
  
  #On sort l'image via ggplot.
  df1 <- as.data.frame(tableau1im,wide="c") %>% mutate(rgb.val=rgb(c.1,c.2,c.3))
  
  #On met la couleur moyenne par gros pixels
  df1$pix_x<-cut(df1$x,breaks=max(df1$x)/rangsimplif,include.lowest = T)
  df1$pix_y<-cut(df1$y,breaks=max(df1$y)/rangsimplif,include.lowest = T)
  df1<-df1%>%arrange(x)%>%mutate(ordrex=rank(pix_x,ties.method = "average"))
  df1<-df1%>%arrange(y)%>%mutate(ordrey=rank(pix_y,ties.method = "average"))
  
  ValMoy<-df1%>%group_by(ordrex,ordrey)%>%
    summarise(MoyRouge=mean(c.1),
              MoyVert=mean(c.2),
              MoyBleu=mean(c.3))%>%
    mutate(rgb.val=rgb(MoyRouge,MoyVert,MoyBleu),
           rgb.val1=rgb(round(MoyRouge,1),round(MoyVert,1),round(MoyBleu,1)))
  
  CouleursPresentes<-ValMoy%>%
    group_by(rgb.val1)%>%
    count()%>%
    arrange(desc(n))
  
  x<-nombrecouleurs
  Garde<-CouleursPresentes[1:x,]
  AModif<-CouleursPresentes[(x+1):(nrow(CouleursPresentes)),]
  
  Comparaisons<-unique(Garde$rgb.val1)
  GetPlusProche<-function(x){
    valeur<-min(round(chroma_distance(x,Comparaisons),3))
    Comparaisons[round(chroma_distance(x,Comparaisons),3)==valeur]
  }
  
  AModif$Remplacement<-AModif$rgb.val1%>%map_chr(.f = GetPlusProche)
  
  CoulSimpl<-rbind(ValMoy%>%select(ordrex,ordrey,rgb.val1)%>%
                     filter(rgb.val1%in%Garde$rgb.val1)%>%
                     mutate(couleur=rgb.val1),
                   ValMoy%>%select(ordrex,ordrey,rgb.val1)%>%
                     filter(rgb.val1%in%AModif$rgb.val1)%>%left_join(AModif%>%select(-n)%>%
                                                                       mutate(couleur=Remplacement)))
  
  Legende<-Garde%>%ungroup()%>%mutate(num=letters[1:x])%>%mutate(couleur=rgb.val1)
  
  Graph<-CoulSimpl%>%left_join(Legende,by=c("couleur"="couleur"))
  
  PosLeg<-Legende%>%ggplot+
    geom_text(aes(num,rep(0,nrow(Legende)), label=num),vjust=-0.5)+
    geom_tile(aes(num,rep(1,nrow(Legende)), fill=couleur),colour="#222222")+
    theme_void()+
    scale_fill_identity()
  
  Lettres<-ggplot(Graph,aes(ordrex,ordrey))+geom_text(aes(label=num),size=3)+
    geom_tile(colour="#22222250",fill=NA)+
    scale_y_reverse()+
    coord_fixed(ratio=1)+theme_void()
  
  Solution<-ggplot(CoulSimpl,aes(ordrex,ordrey))+geom_tile(aes(fill=couleur))+
    scale_fill_identity()+scale_y_reverse()+
    coord_fixed(ratio=1)+theme_void()
  
  sortunjpeg(PosLeg+Lettres+patchwork::plot_layout(heights = c(1,10)),1500,1500,paste0("ACOLORIER ",titre))
  sortunjpeg(Solution,1500,1500,paste0("SOLUTION ",titre))
  
}
