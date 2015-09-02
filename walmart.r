  library(maps) 
  library(fields) #for US() map, which is pretty straightforward than map() as its used minimally

#open the stores opened dataset, rename
  #to get the the date into year, month and day using strsplit()
  d<-read.csv(‘data/store_openings.csv’)
  names(d)=c(“store.number”,”opendate”,”date.super”,”conversion”,”fips_state”,”county”,”street.address”,”store.city”,”store.state”,”zipcode”,”type.of.store”)
  opendate_d<-t(simplify2array(strsplit(as.character(d$opendate),”/”)))
  opendate_d<-array(as.integer(opendate_d),dim=c(nrow(opendate_d),3))
  #open zipcode with which the point shall be plotted, exclude the below regions
  zips<-read.csv(‘data/zipcodes/zipcode.csv’)
  zips<-zips[zips$state!=”HI”,] 
  zips<-zips[zips$state!=”PR”,]  
  zips<-zips[zips$state!=”AS”,] 
  zips<-zips[zips$state!=”VI”,] 
  zips<-zips[zips$state!=”AK”,] 
  
  new_stores_in_months<-numeric(diff(range(opendate_d[,3]))*12)
  mcount<-2
  
  stores_list<-as.list(rep(1,diff(range(opendate_d[,3]))*12+1))
  stores_list[[1]]<-NA
  
  for(year in min(opendate_d[,3]):max(opendate_d[,3]) )
  {
      for(month in 1:12)
      {
          new_stores_in_months[mcount-1]<-(sum(opendate_d[,1]==month & opendate_d[,3]==year))
          stores_list[[mcount+1]]<-c(stores[[mcount]],which(opendate_d[,1]==month & opendate_d[,3]==year))
          mcount<-mcount+1 
      }
  }
  
  index_zip<-numeric(length(stores_list[[542]]))
  for( i in stores_list[[542]] )
  {
     if(d$zipcode[i] %in% zips$zip)
      index_zip[i]<-which(zips$zip==d$zipcode[i])
     else
      index_zip[i]<-NA
  }
  
  total.scores<-1
  i<-1
  for(year in min(opendate_d[,3]):max(opendate_d[,3]) )
  {
      for(month in 1:12)
      {
        png(paste(year,’_’,100+month,’.png’,sep=”),width=750,height=500)
        US(main=’new stores of walmart’,col=”red”,bg=”grey”,namesonly=T)
        points(zips$longitude[index_zip[1:(total.scores+new_stores_in_months[i])] ],
              zips$latitude[index_zip[1:(total.scores+new_stores_in_months[i])]],
              pch=19,col=”green”)
        points(zips$longitude[index_zip[total.scores:(total.scores+new_stores_in_months[i])] ],
              zips$latitude[index_zip[total.scores:(total.scores+new_stores_in_months[i])] ],
              col=’blue’,
              pch=19,lwd=1.5)
      text(-120,29,paste(‘Year: ‘,year),col=’red’,cex=1)
      text(-120,28,paste(‘Openings: ‘,new_stores_in_months[i]+1),col=’red’,cex=1)
      text(-120,27,paste(‘Total stores: ‘,total.scores),col=’red’,cex=1)
        total.scores<-total.scores+new_stores_in_months[i]
        i<-i+1
        dev.off()
     }
  }
  #after installing imagemagick on your computer, convert the png to gif by using the system()
  #when working on linux will help overcome the posix issues. 
  #mencoder or ffmpeg are the alternatives if working on linux than imagemagick
  system(‘”E:/Program/ImageMagick-6.8.9-Q16/convert.exe” *.png example.gif’,intern=T)
logfile<-file.remove(list.files(pattern=”.png”)
