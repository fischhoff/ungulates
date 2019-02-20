#http://earthenginepartners.appspot.com/science-2013-global-forest/download_v1.5.html
load("df_deforest.Rdata")
df = df_deforest

#df$gain = NA
df$latitude_word="N"
df$longitude_word="E"
inds = which(df$latitude_10<0)
df$latitude_word[inds]="S"
#inds = which(df$longitude_10<0)
df$longitude_word[df$longitude_10<0]="W"#if it is zero then it is east

#now pad longitude. take absolute value
df$longitude_10_string = formatC(abs(df$longitude_10), width = 3, flag = "0")
df$latitude_10_string = formatC(abs(df$latitude_10), width = 2, flag = "0")

#combine number and letter. take absolute value so that it matches format of urls
df$latitude_word=paste0(df$latitude_10_string, df$latitude_word)
df$longitude_word=paste0(df$longitude_10_string, df$longitude_word)

#beginning of url
begin = "https://storage.googleapis.com/earthenginepartners-hansen/GFC-2017-v1.5/Hansen_GFC-2017-v1.5_gain_"

left=sort(unique(df$longitude_10))
# left=seq(from = -180,to=170,by=10)
top=seq(from = -80,to=90,by=10)
#top=-40
# top=sort(unique(df$latitude_10))

#initialize output as NULL
out = NULL
x = 20
y = 9
for (x in c(1:length(left)))#for each left edge
{
  print("x")
  print(x)
  for (y in 1:length(top))#for each top edge
  {
    print("y")
    
        print(y)
    #get presence/absence with left[x] and top[y] corner
    tempdf=(df[df$longitude_10==left[x]&df$latitude_10==top[y],])
    print(dim(tempdf)[1])
    
    if (dim(tempdf)[1]>0){#if there is at least one presence/absence
      path = paste0(begin, 
                    tempdf$latitude_word[1],
                    "_",
                    tempdf$longitude_word[1],
                    ".tif")
      r = raster(path)
      
      out_spdf = SpatialPointsDataFrame(coords = tempdf[,c("longitude", "latitude")], 
                                        data = tempdf)
      
      # tell R that out coordinates are in the same lat/lon reference system
      projection(out_spdf) <- projection(r)
      # jpeg("test.jpg")
      # par(mar = c(3,3,3,3))
      # plot(r)
      # dev.off()
      locs.vals = raster::extract(r, out_spdf,
                                  df = TRUE, method = "simple")#specify 
      #make into dataframe
      locs.vals.df = data.frame(locs.vals)
      #create row indices
      locs.vals.df$row = seq(1, dim(locs.vals.df)[1])
      names(locs.vals.df)[2]="gain"
      tempdf$row = seq(1, dim(locs.vals.df)[1])
      
      tempdf= merge(tempdf, locs.vals.df, by = "row")
      ###
      #now with bilinear
      # locs.vals = raster::extract(r, out_spdf,
      #                             df = TRUE, method = "bilinear")#specify 
      # #make into dataframe
      # locs.vals.df = data.frame(locs.vals)
      # #create row indices
      # locs.vals.df$row = seq(1, dim(locs.vals.df)[1])
      # names(locs.vals.df)[2]="lossyear_bilinear"
      # tempdf$row = seq(1, dim(locs.vals.df)[1])
      # 
      # tempdf= merge(tempdf, locs.vals.df)
      # ###
      # #now with buffer = 10 km
      # locs.vals = raster::extract(r, out_spdf,
      #                             df = TRUE, buffer = 10000)#specify 
      # #make into dataframe
      # locs.vals.df = data.frame(locs.vals)
      # #create row indices
      # locs.vals.df$row = seq(1, dim(locs.vals.df)[1])
      # names(locs.vals.df)[2]="lossyear_buffer10k"
      # tempdf$row = seq(1, dim(locs.vals.df)[1])
      # 
      # tempdf= merge(tempdf, locs.vals.df)
      # 
      # #now with buffer = 1 km
      # locs.vals = raster::extract(r, out_spdf,
      #                             df = TRUE, buffer = 1000)#specify 
      # #make into dataframe
      # locs.vals.df = data.frame(locs.vals)
      # #create row indices
      # locs.vals.df$row = seq(1, dim(locs.vals.df)[1])
      # names(locs.vals.df)[2]="gain_buffer1k"
      # tempdf$row = seq(1, dim(locs.vals.df)[1])
      # 
      # tempdf= merge(tempdf, locs.vals.df)
      # 
      # #now with buffer = 100 km
      # locs.vals = raster::extract(r, out_spdf,
      #                             df = TRUE, buffer = 100000)#specify 
      # #make into dataframe
      # locs.vals.df = data.frame(locs.vals)
      # #create row indices
      # locs.vals.df$row = seq(1, dim(locs.vals.df)[1])
      # names(locs.vals.df)[2]="gain_buffer100k"
      # tempdf$row = seq(1, dim(locs.vals.df)[1])
      # 
      # tempdf= merge(tempdf, locs.vals.df)
      
      #output to dataframe
      out = rbind(out, tempdf)
      print("out")
      print(dim(out)[1])
    
    }#end if statement
  }#end top
}#end left
out_forest = out
save(out_forest, file = "out_forest.Rdata")




