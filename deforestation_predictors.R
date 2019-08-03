#assign top left corner to each outbreak presence / absence location
load("predictorPoints.Rdata")
df = predictorPoints
df$longitude=df$x
df$latitude = df$y
# inds_long_pos = which(df$longitude>=0)
# df$longitude_10[inds_long_pos]=df$longitude[inds_long_pos] - (df$longitude[inds_long_pos] %% 10)
df$longitude_10=df$longitude - (df$longitude %% 10)
# inds_long_neg = which(df$longitude<0)

df$latitude_10=df$latitude+(10-df$latitude%%10)
predictorPoints = df

save(predictorPoints, file = "predictorPoints.Rdata")
#https://storage.googleapis.com/earthenginepartners-hansen/GFC-2017-v1.5/Hansen_GFC-2017-v1.5_lossyear_10N_020E.tif


