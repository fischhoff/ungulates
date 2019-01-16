#assign top left corner to each outbreak presence / absence location
load("df_combined.Rdata")
df = df_combined
# inds_long_pos = which(df$longitude>=0)
# df$longitude_10[inds_long_pos]=df$longitude[inds_long_pos] - (df$longitude[inds_long_pos] %% 10)
df$longitude_10=df$longitude - (df$longitude %% 10)
# inds_long_neg = which(df$longitude<0)

df$latitude_10=df$latitude+(10-df$latitude%%10)
# for (a in c(1:
#             
#             dim(df)[1]
#             ))
# {
# if (df_combined$longitude[a]>=0)
# {
#   df_combined$longitude_10[a] = df_combined$longitude[a] - (df_combined$longitude[a] %% 10) #do not subtract 10
# #  df_combined$longitude_10[a] = df_combined$longitude[a] + (10-df_combined$longitude[a] %% 10)#this was wrong, resulted in longitude_10 10 too high
#  # print(df_combined$longitude[a])
#   #print(df_combined$longitude_10[a])
# }
#   else#longitude negative
#   {
#     df_combined$longitude_10[a] = df_combined$longitude[a]-(df_combined$longitude[a]%%10)
#     #df_combined$longitude_10[a] = df_combined$longitude[a]+(10-df_combined$longitude[a]%%10)
#   }
# }
#   for (a in c(1:
#               
#               dim(df)[1]
#   ))
#     
#   {   
#  if (df_combined$latitude[a]>=0)
#  {
#    df_combined$latitude_10[a]=df_combined$latitude[a]+(10-df_combined$latitude[a]%%10)
#    
#  }
#   else
#   {
#   df_combined$latitude_10[a]=df_combined$latitude[a]+(10-df_combined$latitude[a]%%10)
#     # print(df_combined$latitude[a])
#     # print(df_combined$latitude_10[a])
#   }
#   
#   }
df_combined = df
save(df_combined, file = "df_combined.Rdata")
#https://storage.googleapis.com/earthenginepartners-hansen/GFC-2017-v1.5/Hansen_GFC-2017-v1.5_lossyear_10N_020E.tif


