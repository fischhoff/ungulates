Gl = raster("Globcover2009_V2.3_Global_/GLOBCOVER_L4_200901_200912_V2.3.tif")

load("out_forest.Rdata")
df = out_forest
out_spdf = SpatialPointsDataFrame(coords = out_forest[,c("longitude", "latitude")], 
                                  data = df)

#tell R that out coordinates are in the same lat/lon reference system
projection(out_spdf) <- projection(Gl)

locs.vals = raster::extract(Gl, out_spdf,
                            df = TRUE, method = "simple")#specify 
#make into dataframe
locs.vals.df = data.frame(locs.vals)
#create row indices
locs.vals.df$row = seq(1, dim(locs.vals.df)[1])
names(locs.vals.df)[2]="globcover"
df$row = seq(1, dim(locs.vals.df)[1])

df= merge(df, locs.vals.df, by = "row")

G_legend = read.csv("Globcover2009_V2.3_Global_/Globcover2009_Legend.csv")

names(G_legend)[names(G_legend)=="Value"]="globcover"
G_legend = G_legend[,c("globcover","label_short")]
df= merge(df, G_legend, by = 
            "globcover")

df_lc = df
save(df_lc, file = "df_lc.Rdata")
