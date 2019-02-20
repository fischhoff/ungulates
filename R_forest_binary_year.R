load("out_forest.Rdata")#load data with deforestation defined
names(out_forest)

df = out_forest#temporary df

#make forest binary (has it occurred in location or not)

table(df$gain,df$case)

mp <- NULL
mapWorld <- borders("world", 
                    ylim = c(-60,80)) # create a layer of borders
mp <- ggplot() +   mapWorld
#Now layer the deforest years on top
df_observed = subset(df, case == 1 &gain == 1)
mp <- mp+ geom_point(data = df_observed, aes(x=longitude, y=latitude, color=gain),
                     alpha = 0.5)
  # theme(legend.position="none")#remove legend+
mp

ggsave(plot = mp, file = "forest_year_binary_map.jpg",
       height = 4, width = 5, units = "in")

#make graph of case vs. deforest
df$case = factor(df$case)
df$gain = factor(df$gain)
df_no_na = subset(df, !is.na(df$gain))
plot <- ggplot(data = df_no_na, aes(x = case, fill=gain, group = gain))+
  geom_bar(stat="count", position='dodge')+
  scale_y_log10()+
  xlab("outbreak presence (1) vs. background (0)")

ggsave(plot = plot,
       filename = "forest_year_binary.jpg")
  
#get summary by disease, case, and gain
df_sum <- df %>%
  group_by(disease, case, gain) %>%
  summarize(count = n())

#get count
df_sum_categories <- df_sum %>%
  group_by(disease) %>%
  summarize(count = n())

df_sum_both = subset(df_sum_categories, count > 2)
df_sum_both$disease

df_deforest = subset(df_no_na, disease %in% df_sum_both$disease)

df_deforest$gain = as.character(df_deforest$gain)
df_deforest$case = as.character(df_deforest$case)

df.test <-
  df_deforest %>% 
  group_by(disease) %>% 
  do(fit = chisq.test(.$gain, .$case)) %>% 
  tidy(fit)
df.test = data.frame(df.test)

plot <- ggplot(data = df_deforest, aes(x = case, fill=gain, group = gain))+
  geom_bar(stat="count", position=position_dodge(preserve = "single"))+
  scale_y_log10()+
  facet_wrap(~disease)+
  theme(strip.text.x = element_text(size = 6, colour = "black"))+
  xlab("outbreak presence (1) vs. background (0)")

plot
ggsave(plot = plot,
       filename = "forest_year_binary_facet.jpg", height = 7, width = 7, units = "in")


plot <- ggplot(data = df_deforest, aes(x = case, fill=gain, group = gain))+
  geom_bar(stat="count", position='dodge')+
  scale_y_log10()+
  facet_wrap(~animal.species)+
  theme(strip.text.x = element_text(size = 6, colour = "black"))+
  xlab("outbreak presence (1) vs. background (0)")

plot
ggsave(plot = plot,
       filename = "forest_year_binary_facet_animal_species.jpg", height = 7, width = 7, units = "in")

df.test <-
  df_deforest %>% 
  group_by(animal.origin) %>% 
  do(fit = chisq.test(.$gain, .$case)) %>% 
  tidy(fit)
df.test = data.frame(df.test)


plot <- ggplot(data = df_deforest, aes(x = case, fill=gain, group = gain))+
  geom_bar(stat="count", position='dodge')+
  scale_y_log10()+
  facet_wrap(~animal.origin)+
  theme(strip.text.x = element_text(size = 6, colour = "black"))+
  xlab("outbreak presence (1) vs. background (0)")

plot
ggsave(plot = plot,
       filename = "forest_year_binary_facet_animal.origin.jpg", height = 7, width = 7, units = "in")




#chisquare test
print(chisq.test(df_no_na$deforest_year,df_no_na$case))
