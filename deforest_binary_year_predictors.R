load("predictorPoints.Rdata")
df = predictorPoints

df = out_deforest#temporary df
df$year = 2019
#make deforest binary (has it occurred in location or not)

df$deforest = NA
df$deforest[df$lossyear>0]=1
df$deforest[df$lossyear==0]=0

# df$lossyear[df$lossyear == 0]=NA
# df$lossyear[df$deforest == 1]= df$lossyear[df$deforest == 1]+2000
# table(df$deforest,df$case)
# 
# df$deforest_year = NA
# df$deforest_year[df$lossyear<=df$year]=1#deforest loss year is less than or equal to year of outbreak
# #define as 0 if deforest year was after outbreak year OR never deforested
# df$deforest_year[df$lossyear>df$year]=0
# df$deforest_year[df$deforest==0]=0#not deforested 

predictorPoints = df
save(predictorPoints, file = "predictorPoints.Rdata")

# tmp = subset(df, is.na(deforest_year))
#Using GGPLOT, plot the Base World Map
# mp <- NULL
# mapWorld <- borders("world", 
#                     ylim = c(-60,80)) # create a layer of borders
# mp <- ggplot() +   mapWorld
# #Now layer the deforest years on top
# df$deforest_year=factor(df$deforest_year)
# df_observed = subset(df, case == 1 &deforest_year == 1)
# mp <- mp+ geom_point(data = df_observed, aes(x=longitude, y=latitude, color=lossyear),
#                      alpha = 0.5)
#   # theme(legend.position="none")#remove legend+
# mp
# 
# # plot <- ggplot(df, aes(x = longitude, y = latitude, color = animal.species))+
# #   geom_point(alpha = 0.5)
# # 
# # plot
# ggsave(plot = mp, file = "deforest_year_binary_map.jpg",
#        height = 4, width = 5, units = "in")
# 
# #make graph of case vs. deforest
# df$case = factor(df$case)
# df$deforest_year = factor(df$deforest_year)
# 
# df_no_na = subset(df, !is.na(df$deforest_year))
# 
# plot <- ggplot(data = df_no_na, aes(x = case, fill=deforest_year, group = deforest_year))+
#   geom_bar(stat="count", position='dodge')+
#   scale_y_log10()+
#   xlab("outbreak presence (1) vs. background (0)")
# 
# ggsave(plot = plot,
#        filename = "deforest_year_binary.jpg")
#   
# # fakedata <- 
# #   data.frame(groups = sample(c("pop1", "pop2", "pop3", "pop4"), 120, replace = T),
# #              results = sample(c("A","B","C"), 120, replace = TRUE),
# #              test = sample(c("test1", "test2","test3"), 120, replace = TRUE))
# # 
# # fakedata.test <-
# #   fakedata %>% 
# #   group_by(groups) %>% 
# #   do(fit = chisq.test(.$results, .$test)) %>% 
# #   tidy(fit)
# 
# df_sum <- df_no_na %>%
#   group_by(disease, case, deforest_year) %>%
#   summarize(count = n())
# 
# df_sum_categories <- df_sum %>%
#   group_by(disease) %>%
#   summarize(count = n())
# 
# df_sum_both = subset(df_sum_categories, count > 2)#must be seen as deforest in at least one of outbreak and background
# df_sum_both$disease
# 
# df_no_na = subset(df_no_na, disease %in% df_sum_both$disease)
# df_deforest= df_no_na
# df_deforest$deforest_year = as.character(df_deforest$deforest_year)
# df_deforest$case = as.character(df_deforest$case)
# 
# df.test <-
#   df_deforest %>% 
#   group_by(disease) %>% 
#   do(fit = chisq.test(.$deforest_year, .$case)) %>% 
#   tidy(fit)
# df.test = data.frame(df.test)
# 
# plot <- ggplot(data = df_deforest, aes(x = case, fill=deforest_year, group = deforest_year))+
#   geom_bar(stat="count", position=position_dodge(preserve = "single"))+
#   scale_y_log10()+
#   facet_wrap(~disease)+
#   theme(strip.text.x = element_text(size = 6, colour = "black"))+
#   xlab("outbreak presence (1) vs. background (0)")
# 
# plot
# ggsave(plot = plot,
#        filename = "deforest_year_binary_facet.jpg", height = 7, width = 7, units = "in")
# 
# 
# plot <- ggplot(data = df_no_na, aes(x = case, fill=deforest_year, group = deforest_year))+
#   geom_bar(stat="count", position=position_dodge(preserve = "single"))+
#   scale_y_log10()+
#   facet_wrap(~animal.species)+
#   theme(strip.text.x = element_text(size = 6, colour = "black"))+
#   xlab("outbreak presence (1) vs. background (0)")
# 
# plot
# ggsave(plot = plot,
#        filename = "deforest_year_binary_facet_animal_species.jpg", height = 7, width = 7, units = "in")
# 
# df.test <-
#   df_deforest %>% 
#   group_by(animal.origin) %>% 
#   do(fit = chisq.test(.$deforest_year, .$case)) %>% 
#   tidy(fit)
# df.test = data.frame(df.test)
# 
# 
# plot <- ggplot(data = df_deforest, aes(x = case, fill=deforest_year, group = deforest_year))+
#   geom_bar(stat="count", position='dodge')+
#   scale_y_log10()+
#   facet_wrap(~animal.origin)+
#   theme(strip.text.x = element_text(size = 6, colour = "black"))+
#   xlab("outbreak presence (1) vs. background (0)")
# 
# plot
# ggsave(plot = plot,
#        filename = "deforest_year_binary_facet_animal.origin.jpg", height = 7, width = 7, units = "in")
# 
# 
# 
# 
# #chisquare test
# print(chisq.test(df_no_na$deforest_year,df_no_na$case))
