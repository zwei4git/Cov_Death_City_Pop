library(tidyverse)
library(ggplot2)
library(gganimate)
library(gifski)
library(scales)


cov_df<-read.csv2('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv',header = TRUE,sep = ',')
cov_df<-as.data.frame(cov_df)
head(cov_df)
colnames(cov_df)

#City pop: https://en.wikipedia.org/wiki/List_of_United_States_cities_by_population

##Data prep, set date format
#manually entered cities by picking the closest death # date to city pop. 
#can also use fuzzy_merge
cov_df %>%
  mutate(
    date = as.Date(date, '%Y-%m-%d'),
    city = if_else(
      deaths == 121934,
      'Hartford, CT \n Population: 122,105',
      if_else(
        deaths == 151172,
        'Pasadena, TX \n Population: 151,227',
        if_else(
          deaths == 200738,
          'Salt Lake City, UT \n Population: 200,567',
          if_else(
            deaths == 230510,
            'Richmond, VA \n Population: 230,436',
            if_else(
              deaths == 259844,
              'Madison, WI \n Population: 259,680',
              if_else(
                deaths == 299328,
                'Pittsburgh, PA \n Population: 300,286',
                if_else(
                  deaths == 350329,
                  'Anaheim, CA \n Population: 350,365',
                  if_else(
                    deaths == 401824,
                    'New Orleans, LA \n Population: 390,144', NULL)))))))))->to_plot

to_plot%>%
  ggplot(aes(x =date, y=deaths,group=1))+
  geom_label(aes(x =date-24,label=city,group=city),show.legend = FALSE,color='black',
             hjust=1,size=2,
             data=to_plot[!is.na(to_plot$city),])+
  geom_line(color='white')+
  geom_point(aes(group=city),
             size=2,color='white',data=to_plot[!is.na(to_plot$city),])+
  scale_y_continuous(labels = scales::comma)+
  scale_x_date(date_breaks = "1 month", 
               labels=date_format("%b-%Y"))+
  labs(subtitle = 'Cumulative count of death',
       x=NULL,y=NULL)+
  theme_bw()+
  theme(plot.margin = margin(t = 40,r = 20,l = 40,b = 40),
        panel.border = element_blank(),
        axis.line = element_line(color = 'white'),
        axis.text = element_text(color = 'white'),
        axis.text.x = element_text(angle = 45,vjust=1,hjust = 1),
        title = element_text(color = 'white'),
        panel.grid = element_blank(),
        plot.title.position = "plot",
        plot.background = element_rect(fill = 'black'),
        panel.background = element_rect(fill = 'black'))+
  transition_reveal(date)+
  view_follow(fixed_x = c(as.Date('2020-01-21','%Y-%m-%d'), NA))->anim_plot

animate(anim_plot,duration = 24, fps = 10, end_pause = 80,
        height=800,width=1000, res=150,renderer = gifski_renderer())


