######Loading Data

library(tidyverse)

cces <- drop_na(read_csv(url("https://www.dropbox.com/s/ahmt12y39unicd2/cces_sample_coursera.csv?raw=1")))

cel <- drop_na(read_csv(url("https://www.dropbox.com/s/4ebgnkdhhxo5rac/cel_volden_wiseman%20_coursera.csv?raw=1")))




#Checking the gender status of respondents.


cces$gender <- recode(cces$gender, "1"="Male","2"="Female")

ggplot(cces,aes(x=gender,fill=gender))+geom_bar()+
  scale_fill_manual(values = c("red","blue"))



#Checking whether there is a connection between importance of religion and income level across genders.




ggplot(cces,aes(x=pew_religimp,y=faminc_new,color=gender))+geom_jitter()+
  scale_color_manual(values = c("red","blue"))+facet_wrap(~gender)+
  labs(x="Religion Importance",y="Annual Income",
       title="Relation between Faith vs Income Level")



#Frequency of different ideologies


cces$ideology <- recode(cces$ideo5,"1"="Very Liberal","2"="Liberal","3"="Moderate","4"="Conservative","5"="Very Conservatite")

ggplot(cces,aes(x=ideology,fill=ideology))+geom_histogram(stat="count",position ="dodge")+
  theme_dark()




#Checking how many bills did the members introduce in terms of both Democrats and Republicans across the years.


cel$dem <- recode(cel$dem, "1"="Democrat","0"="Republican")

ggplot(cel,aes(x=year,y=all_bills,linetype=dem,group=dem,color=dem))+geom_line()+
  scale_color_manual(values = c("blue","red"))+
  annotate("text",x=1985,y=250,label="Bills peaked before 1980",size=3)+
  labs(y="Bills Introduced")+theme(axis.text.x =element_text(angle=45,hjust=1),
                                   axis.title = element_text(color="purple"),
                                   legend.position = "top")+
  guides(fill=guide_legend(title="Party"))


