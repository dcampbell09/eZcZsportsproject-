
#DylanCampbell eZcZ Stat Class

```{r}
#call work folder
getwd()
```

```{r}
#set it to project folder
setwd("/Users/dcampbell/Desktop/eZcZ Data Project/")
```

```{r}
#call packages 
library(tidyverse)
library(ggplot2)
library(PerformanceAnalytics)
```

```{r}
#call data
fg_data <- read.csv("FranGraphsSecondaryData.csv")
fg_data

```{r}
#select column regarding player ID 
player_data <- fg_data%>%arrange(Name,Season)
player_data$seas_ind <- 1
```

```{r}
#filter to show 1 & 2 year player data only
player_data <- player_data%>%group_by(Name)%>%mutate(cs = cumsum(seas_ind))
one_two_yr_players <- player_data%>%filter(cs %in% c(1,2))
```

```{r}
#create percentages as numeric 
one_two_yr_players$BB. <- as.numeric(sub("%","", one_two_yr_players$BB.))
one_two_yr_players$K. <- as.numeric(sub("%","", one_two_yr_players$K.))
```

```{r}
#filter player ids who have 2 seasons in dataset 
ids <-  one_two_yr_players%>%
  group_by(playerid)%>%
  summarise(ct=length(playerid))%>%
  filter(ct==2)%>%
  select(playerid)

```{r}
#filter ids, arrange by name, season
one_two_yr_players <- one_two_yr_players%>%
  filter(playerid %in% ids$playerid)%>%
  arrange(Name,Season)
```

```{r}
#label season count as factor
one_two_yr_players$cs <- as.factor(one_two_yr_players$cs)
```

```{r}
#plot wRC between seasons 1 & 2
ggplot(data=one_two_yr_players,aes(x=wRC.))+
  geom_density(aes(fill=cs),alpha=.4)+
  theme_bw()+facet_wrap(~cs)
```

```{r}
#install plotly
install.packages('plotly')
library(plotly)

```{r}
#scale BB% & K% between 1st and 2nd year
ggplotly(
  ggplot(data=one_two_yr_players,aes(x=scale(BB.),y=scale(K.)))+
    geom_point(aes(color=cs,text=Name))+
    theme_bw()+facet_wrap(~cs)+geom_smooth()
)

```{r}
#label 1st season as x, 2nd season as y
x <- one_two_yr_players%>%
  filter(cs==1)%>%
  select(wRC.)
y <- one_two_yr_players%>%
  filter(cs==2)%>%
  select(wRC.)

```

```{r}
#compare each player's 1st and 2nd season
compare_frame <- cbind(x,y$wRC.)
colnames(compare_frame) <- c("Name","wRC1","wRC2")
```

```{r}
#Correlation plot of wRC from 1st year to 2nd year
ggplotly(
  ggplot(data=compare_frame,aes(x=wRC1,y=wRC2))+
    geom_point(aes(text=Name))+theme_bw()+geom_smooth(method="lm")+
    ggtitle("wRC Season 2 vs. wRC Season 1"))
```

```{r}
#break data into season, remove season year and count
season_1 <- one_two_yr_players%>%filter(cs==1)%>%select(-c(seas_ind,cs))
season_2 <- one_two_yr_players%>%filter(cs==2)%>%select(-c(seas_ind,cs))

#merge each player's 1st and second seasons
combined <- merge(season_1,season_2,by="playerid")

#create ratios
combined <- combined%>%mutate(
  wrc_ratio=wRC..y/wRC..x)
```

```{r}
#walk % in relation to wRC ratio 
ggplotly(
ggplot(data=combined,aes(x=BB..x,y=BB..y))+
  geom_point(aes(col=wrc_ratio,text=Name.x))+
  theme_bw()+
  geom_smooth(method="lm")+
  labs(x="BB% Season 1",
       y="BB% Season 2")  +
  scale_colour_gradientn(colours = terrain.colors(10))+
  ggtitle("BB% Season 2 vs. BB% Season 1")
)
lm_BB <- lm(data=combined,BB..y~BB..x)
summary(lm_BB)
par(mfrow=c(2,2))
plot(lm_BB)
```

```{r}
#ISO in relation to wRC ratio
ggplotly(
  ggplot(data=combined,aes(x=ISO.x,y=ISO.y))+
    geom_point(aes(col=wrc_ratio,text=Name.x))+
    theme_bw()+
    geom_smooth(method="lm")+
    labs(x="ISO Season 1",
         y="ISO Season 2")  +
    scale_colour_gradientn(colours = terrain.colors(10))+
    ggtitle("ISO Season 2 vs. ISO Season 1")
)

lm_ISO <- lm(data=combined,ISO.y~ISO.x)
summary(lm_ISO)
par(mfrow=c(2,2))
plot(lm_ISO)

```{r}
#wOBA in relation to wRC ratio
ggplotly(
  ggplot(data=combined,aes(x=wOBA.x,y=wOBA.y))+
    geom_point(aes(col=wrc_ratio,text=Name.x))+
    theme_bw()+
    geom_smooth(method="lm")+
    labs(x="wOBA Season 1",
         y="wOBA Season 2")  +
    scale_colour_gradientn(colours = terrain.colors(10))+
    ggtitle("wOBA Season 2 vs. wOBA Season 1")
)

lm_wOBA <- lm(data=combined,wOBA.y~wOBA.x)
summary(lm_wOBA)
par(mfrow=c(2,2))
plot(lm_wOBA)

#find outliers and remove them using NHL Data from Case Study

```{r}
#import secondary data to find plate discipline 
setwd("/Users/dcampbell/Desktop/eZcZ Data Project/")
fg_pd <- read.csv("FgPlateDiscipline .csv")
```

```{r}
#filter to show 1 & 2 year player data only
player_pd <- fg_pd%>%arrange(Name,Season)
player_pd$seas_ind <- 1

player_pd <- player_pd%>%group_by(Name)%>%mutate(cs = cumsum(seas_ind))
one_two_pd <- player_pd%>%filter(cs %in% c(1,2))
```

```{r}
#turn percentages into numeric 
one_two_pd$O.Swing. <- as.numeric(sub("%","", one_two_pd$O.Swing.))
one_two_pd$Z.Swing. <- as.numeric(sub("%","", one_two_pd$Z.Swing.))
one_two_pd$Swing. <- as.numeric(sub("%","", one_two_pd$Swing.))
one_two_pd$O.Contact. <- as.numeric(sub("%","", one_two_pd$O.Contact.))
one_two_pd$Z.Contact. <- as.numeric(sub("%","", one_two_pd$Z.Contact.))
one_two_pd$Contact. <- as.numeric(sub("%","", one_two_pd$Contact.))
one_two_pd$Zone. <- as.numeric(sub("%","", one_two_pd$Zone.))
one_two_pd$F.Strike. <- as.numeric(sub("%","", one_two_pd$F.Strike.))
one_two_pd$SwStr. <- as.numeric(sub("%","", one_two_pd$SwStr.))
```

```{r}
#plate discipline 
pd_ids <-  one_two_pd%>%
  group_by(playerid)%>%
  summarise(ct=length(playerid))%>%
  filter(ct==2)%>%
  select(playerid)
```

```{r}
#arrange by name, season
one_two_pd <- one_two_pd%>%
  filter(playerid %in% ids$playerid)%>%
  arrange(Name,Season)
```

```{r}
#label season as count factor
one_two_pd$cs <- as.factor(one_two_pd$cs)
```

```{r}
#label first and second season
x_pd <- one_two_pd%>%
  filter(cs==1)
y_pd <- one_two_pd%>%
  filter(cs==2)
```

```{r}
#label first and second season  
season_1_pd <- one_two_pd%>%filter(cs==1)%>%select(-c(seas_ind,cs))
season_2_pd <- one_two_pd%>%filter(cs==2)%>%select(-c(seas_ind,cs))

```{r}
#merge players
combined_pd <- merge(season_1_pd,season_2_pd,by="playerid")

```{r}
#create contact ratio
combined_pd <- combined_pd%>%mutate(
  contact_ratio = Contact..y/Contact..x)
```

```{r}
#look at swing% relative to contact ratio
ggplotly(
  ggplot(data=combined_pd,aes(x=Swing..x,y=Swing..y))+
  geom_point(aes(col=contact_ratio,text=Name.x))+
  theme_bw()+
  stat_smooth(method="lm", col="red", size=1)+
  labs(x="BB% Season 1",
        y="BB% Season 2")  +
  ggtitle("BB% Season 2 vs. BB% Season 1")
)

lm_swingrate <- lm(data=combined_pd,Swing..y~Swing..x)
summary(lm_swingrate)
par(mfrow=c(2,2))
plot(lm_swingrate)
```

```{r}
#merge both data frames
offensive_numbers <- merge(combined,combined_pd,by.x= 'playerid' ,by.y= 'playerid')

offensive_numbers <- offensive_numbers%>%mutate(
  walk_ratio = BB..y/BB..x)
```

```{r}
#plotly contac raio relative to walk ratio
ggplotly(
  ggplot(data=offensive_numbers,aes(x=Contact..x,y=Contact..y))+
  geom_point(aes(col=walk_ratio,text=Name.x.x))+
  theme_bw()+
  stat_smooth(method="lm",col="red",size=1)+
  labs(x="Contact% Season 1",
       y ="Contact% Season 2") +
    ggtitle("Contact% Season 2 vs. Contact% Season 1")
)

lm_contactrate <- lm(data=offensive_numbers,Contact..y~Contact..x)
summary(lm_contactrate)
par(mfrow=c(2,2))
plot(lm_contactrate)

ggplotly(
  ggplot(data=offensive_numbers,aes(x=Contact..x,y=Contact..y))+
    geom_point(aes(col=walk_ratio,text=Name.x.x))+
    theme_bw()+
    stat_smooth(method="lm",col="red",size=1)+
    labs(x="Contact% Season 1",
         y ="Contact% Season 2") +
    scale_colour_gradientn(colours = terrain.colors(10))+
    ggtitle("Contact% Season 2 vs. Contact% Season 1")
)

ggplotly(
  ggplot(data=offensive_numbers,aes(x=,y=Contact..y))+
    geom_point(aes(col=walk_ratio,text=Name.x.x))+
    theme_bw()+
    stat_smooth(method="lm",col="red",size=1)+
    labs(x="Contact% Season 1",
         y ="Contact% Season 2") +
    ggtitle("Contact% Season 2 vs. Contact% Season 1")
)
```
```{r}
#outside contact percentage 
ggplotly(
  ggplot(data=offensive_numbers,aes(x=O.Contact..x,y=Contact..y))+
    geom_point(aes(col=contact_ratio,text=Name.x.x))+
    theme_bw()+
    stat_smooth(method="lm",col="red",size=1)+
    labs(x="Outside Contact% Season 1",
         y ="Outside Contact% Season 2") +
    scale_colour_gradientn(colours = terrain.colors(10)) +
    ggtitle("Outside Contact% Season 2 vs. Outside Contact% Season 1")
)

lm_o_contact <- lm(data=offensive_numbers,O.Contact..y~O.Contact..x)
summary(lm_o_contact)
par(mfrow=c(2,2))
plot(lm_o_contact)
```

```{r}
#Outside Swing Rate
ggplotly(
  ggplot(data=offensive_numbers,aes(x=O.Swing..x,y=O.Swing..y))+
    geom_point(aes(col=contact_ratio,text=Name.x.x))+
    theme_bw()+
    stat_smooth(method="lm",col="red",size=1)+
    labs(x="Outside Swing Rate Season 1",
         y ="Outside Swing Rate Season 2") +
    ggtitle("Outside Swing Rate Season 2 vs. Outside Swing Rate Season 1")
)

lm_o_swing <- lm(data=offensive_numbers,O.Swing..y~O.Swing..x)
summary(lm_o_swing)
par(mfrow=c(2,2))
plot(lm_o_swing)
```
```{r}
offensive_numbers <- offensive_numbers%>%mutate(
  o_contact_ratio = O.Contact..y/O.Contact..x)
```

```{r}
ggplotly(
  ggplot(data=offensive_numbers,aes(x=O.Swing..x,y=O.Swing..y))+
    geom_point(aes(col=o_contact_ratio,text=Name.x.x))+
    theme_bw()+
    stat_smooth(method="lm",col="red",size=1)+
    labs(x="Outside Swing Rate Season 1",
         y ="Outside Swing Rate Season 2") +
    ggtitle("Outside Swing Rate Season 2 vs. Outside Swing Rate Season 1")
)

lm_o_contact <- lm(data=offensive_numbers,O.Swing..y~O.Swing..x)
summary(lm_o_swing)
par(mfrow=c(2,2))
plot(lm_o_swing)
```

```{r}
ggplotly(
  ggplot(data=offensive_numbers,aes(x=O.Swing..x,y=O.Swing..y))+
    geom_point(aes(col=wrc_ratio,text=Name.x.x))+
    theme_bw()+
    stat_smooth(method="lm",col="red",size=1)+
    labs(x="Outside Swing Rate Season 1",
         y ="Outside Swing Rate Season 2") +
    scale_colour_gradientn(colours = terrain.colors(10))+
    ggtitle("Outside Swing Rate Season 2 vs. Outside Swing Rate Season 1")
)
```
```{r}
ggplotly(
  ggplot(data=offensive_numbers,aes(x=Contact..x,y=Contact..y))+
    geom_point(aes(col=wrc_ratio,text=Name.x.x))+
    theme_bw()+
    stat_smooth(method="lm",col="red",size=1)+
    labs(x="Outside Swing Rate Season 1",
         y ="Outside Swing Rate Season 2") +
    scale_colour_gradientn(colours = terrain.colors(10))+
    ggtitle("Outside Swing Rate Season 2 vs. Outside Swing Rate Season 1")
)
```
```{r}
ggplotly(
  ggplot(data=combined,aes(x=wOBA.x,y=wOBA.y))+
    geom_point(aes(col=wrc_ratio,text=Name.x))+
    theme_bw()+
    geom_smooth(method="lm")+
    labs(x="wOBA Season 1",
         y="wOBA Season 2")  +
    scale_colour_gradientn(colours = terrain.colors(10))+
    ggtitle("wOBA Season 2 vs. wOBA Season 1")
)
```

```{r}
#woba ratio then relative to BB%
offensive_numbers <- offensive_numbers%>%mutate(
  woba_ratio=wOBA.y/wOBA.x
)

ggplotly(
  ggplot(data=offensive_numbers,aes(x=BB..x,y=BB..y))+
    geom_point(aes(col=woba_ratio,text=Name.x.x))+
    theme_bw()+
    stat_smooth(method="lm",col="red",size=1)+
    labs(x="BB% Season 1",
         y ="BB% Season 2") +
    scale_colour_gradientn(colours = terrain.colors(10))+
    ggtitle("BB% Season 2 vs. BB% Season 1")
)
```
```{r}
ggplotly(
  ggplot(data=offensive_numbers,aes(x=O.Contact..x,y=O.Contact..y))+
    geom_point(aes(col=wrc_ratio,text=Name.x.x))+
    theme_bw()+
    stat_smooth(method="lm",col="red",size=1)+
    labs(x="Contact Rate Season 1",
         y ="Contact Rate Season 2") +
    scale_colour_gradientn(colours = terrain.colors(10))+
    ggtitle("Contact Rate Season 2 vs. Contact Rate Season 1")
)
```



