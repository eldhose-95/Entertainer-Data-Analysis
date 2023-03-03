#                 ENTERTAINER DATA ANALYSIS USING R
###                   AUTHOR: ELDHOSE VARGHESE

pacman::p_load(pacman, rio, tidyverse) # loading required packages

#            Importing all the data files

df1 <- import("E:/Data/Entertainer - Basic Info.xlsx") %>% as_tibble()
df1
df2 <- import("E:/Data/Entertainer - Breakthrough Info.xlsx") %>% as_tibble()
df2
df3 <- import("E:/Data/Entertainer - Last work Info.xlsx") %>% as_tibble()
df3
df <- import("E:/Data/Entertainer - Awards info.xlsx") %>% as_tibble()
df

# After examining all the 4 data files, only the last file contains 
# relevant data which can be used for analysis and the rest is ignored

#                     1. Distribution of Entertainers

g <- df %>% ggplot(aes(Profession))
g + geom_bar(fill = "#9932CC")

# Most of the entertainers are Actors (almost 2 * singers)

df %>%
  ggplot(aes(Profession, fill = Gender)) + 
  geom_bar(position = position_dodge()) +
  theme(legend.position = "bottom")

# In both category of Entertainers(actors & singers) the majority are males

#                     2. Entertainer Performance

df <- df[order(df$Total_Awards_won,decreasing = TRUE),]
df

barplot(df$Total_Awards_won,names.arg=df$Entertainer,
        xlab="Entertainer",ylab="Total_Awards_won",col="blue",
        main="Top entertainers",border="red")

top <- df[1:5,]
top

barplot(top$Total_Awards_won,names.arg=top$Entertainer,
        xlab="Entertainer",ylab="Total_Awards_won",col="blue",
        main="Top 5 entertainers",border="red")

# Meryl Streep recieved the highest number of awards 
# followed by Leonardo Dicaprio and Tom hanks
 
                     # 2.1 OSCAR AWARDS

df <- df[order(df$Oscar_Award,decreasing = FALSE),]
df

osc <- df[69:70,]
osc

barplot(osc$Oscar_Award,names.arg=osc$Entertainer,
        xlab="Total_Oscar_Awards",ylab="Entertainer",col="red",
        main="Oscar won by Entertainer",border="black",horiz=TRUE)

#Katherine Hepburn has got the highest number of Oscar awards 
#followed by Meryl Streep


                       # 2.2  GRAMMY AWARDS

df <- df[order(df$Grammy_Award,decreasing = TRUE),]
df

gra <- df[1:5,]
gra

barplot(gra$Grammy_Award,names.arg=gra$Entertainer,
        xlab="Entertainer",ylab="Total_Grammy_Awards",col="green",
        main="Grammy won by Entertainer",border="black")

# Stevie Wonder received the highest number of Grammy Awards

                        # 2.3  EMMY AWARDS

df <- df[order(df$Emmy_Award,decreasing = FALSE),]
df

emm <- df[69:70,]
emm

barplot(emm$Emmy_Award,names.arg=emm$Entertainer,
        xlab="Total_Emmy_Awards",ylab="Entertainer",col="orange",
        main="Emmy won by Entertainer",border="black",horiz=TRUE)

# Oprah Winfrey won the highest number of Emmy Awards

                       # 3.NOMINEES FROM EACH CATEGORY

dfn = df %>% group_by(Profession)  %>%
  summarise(total_nominations = sum(Total_Nominations),
            .groups = 'drop')
dfn

piepercent<-round(100 * dfn$total_nominations /
                      sum(dfn$total_nominations),0)

pie(dfn$total_nominations, labels = piepercent ,
    main = "Percentage Nominations by Profession",
    col = topo.colors(length(dfn$total_nominations)))

legend("topright",dfn$Profession,cex = 0.7,
          fill = topo.colors(length(dfn$total_nominations))) 

                      #  4. TOTAL AWARDS BY PROFESSION

dft = df %>% group_by(Profession)  %>%
  summarise(Total_Awards_won = sum(Total_Awards_won),
            .groups = 'drop')
dft

pie(dft$Total_Awards_won, labels = dft$Total_Awards_won ,
    main = "Total Awards by Profession",
    col = heat.colors(length(dft$Total_Awards_won)))

legend("topright",dfn$Profession,cex = 0.7,
       fill = heat.colors(length(dft$Total_Awards_won))) 






