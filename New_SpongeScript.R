library(tidyr)
library(dplyr)
library(ggplot2)
library(gridExtra) #puts the plots together 

#call in the data
rawsponge <- read.csv("Spongedata.csv",
                      header = TRUE,
                      stringsAsFactors = TRUE)

##### Clean the data #####

#make all the na's into NA 
rawsponge <- na_if(rawsponge, "na") 

#create a new dataframe to preserve the whole sponge rows in 
wholesponges <- rawsponge

#cut out NA rows 
#this isnt necessary but it removes warming messages so i like it
wholesponges <- filter(wholesponges,
                       OscPer != "NA") 

#first make all the values characters so they can be make numeric
wholesponges[ ,2:18]<- 
  sapply(wholesponges[ ,2:18], as.character)

#make all the values numeric so they can be used in geom_smooth functions
wholesponges[ ,2:18]<- 
  sapply(wholesponges[ ,2:18], as.numeric)

##### Flow plot #####

#make an axis label with a superscript
labelAY <- expression(Flow~Rate~(ml~cm^-1))

#make the plot for flow 
flowplot <- ggplot(wholesponges, 
                   aes(x = VolumeSponge, y = QperSponge)) +
  geom_point()+
  geom_smooth(formula = y ~ x,
              method=lm,
              color="black",
              fill="grey60",
              size=0.7,
              se=TRUE,
              level = 0.95)+
  theme_minimal() +
  theme(axis.line = element_line(colour="black"))+
  annotate(geom="text", x=1.5, y=0.9, 
           label="A", size=8,family="serif")+
  ylab(labelAY)+
  xlab("Sponge Volume (ml)")

flowplot

##### Gross resp. plot #####

#make an axis label with a superscript
labelBY <- expression(Gross~Resp.~Rate~(μmol~s^-1))

#make the plot for gross respiration 
grossrespplot <- ggplot(wholesponges, 
                        aes(x = VolumeSponge, y = OxRemovedSponge))+
  geom_point()+
  geom_smooth(formula = y ~ x,
              method=lm,
              color="black",
              fill="grey60",
              size=0.7,
              se=TRUE,
              level = 0.95)+
  theme_minimal() +
  theme(axis.line = element_line(colour="black"))+
  annotate(geom="text", x=1.5, y=17, 
           label="B", size=8, family="serif")+
  ylab(labelBY)+
  xlab("Sponge Volume (ml)")

grossrespplot

##### Oscula plot #####

#make an axis label with a superscript
labelCY2 <- expression(Surface~Area~(cm^2))

#set the coefficent to make the second axis scale
coeff <- 0.001

#make the plot for oxygen removed 
osculaplot <- ggplot(wholesponges, 
                     aes(x = VolumeSponge))+
  geom_point(aes(y = OscPer))+
  geom_point(aes(y = SurfaceArea / coeff), shape = 21)+
  geom_smooth(aes(y = OscPer),
              formula = y ~ x,
              method=lm,
              color="black",
              size=0.7,
              se=FALSE)+
  geom_smooth(aes(y = SurfaceArea / coeff),
              formula = y ~ x,
              method=lm,
              color="black",
              linetype = "dashed",
              size=0.7,
              se=FALSE)+
  scale_y_continuous(
    name = "Number of Oscula",
    sec.axis = sec_axis(~.*coeff, 
                        name = labelCY2))+
  theme_minimal() +
  theme(axis.line = element_line(colour="black"))+
  annotate(geom="text", x=2, y=70, 
           label="C", size=8, family="serif")+
  xlab("Sponge Volume (ml)")

osculaplot

##### Speed plot #####

#make an axis label with a superscript
labelDX <- expression(SA~per~Oscula~(cm^2))

labelDY <- expression(Exit~speed~(cm~s^-1))

#make the plot for speed 
speedplot <- ggplot(rawsponge, 
                    aes(x = QperOsc, y = Exspeed))+
  geom_point()+
  geom_smooth(formula = y ~ x,
              method=lm,
              color="black",
              fill="grey60",
              size=0.7,
              se=TRUE,
              level = 0.95)+
  theme_minimal() +
  theme(axis.line = element_line(colour="black"))+
  annotate(geom="text", x=0.01, y=1.75, 
           label="D", size=8,family="serif")+
  ylab(labelDY)+
  xlab(labelDX)

speedplot

##### put the plots together #####

#puts the plots together nicely 
grid.arrange(flowplot, grossrespplot, osculaplot, speedplot, ncol = 2) 

##### Dry plot just to check #####

#this isn't necessary we just did it to check something
dryplot <- ggplot(wholesponges, 
                  aes(x = gdw, y = QperSponge)) +
  geom_point()+
  geom_smooth(formula = y ~ x,
              method=lm,
              color="black",
              fill="grey60",
              size=0.7,
              se=TRUE,
              level = 0.95)+
  theme_minimal() +
  theme(axis.line = element_line(colour="black"))+
  ylab(labelAY)+
  xlab("Weight")

dryplot
