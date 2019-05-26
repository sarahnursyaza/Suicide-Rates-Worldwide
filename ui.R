library(shiny)
library(dplyr) 
library(stringr)
library(ggplot2)
library(DBI)
library(RPostgres)
library(shinydashboard)
library(tidyr) 
library(lattice)
library(ggrepel)
library(ggpubr)
library(shinyWidgets)
library(DT)
library(ggthemes)
library(devtools)


source("functions.R")
#=============================================Load Data=========================================================

data <- read.csv("datasuicide.csv", header = TRUE)
ls(data)
data <- data %>% select(country, year, suicides_no,sex,age, population,suicides.100k.pop, gdp_for_year...., gdp_per_capita....,HDI.for.year)


average_temp <- read.csv("average_temp.csv", header = TRUE)
ls(average_temp)
average_temp<- average_temp %>% select(country,Average)
average_temp.cor <- average_temp
names(average_temp)<- c("X1","Average_temperature")


Gini_data <- read.csv("Gini.csv", header = TRUE)
ls(Gini_data)

Gini_data.cor <- Gini_data
names(Gini_data.cor) <- c("country","avg.Gini")


### summarize Data 
Countries.information <- data.frame(matrix(ncol=10,nrow = length(unique(data$country))))

Countries.information[,1:2] <-  data %>%
  select(country,year) %>%
  group_by(country) %>%
  summarise(
    year_start = (min(year,na.rm=TRUE)),
    year_end = (max(year,na.rm=TRUE))) %>%
  unite("Years",year_start,year_end,sep = " - ")

Countries.information[,3:7] <-  data %>%
  select(country,year,suicides_no,suicides.100k.pop,HDI.for.year,gdp_per_capita....) %>%
  group_by(country) %>%
  summarise(
    Total_Cases = sum(suicides_no,na.rm = TRUE),
    Total_Cases_per_100k = mean(suicides.100k.pop,na.rm = TRUE),
    Avg_GDP_per_capita = mean(gdp_per_capita....,na.rm=TRUE),
    Avg_HDI = mean(HDI.for.year,na.rm=TRUE))


avg.suicide_rates<- data %>% 
  select(country,year,suicides.100k.pop) %>% 
  group_by(country,year) %>% 
  summarise(yearly.suicide_rate = sum(suicides.100k.pop,na.rm = TRUE)) %>% 
  group_by(country) %>% 
  summarise(avg.suicide_rate = mean(yearly.suicide_rate,na.rm = TRUE)) %>% 
  select(avg.suicide_rate)

Countries.information[,8] <- round(avg.suicide_rates[,1],1)


#=================================  Plot country  =================================


P.data <- function(List){
  
  Country<-List[1]
  start<-List[2]
  end<-List[3]
  Sex<-List[[4]]
  Age<-List[[5]]
  
  Out.f.data <- data %>% 
    filter(
      country == Country&
        year >= start &
        year <= end &
        sex %in% Sex &
        age %in% Age)
  
  data$year <- as.factor(data$year)
  
  if(length(Sex)==2 & length(Age)>1){
    return(ggplot(Out.f.data,aes(x=Out.f.data$year,y=suicides.100k.pop,fill=age))+
             geom_col(width = 0.7)+
             facet_wrap(~sex,nrow = 2)+
             ggtitle("Suicide rates per year")+
             theme_economist() + 
             theme(panel.spacing = unit(2, "lines"))+
             ylab("cases/100k")+
             xlab("Year")+
             theme(legend.position="bottom",legend.title=element_blank(),legend.spacing.x = unit(0.5, 'cm'))+
             labs(fill="Age Groups")+
             scale_x_continuous(limits = c(round(min(Out.f.data$year),0), round(max(Out.f.data$year),0))))
  }
  
  else if(length(Sex)==3 & length(Age)==1){
    return(ggplot(Out.f.data,aes(x=year,y=suicides.100k.pop))+
             geom_col(fill="#4279E8",width = 0.7)+
             ggtitle("Suicide rates per year")+
             theme_economist() + 
             ylab("cases/100k")+
             xlab("Year")+
             scale_x_continuous(limits = c(round(min(Out.f.data$year),0), round(max(Out.f.data$year),0))))
  } 
  
  else if(length(Sex)==3 & length(Age)>1){
    return(ggplot(Out.f.data,aes(x=year,y=suicides.100k.pop,fill=age))+
             geom_col(width = 0.7)+
             ggtitle("Suicide rates per year")+
             theme_economist() + 
             ylab("cases/100k")+
             xlab("Year")+
             theme(legend.position="bottom",legend.title=element_blank(),legend.spacing.x = unit(0.5, 'cm'))+
             labs(fill="Age Groups")+
             scale_x_continuous(limits = c(round(min(Out.f.data$year),0), round(max(Out.f.data$year),0))))
  } 
  
  else if(length(Sex)==2 & length(Age)==1){
    return(ggplot(Out.f.data,aes(x=year,y=suicides.100k.pop))+
             geom_col(fill="#4279E8",width = 0.7)+
             facet_wrap(~sex,nrow = 2)+
             theme_economist() + 
             theme(panel.spacing = unit(2, "lines"))+
             ggtitle("Suicide rates per year")+
             ylab("cases/100k")+
             xlab("Year")+
             labs(fill="Age Groups")+
             scale_x_continuous(limits = c(round(min(Out.f.data$year),0), round(max(Out.f.data$year),0))))
  }
  
  else if(length(Sex)==1 & length(Age)>1){
    return(ggplot(Out.f.data,aes(x=year,y=suicides.100k.pop,fill=age))+
             geom_col(width = 0.7)+
             ggtitle("Suicide rates per year")+
             theme_economist() + 
             ylab("cases/100k")+
             xlab("Year")+
             theme(legend.position="bottom",legend.title=element_blank(),legend.spacing.x = unit(0.5, 'cm'))+
             labs(fill="Age Groups")+     
             scale_x_continuous(limits = c(round(min(Out.f.data$year),0), round(max(Out.f.data$year),0))))
    
  }
  
  else if(length(Sex)==1 & length(Age)==1){
    return(ggplot(Out.f.data,aes(x=year,y=suicides.100k.pop))+
             geom_col(fill="#4279E8",width = 0.7)+
             ggtitle("Suicide rates per year")+
             theme_economist() + 
             ylab("cases/100k")+
             xlab("Year")+
             scale_x_continuous(limits = c(round(min(Out.f.data$year),0), round(max(Out.f.data$year),0))))
  }
  else{return("Error")}
}


#=================================  Plots comparison  =================================


PP.compare <- function(List){
  countries<-List[[1]]
  start<-List[2]
  end<-List[3]
  Sex<-List[[4]]
  Age<-List[[5]]
  data_compared <- data %>%
    select(country,year,sex,age,suicides.100k.pop) %>%
    filter(country %in% countries,
           year >= start,
           year <= end,
           sex %in% Sex,
           age %in% Age) %>%
    group_by(country,year) %>%
    summarise(total_suicides.100k.pop = sum(suicides.100k.pop,na.rm = TRUE))
  
  Box.all <- ggplot(data = data_compared,
                    aes(x=country,y=total_suicides.100k.pop,color=country))+
    geom_boxplot(width=0.6,size=1)+
    ggtitle("Average suicide rates by country")+
    ylab("Average Suicide Rate")+
    theme_economist() + 
    theme(legend.position="none")+
    scale_fill_economist()
  return(Box.all)
}


LL.compare <- function(List){
  countries<-List[[1]]
  start<-List[2]
  end<-List[3]
  Sex<-List[[4]]
  Age<-List[[5]]
  
  data_compared <- data %>%
    select(country,year,sex,age,suicides.100k.pop) %>%
    filter(country %in% countries,
           year >= start,
           year <= end,
           sex %in% Sex,
           age %in% Age) %>%
    group_by(country,year) %>%
    summarise(total_suicides.100k.pop = sum(suicides.100k.pop,na.rm = TRUE))
  
  Line.lc <- ggplot(data = data_compared,
                    mapping=aes(x=year,y=total_suicides.100k.pop,group=country,color=country))+
    geom_line(size=1.3)+
    geom_point(size=2)+
    theme_linedraw()+
    ggtitle("Suicide rates by country")+
    ylab("Suicide rate")+
    theme_economist() + 
    theme(legend.position="bottom",legend.title=element_blank(),legend.spacing.x = unit(0.5, 'cm'))+
    scale_fill_economist()+
    scale_x_continuous(limits = c(min(data_compared$year), 
                                  max(data_compared$year)))
  return(Line.lc)
}


#=================================  ANOVA test =========================================================

Anova.compare.f <- function(List){
  countries<-List[[1]]
  start<-List[2]
  end<-List[3]
  Sex<-List[[4]]
  Age<-List[[5]]
  
  data_compared <- data %>%
    select(country,year,sex,age,suicides.100k.pop) %>%
    filter(country %in% countries,
           year >= start,
           year <= end,
           sex %in% Sex,
           age %in% Age) %>%
    group_by(country,year) %>%
    summarise(total_suicides.100k.pop = sum(suicides.100k.pop,na.rm = TRUE))
  
  Anova.compare<- aov(total_suicides.100k.pop ~ country,data_compared)
  
  summary(Anova.compare) # result ANOVA test
  
  TukeyTable <- TukeyHSD(Anova.compare)$country%>%  # result Tukey test
    as.data.frame() %>%
    tibble::rownames_to_column(var = "Pair") %>%
    transmute(
      Pair, 
      `P-Value` = round(`p adj`, 5), 
      Significance = `p adj` < 0.05      )
  
  return(TukeyTable)
  
}


#=================================  Plots correlations  =================================

Cor.plots <- function(List){
  start<-List[1]
  end<-List[2]
  Sex<-List[[3]]
  Age<-List[[4]]
  factor<-List[5]
  
  data_cor <- data %>%
    select(country,year,sex,age,suicides.100k.pop,gdp_per_capita....,HDI.for.year) %>%
    filter(year >= start,
           year <= end,
           sex %in% Sex,
           age %in% Age) %>%
    group_by(country,year) %>%
    summarise(total_suicides.100k.pop = sum(suicides.100k.pop,na.rm = TRUE),
              GDP_capita = mean(gdp_per_capita....,na.rm = TRUE),
              Avg.HDI = mean(HDI.for.year,na.rm = TRUE)) %>% 
    group_by(country) %>% 
    summarise(Avg.rate = mean(total_suicides.100k.pop,na.rm = TRUE),
              GDP_capita = mean(GDP_capita,na.rm = TRUE),
              Avg.HDI = mean(Avg.HDI,na.rm = TRUE))
  
  if (factor =="Gini Index"){data.cor.Gini <- merge(data_cor,Gini_data.cor,by="country")
  
  ggplot(data = data.cor.Gini,aes(x=avg.Gini,y=Avg.rate))+
    geom_point(size = 2,aes(x=avg.Gini,y=Avg.rate),color="black")+
    geom_smooth(method='lm',color="red",na.rm=TRUE,se=FALSE)+
    ggtitle("Suicide rates Vs. Average Gini Index")+
    ylab("Average Suicide Rate (per 100.k)")+
    xlab("Average Gini Index")+
    theme(axis.text.x = element_text(angle=65, vjust=0.7))+
    theme_economist()+
    stat_cor(method = "pearson",label.x.npc = 0.7)
  }
  else if (factor =="Temperature"){data.cor.temp <- merge(data_cor,average_temp.cor,by="country")
  
  ggplot(data=data.cor.temp,aes(x=Average,y=Avg.rate))+
    geom_point(size = 2,aes(x=Average,y=Avg.rate),color="black")+
    geom_smooth(method='lm',color="red",na.rm=TRUE,se=FALSE)+
    ggtitle("Suicide rates Vs. Average Anual Temperature (Celsius)")+
    ylab("Average Suicide Rate (per 100.k)")+
    xlab("Average Temperature")+
    theme(axis.text.x = element_text(angle=65, vjust=0.7))+
    theme_economist()+
    stat_cor(method = "pearson",label.x.npc = 0.7)
  }
  else if (factor =="Human Developement Index"){
    ggplot(data=data_cor,aes(x=Avg.HDI,y=Avg.rate))+
      geom_point(size = 2,aes(x=Avg.HDI,y=Avg.rate),color="black")+
      geom_smooth(method='lm',color="red",show.legend = TRUE,na.rm=TRUE,se=FALSE)+
      ggtitle("Suicide rates Vs. Human Developement Index")+
      ylab("Average Suicide Rate (per 100.k)")+
      xlab("Human Developement Index")+
      theme(axis.text.x = element_text(angle=65, vjust=0.7))+
      theme_economist()+
      stat_cor(method = "pearson",label.x.npc = 0.7)
  }
  else if (factor =="Gross Domestic Product"){
    ggplot(data=data_cor,aes(x=GDP_capita,y=Avg.rate))+
      geom_point(size = 2,aes(x=GDP_capita,y=Avg.rate),color="black")+
      geom_smooth(method='lm',color="red",na.rm=TRUE,se=FALSE)+
      ggtitle("Suicide rates Vs. GDP per capita")+
      ylab("Average Suicide Rate (per 100.k)")+
      xlab("GDP per person ($)")+
      theme(axis.text.x = element_text(angle=65, vjust=0.7))+
      theme_economist()+
      stat_cor(method = "pearson",label.x.npc = 0.7)
  }
  
}

#LIST1 <- list("Australia",2006,2015,list("female","male","x"),list("5-14 years","15-24 years","25-34 years","35-54 years","55-74 years","75+ years"))
#Var <- P.data(LIST)
#table(Var)
#L.compare(LIST)
#P.compare(LIST)
#LIST_3 <- list(list("Singapore","Armenia","Italy"), 2008,2012,list("female","male"),list("15-24 years","75+ years"))
#test <- Anova.compare.f(LIST_3)

# ====================  UI ========================================================================


ui <- fluidPage(
  
  includeCSS("styles.css"),
  
  fluidRow( div(id="titlePanel",br(),
                
                titlePanel(h2("Global Suicide Rates Analysis")),br())),
  
  column(11,subset = 1,tabsetPanel(
    
 # ====================  Tab Country view  ========================================================================
    
    tabPanel("Country View", fluid=TRUE,
             
             fluidRow(column(10,offset=1, 
                             h3("The following graphic presents detailed information 
        regarding suicide rates in a specific country. 
        Use the control panel on the left to filter the data."))),hr(),
             
             sidebarLayout(
               
               sidebarPanel(id="sidebarPanel", width=2,br(),
                            
                            sliderInput(
                              inputId = "Years_Slider2",
                              label = strong("Years"), 
                              min = 2006, 
                              max = 2015,
                              value = list(2006,2015),
                              round = TRUE,
                              ticks = FALSE,
                              dragRange=TRUE,
                              sep="",
                              width = "100%"),
                            fluidRow(br(),
                                     
                                     radioButtons(inputId ="Age",label = strong("Ages:"), 
                                                  choices=c("All"="All_Ages","5-14"="5-14 years",
                                                            "15-24"="15-24 years","25-34"="25-34 years",
                                                            "35-54"="35-54 years","55-74"="55-74 years",
                                                            "75+"="75+ years")),
                                     radioButtons(inputId ="Sex",label = strong("Sex:"), 
                                                  choices=c("Both"="Both_Sexes","Females"="female",
                                                            "Males"="male","Compare"="Compare_Sexes"))
                            )),
               
               mainPanel(
                 fluidRow(column(11,offset = 1,br(),
                                 selectInput(
                                   inputId = "Country",
                                   label = "Select Country",
                                   width = "35%",
                                   choices = unique(data$country),
                                   selected = "Italy"),br(),
                                 
                                 plotOutput("Plot", width = "70%",height=430),
                                 fluidRow(br(),h5("4Feet")))))
               
             )),
    
 # ====================  Tab Comparison view  ========================================================================
    
    tabPanel("Comparison View", fluid=TRUE,
             
             fluidRow(column(10,offset=1, 
                             h3("The following graphics compare the the suicide rate in different
        countries. Tukey-Anova test results are available in the table on the right."))),hr(),
             
             sidebarLayout(
               
               sidebarPanel(id="sidebarPanel",width=2,br(),
                            
                            sliderInput(
                              inputId = "Years_Slider3",
                              label = strong("Years"), 
                              min = 2006, 
                              max = 2015,
                              round = TRUE,
                              ticks = FALSE,
                              value = list(2006,2015),
                              dragRange=TRUE,
                              sep="",
                              width = "100%"),
                            
                            fluidRow(br(),
                                     
                                     radioButtons(inputId ="Age_c",label = strong("Ages:"), 
                                                  choices=c("All"="All_Ages","5-14"="5-14 years",
                                                            "15-24"="15-24 years","25-34"="25-34 years",
                                                            "35-54"="35-54 years","55-74"="55-74 years",
                                                            "75+"="75+ years")),              
                                     radioButtons(inputId ="Sex_c",label = strong("Sex:"), 
                                                  choices=c("Both"="Both_Sexes","Females"="female","Males"="male"))
                            )),
               
               mainPanel(br(),fluidRow(column(7,offset=1,
                                              
                                              selectizeInput("checked_countries","Select Countries",width = "75%",
                                                             choices = unique(data$country),
                                                             selected=c("Italy","Singapore","Belgium","United Kingdom"), multiple=TRUE,
                                                             options = list(maxItems = 15,highlight=TRUE,closeAfterSelect=TRUE)),br(),
                                              plotOutput("Comparison_ALL.l.Plot", height=300, width = 550),hr(),
                                              plotOutput("Comparison_ALL.Plot", height=250, width = 550)),
                                       
                                       column(2,offset = 2,br(),
                                              DT::dataTableOutput("tukey.table",width="200%"),br(),br(),
                                              fluidRow(h5("4Feet")))
                                       
               ))
             )),
    
 # ====================  Tab correlation ========================================================================
    
    tabPanel("Correlations", fluid=TRUE, 
             fluidRow(column(10,offset=1, 
                             h3("Here you can explore global correlations between suicide rates to
        several financial and climate factors. Every dot shows a different country. The red line 
        indicates the direction and the strength of the correlation. Significance is shown by a 
           p-value < 0.05"))),hr(),
             
             
             sidebarLayout(
               
               sidebarPanel(id="sidebarPanel",width=2,br(),
                            
                            sliderInput(
                              inputId = "Years_Slider4",
                              label = strong("Years"), 
                              min = 2006, 
                              max = 2015,
                              value = list(2006,2015),
                              round = TRUE,
                              ticks = FALSE,
                              dragRange=TRUE,
                              sep="",
                              width = "100%"),
                            
                            fluidRow(br(),
                                     
                                     radioButtons(inputId ="Age_cor",label = strong("Ages:"), 
                                                  choices=c("All"="All_Ages","5-14"="5-14 years",
                                                            "15-24"="15-24 years","25-34"="25-34 years",
                                                            "35-54"="35-54 years","55-74"="55-74 years",
                                                            "75+"="75+ years")),              
                                     radioButtons(inputId ="Sex_cor",label = strong("Sex:"), 
                                                  choices=c("Both"="Both_Sexes","Females"="female","Males"="male"))
                            )),
               
               mainPanel(br(),column(11,offset = 1,
                                     fluidRow(
                                       column(4,
                                              selectInput(label = "Select Factor",inputId = "factor",
                                                          choices = c("Gross Domestic Product",
                                                                      "Human Developement Index",
                                                                      "Gini Index",
                                                                      "Temperature")),
                                              plotOutput("cor.plots",width = 500, height = 400)),
                                       
                                       column(6,offset = 3,
                                              br(),br(),br(),br(),
                                              wellPanel(textOutput("Text")))
                                     ),
                                     fluidRow(h5("4Feet")))
               )))
    
   
  ))
)