if (!require("pacman")){
  install.packages("pacman")
}
pacman::p_load(
  tidyverse, kableExtra, flextable, glue, ggplot2, gridExtra
)# install packages

data=read.csv('Number of people with and without energy access (OWID based on World Bank, 2021).csv', header=TRUE)#importing data

glimpse(data1)

data=data%>%
  rename(accessE=Number.of.people.with.access.to.electricity, unableE=Number.of.people.without.access.to.electricity,
         cleanF=number_with_clean_fuels_cooking, uncleanF=number_without_clean_fuels_cooking)# rename the column names

summary(data)# to check the structure of the dataset 

data %>%
  select(-Entity, -Year)%>%
  map_dbl(mean, na.rm=TRUE)# to calculate means

data %>%
  select(-Entity, -Year)%>%
  map_dbl(sd, na.rm=TRUE)# to calculate standard deviations

data[data==0]=NA
data1=data[complete.cases(data), ]# to remove the NA values in dataset

data1$ratioE=data1$accessE/data1$UnableE# shows the ratio of people with electricity to those without electricity
data1$ratioF=data1$cleanF/data1$uncleanF# show the ratio of people with clean fuels to cooking to those without clean fuels

data1=data1%>%
  group_by(Entity)# set countries by group

QuickSelect=function(Entity, Year){
  result=data1[data1$Entity==Entity & data1$Year==Year, ]
  if (nrow(result)==0){
    returen(NA)
  }else{
      return(result)
    }
}#A function to help the reader quick select the dataset
QuickSelect("Afghanistan", 2016)#Example

plot1=function(Country_name){
  country_data=data1%>%
    filter(Entity==Country_name)%>%
  select(Year, ratioE, ratioF)
  if(nrow(country_data)==0){
    return(NA)
  }
  p=ggplot(country_data,aes(x=Year, group=1))+
  geom_point(aes(y=ratioE,color="ratioE"),size=3)+
  geom_point(aes(y=ratioF,color="ratioF"),size=3)+
  labs(
    title=glue("The ratio between numbers of people with access and without to electricity and clean fuels over time in {Country_name}"),
    x="Year",
    y="Ratio"
  )+
    scale_color_manual(values=c("ratioE"="blue", "ratioF"="red"),
                       labels=c("ratioE", "ratioF"))
  print(p)
}#A function to help reader to generate the ratio between numbers of people with access and without to electricity and clean fuels in different countries over time
plot1("Afghanistan")#Example



plot2=function(variable_name){
  data1%>%
    ggplot(aes_string(x="Year", y=variable_name))+
    geom_point()+
    labs(
      title=glue("The trend about {variable_name}"),
      y=variable_name
    )
  }# A function to help reader generate the trends of people able and unable access to electricity and clean fuels
plot2("unableE")#Example

plot3=function(year, Variable_name){
  year_data=data1%>%
    filter(Year==year)%>%
    select(-Entity)
    if(nrow(year_data)==0){
      return(NA)
    }
    ggplot(year_data,aes(x="1", y=.data[[Variable_name]]))+
    geom_boxplot()+
    labs(
      title=glue("The boxplot about {Variable_name} in {year}"),
      x="Year",
      y=glue("{Variable_name}")
      )
}
plot3("2005", "accessE") # Example

