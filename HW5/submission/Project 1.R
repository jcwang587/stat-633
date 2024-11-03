library(tidyverse)
library(plotly)
library(countrycode)

#load in world data
world.data <- read.csv('world-data-2023.csv')

#load in happiness data
happy.data <- read.csv('WHR2023.csv')

#clean happy.data
happy.data <- happy.data %>%
                select(c(Country.name, Ladder.score, Logged.GDP.per.capita,Social.support, Healthy.life.expectancy, Freedom.to.make.life.choices,
                         Generosity,Perceptions.of.corruption)) %>%
                rename(c(happiness.score = Ladder.score, Country = Country.name))

#Change some country names so they join properly
old.name <- c('Republic of Ireland', 'Czech Republic', 'Republic of the Congo', "Turkey", 'The Gambia', 
              'Democratic Republic of the Congo')
new.name <- c('Ireland', 'Czechia','Congo (Brazzaville)',
              'Turkiye','Gambia','Congo (Kinshasa)')

#Clean the world data by dropping certain rows and 
world.data <- world.data %>%
                select(-c(Capital.Major.City, Largest.city, Abbreviation, Currency.Code,Calling.Code)) %>%
                mutate(Country.join = replace(Country, Country %in% old.name, new.name)) %>%
                rename(Density.P.Km2 = Density..P.Km2., Agricultural.Land.Percent = Agricultural.Land....,
                       Land.Area.Km2 = Land.Area.Km2., CPI.Change = CPI.Change....,Forested.Area.Percent = Forested.Area....,
                       Gross.Primary.Education.Enrollment.Percent = Gross.primary.education.enrollment...., Gross.Tertiary.Education.Enrollment.Percent = Gross.tertiary.education.enrollment....,
                       Population.Labor.force.participation.Percent = Population..Labor.force.participation....,
                       Tax.Revenue.Percent = Tax.revenue....)


### Remove Percent Signs
remove.char <- function(x){
  
  #create vector to remove percent, comma, and dollar sign
    vec <- gsub('%','',x)
    vec <- gsub(',','',vec)
    vec <- gsub('\\$','',vec)
  
    #make numeric
    vec <- as.numeric(vec)
    
    #return vector
    return(vec)
}

world.data$Density.P.Km2 <- remove.char(world.data$Density.P.Km2)
world.data$Agricultural.Land.Percent <- remove.char(world.data$Agricultural.Land.Percent)
world.data$Land.Area.Km2 <- remove.char(world.data$Land.Area.Km2)
world.data$Armed.Forces.size <- remove.char(world.data$Armed.Forces.size)
world.data$Co2.Emissions <- remove.char(world.data$Co2.Emissions)
world.data$CPI <- remove.char(world.data$CPI)
world.data$CPI.Change <- remove.char(world.data$CPI.Change)
world.data$Forested.Area.Percent <- remove.char(world.data$Forested.Area.Percent)
world.data$Gasoline.Price <- remove.char(world.data$Gasoline.Price)
world.data$GDP <- remove.char(world.data$GDP)
world.data$Gross.Primary.Education.Enrollment.Percent <- remove.char(world.data$Gross.Primary.Education.Enrollment.Percent)
world.data$Gross.Tertiary.Education.Enrollment.Percent <- remove.char(world.data$Gross.Tertiary.Education.Enrollment.Percent)
world.data$Minimum.wage <- remove.char(world.data$Minimum.wage)
world.data$Out.of.pocket.health.expenditure <- remove.char(world.data$Out.of.pocket.health.expenditure)
world.data$Population <- remove.char(world.data$Population)
world.data$Population.Labor.force.participation.Percent <- remove.char(world.data$Population.Labor.force.participation.Percent)
world.data$Tax.Revenue.Percent <- remove.char(world.data$Tax.Revenue.Percent)
world.data$Total.tax.rate <- remove.char(world.data$Total.tax.rate)
world.data$Unemployment.rate <- remove.char(world.data$Unemployment.rate)
world.data$Urban_population <- remove.char(world.data$Urban_population)


# Join the two datasets
final.data <- happy.data %>%
                left_join(world.data, by = join_by(Country == Country.join)) %>%
                select(-Country.y) %>%
                dplyr::filter(!(Country %in% c('Kosovo', 'Hong Kong S.A.R. of China', 'State of Palestine', 'Taiwan Province of China', 'State of Palestine'))) %>%
                mutate(iso.code = countrycode::countrycode(Country, origin = 'country.name', destination = 'iso3c'))

final.data$hover <- with(final.data, paste(Country, '<br>', 'Population (in millions):', round(Population / 1000000,2), '<br>', 
                          'Life.expectancy: ', Life.expectancy, '<br>', 'GDP Per Cap: $', round(exp(Logged.GDP.per.capita),0)))

#Write out to csv
#write.csv(final.data, 'HW5.data.csv')


####################################################################

## Make Plots

####################################################################


ggplot(data = final.data, mapping = aes(x = Population, y = happiness.score)) +
  geom_point()


p <- ggplot(data = final.data, mapping = aes(x = Life.expectancy, y = happiness.score, color = Physicians.per.thousand )) +
      geom_point(aes(ids = Country))
ggplotly(p)

p <- ggplot(data = final.data, mapping = aes(x = Healthy.life.expectancy, y = happiness.score, color = Physicians.per.thousand > 1)) +
  geom_point(aes(ids = Country))
ggplotly(p)


plot_ly(data = final.data, type = 'choropleth', locations = ~iso.code, z = ~happiness.score, 
        text = ~hover, colors = 'Spectral') %>%
  colorbar(title = 'Happiness Score') %>%
  layout(title = '2023 World Happiness Score for 133 Countries')
