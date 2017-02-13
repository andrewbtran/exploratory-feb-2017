library(albersusa)
library(sf)
library(sp)
library(rgeos)
library(maptools)
library(ggplot2)
library(ggalt)
library(ggthemes)
library(viridis)
library(scales)

library(tidyverse)

us <- usa_composite()
us_map <- fortify(us, region="name")
states_en <- read.csv("state_energy_cutoffs.csv", stringsAsFactors=F)


states_en_date <- states_en

states_en_date$based <- states_en_date$date.based
states_en_date$based_type <- "Dates"
states_en_temp <- states_en

states_en_temp$based <- states_en_date$temp.based
states_en_temp$based_type <- "Temperature"

states_en_total <- rbind(states_en_date, states_en_temp)


par(mar=c(0,0,1,0))

gg <- ggplot()
gg <- gg + geom_map(data=us_map, map=us_map,
                    aes(x=long, y=lat, map_id=id),
                    color="#2b2b2b", size=0.1, fill=NA)
gg <- gg + geom_map(data=states_en_total, map=us_map,
           aes(fill=based, map_id=state),
           color="white", size=0.1)
gg <- gg + coord_proj(us_laea_proj) 
gg <- gg + theme_map(base_family="Arial Narrow")
gg <- gg + scale_fill_discrete(name=NULL)
gg <- gg +  theme(legend.position="top", 
        legend.key.width=unit(3, "lines")) 
gg <- gg +  facet_wrap(~based_type)
gg <- gg + labs(x=NULL, y=NULL,
                title="Seasonal or temperature-based power termination protections",
                caption="Source: U.S. Department of Health and Human Services")
gg <- gg + theme(panel.grid.major.y=element_blank())
gg <- gg + theme(panel.grid.minor=element_blank())
gg <- gg + theme(axis.line.y=element_line(color="#2b2b2b", size=0.15))
gg <- gg + theme(axis.text.y=element_text(margin=margin(r=-5, l=0)))
gg <- gg + theme(plot.margin=unit(rep(30, 4), "pt"))
gg <- gg + theme(plot.title=element_text(face="bold"))
gg <- gg + theme(plot.subtitle=element_text(margin=margin(b=10)))
gg <- gg + theme(plot.caption=element_text(size=8, margin=margin(t=10)))
gg <- gg + theme(strip.background = element_blank(),strip.text = element_text(size=13))

gg

deaths <- read.table("hypo_hyper_1999_2015.txt", sep="\t", header=T)
#deaths <- read.table("hypo_hyper_2005_2015.txt", sep="\t", header=T)
#deaths <- read.table("multiple_causes2015.txt", sep="\t", header=T)

cold_deaths <- subset(deaths, Multiple.Cause.of.death.Code=="X31")
cold_deaths$rate <- round(cold_deaths$Deaths/cold_deaths$Population*1000000,1)
names(cold_deaths)[names(cold_deaths) == 'State'] <- 'state'

cold_deaths <- cold_deaths %>%
  group_by(state) %>%
  summarize(rate=mean(rate))

heat_deaths <- subset(deaths, Multiple.Cause.of.death.Code=="X30")
heat_deaths$rate <- round(heat_deaths$Deaths/heat_deaths$Population*1000000,1)
names(heat_deaths)[names(heat_deaths) == 'State'] <- 'state'
heat_deaths <- heat_deaths %>%
  group_by(state) %>%
  summarize(rate=mean(rate))


states_en_cold <- left_join(states_en_total, cold_deaths)
states_en_heat <- left_join(states_en_total, heat_deaths)
states_en_cold$death <- "hypothermia"
states_en_heat$death <- "hyperthermia"

states_en_death <- rbind(states_en_cold, states_en_heat)


states_en_death$date.temp <- ""
states_en_death$date.temp <- ifelse(states_en_death$date.based=="no" & states_en_death$temp.based=="no", "no",  states_en_death$date.temp)
states_en_death$date.temp <- ifelse(states_en_death$date.based=="yes" & states_en_death$temp.based=="no", "mixed",  states_en_death$date.temp)
states_en_death$date.temp <- ifelse(states_en_death$date.based=="no" & states_en_death$temp.based=="yes", "mixed",  states_en_death$date.temp)
states_en_death$date.temp <- ifelse(states_en_death$date.based=="yes" & states_en_death$temp.based=="yes", "yes",  states_en_death$date.temp)

boxp <- ggplot(states_en_death, aes(date.temp, rate)) 
boxp <- boxp + geom_boxplot() 
boxp <- boxp + facet_wrap(~death)
boxp <- boxp + labs(x=NULL, y=NULL,
                title="Rate of deaths in states with power termination protections",
                caption="Source: Centers for Diseas Control and Prevention")
boxp <- boxp + theme(panel.grid.major.y=element_blank())
boxp <- boxp + theme(panel.grid.minor=element_blank())
boxp <- boxp + theme(axis.line.y=element_line(color="#2b2b2b", size=0.15))
boxp <- boxp + theme(axis.text.y=element_text(margin=margin(r=-5, l=0)))
boxp <- boxp + theme(plot.margin=unit(rep(30, 4), "pt"))
boxp <- boxp + theme(plot.title=element_text(face="bold"))
boxp <- boxp + theme(plot.subtitle=element_text(margin=margin(b=10)))
boxp <- boxp + theme(plot.caption=element_text(size=8, margin=margin(t=10)))
#boxp <- boxp + theme(strip.background = element_blank(),strip.text = element_text(size=15))
boxp <- boxp + theme_minimal()

boxp

boxp <- ggplot(states_en_cold, aes(based, rate)) 
boxp <- boxp + geom_boxplot() 
boxp <- boxp + facet_wrap(~based_type)
boxp <- boxp + labs(x=NULL, y=NULL,
                    title="Rate of deaths in states with power termination protections",
                    caption="Source: Centers for Diseas Control and Prevention")
boxp <- boxp + theme(panel.grid.major.y=element_blank())
boxp <- boxp + theme(panel.grid.minor=element_blank())
boxp <- boxp + theme(axis.line.y=element_line(color="#2b2b2b", size=0.15))
boxp <- boxp + theme(axis.text.y=element_text(margin=margin(r=-5, l=0)))
boxp <- boxp + theme(plot.margin=unit(rep(30, 4), "pt"))
boxp <- boxp + theme(plot.title=element_text(face="bold"))
boxp <- boxp + theme(plot.subtitle=element_text(margin=margin(b=10)))
boxp <- boxp + theme(plot.caption=element_text(size=8, margin=margin(t=10)))
#boxp <- boxp + theme(strip.background = element_blank(),strip.text = element_text(size=15))
boxp <- boxp + theme_minimal()

boxp