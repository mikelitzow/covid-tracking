library(tidyverse)

download.file("https://covidtracking.com/api/us/daily.csv", "~current.csv") 

dat <- read.csv("~current.csv")

theme_set(theme_bw())

ggplot(dat, aes(date, positive)) +
  geom_line() +
  geom_point() +
  ylab("Cumulative cases") +
  ggtitle("Total reported US cases", subtitle = "Data from covidtracking.com")

ggsave("figs/Cumulative reported US cases.png", width=6, height = 4, units="in")

dat$day <- c(rep(NA, 7), 1:(nrow(dat)-7))

dat <- dat %>%
  filter(day >= 1)

dat$double.7 <- 27
dat$double.4 <- 27
dat$double.3 <- 27

plot.dat <- dat %>%
  select(day, death, double.3, double.4, double.7)

plot.dat <- rbind(plot.dat,
                    data.frame(day=(nrow(dat)+1):50,
                               death=NA,
                               double.3=NA,
                               double.4=NA,
                               double.7=NA))
  

for(i in 2:nrow(plot.dat)){
  
plot.dat$double.7[i] <- plot.dat$double.7[(i-1)]*1.1225
plot.dat$double.4[i] <- plot.dat$double.4[(i-1)]*1.1887
plot.dat$double.3[i] <- plot.dat$double.3[(i-1)]*1.259
}


plot.dat <- plot.dat %>%
  pivot_longer(-day, names_to = "name", values_to = "value")

plot.dat$order <- ifelse(plot.dat$name=="double.3", 1,
                         ifelse(plot.dat$name=="double.4", 2,
                                ifelse(plot.dat$name=="double.7",3,4)))
plot.dat$name <- reorder(plot.dat$name, plot.dat$order)

plot.dat <- plot.dat %>%
  arrange(name)
# View(plot.dat)

cb <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ggplot(plot.dat, aes(day, value, color=name)) +
  geom_line(size=rep(c(0.5,0.5,0.5,1.5), each=(nrow(plot.dat)/4))) +
  coord_trans(y="pseudo_log") + 
  scale_y_continuous(breaks = c(50,200,500,1000,2000,10000,50000,100000,200000,500000,1000000)) +
  scale_color_manual(values=cb[c(2,4,6,7)], 
                     labels=c("double every 3 days",
                              "double every 4 days",
                              "double every 7 days",
                              "observed")) +
  theme(legend.title = element_blank(), legend.position = c(0.2,0.8)) +
  ylab("deaths") + 
  ggtitle("Cumulative US deaths: observed vs. different doubling rates",
          subtitle = "Day 1 = March 11; data from covidtracking.com")

ggsave("figs/Cumulative US deaths.png", width=6, height=4, units='in')

download.file("http://covidtracking.com/api/states/daily.csv", "~current.state")

state.dat <- read.csv("~current.state")

state.dat

names(state.dat)

wa.dat <- state.dat %>%
  filter(state=="WA")

ggplot(wa.dat, aes(date, positive)) +
  geom_line() +
  geom_point() +
  ylab("Cumulative cases") +
  ggtitle("Total reported Washington cases", subtitle = "Data from covidtracking.com")

ggsave("figs/Cumulative reported Washington cases.png", width=6, height = 4, units="in")

wa.dat

ny.dat <- state.dat %>%
  filter(state=="NY")

ggplot(ny.dat, aes(date, positive)) +
  geom_line() +
  geom_point() +
  ylab("Cumulative cases") +
  ggtitle("Total reported New York cases", subtitle = "Data from covidtracking.com")

ggsave("figs/Cumulative reported New York cases.png", width=6, height = 4, units="in")

ma.dat <- state.dat %>%
  filter(state=="MA")

ggplot(ma.dat, aes(date, positive)) +
  geom_line() +
  geom_point() +
  ylab("Cumulative cases") +
  ggtitle("Total reported Massachusetts cases", subtitle = "Data from covidtracking.com")

ggsave("figs/Cumulative reported Massachusetts cases.png", width=6, height = 4, units="in")
