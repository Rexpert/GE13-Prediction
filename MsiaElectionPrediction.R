library(dplyr)
library(forcats)

library(tidyr)
library(leaflet)
library(rgdal)
library(htmltools)

library(ggplot2)


#-----------dataCleaning--------------------------------------------------------
# rm(list = setdiff(ls(), c("dataSet", "ex")))
# setwd("~/R/Machine Learning/Multivariate/Maps")

dataElection <- read.csv("./data/dataElection.csv")

dataElection <- dataElection[-c(1,5)]
demographic <- dataElection[1:6]
colnames(demographic) <- c("Age", "Race", "Religion", "State", 
                           "ElectricalBill", "HighestEducation")
questionaire <- dataElection[7:length(dataElection)]
colnames(questionaire) <- c("Registered", "Internet", "DasarTransformasiNasional", "TunM",
                            "ChangeGovernment", "GST", "ReligionIssue", "Najib", "PreferredParty")
check <- function(v) {
  is.factor(v) & (levels(v)[1] == "Tidak")
}
change <- function(v) {
  fct_relevel(v, "Ya")
}

questionaire <- mutate_if(questionaire, check, change)
demographic$Race <- fct_relevel(demographic$Race, "Lain-lain", after = Inf)
demographic$Religion <- fct_relevel(demographic$Religion, "Lain-lain", after = Inf)

# ----------maps----------------------------------------------------------------
rawData <- data.frame("NAME_1" = demographic$State,
                      "PreferredParty" = questionaire$PreferredParty)

tidyData <- rawData %>%
  count(NAME_1, PreferredParty) %>%
  spread(PreferredParty, n)

tidyData[is.na(tidyData)] <- as.integer(0)

modeElection <- 
  lapply(1:nrow(tidyData), function(x) {
    tidyData[x,which.max(tidyData[x,-1])+1]
  }) %>%
  unlist()

summ <- 
  rawData %>%
  count(NAME_1, PreferredParty) %>%
  group_by(NAME_1) %>%
  summarise(total = sum(n)) %>%
  cbind(freq = modeElection) %>%
  cbind(mode = names(modeElection)) %>%
  .[c(1,4,3,2)] %>%
  mutate(percent = round(freq/total*100, 2), total = NULL, freq = NULL)

shapeMy <- readOGR(path.expand("./shape/gadm36_MYS_shp/gadm36_MYS_1.shp"), 
                   "gadm36_MYS_1")
levels(summ$NAME_1)[levels(summ$NAME_1) == "Terengganu"] <- "Trengganu"
shapeMy@data <- left_join(shapeMy@data, summ, by = "NAME_1")
pal <- colorFactor(c("#010080", "#E30502","#008800", "#585858"), 
                   shapeMy@data$mode)

text <- mapply(function(state, mode, percent) {
  HTML(sprintf("<font size = \"3\"><strong>%s:</strong><br> %s: %s%%</font>", 
               htmlEscape(state), htmlEscape(mode), htmlEscape(percent)))
  }, shapeMy$NAME_1, shapeMy$mode, shapeMy$percent, SIMPLIFY = F)

leaflet(shapeMy) %>% addTiles() %>% setView(109.6181, 4.1406, zoom = 6) %>%
  addPolygons(fillColor = ~pal(shapeMy@data$mode), fillOpacity = 0.5, 
              color = "#000000", weight = 1, label = ~text)

# ----------barChart------------------------------------------------------------
rawData <- data.frame("Religion" = demographic$Religion, ReligionIssue = questionaire$ReligionIssue)

tidyData <-
  rawData %>%
  count(Religion, ReligionIssue)

tidyData <- 
  tidyData %>% 
  group_by(Religion) %>%
  summarise(total = sum(n)) %>%
  left_join(tidyData, ., by = "Religion") %>%
  mutate(percent = n/total*100, n = NULL, total = NULL)

tidyData[is.na(tidyData)] <- as.integer(0)

ggplot(tidyData, aes(x = Religion, y = percent, fill = ReligionIssue)) + 
  geom_col(color="black", position = position_dodge2(preserve = "single", padding = 0)) +
  theme_minimal() + 
  ggtitle("Does Religion Issue Matter?") + 
  theme(plot.title = element_text(hjust = 0.5, size = 30))

# ----------pieChart------------------------------------------------------------
rawData <- questionaire[c(3, 8)] %>%
  table() %>%
  as.data.frame.matrix() %>%
  .["Ya", ] %>%
  gather(key = "Rating", value = "freq") %>%
  mutate(pos = cumsum(freq)- freq/2) %>%
  mutate(ymin = lag(cumsum(freq), default = 0), ymax = cumsum(freq))

# Normal Pie Chart
# 
# ggplot(rawData, aes(x = "", y = freq, fill = Rating)) +
#   geom_bar(stat = "identity") +
#   coord_polar("y") +
#   ggtitle("Datuk Seri Najib's performance given that\n Dasar Transformasi Nasional is Agreed") +
#   theme_minimal() +
#   theme(
#     axis.title.x = element_blank(),
#     axis.title.y = element_blank(),
#     axis.text.x = element_blank(),
#     panel.border = element_blank(),
#     panel.grid=element_blank(),
#     axis.ticks = element_blank(),
#     plot.title=element_text(size=14, face="bold", hjust = 0.5)
#   ) +
#   scale_fill_brewer(palette = "OrRd", direction = -1) +
#   geom_text(aes(x= "", y = pos, label = freq), size = 5, nudge_x = 0.6)


# Donut chart

ggplot(rawData) + 
  geom_rect(aes(fill = Rating, ymax=ymax, ymin=ymin, xmax=4, xmin=2)) +
  coord_polar("y") +
  ggtitle("Datuk Seri Najib's performance given that\n Dasar Transformasi Nasional is Agreed") + 
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold", hjust = 0.5)
  ) +
  scale_fill_brewer(palette = "OrRd", direction = -1) +
  xlim(c(0,4)) +
  geom_text(aes(x= 3, y = pos, label = freq), size = 5)
