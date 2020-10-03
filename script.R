setwd("/home/ashari/Documents/Project/Pelatihan Himpunan EDA/") #silahkan diatur sesuai direcytory masing"

install.packages("tidyverse")
install.packages("skimr")
install.packages("plotly")
install.packages("highcharter")

library(tidyverse)
library(skimr)
library(plotly)
library(highcharter)


data_fifa <- read_csv("fifa/fifa_eda.csv")

#data_fifa <- read.csv("fifa/fifa_eda.csv", stringsAsFactors = T)
head(data_fifa, 7) #melihat 7 data pertama
tail(data_fifa, 10) #melihat 10 data terakhir
View(data_fifa) #melihat tabel dara

str(data_fifa) #melihat struktur data
glimpse(data_fifa) #struktur data juga

# kita memisahkan data yang ada nilai X19 nya
#caranya kita temukan kode unik pada data tersebut

Guangzhou <- data_fifa %>% filter(Club == "Guangzhou R&F")
View(Guangzhou)

names(Guangzhou)

Guangzhou[8] <- Guangzhou[9]

index_kolom <- c(8:18)
index_kolom

for (i in index_kolom) {
    Guangzhou[i] <- Guangzhou[i + 1]
  
}

data_fifa <- data_fifa  %>% filter((Club != "Guangzhou R&F") %>% replace_na(TRUE))
dim(data_fifa)

data_fifa <- data_fifa[-19] # kolom 19 dibuang
Guangzhou <- Guangzhou[-19] #kolom 19 dibuang

data_fifa <- data_fifa %>% rbind(Guangzhou)

write_csv(data_fifa, "data_fifa_clean.csv")

data_fifa <- read_csv("data_fifa_clean.csv")
glimpse(data_fifa)

data_fifa$ID <- as.character(data_fifa$ID)
data_fifa$`International Reputation` <- as.factor(data_fifa$`International Reputation`)
data_fifa$`Skill Moves` <- as.factor(data_fifa$`Skill Moves`)
data_fifa$Joined <- data_fifa$Joined
glimpse(data_fifa)

#ringkasan data
summary(data_fifa)
skim(data_fifa)

### deal with NA's
#club kita isi "no_club"
# international reputation "1"
# skill moves kita isi 2
# value 2445
# Weight 166

data_fifa <- data_fifa %>% 
  replace_na(list(Club = 'No_club', `International Reputation` = "1",
                  `Skill Moves` = "2", Value = mean(data_fifa$Value, na.rm=T), 
                  Weight = mean(data_fifa$Weight, na.rm = T), `Preferred Foot` = 'Right'))

skim(data_fifa)

### Graphical
fifa_skim <- skim(data_fifa)
fifa_skim <- fifa_skim[-7,]

fifa_skim <- data.frame(fifa_skim)
kolom_factor <- fifa_skim %>% filter(skim_type %in% c("factor","character"))
factor <- kolom_factor$skim_variable
factor


kolom_numerik <- fifa_skim %>% filter(skim_type == "numeric")
numerik <- kolom_numerik$skim_variable
numerik

fifa_kategorik <- data_fifa[factor]
head(fifa_kategorik)

fifa_numerik <- data_fifa[numerik]
head(fifa_numerik)

ggplot(fifa_kategorik, aes(x=Position)) +
  geom_bar()

position_order <- c("GK", "CB", "LB","RB", "LCB", "RCB", "LWB", "RWB", "CDM", "LDM", "RDM",
                    "CM", "LCM","RCM", "CAM", "LAM", "RAM", "LM", "RM","LW","RW",
                    "LS", "RS", "LF", "RF","ST", "CF")
fifa_kategorik$Position <- factor(fifa_kategorik$Position, levels = position_order)    
ggplot(fifa_kategorik, aes(x=Position)) +
  geom_bar(color = "black", fill = "steelblue") + ggtitle("Jumlah Pemain Berdasarkan Posisi")

fifa_kategorik <- fifa_kategorik[-(1:2)]

gather_kat <- gather(fifa_kategorik, "Var", "Value")
ggplot(gather_kat, aes(x=Value)) +
  geom_bar() + facet_wrap(~Var, scales = "free")

head(fifa_numerik)
gather_num <- gather(fifa_numerik, "Var", "Value")
ggplot(gather_num, aes(x=Value)) +
  geom_histogram() + facet_wrap(~Var, scales = "free")

ggplot(gather_num, aes(x=Value)) +
  geom_density(alpha=0.5, color="black", fill = "steelblue") + 
  facet_wrap(~Var, scales = "free")

ggplot(gather_num, aes(x=Value)) +
  geom_boxplot() + facet_wrap(~Var, scales = "free")

##qqplot
ggplot(gather_num, aes(sample=Value)) +
  geom_qq() + geom_qq_line() + facet_wrap(~Var, scales = "free")

#scatterplot
ggplot(fifa_numerik, aes(x=`Release Clause`, y=Value)) +
  geom_point()

ggplot(fifa_numerik, aes(x=Overall, y=Age)) +
  geom_point()

ggplot(fifa_numerik, aes(x=Overall, y=Age)) +
  geom_point(aes(color = fifa_kategorik$`International Reputation`))

#coorplot
library(corrplot)
library(RColorBrewer)
M <-cor(fifa_numerik)
corrplot(M, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))

### agregat data
mean_value_by_nationality <- data_fifa %>% 
  group_by(Nationality) %>% 
  summarise("Mean.Value.by.Country" = mean(Value, na.rm = T)) %>%
  arrange(desc(Mean.Value.by.Country)) 
head(mean_value_by_nationality)

g <- ggplot(mean_value_by_nationality[1:10,], aes(x = reorder(Nationality, -Mean.Value.by.Country),
  y = Mean.Value.by.Country, nationality = Nationality, mean = Mean.Value.by.Country)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  ggtitle("Mean Value Players by Country") +
  xlab("Nationality") + ylab("Mean Value")
g
ggplotly(g,tooltip=c("nationality", "mean"))


total_value_by_nationality <- data_fifa %>% 
  group_by(Nationality) %>% 
  summarise("Total Value by Country" = sum(Value)) %>%
  arrange(desc(`Total Value by Country`))
head(total_value_by_nationality)

g <- ggplot(total_value_by_nationality[1:10,], aes(x = reorder(Nationality, -`Total Value by Country`),
                                                   y = `Total Value by Country`, nationality = Nationality, Total = `Total Value by Country`)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  ggtitle("Total Value Players by Country") +
  xlab("Nationality") + ylab("Mean Value")
g
ggplotly(g,tooltip=c("nationality", "Total"))

ggplotly(ggplot(data_fifa, aes(x=Age, y=Value)) + 
  geom_point(color = "steelblue") + ggtitle("Scatter Plot Age and Value"))

ggplotly(ggplot(data_fifa, aes(x=Wage, y=Overall)) + 
  geom_point(color = "steelblue")  + ggtitle("Scatter Plot Age and Overall"))


View(data_fifa %>% arrange(desc(Value)) %>% slice(1:120))

club_name <- "Arsenal"
club_data <- data_fifa %>% filter(Club == club_name)
count <- 10


club_overall <- mean(club_data$Overall)
club_overall <- round(club_overall, 2)
club_overall
club_value <- sum(club_data$Value)
print(paste(club_value, "€"))

club_players <- dim(club_data)
club_players
club_players <- club_players[1]
club_players
club_wage <- mean(club_data$Wage)
club_wage
club_age <- median(club_data$Age)
club_age
cat(paste0(" Overall ", club_overall, "\n",
    " Total Value ", club_value, "€", "\n"),
    "Number of Players ", club_players, "\n",
    "Mean Wage ", club_wage, "\n",
    "Median Age ", club_age)

overall <- club_data[, c("Name", "Overall")] %>% arrange(desc(Overall))
overall[1:count,] %>% 
  hchart(type = "bar", hcaes(x = Name, y = Overall), name ="Overall") %>%
  hc_title(text = paste0("Top Overall in ", club_name)) %>%
  hc_subtitle(text = "Source: kaggle.com")

wage <- club_data[, c("Name", "Wage")] %>% arrange(desc(Wage))
wage[1:count,] %>% 
  hchart(type = "bar", hcaes(x = Name, y = Wage), name ="Wage") %>%
  hc_title(text = paste0("Top Expensive Wage in ", club_name)) %>%
  hc_subtitle(text = "Source: kaggle.com")

### Players
players <- "P. Čech"
data_players <- data_fifa %>% filter(Name == players)
players_name <- data_players$Name
players_name
players_club <- data_players$Club
players_club
players_position <- data_players$Position
players_position
players_age <- data_players$Age
players_age
players_nationality <- data_players$Nationality
players_nationality
players_overall <- data_players$Overall
players_overall
players_foot <- data_players$`Preferred Foot`
players_foot
players_contract <-data_players$`Contract Valid Until`
players_contract
