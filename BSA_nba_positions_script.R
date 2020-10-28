library(magrittr)
library(textreadr)
library(rvest)
library(dplyr)

nba = read.csv('/users/stephenchen/Downloads/nba_data.csv')

datalist <- list()

a <- 1


for (i in 2010:2019){
  
  url_first_part <- "https://widgets.sports-reference.com/wg.fcgi?css=1&site=bbr&url=%2Fleagues%2FNBA_"
  
  second_part <- "_advanced.html&div=div_advanced_stats"
  
  url <- paste0(url_first_part, i, second_part)
  
  advanced <- url %>% read_html() %>% html_table()
  
  advanced <- advanced[[1]]
  
  advanced$Season <- i
  
  datalist[[a]] <- advanced
  
  a <- a + 1
  
}

data <- do.call("rbind", datalist)
data

# scraping per, ows, dws, ws
# data2 = read.csv('/users/stephenchen/Downloads/nba_advancedV4.csv')
data2 = data
data2 = data2[,c(2,3,30)]
data2$Player = as.character(data2$Player)
data2$junk = iconv(data2$Player, from = 'latin1', to = 'ASCII')


# fixing weird characters in per/ws data
junk2 = as.data.frame(cbind('player' = levels(as.factor(data2$Player[is.na(data2$junk)])), 'translate' = NA))
junk2$translate = as.character(junk2$translate)

junk2$translate[junk2$player == 'Ã\u0081lex Abrines'] = 'Alex Abrines'
junk2$translate[junk2$player == 'Alexis AjinÃ§a'] = 'Alexis Ajinca'
junk2$translate[junk2$player == 'Ã\u0096mer AÅ\u009fÄ±k'] = 'Omer Asik'
junk2$translate[junk2$player == 'Anderson VarejÃ£o'] = 'Anderson Varejao'
junk2$translate[junk2$player == 'AndrÃ©s Nocioni'] = 'Andres Nocioni'
junk2$translate[junk2$player == 'Andris BiedriÅ\u0086Å¡'] = 'Andris Biedrins'
junk2$translate[junk2$player == 'Ã\u0081ngel Delgado'] = 'Angel Delgado'
junk2$translate[junk2$player == 'Ante Å½iÅ¾iÄ\u0087'] = 'Ante Zizic'
junk2$translate[junk2$player == 'Boban MarjanoviÄ\u0087'] = 'Boban Marjanovic'
junk2$translate[junk2$player == 'Bogdan BogdanoviÄ\u0087'] = 'Bogdan Bogdanovic'
junk2$translate[junk2$player == 'Bojan BogdanoviÄ\u0087'] = 'Bojan Bogdanovic'
junk2[12,'translate']= 'Cristiano Felicio'
junk2$translate[junk2$player == 'DÅ¾anan Musa'] = 'Dzanan Musa'
junk2$translate[junk2$player == 'Dairis BertÄ\u0081ns'] = 'Dairis Bertans'
junk2$translate[junk2$player == 'Damjan RudeÅ¾'] = 'Damjan Rudez'
junk2[16,'translate'] = 'Dario Saric'
junk2$translate[junk2$player == 'Darko MiliÄiÄ'] = 'Darko Milicic'
junk2$translate[junk2$player == 'DÄvis BertÄns'] = 'Davis Bertans'
junk2$translate[junk2$player == 'Dennis SchrÃ¶der'] = 'Dennis Schroder'
junk2$translate[junk2$player == 'Donatas MotiejÅ«nas'] = 'Donatas Motiejunas'
junk2$translate[junk2$player == 'DontÃ© Greene'] = 'Donte Greene'
junk2$translate[junk2$player == 'Eduardo NÃ¡jera'] = 'Eduardo Najera'
junk2$translate[junk2$player == 'Ersan Ä°lyasova'] = 'Ersan Ilyasova'
junk2[24,'translate'] = 'Francisco Garcia'
junk2$translate[junk2$player == 'Goran DragiÄ\u0087'] = 'Goran Dragic'
junk2$translate[junk2$player == 'Greivis VÃ¡squez'] = 'Greivis Vasquez'
junk2$translate[junk2$player == 'Gustavo AyÃ³n'] = 'Gustavo Ayon'
junk2$translate[junk2$player == 'Hedo TÃ¼rkoÄ\u009flu'] = 'Hedo Turkoglu'
junk2$translate[junk2$player == 'Jakob PÃ¶ltl'] = 'Jakob Poeltl'
junk2$translate[junk2$player == 'Jan VeselÃ½'] = 'Jan Vesely'
junk2$translate[junk2$player == 'Jonas ValanÄ\u008diÅ«nas'] = 'Jonas Valanciunas'
junk2$translate[junk2$player == 'Jorge GutiÃ©rrez'] = 'Jorge Gutierrez'
junk2$translate[junk2$player == 'JosÃ© CalderÃ³n'] = 'Jose Calderon'
junk2$translate[junk2$player == 'Juan HernangÃ³mez'] = 'Juancho Hernangomez'
junk2$translate[junk2$player == 'Jusuf NurkiÄ\u0087'] = 'Jusuf Nurkic'
junk2$translate[junk2$player == 'Kevin SÃ©raphin'] = 'Kevin Seraphin'
junk2$translate[junk2$player == 'Kristaps PorziÅ\u0086Ä£is'] = 'Kristaps Porzingis'
junk2$translate[junk2$player == 'Luka DonÄ\u008diÄ\u0087'] = 'Luka Doncic'
junk2$translate[junk2$player == 'Manu GinÃ³bili'] = 'Manu Ginobili'
junk2$translate[junk2$player == 'MickaÃ«l Gelabale'] = 'Mickael Gelabale'
junk2$translate[junk2$player == 'MickaÃ«l PiÃ©trus'] = 'Mickael Pietrus'
junk2$translate[junk2$player == 'MiloÅ¡ TeodosiÄ\u0087'] = 'Milos Teodosic'
junk2$translate[junk2$player == 'Mirza TeletoviÄ\u0087'] = 'Mirza Teletovic'
junk2$translate[junk2$player == 'Nemanja NedoviÄ\u0087'] = 'Nemanja Nedovic'
junk2$translate[junk2$player == 'NenÃª'] = 'Nene'
junk2$translate[junk2$player == 'Nenad KrstiÄ\u0087'] = 'Nenad Krstic'
junk2$translate[junk2$player == 'NicolÃ¡s Brussino'] = 'Nicolas Brussino'
junk2[47,'translate'] = 'Nicolas Laprovittola'
junk2$translate[junk2$player == 'Nikola JokiÄ\u0087'] = 'Nikola Jokic'
junk2$translate[junk2$player == 'Nikola MirotiÄ\u0087'] = 'Nikola Mirotic'
junk2$translate[junk2$player == 'Nikola PekoviÄ\u0087'] = 'Nikola Pekovic'
junk2$translate[junk2$player == 'Nikola VuÄ\u008deviÄ\u0087'] = 'Nikola Vucevic'
junk2$translate[junk2$player == 'Ognjen KuzmiÄ\u0087'] = 'Ognjen Kuzmic'
junk2$translate[junk2$player == 'Peja StojakoviÄ\u0087'] = 'Peja Stojakovic'
junk2$translate[junk2$player == 'Pero AntiÄ\u0087'] = 'Pero Antic'
junk2$translate[junk2$player == 'PrimoÅ¾ Brezec'] = 'Primoz Brezec'
junk2$translate[junk2$player == 'Rasho NesteroviÄ\u0087'] = 'Rasho Nesterovic'
junk2$translate[junk2$player == 'Roko UkiÄ\u0087'] = 'Roko Ukic'
junk2$translate[junk2$player == 'Rudy FernÃ¡ndez'] = 'Rudy Fernandez'
junk2$translate[junk2$player == 'Sasha PavloviÄ\u0087'] = 'Sasha Pavlovic'
junk2$translate[junk2$player == 'Sasha VujaÄ\u008diÄ\u0087'] = 'Sasha Vujacic'
junk2[61,'translate'] = 'Sergio Rodriguez'
junk2$translate[junk2$player == 'Skal LabissiÃ¨re'] = 'Skal Labissiere'
junk2$translate[junk2$player == 'Tibor PleiÃ\u009f'] = 'Tibor Pleiss'
junk2$translate[junk2$player == 'TimothÃ© Luwawu-Cabarrot'] = 'Timothe Luwawu-Cabarrot'
junk2$translate[junk2$player == 'TomÃ¡Å¡ SatoranskÃ½'] = 'Tomas Satoransky'
junk2[66,'translate'] = 'Vitor Faverani'
junk2$translate[junk2$player == 'Vladimir RadmanoviÄ\u0087'] = 'Vladimir Radmanovic'
junk2$translate[junk2$player == 'Willy HernangÃ³mez'] = 'Willy Hernangomez'
junk2$translate[junk2$player == 'Willy HernangÃ³mez'] = 'Willy Hernangomez'
junk2$translate[junk2$player == 'Zoran DragiÄ\u0087'] = 'Zoran Dragic'

# change the special character names in data2
data2$Player = as.factor(data2$Player)
for (i in 1:length(junk2$player)){
  for (j in 1:length(levels(data2$Player))){
    if (junk2$player[i] == levels(data2$Player)[j]){
      levels(data2$Player)[j] = junk2$translate[i]
    }
  }
}
colnames(data2) = c('PLAYER_NAME','POS','YEAR','junk')

# removing names in data2 with *
junk3 = as.data.frame(cbind('player' = levels(as.factor(as.character(data2$PLAYER_NAME[grepl("/|:|\\?|<|>|\\|\\\\|\\*",data2$PLAYER_NAME)]))), 'translate' = NA))
junk3$player = as.character(junk3$player)
junk3$translate = as.character(junk3$translate)

for (i in 1:length(junk3$player)){
  junk3$translate[i] = substr(junk3$player[i], 1, nchar(junk3$player[i]) - 1)
}

data2$test = data2$PLAYER_NAME
for (i in 1:length(junk3$player)){
  for (j in 1:length(levels(data2$PLAYER_NAME))){
    if (junk3$player[i] == levels(data2$PLAYER_NAME)[j]){
      levels(data2$PLAYER_NAME)[j] = junk3$translate[i]
    }
  }
}
data2 = data2[,-7]

# consistent name spelling across nba and data2
test = as.data.frame(cbind(levels(data2$PLAYER_NAME),NA))
test$V1 = as.character(test$V1)
test$V2 = as.character(test$V2)
test$V3 = test$V1
test$V3[2] = 'AJ Price'
test$V3[148] = 'CJ Miles'
test$V3[228] = 'DJ White'
test$V3[226] = 'DJ Mbenga'
test$V3[392] = 'Pooh Jeter'
test$V3[402] = 'Frank Mason'
test$V3[429] = 'Glen Rice'
test$V3[484] = 'JR Smith'
test$V3[566] = 'Jefferey Taylor'
test$V3[871] = 'Mitchell Creek'
test = test[-955,]
rownames(test) = 1:nrow(test)
test$V3[1010] = 'Flip Murray'
test$V3[1095] = 'Svi Mykhailiuk'
test$V3[1097] = 'TJ Leaf'
test$V3[1103] = 'Taurean Prince'
test$V3[1202] = 'Wes Iwundu'
for (i in 1:length(test$V1)){
  test$V2[i] = agrep(test$V3[i], levels(nba$PLAYER_NAME), value = TRUE)
}
test$V2[1233] = 'Zoran Dragic'
test$V2[73] = 'Anthony Parker'
test$V2[114] = 'Bojan Bogdanovic'
test$V2[129] = 'Brandon Roy'
test$V2[253] = 'Danny Green'
test$V2[276] = 'Davis Bertans'
test$V2[306] = 'Derrick Rose'
test$V2[348] = 'Drew Gordon'
test$V2[513] = 'James Jones'
test$V2[534] = 'Jaren Jackson Jr.'
test$V2[544] = 'Jason Collins'
test$V2[564] = 'Jeff Foster'
test$V2[596] = 'Joe Johnson'
test$V2[608] = 'John Collins'
test$V2[634] = 'Jordan Hill'
test$V2[648] = 'Josh Hart'
test$V2[765] = 'Lou Williams'
test$V2[784] = 'Malcolm Lee'
test$V2[800] = 'Marcus Cousin'
test$V2[805] = 'Marcus Morris Sr.'
test$V2[848] = 'Michael Redd'
test$V2[875] = 'Mo Williams'
test$V2[899] = 'Nick Johnson'
test$V2[894] = 'Nene'
test$V2[920] = 'Omari Johnson'
test$V2[1024] = 'Ryan Anderson'
test$V2[1044] = 'Sean May'
test$V2[1045] = 'Sean Williams'
test$V2[1124] = 'Tim Frazier'
test$V2[1138] = 'Tony Parker'
test$V2[1157] = 'Trey Johnson'
test$V2[1166] = 'Troy Williams'

for (i in 1:length(test$V1)){
  for (j in 1:length(levels(data2$PLAYER_NAME))){
    if (test$V1[i] == levels(data2$PLAYER_NAME)[j]){
      levels(data2$PLAYER_NAME)[j] = test$V2[i]
    }
  }
}

data2 = data2[,c(-4,-5)]
data2 = data2 %>% distinct(PLAYER_NAME, .keep_all = TRUE)
write.csv(data2, '/users/stephenchen/Documents/nba_positions.csv', row.names = FALSE)

datalist <- list()

a <- 1


for (i in 2020){
  
  url_first_part <- "https://widgets.sports-reference.com/wg.fcgi?css=1&site=bbr&url=%2Fleagues%2FNBA_"
  
  second_part <- "_advanced.html&div=div_advanced_stats"
  
  url <- paste0(url_first_part, i, second_part)
  
  advanced <- url %>% read_html() %>% html_table()
  
  advanced <- advanced[[1]]
  
  advanced$Season <- i
  
  datalist[[a]] <- advanced
  
  a <- a + 1
  
}

data3 <- do.call("rbind", datalist)

data3 = data3[-which(data3$Player == 'Player'),]
data3$MPG = as.integer(data3$MP) / as.integer(data3$G)

data3 = data3[,-20]
data3 = data3[,-24]

data3_filtered = data3 %>% filter(MPG > 12 & as.integer(G) > 17) %>% select(Player, Season, Pos, MPG, G, PER, WS, `TS%`)
data3_filtered$Player = as.factor(data3_filtered$Player)
for (i in 1:length(junk2$player)){
  for (j in 1:length(levels(data3_filtered$Player))){
    if (junk2$player[i] == levels(data3_filtered$Player)[j]){
      levels(data3_filtered$Player)[j] = junk2$translate[i]
    }
  }
}

data3_filtered = data3_filtered %>% distinct(Player, .keep_all = TRUE)

colnames(data3_filtered) = c('PLAYER','SEASON', 'POS', 'MIN','GP','PER','WS','TS_PCT')

write.csv(data3_filtered, '/users/stephenchen/Documents/nba_2020_BR.csv', row.names = FALSE)
