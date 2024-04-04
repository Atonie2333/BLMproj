##Player Hetero##
#Import club status
library(readxl)
library(lfe)
library(xtable)
club_status <- read_xlsx("club_status(1).xlsx")
cols_to_remove <- c("SDLP","GRN", "PLC", "SNP", "LDM", "ALL", "IND", "DUP", "sdlp", "plc", "all", "ind", "dup")
club_status_nonzero <- club_status[, !names(club_status) %in% cols_to_remove]
#Classify eu countries
eu_countries <- c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN",
                  "FRA", "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX",
                  "MLT", "NLD", "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE")

#Define classification function
classify_position <- function(position){
  if(grepl("FW|LW|RW|AM", position)){
    return("Forward")
  } else if(grepl("CM|LM|RM|DM", position)){
    return("Middle")
  } else {
    return("Back")
  }
}

#Add variables
hetero_data <- PC.data %>%
  left_join(club_status_nonzero, by = "club", relationship = "many-to-many") %>%
  #Add nation variable equal to 1 if nation is England, 0 otherwise
  mutate(eng = ifelse(Nation == "eng ENG",1,0),
         is_eu = ifelse(sapply(strsplit(as.character(Nation), " "), `[`, 2) %in% eu_countries, 1, 0),
         Position_Category = sapply(Pos, classify_position),
         Position_dummy = model.matrix(~as.factor(Position_Category) - 1, data))

# Explore hetero across is positions
Position_F <- data.frame(Dependent_Variable = character(),
                            Coefficient = numeric(),
                            Signif.level = character(),
                            SE = numeric(),
                            Obs = numeric(),
                            stringsAsFactors = FALSE)

for (y in c(names(Pass_subset), 
            names(Possession_subset), 
            names(Defend_subset), 
            names(Shot_subset))) {
  formula <- as.formula(paste(y, "~ post_Howmany | Player + after"))  
  model <- felm(formula, data = hetero_data %>% filter(Position_Category == "Forward"))
  
  result <- summary(model)
  pvalues <- coef(summary(model))[,"Pr(>|t|)"]
  stars <- ifelse(pvalues<0.001, "***", ifelse(pvalues<0.01, "**", ifelse(pvalues<0.05, "*", "")))
  row <- data.frame(Dependent_Variable = y,
                    Coefficient = coef(model),
                    Signif.level = stars,
                    SE = sqrt(diag(vcov(model))),
                    Obs = length(model$residuals),
                    stringsAsFactors = FALSE)
  
  Position_F <- rbind(Position_F, row)
}

Position_M <- data.frame(
                         Coefficient = numeric(),
                         Signif.level = character(),
                         SE = numeric(),
                         Obs = numeric(),
                         stringsAsFactors = FALSE)

for (y in c(names(Pass_subset), names(Possession_subset), 
            names(Defend_subset), names(Shot_subset))) {
  formula <- as.formula(paste(y, "~ post_Howmany | Player + after"))  
  model <- felm(formula, data = hetero_data %>% filter(Position_Category == "Middle"))
  
  result <- summary(model)
  pvalues <- coef(summary(model))[,"Pr(>|t|)"]
  stars <- ifelse(pvalues<0.001, "***", ifelse(pvalues<0.01, "**", ifelse(pvalues<0.05, "*", "")))
  row <- data.frame(
                    Coefficient = coef(model),
                    Signif.level = stars,
                    SE = sqrt(diag(vcov(model))),
                    Obs = length(model$residuals),
                    stringsAsFactors = FALSE)
  
  Position_M <- rbind(Position_M, row)
}

Position_B <- data.frame(
                         Coefficient = numeric(),
                         Signif.level = character(),
                         SE = numeric(),
                         Obs = numeric(),
                         stringsAsFactors = FALSE)

for (y in c(names(Pass_subset), names(Possession_subset), 
            names(Defend_subset), names(Shot_subset))) {
  formula <- as.formula(paste(y, "~ post_Howmany | Player + after"))  
  model <- felm(formula, data = hetero_data %>% filter(Position_Category == "Back"))
  
  result <- summary(model)
  pvalues <- coef(summary(model))[,"Pr(>|t|)"]
  stars <- ifelse(pvalues<0.001, "***", ifelse(pvalues<0.01, "**", ifelse(pvalues<0.05, "*", "")))
  row <- data.frame(
                    Coefficient = coef(model),
                    Signif.level = stars,
                    SE = sqrt(diag(vcov(model))),
                    Obs = length(model$residuals),
                    stringsAsFactors = FALSE)
  
  Position_B <- rbind(Position_B, row)
}
print(xtable(cbind(Position_F, Position_M, Position_B)), 
      include.rownames=FALSE, 
      hline.after=c(-1,0,nrow(Position_F)), 
      file="tables/Position.tex",
      type="latex")

# Explore hetero across is England player.
Eng_Results_1 <- data.frame(Dependent_Variable = character(),
                              Coefficient = numeric(),
                              Signif.level = character(),
                              SE = numeric(),
                              Obs = numeric(),
                              stringsAsFactors = FALSE)
#Explore hetero across is EU.
for (y in c(names(Pass_subset), 
            names(Possession_subset),
            names(Shot_subset))) {
  formula <- as.formula(paste(y, "~ post_Howmany| Player + after"))  
  model <- felm(formula, data = hetero_data %>% filter(eng == 1))
  
  result <- summary(model)
  pvalues <- coef(summary(model))[,"Pr(>|t|)"]
  stars <- ifelse(pvalues<0.001, "***", ifelse(pvalues<0.01, "**", ifelse(pvalues<0.05, "*", "")))
  row <- data.frame(Dependent_Variable = y,
                    Coefficient = coef(model),
                    Signif.level = stars,
                    SE = sqrt(diag(vcov(model))),
                    Obs = length(model$residuals),
                    stringsAsFactors = FALSE)
  
  Eng_Results_1 <- rbind(Eng_Results_1, row)
}
#Non-England Players
Eng_Results_0 <- data.frame(
  Coefficient = numeric(),
  Signif.level = character(),
  SE = numeric(),
  Obs = numeric(),
  stringsAsFactors = FALSE)
#Add a intersection term to divide Eng players and non-Eng players
for (y in c(names(Pass_subset), 
            names(Possession_subset),
            names(Shot_subset))) {
  formula <- as.formula(paste(y, "~ post_Howmany| Player + after"))  
  model <- felm(formula, data = hetero_data %>% filter(eng == 0))
  
  result <- summary(model)
  pvalues <- coef(summary(model))[,"Pr(>|t|)"]
  stars <- ifelse(pvalues<0.001, "***", ifelse(pvalues<0.01, "**", ifelse(pvalues<0.05, "*", "")))
  row <- data.frame(
    Coefficient = coef(model),
    Signif.level = stars,
    SE = sqrt(diag(vcov(model))),
    Obs = length(model$residuals),
    stringsAsFactors = FALSE)
  
  Eng_Results_0 <- rbind(Eng_Results_0, row)
}

print(xtable(cbind(Eng_Results_1, Eng_Results_0)), 
      include.rownames=FALSE, 
      hline.after=c(-1,0,nrow(Eng_Results_0)), 
      file="D:/Xujianuo/socialmedia paper/BLMproj/BLMCode/tables/Eng.tex",
      type="latex")

# Explore hetero across is EU player.
EU_Results_1 <- data.frame(Dependent_Variable = character(),
                            Coefficient = numeric(),
                            Signif.level = character(),
                            SE = numeric(),
                            Obs = numeric(),
                            stringsAsFactors = FALSE)
#Explore hetero across is_eu.
for (y in c(names(Pass_subset), names(Possession_subset),
            names(Shot_subset))) {
  formula <- as.formula(paste(y, "~ post_Howmany | Player + after"))  
  model <- felm(formula, data = hetero_data %>% filter(is_eu == 1))
  
  result <- summary(model)
  pvalues <- coef(summary(model))[,"Pr(>|t|)"]
  stars <- ifelse(pvalues<0.001, "***", ifelse(pvalues<0.01, "**", ifelse(pvalues<0.05, "*", "")))
  row <- data.frame(Dependent_Variable = y,
                    Coefficient = coef(model),
                    Signif.level = stars,
                    SE = sqrt(diag(vcov(model))),
                    Obs = length(model$residuals),
                    stringsAsFactors = FALSE)
  
  EU_Results_1 <- rbind(EU_Results_1, row)
}
#Non-EU Players
EU_Results_0 <- data.frame(
  Coefficient = numeric(),
  Signif.level = character(),
  SE = numeric(),
  Obs = numeric(),
  stringsAsFactors = FALSE)
#Add a intersection term to divide EU players and non-EU players
for (y in c(names(Pass_subset), names(Possession_subset),
            names(Shot_subset))) {
  formula <- as.formula(paste(y, "~ post_Howmany | Player + after"))  
  model <- felm(formula, data = hetero_data %>% filter(is_eu == 0))
  
  result <- summary(model)
  pvalues <- coef(summary(model))[,"Pr(>|t|)"]
  stars <- ifelse(pvalues<0.001, "***", ifelse(pvalues<0.01, "**", ifelse(pvalues<0.05, "*", "")))
  row <- data.frame(
    Coefficient = coef(model),
    Signif.level = stars,
    SE = sqrt(diag(vcov(model))),
    Obs = length(model$residuals),
    stringsAsFactors = FALSE)
  
  EU_Results_0 <- rbind(EU_Results_0, row)
}

print(xtable(cbind(EU_Results_1, EU_Results_0)), 
      include.rownames=FALSE, 
      hline.after=c(-1,0,nrow(EU_Results_0)), 
      file="D:/Xujianuo/socialmedia paper/BLMproj/BLMCode/tables/EU.tex",
      type="latex")

#Define blackplayers
blackplayers<-c("Tammy Abraham","Ché Adams","Tosin Adarabioyo", "Dennis Adeniran",
                "Albert Adomah", "Benik Afobe","Daniel Agyei","Ahmed El Mohamady",
                "Ali Koiki", "Ola Aina","Nathan Aké", "Michail Antonio", 
                "Ibrahim Amadou","Daniel Amartey","Joseph Anang","Tino Anjorin", 
                "Benny Ashley-Seal","Christian Atsu","Sérge Aurier",
                "Pierre -Emerick Auvameyang","Taiwo Awoniyi",
                "Jordan Ayew","Abdul Baba","Eric Bailly","Tiémoué Bakayoko",
                "Folarin Balogun","Beni Baningime","Calvin Bassey",
                "Michy Batshuayi","Gavin Bazunu","Benjamin Mendy","Jayden Bennettes",
                "Christian Benteke","Steven Bergwijn","Bernardo",
                "Ryan Bertrand", "Philip Billing","Yves Bissouma","Joshua Bohui",
                "Tolaji Bola","Yannick Bolasie","Willy-Arnaud Boly","Gaëtan Bong",
                "Sofiane Boufal","Rhian Brewster","Isaiah Brown",
                "Vontae Campbell","Étienne Capoue", "Dominic Calvert-Lewin",
                "Cameron Carter-Vickers","Trevoh Chalobah", "Tahith Chong",
                "Hamza Choudhury","Nathaniel Clyne", "Jermain Defoe",
                "Danilo","Arnaut Danjuma","Kevin Danso","Keinan Davis",
                "Tom Dele-Bashiru","Fabian Delph","Jason Denayer",
                "Mousa Demlélé","Fousseni Diabaté","Mohamed Diamé", "Grady Diangna",
                "Alpha Dionkou","Issa Diop","Moussa Djenepo",
                "Domingos Quina","Abdoulayes Doucouré","Douglas Luiz","Ovie Ejaria",
                "Emerson","Niall Ennis","Bright Enobakhare","Timothy Eyoma",
                "Fabinho","Malachi Fagan-Walcott","Edimilson Fernandes","Fernandinho",
                "Timothy Fosu-Mensah","Dimitri Foulquier","Fred","Jean-Philippe Gbamin",
                "Gedson Fernandes","Morgan Gibbs-White","Ben Godfrey","Claudio Gomes",
                "Joe Gomez","John-Kymani Gordon","Lee Grant","Demariai Gray",
                "Andre Green","Mason Greenwood","Marc Guéhi","Idrissa Gueye",
                "Sébastien Haller","Kortney Hause","Kaine Hayden","Michael Hector",
                "Hélder Costa", "Rushian Hepburn-Murphy","Onel Hernández","Mason Holgate",
                "Callum Hudson-Odoi","Cameron Humphreys","Joseph Hungbo","Jordon Ibe",
                "Adam Idah","Odion Ighalo","Kelechi Iheanacho","Ivan Cavaleiro",
                "Alex Iwobi","José Izquierdo","Jacob Murphy","Reece James",
                "Alexandre Jankewitz", "João Pedro","Joelinton","Darnell Johnson",
                "Christian Kabasele","Yones Kaboul","Sullay Kaikai","N'Golo Kanté",
                "Moise Kean","Naby Keïta", "James Justin", "Cheikhou Kouyaté",
                "Lloyd Kelly","Joshua King","Nya Kirby","Anthony Knockaert",
                "Jonathan Kodjia","Vincent Kompany","Ezri Konsa","Billy Koumetio",
                "Alexandre Lacazette","Ethan Laird","Taria Lamptey","Yasser Larouci",
                "Jamaal Lascelles","Achraf Lazaar","Valentino Lazaro","Moritz Leitner",
                "Mario Lemina", "Romelu Lukaku", "Pablo Marí","Adrian Mariappa",
                "Jefferson Lerma","Thakgalo Leshabela","Jesse Lingard","Jürgen Locadia",
                "Ruben Loftus-Cheek","Ademola Lookman","Louis Thompson","Lucas Moura",
                "Dodi Lukébakio","Brooklyn Lyons-Foster","Ian Maatsen","Jacob Maddox",
                "Riyad Mahrez","Ainsley Maitland-Niles","Sadio Mané","Eliaquim Mangala",
                "Anthony Martial","Cuco Martina","Arthur Masuaku","Joël Matip",
                "David McGoldrick","D'Mani Mellor","Nampalys Mendy","Teden Mengi",
                "Yerry Mina","Tyrone Ming", "Dominic Revan","Ricardo Pereira",
                "Tyrick Mitchell","Mohamed Salah","Wes Morgan","Victor Moses",
                "Joel Mumbongo","Marvelous Nakamba","Wilfred Ndidi",
                "Tanguy Ndombélé","Reiss Nelson","Jeremy Ngakia",
                "Oumar Niasse","Eddie Nketiah","Georges-Kévin Nkoudou",
                "Danel Nlundulu","Felix Nmecha","Michael Obafemi","Nnamdi Ofoborh",
                "Angelo Ogbonna","Sheyi Ojo","Stefano Okaka",
                "Josh Onomah","Divock Origi","Owen Otasowie","Reece Oxford",
                "Kasey Palmer","Pedro Obiang","Terell Pennant","Nicolas Pépé",
                "Brandon Pierrick","Paul Pogba","Pedro Porro", "Callum Wilson",
                "Jason Puncheon","Rafael Camacho","Largie Ramazani","Kayne Ramsay",
                "Darren Randolph","Marcus Rashford","Nathan Redmond","Winston Reid",
                "Taylor Richards","Richarlison","Jairo Riedewald","Roberto Firmino",
                "Callum Robinson","Roderick Miranda","Salomón Rondón","Danny Rose",
                "Antonio Antonio Rüdiger","Allan Saint-Maximin", "Xande Silva",
                "Henri Saivet","Bukayo Saka","Mamadou Sakho","Bakary Sako",
                "William Saliba","Mbwana Samatta","Austin Samuels","Robert Sánchez",
                "Dion Sanderson","Leroy Sané","Philippe Sandler",
                "Ismaïla Sarr","Christian Saydee","Jeffrey Schlupp","Ken Sema",
                "Ryan Sessegnon","Djibril Sidibé","Sidnei Tavares","Ellis Simms",
                "Jerome Sinclair","Moussa Sissoko","Pape Souaré","Bayli Spencer-Adams",
                "Jack Spong","Junior Stanislas","Zack Steffen","Dujon Sterling",
                "Daniel Sturridge","Isaac Success","Japhet Tanganga","Percy Tau",
                "Neil Taylor","Nathan Tella","Alexander Tettey",
                "Dominic Thompson","Youri Tielemans","Timi Odusina","Fikayo Tomori",
                "Cenk Tosun","Andros Townsend","Adama Traoré","Nathan Trott",
                "Axel Tuanzebe","Antonio Valencia","Yan Valery","Patrick van Aanholt",
                "Virgil van Dijk","Theo Walcott","Kyle Walker-Peters",
                "Aaron Wan-Bissaka","Victor Wanyama","Danny Welbeck","Wesley",
                "Georginio Wijnaldum","Jetro Willems","Willan","Joe Willock",
                "Romaric Yapi","DeAndre Yedlin","Ashley Young","Wifried Zaha",
                "Marvin Zeegelaar","Jordan Zemura","Kurt Zouma"
)
na_race<-c("Brennan Camp","Leonardo Campana","Samir Carruthers","Kane Crichlow",
           "Andrew Crofts","Renat Dadashov","Sam Dalby","James Daly",
           "Jake Doyle-Hayes","Anwar EI Ghazi", "Mikkel Diskerud",
           "Akin Famewo","Will Ferry","Flávio Cristóvão","Marcel Franke",
           "Brooklyn Genesini","Paul Gladon","Paul Glatzel","Joseph Hardy",
           "Nathan Harker","Ben Heneghan","Kaylen Hinds","Rob Holding",
           "Ricky Holmes","Nathaniel Shio Hong Wan","Lukas Jensen",
           "Tyreece John-Jules","George Johnston","Soufyan Ahannach","Allan",
           "Billy Arce","Antonio Barreca","Mason barrett","Nathan Baxter",
           "Bernardo Rosa","Luke Bolton","Rocky Bushiri","Jack Bycrofy")
#hetero_data$blackplayer <- as.integer(data$Player %in% blackplayers)
#hetero_data$blackplayer[data$Player %in% na_race] <- NA

hetero_data <- hetero_data %>%
  mutate(blackplayer = ifelse(Player %in% blackplayers, 1, 0))

# Explore hetero across is Blackplayer.
Black_Results_1 <- data.frame(Dependent_Variable = character(),
                            Coefficient = numeric(),
                            Signif.level = character(),
                            SE = numeric(),
                            Obs = numeric(),
                            stringsAsFactors = FALSE)
#Add a intersection term to divide Black players and non-Black players
for (y in c(names(Pass_subset), names(Possession_subset), 
            names(Shot_subset))) {
  formula <- as.formula(paste(y, "~ post_Howmany| Player + after"))  
  model <- felm(formula, data = hetero_data %>% filter(blackplayer == 1))
  
  result <- summary(model)
  pvalues <- coef(summary(model))[,"Pr(>|t|)"]
  stars <- ifelse(pvalues<0.001, "***", ifelse(pvalues<0.01, "**", ifelse(pvalues<0.05, "*", "")))
  row <- data.frame(Dependent_Variable = y,
                    Coefficient = coef(model),
                    Signif.level = stars,
                    SE = sqrt(diag(vcov(model))),
                    Obs = length(model$residuals),
                    stringsAsFactors = FALSE)
  
  Black_Results_1 <- rbind(Black_Results_1, row)
}
#Non-Black Players
Black_Results_0 <- data.frame(
  Coefficient = numeric(),
  Signif.level = character(),
  SE = numeric(),
  Obs = numeric(),
  stringsAsFactors = FALSE)
#Add a intersection term to divide Black players and non-Black players
for (y in c(names(Pass_subset), names(Possession_subset), 
            names(Shot_subset))) {
  formula <- as.formula(paste(y, "~ post_Howmany| Player + after"))  
  model <- felm(formula, data = hetero_data %>% filter(blackplayer == 0))
  
  result <- summary(model)
  pvalues <- coef(summary(model))[,"Pr(>|t|)"]
  stars <- ifelse(pvalues<0.001, "***", ifelse(pvalues<0.01, "**", ifelse(pvalues<0.05, "*", "")))
  row <- data.frame(
    Coefficient = coef(model),
    Signif.level = stars,
    SE = sqrt(diag(vcov(model))),
    Obs = length(model$residuals),
    stringsAsFactors = FALSE)
  
  Black_Results_0 <- rbind(Black_Results_0, row)
}

print(xtable(cbind(Black_Results_1, Black_Results_0)), 
      include.rownames=FALSE, 
      hline.after=c(-1,0,nrow(Black_Results_0)), 
      file="D:/Xujianuo/socialmedia paper/BLMproj/BLMCode/tables/Black.tex",
      type="latex")

