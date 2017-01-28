#Załaduj dane z pliku (dane uzyskane są z serwisu TowerDate)
#Plik dolaczony jest na moodlach
Store1 <- read.csv("/Users/andrzej/Downloads/towerdata_results_1.csv", na.strings = "", head = TRUE)
#Czyszczenie niepotrzebnych znakow z tytulow kolumn
names(Store1) <- gsub("_|\\.\\.\\.|\\." , "", names(Store1))
#Identyfikuj ktore wiersze zawieraja NA (Not Available) wartosci
which(rowSums(is.na(Store1))==ncol(Store1))
#Usun niepotrzebne kolumny
Store1new <- Store1[c(-1,-4,-10,-11,-14,-15,-16,-17,-18,-19,-20,-21,-22,-23,-24,-25,-26,-27,-28,
                      -29,-30,-31,-32,-33,-34,-35,-36)] 
#Usun wiersze z pustymi wartosciami
Store1df <- Store1new[!!rowSums(!is.na(Store1new)),]
#Konwertuj csv do data.frame
df <- as.data.frame(Store1df)
#Zapisz data.frame
save(df, file = "df.Rda")
#pokaz data.frame
head(df)
#Informacje o data.frame
str(df)
#Uzupełnij dane przykładowymi wartosciami (histograms)
#Wloz wartosci do komorek z NA
indx1 <- which(is.na(df), arr.ind=TRUE)
df[indx1] <- c("25-34", "Male", "50k-75k", "Single",
               "No", "Own", "150k-200k","Completed High School", "Professional"
               )[indx1[,2]]

#Import biblioteki matematycznej Cluser
library(cluster)
#Create dissimilarity matrix
#Tworz macierz niepodobieńtw
#użycie funkcji gower do znalezienia odleglosci miedzy zmiennymi
daisy1 <- daisy(df, metric = "gower", type = list(ordratio = c(1:9))) 
#Zastosowanie algorytmu PAM z 3 klastrami
k1answers <- pam(daisy1, 3, diss = TRUE)
#Pokaz liczbe obserwacji na pojedynczy klaster
k1answers$id.med
#Wyswietl informacje o klasterze
k1answers$clusinfo
#Grupuj wiersze po klaserze
Groups1 <- k1answers$clustering
#Coerce object to data.frame
clustgroups <- as.data.frame(Groups1, row.names = NULL)
#Zapisz wyniki .CSV file
#Zapisz wierz i do jakiego klastra nalezy (klient [spacja] nr klastra )
write.table(clustgroups, file <- "/Users/andrzej/Downloads/Solutions1.csv")

#Wizalizacja
#histogram Age - DONE
res <- ordered(Store1$Age, levels = c("18-20", "21-24", "25-34", 
                                      "35-44", "45-54", "55-64", "65+"))

#Ustaw pozycje
par(mar=c(6,6,2,0.5))
#Tworz wykres
plot(res, main = "Rozkład wieku klientów", xlab = "", ylab = "", las=2, ylim = c(0,1000))
mtext(text="Wiek", side=1, line=5)
mtext(text="Liczba", side=2, line=5)


#histogram Płeć
plot(Store1$Gender, main = "Rozkład płci klientów", xlab = "Płeć", 
     ylab = "Liczba", ylim = c(0,4000))


#histogram HomeOwnerStatus - DONE
par(mar=c(5,5,3,1))

plot(Store1$HomeOwnerStatus, main = "Rozkład of status mieszkania", 
     xlab = "Status właśności domu", ylab = "Liczba", ylim = c(0,3000))


#Wykres przychodów 
res <- ordered(Store1$HouseholdIncome, levels = c("0-15k", "15k-25k", "25k-35k", 
                                                  "35k-50k", "50k-75k", "75k-100k", 
                                                  "100k-125k", "125k-150k", "150k-175k", 
                                                  "175k-200k", "200k-250k", "250k+"))
par(mar=c(7,5,4,1))
#Tworz wykres
plot(res, main = "Rozkład przychodu domowego klientów", xlab = "", 
     ylab = "", las=2, ylim = c(0,700))
mtext(text="Przychód domowy", side=1, line=5.5)
mtext(text="Liczba", side=2, line=4)


plot(Store1$MaritalStatus, main = "Rozkład stanu cywilnego klientów", xlab = "Stan cywilny", 
     ylim = c(0,3500), ylab = "Liczba")


plot(Store1$PresenceofChildren, main = "Rozkład posiadania dzieci", xlab = "Posiadanie dzieci", 
     ylim = c(0,4500), ylab = "Liczba")


#Ustaw kolejnosc
res1 <- ordered(Store1$HomeMarketValue, levels = c("1k-25k", "25k-50k", "50k-75k",
                                                   "75k-100k", "100k-150k",
                                                   "150k-200k", "200k-250k", 
                                                   "250k-300k", "300k-350k", 
                                                   "350k-500k", "500k-1mm", "1mm+"))
#Ustawienie wymiarow (polozenia wykresu)
par(mar = c(7,5,4,1))
#Create plot
plot(res1, main = "Rozkład wartości rynkowej klinetów", 
     xlab = "", ylab = "", las = 2, ylim = c(0,1000))
mtext(text = "Wartośc rynkowa", side = 1, line = 5.5)
mtext(text = "Liczba", side = 2, line = 4)


#Ustaw kolejnosc
res2 <- ordered(Store1$Occupation, levels = c("Blue Collar Worker", "Business Owner", 
                                              "Civil Service", "Exec/Upper Manage", 
                                              "Health Services", "Homemaker", "Middle Manage", 
                                              "Military Personnel", "Nurse", "Part Time", 
                                              "Professional", "Retired", "Secretary", 
                                              "Student", "Teacher", "Technology"))
par(mar=c(11,5,2,1))
plot(res2, main = "Rozkład zawodów klientów", 
     xlab = "", ylab = "", las = 2, ylim = c(0,600))
mtext(text = "Zawód", side = 1, line = 10)
mtext(text = "Liczba", side = 2, line = 4)


# edukacja
res3 <- ordered(Store1$Education, levels = c("Completed High School", "Attended College", 
                                             "Completed College", "Completed Graduate School", 
                                             "Attended Vocational/Technical"))
par(mar=c(14,5,2,1))
plot(res3, main = "Rozkład wykształcenia klientów", 
     xlab = "", ylab = "", las = 2, ylim = c(0,2000))
mtext(text = "Wykształcenie", side = 1, line = 12)
mtext( text = "Liczba", side = 2, line = 4)


res4 <- ordered(Store1$LengthofResidence, levels = c("Less than 1 year", "1 Year",  
                                                     "2 Years",  "3 Years",  "4 Years", 
                                                     "5 Years",  "6 Years",  "7 Years",  
                                                     "8 Years",  "9 Years",  "10 Years",  
                                                     "11-15 years", "16-19 years",  "20+ years"))
par(mar=c(10,5,2,1))
#Tworz wykres
plot(res4, main = "Rozkład lat zamieszkania klientów", 
     xlab = "", ylab = "", las = 2, ylim = c(0,600))
mtext(text = "Lata zamieszkania", side = 1, line = 8)
mtext(text = "Liczba", side = 2, line = 3.5)
