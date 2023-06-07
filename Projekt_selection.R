#Wczytanie niezbędnych bibliotek
library(datasets)
library(plotly)
library(tidyverse)

#wczytanie bazy danych z oryginalnego pliku
Dogs_original <-read_csv("/Volumes/Dysk D/Studia/Projekt/KUL100OD1001.csv")

#Stworzenie ramki danych (w oddzielnym kroku, aby zachować oryginalną bazę danych w pamięci, 
#a wszelkie dalsze operacje wykonywać już na ramce danych)
Dogs <- Dogs_original

#przeglądanie bazy danych
glimpse(Dogs)
View(Dogs)
head(Dogs)
summary(Dogs)

###Data wrangling
#Zmiana nazw kolumn
colnames(Dogs)[1] <- "Year"
colnames(Dogs)[2] <- "OwnerID"
colnames(Dogs)[3] <- "OwnerAgeNum"
colnames(Dogs)[4] <- "OwnerAge"
colnames(Dogs)[7] <- "OwnerGender"
colnames(Dogs)[8] <- "OwnerGenderNum"
colnames(Dogs)[9] <- "DistrictNum"
colnames(Dogs)[10] <- "District"
colnames(Dogs)[12] <- "AreaNum"
colnames(Dogs)[13] <- "Area"
colnames(Dogs)[15] <- "BreedPrimary"
colnames(Dogs)[16] <- "BreedSecondary"
colnames(Dogs)[18] <- "BreedClassification"
colnames(Dogs)[19] <- "BreedClassificationNum"
colnames(Dogs)[21] <- "BreedType"
colnames(Dogs)[22] <- "BreedTypeNum"
colnames(Dogs)[23] <- "DogYear"
colnames(Dogs)[24] <- "DogAgeNumber"
colnames(Dogs)[25] <- "DogAgeCategory"
colnames(Dogs)[28] <- "DogGender"
colnames(Dogs)[29] <- "DogGenderNum"
colnames(Dogs)[30] <- "DogColor"
colnames(Dogs)[31] <- "DogValues"

#przeczyszczenie tabeli z niepotrzebnych kolumn
Dogs_selection <- Dogs %>%
  select('Year', 'OwnerID', 'OwnerAge', 'OwnerGender', 
    'District', 'Area', 'BreedPrimary', 'BreedSecondary', 
    'BreedClassification', 'BreedType', 'DogYear', 'DogAgeNumber', 
    'DogAgeCategory', 'DogGender', 'DogColor','DogValues')


count(Dogs, Year) #zliczanie linii! czyli jak mamy więcej psów w niektórych rekordach
count(Dogs, OwnerID, sort = TRUE) #wyświetli 10 pierwszych
view(count(Dogs, OwnerID, sort = TRUE)) #wyświetli wszystkie dane w tabeli
view(count(Dogs, Year, OwnerID, sort = TRUE)) #w podziale na lata - sprawdzenie ile najwięcej psów przypada
#per jeden właściciel -> max 11; nie trzeba rozpatrywać tego pod kątem osoba prywatna vs np. hodowca

#uwzględnione linie, w których mamy więcej niż 1 psa (czyli np. kilka psów o tych samych cechach per OwnerID)
DogsCountPerYear <- count(Dogs, Year, wt = DogValues) #inne wartości niż z counta powyżej - ta linia tutaj jest dopiero poprawna!

#nie wyświetli wykresu jeśli x jest factorem!!!
plot(DogsCountPerYear)
ggplot(DogsCountPerYear, aes (x = Year, y = n)) + geom_line()
#zakładamy, że mamy pełne dane - wpisać to w projekcie.


#sprawdzamy
view(filter(Dogs, OwnerID == 105585))
#czy psy się powtarzają?
view(filter(Dogs, OwnerID == 105585, BreedPrimary == 'Papillon', DogYear == 2012))

view(filter(Dogs, OwnerID == 82452, DogYear == 1999))

### wyświetlimy liczbę psów per kategoria wiekowa właściciela; na jednym wykresie
Dogs_Owners_Age %>% 
  #filter(BreedClassification != 'Rassehund') %>% 
  ggplot(aes (x = Year, y = DogSum, col = OwnerAge)) + geom_line()

### psy per kategoria wiekowa właściciela; oddzielne wykresy; takie same skale
Dogs_Owners_Age %>% 
  #filter(BreedClassification != 'Rassehund') %>% 
  ggplot(aes (x = Year, y = DogSum)) + geom_line() + 
  facet_wrap(vars(OwnerAge))

### psy per kategoria wiekowa właściciela; oddzielne wykresy; niezależne skale
Dogs_Owners_Age %>% 
  #filter(BreedClassification != 'Rassehund') %>% 
  ggplot(aes (x = Year, y = DogSum)) + geom_line() + 
  facet_wrap(vars(OwnerAge), scales = "free")


view(filter(Dogs_selection, OwnerAge == "70- bis 79-Jährige"))
view(filter(Dogs, OwnerID == 121522))

### sprawdzamy ile jest szczeniaków per year i per OwnerAge
Puppies <- Dogs %>%
  filter(DogAgeNumber == 0)

Puppies_sum <- count(Dogs, Year, OwnerAge, wt = DogValues, name = "DogSum")
Puppies_sum %>% 
  ggplot(aes (x = Year, y = DogSum)) + geom_line() + 
  facet_wrap(vars(OwnerAge))
### okazuje się, że grupa 70-79 oraz 80-89 nadal aktywnie nabywa szczeniaki, dlatego może warto
#zostawić te dwie grupy do analizy. Ale z kolei może lepiej je zsumować i rozważać jako jedną 
#patrząc na liczbę szczeniaków per kategoria wiekowa w 2023, 70-79 oraz 80-89 łącznie stanowią ok. 10% wszystkich szczeniaków

### wracamy do poprzedniej bazy Dogs; puppies była stworzona na potrzeby sprawdzenia OwnerAge kategorii

### teraz chcemy połączyć mniejsze kategorie oraz zmienić nazwy etykiet; skala jednakowa dla wszystkich grup
Dogs_Owners_Age %>% 
  mutate(OwnerAge = fct_collapse(OwnerAge, 
    "10 - 29 years old" = c("10- bis 19-Jährige", "20- bis 29-Jährige"), 
    "30 - 39 years old" = c("30- bis 39-Jährige"),
    "40 - 49 years old" = c("40- bis 49-Jährige"),
    "50 - 59 years old" = c("50- bis 59-Jährige"),
    "60 - 69 years old" = c("60- bis 69-Jährige"),
    "70 - 99 years old" = c("70- bis 79-Jährige", "80- bis 89-Jährige", "90- bis 99-Jährige")) )%>%
  filter(OwnerAge != "Unbekannt") %>% 
  summarize(DogSum = sum(DogSum),.by = c(Year, OwnerAge)) %>%
  #mutate(GrowthRate = DogSum/lag(DogSum), .by = OwnerAge) %>%
  ggplot(aes (x = Year, y = DogSum, col = OwnerAge)) + geom_line() + 
  facet_wrap(vars(OwnerAge)) + 
  geom_hline(yintercept = 1, linetype = "dashed")

#wnioski:
# grupy: 30-39, 40-49 - największy skok po 2020; największa liczebność w 2023
# grupa 50-59 - najwyższy punkt bazowy, ale wzrost już nie taki znaczny (ale widoczny)
# grupy 10-29, 60-69, 70-99 - tylko nieznaczny wzrost; impakt nie tak silny

### teraz chcemy połączyć mniejsze kategorie oraz zmienić nazwy etykiet; skala indywidualna
Dogs_Owners_Age %>% 
  mutate(OwnerAge = fct_collapse(OwnerAge, 
    "10 - 29 years old" = c("10- bis 19-Jährige", "20- bis 29-Jährige"), 
    "30 - 39 years old" = c("30- bis 39-Jährige"),
    "40 - 49 years old" = c("40- bis 49-Jährige"),
    "50 - 59 years old" = c("50- bis 59-Jährige"),
    "60 - 69 years old" = c("60- bis 69-Jährige"),
    "70 - 99 years old" = c("70- bis 79-Jährige", "80- bis 89-Jährige", "90- bis 99-Jährige")) )%>%
  filter(OwnerAge != "Unbekannt") %>% 
  summarize(DogSum = sum(DogSum),.by = c(Year, OwnerAge)) %>%
  #mutate(GrowthRate = DogSum/lag(DogSum), .by = OwnerAge) %>%
  ggplot(aes (x = Year, y = DogSum, col = OwnerAge)) + geom_line() + 
  facet_wrap(vars(OwnerAge), scales = "free") + 
  geom_hline(yintercept = 1, linetype = "dashed")




