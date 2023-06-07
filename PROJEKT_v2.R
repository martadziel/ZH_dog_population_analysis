#Wczytanie niezbędnych bibliotek
library(datasets)
#library(lubridate) #już część pakietu tidyverse
#library(dplyr) #już część pakietu tidyverse
library(ggplot2) #już część pakietu tidyverse
library(plotly)
#library(tidyr) #już część pakietu tidyverse
library(tidyverse)


#wczytanie bazy danych z oryginalnego pliku
Dogs_original <-read_csv("/Volumes/Dysk D/Studia/Projekt/KUL100OD1001.csv")

#Stworzenie ramki danych (w oddzielnym kroku, aby zachować oryginalną bazę danych w pamięci, 
#a wszelkie dalsze operacje wykonywać już na ramce danych)
Dogs <- Dogs_original
glimpse(Dogs)
#przeglądanie bazy danych
View(Dogs)
head(Dogs)
summary(Dogs)

###Data wrangling
#Zmiana nazw kolumn
colnames(Dogs)[1] <- "Year"
colnames(Dogs)[3] <- "OwnerID"
colnames(Dogs)[4] <- "OwnerAgeNum"
colnames(Dogs)[5] <- "OwnerAge"
colnames(Dogs)[8] <- "OwnerGender"
colnames(Dogs)[9] <- "OwnerGenderNum"
colnames(Dogs)[10] <- "DistrictNum"
colnames(Dogs)[11] <- "District"
colnames(Dogs)[13] <- "AreaNum"
colnames(Dogs)[14] <- "Area"
colnames(Dogs)[16] <- "BreedPrimary"
colnames(Dogs)[17] <- "BreedSecondary"
colnames(Dogs)[19] <- "BreedClassification"
colnames(Dogs)[20] <- "BreedClassificationNum"
colnames(Dogs)[22] <- "BreedType"
colnames(Dogs)[23] <- "BreedTypeNum"
colnames(Dogs)[24] <- "DogYear"
colnames(Dogs)[25] <- "DogAgeNumber"
colnames(Dogs)[26] <- "DogAgeCategory"
colnames(Dogs)[29] <- "DogGender"
colnames(Dogs)[30] <- "DogGenderNum"
colnames(Dogs)[31] <- "DogColor"
colnames(Dogs)[32] <- "DogValues"

#Zamiana niektórych kolumn na factory
#Dogs$Year <- as.factor(Dogs$Year)
#Dogs$Year <- as.numeric(as.character(Dogs$Year)) #przywracanie do numerycznej wersji
Dogs$OwnerAge <- as.factor(Dogs$OwnerAge)
Dogs$OwnerGender <- as.factor(Dogs$OwnerGender)
Dogs$District <- as.factor(Dogs$District)
Dogs$Area <- as.factor(Dogs$Area)
Dogs$BreedPrimary <- as.factor(Dogs$BreedPrimary)
Dogs$BreedSecondary <- as.factor(Dogs$BreedSecondary)
Dogs$BreedClassification <- as.factor(Dogs$BreedClassification)
Dogs$BreedType <- as.factor(Dogs$BreedType)
Dogs$DogYear <- as.factor(Dogs$DogYear)
Dogs$DogAgeCategory <- as.factor(Dogs$DogAgeCategory)
Dogs$DogGender <- as.factor(Dogs$DogGender)
Dogs$DogColor <- as.factor(Dogs$DogColor)

#Dogs_factor$nowanazwakolumny <- as.factor(Dogs_factor$DogColor) #R stworzy nową kolumnę

summary(Dogs)

#przeczyszczenie tabeli z niepotrzebnych kolumn
Dogs_selection <- Dogs %>%
  select('Year', 'OwnerID', 'OwnerAge', 'OwnerGender', 
    'District', 'Area', 'BreedPrimary', 'BreedSecondary', 
    'BreedClassification', 'BreedType', 'DogYear', 'DogAgeNumber', 
    'DogAgeCategory', 'DogGender', 'DogColor','DogValues')

summary(Dogs_selection)
# przypisanie cummulative wartości do lat
Dogs3 <- Dogs %>%
  select('Year', 'DogValues') %>%
  group_by(Year) %>%
  summarize(suma_pieskow= sum(DogValues))

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

#poniższe zdupplikować też wyżej; z zapisem wt = DogValues
Dogs_Owners_Age <- count(Dogs, Year, OwnerAge, wt = DogValues, name = "DogSum")

### wyświetlimy liczbę psów per kategoria wiekowa właściciela; na jednym wykresie
Dogs_Owners_Age %>% 
  mutate(OwnerAge = fct_collapse(OwnerAge, 
    "10 - 19 years old" = c("10- bis 19-Jährige"), 
    "20 - 29 years old" = c("20- bis 29-Jährige"),
    "30 - 39 years old" = c("30- bis 39-Jährige"),
    "40 - 49 years old" = c("40- bis 49-Jährige"),
    "50 - 59 years old" = c("50- bis 59-Jährige"),
    "60 - 69 years old" = c("60- bis 69-Jährige"),
    "70 - 79 years old" = c("70- bis 79-Jährige"), 
    "80 - 89 years old" = c("80- bis 89-Jährige"),
    "90 - 99 years old" = c("90- bis 99-Jährige"),
    "n.a." = c("Unbekannt"))) %>%
  ggplot(aes (x = Year, y = DogSum, col = OwnerAge)) + geom_line()

### psy per kategoria wiekowa właściciela; oddzielne wykresy; takie same skale
Dogs_Owners_Age %>% 
  mutate(OwnerAge = fct_collapse(OwnerAge, 
    "10 - 19 years old" = c("10- bis 19-Jährige"), 
    "20 - 29 years old" = c("20- bis 29-Jährige"),
    "30 - 39 years old" = c("30- bis 39-Jährige"),
    "40 - 49 years old" = c("40- bis 49-Jährige"),
    "50 - 59 years old" = c("50- bis 59-Jährige"),
    "60 - 69 years old" = c("60- bis 69-Jährige"),
    "70 - 79 years old" = c("70- bis 79-Jährige"), 
    "80 - 89 years old" = c("80- bis 89-Jährige"),
    "90 - 99 years old" = c("90- bis 99-Jährige"),
    "n.a." = c("Unbekannt"))) %>%
  ggplot(aes (x = Year, y = DogSum)) + geom_line() + 
  facet_wrap(vars(OwnerAge))

### psy per kategoria wiekowa właściciela; oddzielne wykresy; niezależne skale
Dogs_Owners_Age %>% 
  #filter(BreedClassification != 'Rassehund') %>% 
  ggplot(aes (x = Year, y = DogSum)) + geom_line() + 
  facet_wrap(vars(OwnerAge), scales = "free")


view(filter(Dogs_selection, OwnerAge == "80- bis 89-Jährige"))
view(filter(Dogs, OwnerID == 120846))

TenToTwenty <- count(Dogs, Year, OwnerAge, wt = DogValues, name = "DogSum")

TenToTwenty <- Dogs %>%
  count(Year, OwnerAge, wt = DogValues, name = "DogSum") %>%
  filter(OwnerAge == "10- bis 19-Jährige") %>%
  summarize(DogSum = sum(DogSum),.by = c(Year, OwnerAge)) %>%
  view()


### sprawdzamy ile jest szczeniaków per year i per OwnerAge
Puppies <- Dogs %>%
  filter(DogAgeNumber == 0)

Puppies_sum <- count(Puppies, Year, OwnerAge, wt = DogValues, name = "DogSum")
Puppies_sum %>% 
  ggplot(aes (x = Year, y = DogSum)) + geom_line() + 
  facet_wrap(vars(OwnerAge))
### okazuje się, że grupa 70-79 oraz 80-89 nadal aktywnie nabywa szczeniaki, dlatego może warto
#zostawić te dwie grupy do analizy. Ale z kolei może lepiej je zsumować i rozważać jako jedną 
#patrząc na liczbę szczeniaków per kategoria wiekowa w 2023, 70-79 oraz 80-89 łącznie stanowią ok. 10% wszystkich szczeniaków

Dogs_Age %>%
  mutate(DogAgeNumber = as.factor(Dogs_Age$DogAgeNumber)) %>%
  mutate(DogAgeNumber = fct_collapse(DogAgeNumber,
    "0 - Puppy" = c("0"),
    "1-3 - Young Dog" = c("1", "2", "3"),
    "4-9 - Adult Dog" = c("4", "5", "6", "7", "8", "9"),
    "10-12 - Old Dog" = c("10", "11", "12"),
    "13+ - Very Old Dog" = c("13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23")
  )) %>%
  filter(DogAgeNumber!= "999") %>%
  summarize(DogSum = sum(DogSum),.by = c(Year, DogAgeNumber)) %>% 
  ggplot(aes (x = Year, y = DogSum, fill = DogAgeNumber)) + geom_bar(stat = "identity") 






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

### sprawdźmy wzrost absolutny oraz wzrost procentowy, aby się upewnić, 
#czy możemy skupić się w dalszej analizie na tych trzech grupach i pominąć pozostałe

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
  mutate(GrowthRate = DogSum/lag(DogSum), .by = OwnerAge) %>%
  ggplot(aes (x = Year, y = GrowthRate, col = OwnerAge)) + geom_line() + 
  facet_wrap(vars(OwnerAge)) + 
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_text(aes(label= paste(round(GrowthRate, 2), "%")), vjust = -0.3)

### PIOTR: jak dodac linię trendu jeśli chodzi o średni growth rate za poprzednie lata?
#skupimy się na 4 kategoriach; na razie kryterium wyboru jest wzrost 2021 vs 2020 o 0.5 i więcej.
# kolejna hipoteza robocza: w 2023 widać osłabienie trendu - związek z powrotem do pracy z biura, etc.

###spróbujmy zrobić analizę per rasowy / mieszany

Dogs_breed_class <- count(Dogs, Year, BreedClassification, OwnerAge, wt = DogValues, name = "DogSum")

Dogs_breed_class %>%
  mutate(BreedClassification = fct_collapse(BreedClassification,
    "Pure Breed" = c("Rassehund"),
    "Mixed" = c("Mischling, beide Rassen bekannt", "Mischling, beide Rassen unbekannt", 
      "Mischling, sekundäre Rasse unbekannt")
  )) %>%
  filter(OwnerAge!= "Unbekannt") %>%
  summarize(DogSum = sum(DogSum),.by = c(Year, BreedClassification)) %>% 
  ggplot(aes (x = Year, y = DogSum, fill = BreedClassification)) + geom_bar(stat = "identity") +
  geom_text(aes(label= paste(round(GrowthRate, 2), "%")))
 
###Wniosek: patrząc na powyższe wykresy z podziałem na psy rasowe vs mieszane widać, że:
# 1. większość psów w Zurychu stanowią psy rasowe
# 2. w obydwu grupach widoczny jest podobny trend -> w latach 2015 - 2020 liczba psów wzrastała,
# natomiast po 2020 ten wzrost jest silniejszy
# => dlatego ze względu na zbliżony trend w obu kategoriach, nie musimy robić oddzielnej analizy z podziałem na psy rasowe bądź mieszane

### Sprawdźmy teraz rozkład psów ze względu na ich wiek

Dogs_Age <- count(Dogs, Year, DogAgeNumber, wt = DogValues, name = "DogSum")

### wyświetlimy liczbę psów per kategoria wiekowa właściciela; na jednym wykresie
Dogs_Age %>% 
  ggplot(aes (x = Year, y = DogSum, col = OwnerAge)) + geom_line()

### psy per kategoria wiekowa właściciela; oddzielne wykresy; takie same skale
Dogs_Age %>% 
  ggplot(aes (x = Year, y = DogSum)) + geom_line() + 
  facet_wrap(vars(DogAgeNumber))

### jest ponad 20 kategorii jeśli zrobimy analizę po wieku psa; warto by było to może jakoś połączyć,
#ale pytanie jak? 
# growth rate chyba nie ma sensu, ponieważ pies o wieku 0 w 2015 będzie juz psem 1 w 2016...
#spróbujmy na logikę stworzyć kategorie wiekowe

Dogs_Age %>%
  mutate(DogAgeNumber = as.factor(Dogs_Age$DogAgeNumber)) %>%
  mutate(DogAgeNumber = fct_collapse(DogAgeNumber,
    "0 - Puppy" = c("0"),
    "1-3 - Young Dog" = c("1", "2", "3"),
    "4-9 - Adult Dog" = c("4", "5", "6", "7", "8", "9"),
    "10-12 - Old Dog" = c("10", "11", "12"),
    "13+ - Very Old Dog" = c("13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23")
  )) %>%
  filter(DogAgeNumber!= "999") %>%
  summarize(DogSum = sum(DogSum),.by = c(Year, DogAgeNumber)) %>% 
  ggplot(aes (x = Year, y = DogSum)) + geom_line() 



### tylko co mi daje informacja o psim wieku...
# psi wiek - zastanowić się nad powiązaniem tematu z potencjalnym biznesem związanym z psami - sprawdzić liczebność w 2023 pod kątem wieku psa
# jeśli jest dużo młodych, to znaczy, że będzie zapotrzebowanie na biznes

### Sprawdźmy teraz rozkład ze względu na breed type
Dogs_breed_type <- count(Dogs, Year, BreedType, wt = DogValues, name = "DogSum")

Dogs_breed_type %>% 
  ggplot(aes (x = Year, y = DogSum)) + geom_line() + 
  facet_wrap(vars(BreedType))

### Sprawdźmy teraz rozkład ze względu na primary breed
Dogs_breed <- count(Dogs, Year, BreedPrimary, wt = DogValues, name = "DogSum")

#!!!!!!
Dogs_breed %>% 
  ggplot(aes (x = Year, y = DogSum)) + geom_line() + 
  facet_wrap(vars(BreedPrimary))
### przy powyższej funkcji wypluje wszystkie rasy - jest ich bardzo dużo. Musimy ograniczyć
# spróbujmy wziąć top 10 albo top 15 na każdy rok

Rasy <- Dogs_breed %>%
  filter(Year == 2023, BreedPrimary!= "Unbekannt") %>%
  arrange(desc(DogSum))%>%
  slice(1:10)

view(Rasy)
glimpse(Dogs$BreedPrimary)

#chcę wyciągnć wektor ras zeby otem móc go użyć jako filtr
TopBreeds <- Rasy %>%
  select(BreedPrimary)

TopBreeds$BreedPrimary <- as.character(TopBreeds$BreedPrimary)

Breed_vector <- c(TopBreeds$BreedPrimary[1], TopBreeds$BreedPrimary[2], 
  TopBreeds$BreedPrimary[3], TopBreeds$BreedPrimary[4],
  TopBreeds$BreedPrimary[5], TopBreeds$BreedPrimary[6],
  TopBreeds$BreedPrimary[7], TopBreeds$BreedPrimary[8], 
  TopBreeds$BreedPrimary[9], TopBreeds$BreedPrimary[10])

Dogs_BreedSelection <- Dogs %>% 
  subset(BreedPrimary %in% Breed_vector) %>% 
  count(Year, BreedPrimary, OwnerAge, wt = DogValues, name= "DogSum") 

Dogs_BreedSelection %>%
  summarize(DogSum = sum(DogSum),.by = c(BreedPrimary, Year)) %>%
  ggplot(aes (x = Year, y= DogSum)) +
  geom_line() + facet_wrap(vars(BreedPrimary))

Dogs_BreedSelection %>% 
  mutate(OwnerAge = fct_collapse(OwnerAge, 
    "10 - 29 years old" = c("10- bis 19-Jährige", "20- bis 29-Jährige"), 
    "30 - 39 years old" = c("30- bis 39-Jährige"),
    "40 - 49 years old" = c("40- bis 49-Jährige"),
    "50 - 59 years old" = c("50- bis 59-Jährige"),
    "60 - 69 years old" = c("60- bis 69-Jährige"),
    "70 - 99 years old" = c("70- bis 79-Jährige", "80- bis 89-Jährige", "90- bis 99-Jährige")) )%>%
  filter(OwnerAge != "Unbekannt") %>% 
  summarize(DogSum = sum(DogSum),.by = c(Year, OwnerAge, BreedPrimary)) %>%
  ggplot(aes (x = Year, y= DogSum, col = OwnerAge)) +
  geom_line() + facet_wrap(vars(BreedPrimary))
