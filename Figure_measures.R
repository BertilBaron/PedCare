# Abbildung durchgeführte Maßnahmen zum Infektionsschutz (Figure 1 im Provision
# paper)
# Date: 2021/06/15

# Laden benötigte Pakete
library(tidyverse)
library(ggplot2)
library(tidyselect)
library(ggrepel)

# Laden der Daten
# load("Arztumfrage.rda")
df_aerzte <- readxl::read_xlsx("Data/results-aerzte.xlsx")
df_aerzte_complete <- df_aerzte %>%
  filter(lastpage >= 8) %>%
  rename_all(~gsub("\\[","",.)) %>%
  rename_all(~gsub("\\]","",.))


# Anzahl Teilnehmer
N <- df_aerzte_complete %>% nrow()
# Fehlende Werte
# Absolute Anzahl fehlender Werte pro Variable bei Arzt-FB
miss <- map_df(df_aerzte_complete, function(x) sum(is.na(x)))
# Relative Anzahl fehlender Werte pro Variable
miss_ant <- map_df(miss, function(x) round((x/N)*100, digits = 2))

# Funktion für Anteil und Anzahl bei kategoriellen Variablen
get_sums <- function(x, y){
  sums <- df_aerzte_complete %>%
    group_by_at(x) %>%
    summarise(Anzahl = n(),
              Anteil = round((Anzahl*100) / (N - y), 1))
}

# Maßnahmen
sums_mass1 <- get_sums("massnahmen1", miss$massnahmen1)
sums_mass2 <- get_sums("massnahmen2", miss$massnahmen2)
sums_mass3 <- get_sums("massnahmen3", miss$massnahmen3)
sums_mass4 <- get_sums("massnahmen4", miss$massnahmen4)
sums_mass5 <- get_sums("massnahmen5", miss$massnahmen5)
sums_mass6 <- get_sums("massnahmen6", miss$massnahmen6)
sums_mass7 <- get_sums("massnahmen7", miss$massnahmen7)
sums_mass8 <- get_sums("massnahmen8", miss$massnahmen8)
sums_mass9 <- get_sums("massnahmen9", miss$massnahmen9)
sums_mass10 <- get_sums("massnahmen10", miss$massnahmen10)
sums_mass11 <- get_sums("massnahmen11", miss$massnahmen11)
sums_mass12 <- get_sums("massnahmen12", miss$massnahmen12)
sums_mass13 <- get_sums("massnahmen13", miss$massnahmen13)
sums_mass14 <- get_sums("massnahmen14", miss$massnahmen14)
sums_mass15 <- get_sums("massnahmen15", miss$massnahmen15)
sums_mass16 <- get_sums("massnahmen16", miss$massnahmen16)
sums_mass17 <- get_sums("massnahmen17", miss$massnahmen17)

# Datensatz Massnahmen
mass_select <- df_aerzte_complete %>%
  dplyr::select(starts_with("massnahmen"))

# Datenstruktur für Abbildung
#measure <- c(rep("Consultation by appointment only", 4),
 #            rep("Parents and children wait outside the practice prior to the consultation", 4),
 #            rep("Measures to ensure distancing between families in the practice", 4),
 #            rep("Consistent enforcement of mask-wearing for parents", 4),
 #            rep("Consistent enforcement of mask-wearing for children and adolescents aged 10 years and older", 4),
 #            rep("Consistent enforcement of mask-wearing for children 6 – 9 years of age", 4),
 #            rep("Consistent enforcement of mask-wearing for children below 6 years of age", 4),
 #            rep("Consistent enforcement of mask-wearing for practice staff", 4),
 #            rep("Separate consulting hours for children with symptoms consistent with COVID-19 infection", 4),
 #            rep("Spatial separation between children with symptoms compatible with COVID-19 infection and children with other complaints", 4),
  #           rep("Additional hygiene measures for parents and children", 4),
 #            rep("Implementation of additional disinfection measures after each patient contact", 4),
 #            rep("Structural measures to protect against infection", 4),
 #            rep("Use of personal protective equipment during physical examinations of children and adolescents with symptoms of infection", 4),
 #            rep("Use of personal protective equipment when taking nasopharyngeal swabs", 4),
 #            rep("Implementing screening measures among staff", 4),
 #            rep("Regular airing of the practice rooms", 4))
#answer <- rep(c("yes", "no, as not useful", "no, but would be useful", "missing"), 17)
#count<- c(sums_mass1$Anzahl[2], sums_mass1$Anzahl[1], sums_mass1$Anzahl[3], sums_mass1$Anzahl[4],
  #    sums_mass2$Anzahl[2], sums_mass2$Anzahl[1], sums_mass2$Anzahl[3], sums_mass2$Anzahl[4],
  #    sums_mass3$Anzahl[2], sums_mass3$Anzahl[1], sums_mass3$Anzahl[3], sums_mass3$Anzahl[4],
  #    sums_mass4$Anzahl[1], 0, 0, sums_mass4$Anzahl[2],
  #    sums_mass5$Anzahl[1], 0, sums_mass5$Anzahl[2], sums_mass5$Anzahl[3],
  #    sums_mass6$Anzahl[2], sums_mass6$Anzahl[1], sums_mass6$Anzahl[3], sums_mass6$Anzahl[4],
  #    sums_mass7$Anzahl[2], sums_mass7$Anzahl[1], sums_mass7$Anzahl[3], sums_mass7$Anzahl[4],
  #    sums_mass8$Anzahl[2], sums_mass8$Anzahl[1], sums_mass8$Anzahl[3], sums_mass8$Anzahl[4],
  #    sums_mass9$Anzahl[2], sums_mass9$Anzahl[1], sums_mass9$Anzahl[3], sums_mass9$Anzahl[4],
  #    sums_mass10$Anzahl[2], sums_mass10$Anzahl[1], sums_mass10$Anzahl[3], sums_mass10$Anzahl[4],
  #    sums_mass11$Anzahl[2], sums_mass11$Anzahl[1], sums_mass11$Anzahl[3], sums_mass11$Anzahl[4],
   #   sums_mass12$Anzahl[2], sums_mass12$Anzahl[1], sums_mass12$Anzahl[3], sums_mass12$Anzahl[4],
  #    sums_mass13$Anzahl[2], sums_mass13$Anzahl[1], sums_mass13$Anzahl[3], sums_mass13$Anzahl[4],
  #    sums_mass14$Anzahl[2], sums_mass14$Anzahl[1], sums_mass14$Anzahl[3], sums_mass14$Anzahl[4],
  #    sums_mass15$Anzahl[2], sums_mass15$Anzahl[1], sums_mass15$Anzahl[3], sums_mass15$Anzahl[4],
  #    sums_mass16$Anzahl[2], sums_mass16$Anzahl[1], sums_mass16$Anzahl[3], sums_mass16$Anzahl[4],
  #    sums_mass17$Anzahl[1], 0, sums_mass17$Anzahl[2], sums_mass17$Anzahl[3])
#df_mass2 <- data.frame(measure, answer, count)
#summary(df_mass2)
#df_mass2$answer <- ordered(df_mass2$answer, levels = c("missing",
   #                                                    "no, but would be useful",
   #                                                    "no, as not useful",
   #                                                    "yes"))
#df_mass2$measure <- ordered(df_mass2$measure, levels = c(
#  "Consultation by appointment only",
#  "Parents and children wait outside the practice prior to the consultation",
#  "Measures to ensure distancing between families in the practice",
#  "Consistent enforcement of mask-wearing for parents",
#  "Consistent enforcement of mask-wearing for children and adolescents aged 10 years and older",
#  "Consistent enforcement of mask-wearing for children 6 – 9 years of age",
#  "Consistent enforcement of mask-wearing for children below 6 years of age",
#  "Consistent enforcement of mask-wearing for practice staff",
#  "Separate consulting hours for children with symptoms consistent with COVID-19 infection",
#  "Spatial separation between children with symptoms compatible with COVID-19 infection and children with other complaints",
#  "Additional hygiene measures for parents and children",
 # "Implementation of additional disinfection measures after each patient contact",
#  "Structural measures to protect against infection",
#  "Use #of personal protective equipment during physical examinations of children and adolescents with symptoms of infection",
#  "Use of personal protective equipment when taking nasopharyngeal swabs",
#  "Implementing screening measures among staff",
#  "Regular airing of the practice rooms"))

# Abbildung
#plot1 <- ggplot(df_mass2 [order(df_mass2$answer),],
#                aes(fill=answer, y=count,
#                              x = measure)) +
#  geom_bar(position="stack", stat="identity") +
#  geom_text(aes(x = measure, y = count,
#                      label = ifelse(count >= 5, count, paste("")),
#                      fill = answer),
 #                 position = position_stack(vjust = .5)) +
 # coord_flip() +
 # theme(axis.text=element_text(size = 12),
 #       legend.text = element_text(size = 12),
 #       axis.title = element_blank())
#plot1
#ggsave("figure_measures.png", width = 20, height = 10)


# Datenstruktur für Abbildung Anteile
measure2 <- c(rep("Consultation by appointment only", 3),
             rep("Parents and children wait outside the practice prior to the consultation", 3),
             rep("Measures to ensure distancing between families in the practice", 3),
             rep("Consistent enforcement of mask-wearing for parents", 3),
             rep("Consistent enforcement of mask-wearing for children and adolescents aged 10 years and older", 3),
             rep("Consistent enforcement of mask-wearing for children 6 – 9 years of age", 3),
             rep("Consistent enforcement of mask-wearing for children below 6 years of age", 3),
             rep("Consistent enforcement of mask-wearing for practice staff", 3),
             rep("Separate consulting hours for children with symptoms consistent with COVID-19 infection", 3),
             rep("Spatial separation between children with symptoms compatible with COVID-19 infection and children with other complaints", 3),
             rep("Additional hygiene measures for parents and children", 3),
             rep("Implementation of additional disinfection measures after each patient contact", 3),
             rep("Structural measures to protect against infection", 3),
             rep("Use of personal protective equipment during physical examinations of children and adolescents with symptoms of infection", 3),
             rep("Use of personal protective equipment when taking nasopharyngeal swabs", 3),
             rep("Implementing screening measures among staff", 3),
             rep("Regular airing of the practice rooms", 3))
answer2 <- rep(c("yes", "no, as not useful", "no, but would be useful"), 17)
Percentages <- c(sums_mass1$Anteil[2], sums_mass1$Anteil[1], sums_mass1$Anteil[3],
          sums_mass2$Anteil[2], sums_mass2$Anteil[1], sums_mass2$Anteil[3],
          sums_mass3$Anteil[2], sums_mass3$Anteil[1], sums_mass3$Anteil[3],
          sums_mass4$Anteil[1], 0, 0,
          sums_mass5$Anteil[1], 0, sums_mass5$Anteil[2],
          sums_mass6$Anteil[2], sums_mass6$Anteil[1], sums_mass6$Anteil[3],
          sums_mass7$Anteil[2], sums_mass7$Anteil[1], sums_mass7$Anteil[3],
          sums_mass8$Anteil[2], sums_mass8$Anteil[1], sums_mass8$Anteil[3],
          sums_mass9$Anteil[2], sums_mass9$Anteil[1], sums_mass9$Anteil[3],
          sums_mass10$Anteil[2], sums_mass10$Anteil[1], sums_mass10$Anteil[3],
          sums_mass11$Anteil[2], sums_mass11$Anteil[1], sums_mass11$Anteil[3],
          sums_mass12$Anteil[2], sums_mass12$Anteil[1], sums_mass12$Anteil[3],
          sums_mass13$Anteil[2], sums_mass13$Anteil[1], sums_mass13$Anteil[3],
          sums_mass14$Anteil[2], sums_mass14$Anteil[1], sums_mass14$Anteil[3],
          sums_mass15$Anteil[2], sums_mass15$Anteil[1], sums_mass15$Anteil[3],
          sums_mass16$Anteil[2], sums_mass16$Anteil[1], sums_mass16$Anteil[3],
          sums_mass17$Anteil[1], 0, sums_mass17$Anteil[2])

df_mass3 <- data.frame(measure2, answer2, Percentages)
df_mass3$answer2 <- ordered(df_mass3$answer2,
                            levels = c("no, but would be useful",
                                       "no, as not useful",
                                       "yes"))
df_mass3$measure2 <- ordered(df_mass3$measure2, levels = c(
  "Consultation by appointment only",
  "Parents and children wait outside the practice prior to the consultation",
  "Measures to ensure distancing between families in the practice",
  "Consistent enforcement of mask-wearing for parents",
  "Consistent enforcement of mask-wearing for children and adolescents aged 10 years and older",
  "Consistent enforcement of mask-wearing for children 6 – 9 years of age",
  "Consistent enforcement of mask-wearing for children below 6 years of age",
  "Consistent enforcement of mask-wearing for practice staff",
  "Separate consulting hours for children with symptoms consistent with COVID-19 infection",
  "Spatial separation between children with symptoms compatible with COVID-19 infection and children with other complaints",
  "Additional hygiene measures for parents and children",
  "Implementation of additional disinfection measures after each patient contact",
  "Structural measures to protect against infection",
  "Use of personal protective equipment during physical examinations of children and adolescents with symptoms of infection",
  "Use of personal protective equipment when taking nasopharyngeal swabs",
  "Implementing screening measures among staff",
  "Regular airing of the practice rooms"))

item_labels <- c(
  "Consultation by appointment only",
  "Parents and children wait outside the practice prior to \nthe consultation",
  "Measures to ensure distancing between families in the \npractice",
  "Consistent enforcement of mask-wearing for parents",
  "Consistent enforcement of mask-wearing for children and \nadolescents aged 10 years and older",
  "Consistent enforcement of mask-wearing for children 6 – 9 \nyears of age",
  "Consistent enforcement of mask-wearing for children below \n6 years of age",
  "Consistent enforcement of mask-wearing for practice staff",
  "Separate consulting hours for children with symptoms \nconsistent with COVID-19 infection",
  "Spatial separation between children with symptoms compatible \nwith COVID-19 infection and children with other complaints",
  "Additional hygiene measures for parents and children",
  "Implementation of additional disinfection measures \nafter each patient contact",
  "Structural measures to protect against infection",
  "Use of personal protective equipment during physical examinations \nof children and adolescents with symptoms of infection",
  "Use of personal protective equipment when taking \nnasopharyngeal swabs",
  "Implementing screening measures among staff",
  "Regular airing of the practice rooms")

# Abbildung
df_mass4 <- df_mass3 %>%
  group_by(measure2) %>%
  mutate(
    Percentages_pos = c(cumsum(c(0,lag(Percentages)[-1]))[-n()],94)+3
  ) %>%
  ungroup()
plot2 <- ggplot(df_mass4,
                aes(fill = answer2, y = Percentages,
                    x = measure2)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = c("maroon1", "cadetblue1", "aquamarine1")) +
  geom_text(aes(x = measure2, y = Percentages_pos,
                label = ifelse(Percentages >= 4,
                               paste0(Percentages, "%"), paste("")),
                fill = answer2),
            position = position_identity()) +
  coord_flip() +
  theme(axis.text=element_text(size = 14),
        legend.text = element_text(size = 14),
        axis.title.y =  element_blank(),
        axis.title.x = element_text(size = 14),
        legend.title = element_blank()) +
  scale_x_discrete(labels = item_labels) +
  scale_y_continuous(limits=c(0,106),
                     breaks = c(0, 20, 40, 60, 80, 100)) +
           annotate("text", label = paste0("N = ", c(N - miss$massnahmen1,
                                      N - miss$massnahmen2,
                                      N - miss$massnahmen3,
                                      N - miss$massnahmen4,
                                      N - miss$massnahmen5,
                                      N - miss$massnahmen6,
                                      N - miss$massnahmen7,
                                      N - miss$massnahmen8,
                                      N - miss$massnahmen9,
                                      N - miss$massnahmen10,
                                      N - miss$massnahmen11,
                                      N - miss$massnahmen12,
                                      N - miss$massnahmen13,
                                      N - miss$massnahmen14,
                                      N - miss$massnahmen15,
                                      N - miss$massnahmen16,
                                      N - miss$massnahmen17)),
           x = 1:17, y = 105, size = 4.5)
ggsave("figure_measures_perc.png", width = 18, height = 10)


# Unterstützung für Massnahmen benötigt?
sums_unterstuetz <- get_sums("unterstuetz", miss$unterstuetz)

