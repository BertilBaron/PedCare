# Abbildung Likert-Scale Inwiefern beeinflussen folgende Faktoren aktuell Ihren
# Alltag
# Date: 2021/06/16
# Author: NE

# Laden benötigte Pakete
library(tidyverse)
library(ggplot2)
library(likert)
#library(gridExtra)
#library(reshape2)


# Laden der Daten
load("Arztumfrage.rda")

df_aerzte <- df_aerzte %>% 
  filter(lastpage >= 8)

# Datensatz Item-Batterie Alltag
data_alltag <- df_aerzte %>% 
  dplyr::select(matches("alltag"))
#data_alltag <- as.data.frame(data_alltag)
# Antwortkategorien ins Englische
data_alltag <- data_alltag %>% 
  mutate_at(vars(tidyselect::starts_with("alltag")),
            funs(case_when(. == "sehr stark" ~ "very much",
                           . == "ziemlich stark" ~ "considerably",
                           . == "eher schwach" ~ "not so much",
                           . == "überhaupt nicht" ~ "not at all"))) %>% 
  mutate_if(is.character, as.factor)
# richtige Reihenfolge factors
data_alltag <- as.data.frame(lapply(data_alltag, function(X){ordered(X,
                                                          levels = c("not at all",
                                                                     "not so much",
                                                                     "considerably",
                                                                     "very much"))}))
# Variablennamen ins Englische
data_alltag <- data_alltag %>% 
  rename("Staff shortage due to sick leave" = alltag1,
         "Staff shortage due to quarantine of employees" = alltag2,
         "Staff shortage due to preventive measures" = alltag3,
         "Staff shortage due to obligations caring for children or relatives" = 
           alltag4,
         "Higher administrative workload during the COVID-19 pandemic" = alltag5,
         "Time-consuming infection control measures" = alltag6,
         "Costly infection control measures" = alltag7,
         "More patients than at the same time last year" = alltag8,
         "Fewer patients than at the same time last year" = alltag9)
str(data_alltag)
summary(data_alltag)
# Likert Results
result_alltag = likert(data_alltag)

# Abbildung Likert Scale
#plot_likert <- likert.bar.plot(result_alltag, center = 2.5, text.size = 4.5,
 #                              legend = "",
  #                             colors = c("maroon4", "maroon1", 
  #                                        "aquamarine1", "aquamarine4")) +
  #theme_light() +
  #theme(axis.text = element_text(size = 14),
   #     axis.title.y = element_text(size = 12),
   #     legend.text = element_text(size = 14),
   #     legend.position = "bottom",
    #    line = element_blank()) +
 # scale_y_continuous(limits=c(-100,150),
          #           breaks = seq(-100, 100, by = 10)
          #           ) +
 # annotate("text", label = c("N = 289", "N = 308", "N = 307", "N = 309",
      #                       "N = 309", "N = 309", "N = 307", "N = 306",
      #                       "N = 306"), 
       #    x = 1:9, y = 145, size = 4.5)
#ggsave("figure_alltag.png", width = 20, height = 10)

# Abbildung Likert Scale Alternative
data_alltag_alternative <- data_alltag %>% 
  rename("Staff shortage due to sick leave (N = 309)" = alltag1,
         "Staff shortage due to quarantine of employees (N = 307)" = alltag2,
         "Staff shortage due to preventive measures (N = 308)" = alltag3,
         "Staff shortage due to obligations caring for children or relatives (N = 309)" = 
           alltag4,
         "Higher administrative workload during the COVID-19 pandemic (N =307)" = alltag5,
         "Time-consuming infection control measures (N = 306)" = alltag6,
         "Costly infection control measures (N = 309)" = alltag7,
         "More patients than at the same time last year (N = 289)" = alltag8,
         "Fewer patients than at the same time last year (N = 306)" = alltag9)
str(data_alltag_alternative)
summary(data_alltag_alternative)
result_alltag_alternative = likert(data_alltag_alternative)
item_labels <- c(
  "More patients than at the same time last \nyear (N = 289)",
  "Staff shortage due to preventive measures \n(N = 308)",
  "Staff shortage due to quarantine of \nemployees (N = 307)",
  "Staff shortage due to sick leave (N = 309)",
  "Staff shortage due to obligations caring \nfor children or relatives (N = 309)",
  "Costly infection control measures (N = 309)",
  "Higher administrative workload during the \nCOVID-19 pandemic (N =307)",
  "Time-consuming infection control measures \n(N = 306)",
  "Fewer patients than at the same time last \nyear (N = 306)"
)
plot_likert <- likert.bar.plot(result_alltag_alternative, 
                               center = 2.5, 
                               text.size = 4.5,
                               legend = "",
                               colors = c("maroon4", "maroon1", 
                                          "aquamarine1", "aquamarine4")) +
  theme_light() +
  theme(axis.text = element_text(size = 14),
        axis.title.y = element_text(size = 12),
        legend.text = element_text(size = 14),
        legend.position = "bottom") +
  scale_x_discrete(labels = item_labels) 
  #scale_y_continuous(limits = c(100, 80, 60, 40, 20, 0, 20, 40, 60, 80, 100))
ggsave("figure_alltag_alternative.png", width = 20, height = 10)

