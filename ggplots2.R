library(ggplot2)
library(dplyr)

CNPq.data <- read.csv("tabula-Nomes aprovados Universal 2016 (ordem alfabetica).csv", 
                      header = FALSE)

names(CNPq.data) <- c("Nome",
                      "Faixa",
                      "Nome.Inst",
                      "Estado",
                      "Instituicao",
                      "Regiao")
CNPq.data <- CNPq.data[-which(CNPq.data$Regiao == ""), ]
CNPq.data <- CNPq.data[-which(CNPq.data$Estado == ""), ]
CNPq.data$Instituicao <- gsub(" ", "", CNPq.data$Instituicao)

cwur.data <- read.csv("cwur2016.csv",
                      header = TRUE, 
                      sep = ",",
                      stringsAsFactors = FALSE)[, 1:3]
cwur.BR <- cwur.data %>% 
  filter(Location == "Brazil") %>%
  mutate(Ranking = World.Rank, Instituicao = Institution) %>%
  select(Ranking, Instituicao)

# A bit of by-hand editing
cwur.BR$Instituicao <- c("USP", "UFRJ", "UNICAMP",
                         "UFMG", "UFRGS", "UNIFESP",
                         "UERJ", "UFSC", "UFF",
                         "UnB", "UFABC", "UFPR",
                         "UFSCAR", "UFBA", "UFPE", "UFSM")

Univs <- table(CNPq.data$Instituicao)
Univs <- data.frame(Instituicao = names(Univs),
                    Grants      = as.numeric(Univs), 
                    stringsAsFactors = FALSE)

Univs <- Univs %>%
  filter(Instituicao %in% cwur.BR$Instituicao)

mydata <- left_join(Univs, cwur.BR, "Instituicao") %>%
  select(Instituicao, Grants, Ranking) %>%
  arrange(Ranking)

p <- ggplot(data = mydata, mapping = aes(x = Ranking, 
                                         y = Grants, 
                                         label = Instituicao))
p + 
  scale_x_continuous(limits = c(1,1050), 
                     breaks = c(0, 250, 500, 750, 1000)) + 
  geom_smooth(method = "loess") + 
  geom_text(nudge_x = 30, nudge_y = 7, size = 3.5, fontface = "bold") + 
  geom_point(size = 4,
             alpha = 0.5) + 
  annotate(geom = "text", x = 1050, y = 340,
           label = "Posição da universidade no CWUR World Ranking vs.",
           hjust = "right",
           size = 6,
           fontface = "bold") +
  annotate(geom = "text", x = 1050, y = 330,
           label = "Número de projetos aprovados CNPq Universal",
           hjust = "right",
           size = 6,
           fontface = "bold") + 
  annotate(geom = "text", x = 1050, y = 310,
           label = "Fontes:",
           hjust = "right",
           size = 5.5,
           fontface = "bold") + 
  annotate(geom = "text", x = 1050, y = 298,
           label = "https://goo.gl/kxCiHL",
           hjust = "right",
           size = 5.5,
           fontface = "italic") +
  annotate(geom = "text", x = 1050, y = 286,
           label = "http://cwur.org/2016.php",
           hjust = "right",
           size = 5.5,
           fontface = "italic") + 
  annotate(geom = "text", x = 1050, y = 274,
           label = "https://git.io/v1hfI",
           hjust = "right",
           size = 5.5,
           fontface = "italic") +
  theme(axis.title = element_text(size = 20),
        axis.text  = element_text(size = 15))


