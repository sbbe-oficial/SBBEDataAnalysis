### The BEGINNING ~~~~~
##
# Analyses and plots SBBE data | Written by George Pacheco ~


# Cleans environment ~ 
rm(list=ls())


# Sets working directory ~
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# Loads packages ~
pacman::p_load(tidyverse, ggnewscale, ggtext, ggstar, ggrepel, shadowtext, readxl, writexl, cowplot, patchwork, ggpubr, lemon, reshape2, writexl, stringr, lubridate,
               geobr, ggspatial, showtext, png, extrafont, sf, ggiraphExtra, fontawesome, shiny, DT,
               rvest, stringr, purrr, tibble, dplyr, extrafont, emojifont, grid, rsvg, ggimage, rnaturalearthdata, rnaturalearth, ggh4x)


# Loads extra fonts ~
loadfonts()
font_add_google("Cormorant Garamond", "Cormorant")
showtext_auto()


# Loads data ~
fulldf <- read.csv("./SBBELists/SBBEmembers--28Sep26.csv", header = TRUE, stringsAsFactors = FALSE, sep = ",")


# Loads data ~
extra <- read_excel("./SBBELists/SBBE24_InscriçõesExtraordinárias_R.xlsx")
normal <- read_excel("./SBBELists/ListaParticipante_18-12-2024_11-47-55.xlsx") %>%
          dplyr::filter(Inscrição == "Aprovado") %>%
          dplyr::filter(Categoria != "Curso Galaxy - Apenas para inscritos no congresso") %>%
          dplyr::filter(!ID %in% extra$ID) %>%
          dplyr::rename(Sub_Area = `Sub Área`)


ListSBBE26 <- read_excel("./SBBELists/ListaParticipante_06-07-2026_10-52-00.xlsx") %>%
              dplyr::filter(Inscrição == "Aprovado") %>%
              dplyr::mutate(Conference = "SBBE26",
                            `Nome Crachá` = NA,
                            `Instituição` = NA,
                            `Gênero` = NA,
                            Sub_Area = NA)  %>%
              dplyr::select(Conference, Nome, ID, Categoria, `Nome Crachá`, UF, Cidade, `Instituição`, `Gênero`)


# Merges data frames ~
Attendees_df <- rbind(extra, normal)
Attendees_df <- Attendees_df %>%
                dplyr::mutate(Conference = "SBBE24") %>%
                dplyr::select(Conference, Nome, ID, Categoria, `Nome Crachá`, UF, Cidade, `Instituição`, `Gênero`)


# Merges conferences data frames ~
Attendees_df <- bind_rows(Attendees_df, ListSBBE26)


# Gets Stage ~
fulldf$Stage <- ifelse(grepl("Profissional", fulldf$Labels), "Profissional",
                ifelse(grepl("Pos-Graduacao", fulldf$Labels), "Pós-graduação",
                ifelse(grepl("Graduacao", fulldf$Labels), "Graduação", NA)))


fulldfUp <- fulldf %>%
            dplyr::select(Name, Stage, Gender, State, Institution) %>%
            mutate(Data = "Members",
                   Conference = NA) %>%
            mutate(`Nome Crachá` = "") %>%
            mutate(ID = "")


Attendees_dfUp <- Attendees_df %>%
                  dplyr::rename(Name = Nome, State = UF, Institution = `Instituição`, Name = Nome, Gender = `Gênero`, Stage = Categoria) %>%
                  dplyr::select(Conference, Name, Stage, Gender, State, Institution, `Nome Crachá`, ID) %>%
                  mutate(Data = "Attendees")



# Combines data frames ~
fulldfUltra <- rbind(fulldfUp, Attendees_dfUp)


# Corrects Nome Crachá ~
fulldfUltra$`Nome Crachá` <- ifelse(fulldfUltra$Name %in% c("Fernanda de Pinho Werneck"), "Fernanda Werneck", fulldfUltra$`Nome Crachá`)
fulldfUltra$`Nome Crachá` <- ifelse(fulldfUltra$Name %in% c("Jose Alexandre Felizola Diniz Filho"), "Jose Diniz-Filho", fulldfUltra$`Nome Crachá`)
fulldfUltra$`Nome Crachá` <- ifelse(fulldfUltra$Name %in% c("Clarisse Palma da Silva"), "Clarisse Palma-Silva", fulldfUltra$`Nome Crachá`)
fulldfUltra$`Nome Crachá` <- ifelse(fulldfUltra$Name %in% c("Andrea Pedrosa Harand"), "Andrea Pedrosa Harand", fulldfUltra$`Nome Crachá`)
fulldfUltra$`Nome Crachá` <- ifelse(fulldfUltra$Name %in% c("Prof. Dr. Frederico Henning"), "Frederico Henning", fulldfUltra$`Nome Crachá`)
fulldfUltra$`Nome Crachá` <- ifelse(fulldfUltra$Name %in% c("Eduardo Tarazona"), "Eduardo Tarazona", fulldfUltra$`Nome Crachá`)
fulldfUltra$`Nome Crachá` <- ifelse(fulldfUltra$Name %in% c("Fabricio Santos"), "Fabrício R. Santos", fulldfUltra$`Nome Crachá`)


# Corrects Nome Crachá ~
fulldfUltra$Stage <- ifelse(fulldfUltra$Name %in% c("Thomaz Pinotti"), "Mestrando, Doutorando e Pós doutorando", fulldfUltra$Stage)
fulldfUltra$State <- ifelse(fulldfUltra$Name %in% c("Jeferson Duran Fuentes"), "SP", fulldfUltra$State)


# Corrects UF ~
levels(fulldfUltra$State <- gsub("Rio Grande do Sul", "RS", fulldfUltra$State))
levels(fulldfUltra$State <- gsub("Goias", "GO", fulldfUltra$State))
levels(fulldfUltra$State <- gsub("Iowa", "SP", fulldfUltra$State))


# Corrects UF speakers ~
fulldfUltra$State <- ifelse(fulldfUltra$ID %in% c("12821302"), "RS",
                     ifelse(fulldfUltra$ID %in% c("9314289"), "PR",
                     ifelse(fulldfUltra$ID %in% c("9176759"), "SP",
                     ifelse(fulldfUltra$ID %in% c("16590615"), "MG", fulldfUltra$State))))



# Corrects UF speakers ~
fulldfUltra$State <- ifelse(fulldfUltra$`Nome Crachá` %in% c("ALENA MAYO INIGUEZ", "Frederico Henning"), "RJ",
                     ifelse(fulldfUltra$`Nome Crachá` %in% c("Fernanda Werneck"), "AM",
                     ifelse(fulldfUltra$`Nome Crachá` %in% c("Eduardo Tarazona", "Fabrício Santos", "Luiz Bem"), "MG",
                     ifelse(fulldfUltra$`Nome Crachá` %in% c("Ana Tourinho"), "MT",
                     ifelse(fulldfUltra$`Nome Crachá` %in% c("Jose Diniz-Filho"), "GO",
                     ifelse(fulldfUltra$`Nome Crachá` %in% c("Andrea Pedrosa Harand"), "PE",
                     ifelse(fulldfUltra$`Nome Crachá` %in% c("Maria Yamamoto", "Felipe de Oliveira Torquato"), "RN",
                     ifelse(fulldfUltra$`Nome Crachá` %in% c("Clarisse Palma-Silva", "Nelio Bizzo", "Mario Pinna", "Tábita Hünemeier", "Tiago Quental"), "SP",
                     ifelse(fulldfUltra$`Nome Crachá` %in% c("waldemir rosa"), "PR",
                     ifelse(fulldfUltra$`Nome Crachá` %in% c("Kateryna Makov", "Kelly Zamudio", "Santiago Vieyra", "Thomaz Pinotti"), "Estrangeiro", fulldfUltra$State))))))))))


# Corrects State ~
fulldfUltra$State <- ifelse(fulldfUltra$Name %in% c("Alex Anderson Antony Bandeira de Carvalho",
                                                    "Ana Flávia Konig Braz",
                                                    "Bruna Debas Brito",
                                                    "Deborah Miho Chinen Toyomoto",
                                                    "Eduarda Maria de Melo de Faria",
                                                    "Gabriele Tomaz Vieira",
                                                    "George Pacheco",
                                                    "Gustavo Henrique Lopes",
                                                    "Lucas Rafael Cabral Jara",
                                                    "Luiza Teixeira da Silva",
                                                    "Maria Clara Andrade dos Santos",
                                                    "Melissa Vitória Flórido",
                                                    "Rafael Sartori Lam",
                                                    "Ramon Trindade Urbano",
                                                    "rebeca Taborda Ribas Matos",
                                                    "Sofia Demétrio Nicolau",
                                                    "Waldir Miron",
                                                    "Camila Junqueira Mazzoni",
                                                    "Ana Carolina Carnaval",
                                                    "Karina Lucas da Silva-Brandão",
                                                    "Marcelo Gehara",
                                                    "Paula X. Kover"), "Exterior", fulldfUltra$State)


# Corrects State ~
fulldfUltra <- fulldfUltra %>%
               mutate(State = case_when(Conference == "SBBE24" &
                                       (State %in% c("Aberdeen City", "california", "Florida", "Misiones", "Texas", "Córdoba", "Provincia de Buenos Aires",
                                                     "Ontario", "Capital Federal", "La Libertad", "New York") | is.na(State)) ~ "Exterior", TRUE ~ State))


# Corrects Category ~
levels(fulldfUltra$Stage <- sub("CBBE", "SBBE24", fulldfUltra$Stage))


# Converts Nome Crachá to lowercase ~
fulldfUltra$Badge <- tolower(fulldfUltra$`Nome Crachá`)


# Orders df based on Badge ~
fulldfUltra <- fulldfUltra[order(fulldfUltra$Badge), ]


# Gets counts of occurrences ~
NameCounts <- table(fulldfUltra$Badge)


# Prints repeated entries ~
NameCounts[NameCounts > 1]


# Defines entries to exclude ~
RowsToKeep <- !((grepl("alessandra p. lamarca|carlos guerra schrago|clarisse palma-silva|fabrício r. santos|fabricius domingos|fernando sotero|iris hass|nelio bizzo", fulldfUltra$Badge) & fulldfUltra$Stage == "Profissional") |
               (grepl("meari caldeira", fulldfUltra$Badge) & fulldfUltra$Stage == "Mestrando, Doutorando e Pós doutorando") |
               (grepl("lucca fanti", fulldfUltra$Badge) & fulldfUltra$Stage == "Estudante de Graduação"))


# Excludes repeated entries ~
fulldfUltra <- fulldfUltra[RowsToKeep, ]


# Corrects Categoria for repeated entries  ~
fulldfUltra$Stage <- ifelse(fulldfUltra$Badge %in% c("alessandra p. lamarca|carlos guerra schrago|clarisse palma-silva|fabrício r. santos|fabricius domingos|fernando sotero|iris hass|nelio bizzo"), "Profissional + Membro fundador da SBBE", fulldfUltra$Stage)
fulldfUltra$Stage <- ifelse(fulldfUltra$Badge %in% c("meari caldeira"), "Mestrando, Doutorando e Pós doutorando + Membro fundador da SBBE", fulldfUltra$Stage)
fulldfUltra$Stage <- ifelse(fulldfUltra$Badge %in% c("lucca fanti"), "Estudante de Graduação + Membro fundador da SBBE", fulldfUltra$Stage)


# Corrects Institution & State for better visualisation ~
levels(fulldfUltra$Institution <- gsub("Universidade|Universidad|University", "Uni.", fulldfUltra$Institution))
levels(fulldfUltra$Institution <- gsub("Instituto|Institute", "Inst.", fulldfUltra$Institution))
levels(fulldfUltra$State <- gsub("Estrangeiro", "Exterior", fulldfUltra$State))


# Corrects Gender ~
fulldfUltra$Gender <- ifelse(fulldfUltra$Gender %in% c("M", "Masculino"), "Masculino",
                      ifelse(fulldfUltra$Gender %in% c("F", "Feminino"), "Feminino",
                      ifelse(fulldfUltra$Gender %in% c("O", "Outro"), "Outro", "Error")))


# Defines the custom capitalization function ~
capitalize_words <- function(text) {
                    words <- str_split(text, " ")[[1]]
                    exclude_patterns <- c("of", "de", "da", "do", "ABC", "Não-binário", "and", "(EUA)", "del-Rei", "UNIFATECIE")
                    patterns_map <- setNames(exclude_patterns, tolower(exclude_patterns))
                    words <- sapply(words, function(word) {
                    word_lower <- tolower(word)
                    if (word_lower %in% names(patterns_map)) {
                    patterns_map[[word_lower]]}
                    else {str_to_title(word)}})
                    str_c(words, collapse = " ")}


# Apply the function to the pattern column
fulldfUltra$Name <- sapply(fulldfUltra$Name , capitalize_words)
fulldfUltra$Institution <- sapply(fulldfUltra$Institution , capitalize_words)
fulldfUltra$Gender <- sapply(fulldfUltra$Gender, capitalize_words)


# Sets all Brazilian states ~ 
AllBRLStates <- c("AC", "AP", "AM", "PA", "RO", "RR", "TO",
                  "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE", "AL",
                  "GO", "MT", "MS", "DF",
                  "ES", "MG", "RJ", "SP",
                  "PR", "RS", "SC", "Exterior")


# Sets all Brazilian regions ~ 
AllBRLRegions <- c("Norte", "Nordeste", "Centro-Oeste", "Sudeste", "Sul", "Exterior")


# Defines a common set of levels and ordering for Variable ~
variable_levels <- c("AC", "AP", "AM", "PA", "RO", "RR", "TO",
                     "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE", "AL",
                     "GO", "MT", "MS", "DF",
                     "ES", "MG", "RJ", "SP",
                     "PR", "RS", "SC",
                     "Profissional",
                     "Pós-graduação",
                     "Graduação",
                     "Dias até o SBBE24",
                     "Seguidores no Bluesky",
                     "Seguidores no X",
                     "Seguidores no Instagram",
                     "Instituições representadas na SBBE",
                     "Afiliados à SBBE",
                     "Novartis",
                     "Okinawa Inst. of Science and Technology",
                     "École Polytechnique Fédérale de Lausanne", 
                     "Uni. of Ottawa",
                     "Pennsylvania State Uni.",
                     "Southwestern Oklahoma State Uni.",
                     "The City Uni. of New York",
                     "Towson Uni.",
                     "Southwest Uni.",
                     "Uni. of Texas",
                     "Texas A&M Uni.",
                     "Uni. of California — Los Angeles",
                     "Rice Uni.",
                     "Uni. of West Florida",
                     "Uni. of Aberdeen",
                     "Uni. of Oslo",
                     "Uni. of Gothenburg",
                     "Uni. of Copenhagen",
                     "Leibniz Inst. For Zoo and Wildlife Research",
                     "Uni. Nacional de Misiones",
                     "Uni. Nacional de Colombia", 
                     "Uni. Nacional de Trujillo",
                     "Inst. de Biología Subtropical",
                     "Inst. Multidisciplinario de Biología Vegetal",
                     "Uni. Nacional de Córdoba",
                     "Uni. de Buenos Aires",
                     "Uni. Federal de Santa Maria",
                     "Uni. Federal do Rio Grande",
                     "Uni. Federal Rural do Semi-Árido",
                     "Uni. Federal de Pelotas",
                     "Uni. Federal de Ciências da Saúde de Porto Alegre",
                     "Uni. Federal do Rio Grande do Sul",
                     "Uni. Federal da Fronteira Sul",
                     "Pontifícia Uni. Católica do Rio Grande do Sul",
                     "Uni. Federal da Integração Latino-Americana",
                     "Uni. Estadual de Maringá",
                     "Centro Universitário Claretiano",
                     "Uningá",
                     "Museu de História Natural Capão da Imbuia",
                     "Inst. Carlos Chagas — Fiocruz Paraná",
                     "Centro Universitário UNIFATECIE",
                     "Uni. Estadual de Ponta Grossa",
                     "Uni. Estadual do Centro-Oeste",
                     "Secretaria de Educação do Estado do Paraná",
                     "Inst. Federal — Paraná",
                     "Uni. Tecnológica Federal do Paraná",
                     "Pontifícia Uni. Católica do Paraná",
                     "Uni. Federal do Paraná",
                     "Hospital Regional Hans Dieter Schmidt",
                     "Centro Universitário Leonardo da Vinci",
                     "Uni. da Região de Joinville",
                     "Uni. Federal de Santa Catarina",
                     "Uni. do Vale do Paraíba",
                     "Inst. Butantan",
                     "Uni. do Vale do Itajaí",
                     "Uni. Santo Amaro",
                     "Uni. do Oeste Paulista",
                     "Uni. de Mogi Das Cruzes",
                     "Uni. Federal de São Carlos",
                     "Uni. Federal do ABC",
                     "Uni. Federal de São Paulo",
                     "Uni. de São Paulo",
                     "Inst. Biológico — São Paulo", 
                     "Uni. Estadual de Campinas",
                     "Uni. Estadual Paulista",
                     "Unisãojosé",
                     "Uni. Estadual do Norte Fluminense Darcy Ribeiro",
                     "Uni. Federal Fluminense",
                     "Inst. Oswaldo Cruz",
                     "Uni. Federal do Rio de Janeiro",
                     "Uni. Federal do Estado do Rio de Janeiro",
                     "Uni. Estadual do Norte Fluminense", 
                     "Fundação Oswaldo Cruz — Rio de Janeiro",
                     "Fundação Oswaldo Cruz — Amazônia",
                     "Jardim Botânico do Rio de Janeiro",
                     "Museu Nacional",
                     "Uni. do Estado do Rio de Janeiro",
                     "Centro Universitário Serra Dos Órgãos",
                     "Uni. Federal do Espírito Santo",
                     "Inst. Nacional da Mata Atlântica",
                     "Colégio Águia de Prata",
                     "Pontifícia Uni. Católica de Minas Gerais",
                     "Uni. Federal de Uberlândia",
                     "Uni. Federal de Itajubá",
                     "Uni. Federal do Triângulo Mineiro",
                     "Uni. Federal de Juiz de Fora",
                     "Centro Universitário de Patos de Minas",
                     "Uni. do Estado de Minas Gerais",
                     "Uni. Federal de Minas Gerais",
                     "Uni. Federal de Lavras",
                     "Uni. Federal de Viçosa", 
                     "Uni. Federal de São João del-Rei",
                     "Uni. Federal de Jataí",
                     "Empresa Brasileira de Pesquisa Agropecuária",
                     "Uni. de Brasília",
                     "Uni. Federal de Goiás",
                     "Uni. Estadual de Goiás",
                     "Uni. do Estado de Mato Grosso",
                     "Uni. Federal de Mato Grosso",
                     "Uni. Federal de Mato Grosso do Sul",
                     "Uni. Federal da Grande Dourados",
                     "Uni. Estadual de Santa Cruz",
                     "Uni. Federal da Bahia",
                     "Uni. Estadual de Feira de Santana",
                     "Uni. Federal do Recôncavo da Bahia",
                     "Uni. Estadual do Sudoeste da Bahia",
                     "Uni. Federal do Maranhão",
                     "Uni. Federal de Sergipe",
                     "Uni. Federal de Alagoas",
                     "Uni. Federal do Vale do São Francisco",
                     "Uni. de Pernambuco",
                     "Inst. Aggeu Magalhães — Fiocruz Pernambuco",
                     "Inst. Federal — Pernambuco",
                     "Inst. Federal — do Sul de Minas",
                     "Uni. Federal de Pernambuco",
                     "Uni. Federal Rural de Pernambuco",
                     "Uni. Estadual da Paraíba",
                     "Secretaria Municipal de Educação — Paraíba",
                     "Uni. Federal da Paraíba",
                     "Secretaria Municipal de Educação",
                     "Uni. Federal do Rio Grande do Norte",
                     "Museu Paraense Emílio Goeldi",
                     "Uni. Federal do Amapá",
                     "Uni. Federal do Pará",
                     "Inst. Nacional de Pesquisas da Amazônia",
                     "Uni. Federal de Roraima",
                     "Uni. Federal do Amazonas",
                     "Inst. Tecnológico Vale",
                     "Feminino",
                     "Masculino",
                     "Outro",
                     "Exterior",
                     "Sul", 
                     "Sudeste",
                     "Centro-Oeste",
                     "Nordeste",
                     "Norte")


# Checks missing Institution ~
setdiff(fulldfUltra$Institution[fulldfUltra$Data == "Members"], variable_levels)


# Expands fulldf by creating Region ~
fulldfUltra$Region <- ifelse(fulldfUltra$State %in% c("AC", "AM", "AP", "PA", "RR", "RO", "TO"), "Norte",
                      ifelse(fulldfUltra$State %in% c("MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA"), "Nordeste",
                      ifelse(fulldfUltra$State %in% c("DF", "GO", "MT", "MS"), "Centro-Oeste",
                      ifelse(fulldfUltra$State %in% c("MG", "ES", "RJ", "SP"), "Sudeste",
                      ifelse(fulldfUltra$State %in% c("PR", "RS", "SC"), "Sul",
                      ifelse(fulldfUltra$State %in% c("Exterior"), "Exterior", "Error"))))))


# Gets general numbers ~
fulldf_Descriptive <- subset(fulldfUltra, Data == "Members") %>%
                      summarise("Afiliados à SBBE" = n_distinct(Name),
                                "Instituições representadas na SBBE" = n_distinct(Institution)) %>%
                      mutate(Stats = "General") %>%
                      pivot_longer(cols = -Stats, names_to = "Variable", values_to = "Percentage") %>%
                      mutate(n = 0) %>%
                      relocate(n, .before = Percentage) %>%
                      relocate(Stats, .after = Percentage) %>%
                      mutate(Variable = factor(Variable, levels = variable_levels, ordered = TRUE))


# Gets percentage for Institution ~
fulldf_StageMembersPerc <- subset(fulldfUltra, Data == "Members") %>%
                           dplyr::count(Stage) %>%
                           dplyr::mutate(Percentage = n / sum(n)) %>%
                           dplyr::rename(Variable = Stage) %>%
                           dplyr::mutate(Stats = "StageMembers") %>%
                           dplyr::mutate(Variable = factor(Variable, levels = variable_levels, ordered = TRUE))


# Create a data frame with counts and proportions per institution ~
fulldf_StateMembersPerc <- subset(fulldfUltra, Data == "Members") %>%
                           dplyr::count(State) %>%
                           dplyr::mutate(Percentage = n / sum(n)) %>%
                           dplyr::rename(Variable = State) %>%
                           complete(Variable = AllBRLStates, fill = list(n = 0, Percentage = 0)) %>%
                           dplyr::mutate(Variable = factor(Variable, levels = variable_levels, ordered = TRUE)) %>%
                           dplyr::mutate(Stats = "StateMembers")


# Create a data frame with counts and proportions per institution ~
fulldf_RegionMembersPerc <- subset(fulldfUltra, Data == "Members") %>%
                            dplyr::filter(Region != "Error") %>%
                            dplyr::count(Region) %>%
                            dplyr::mutate(Percentage = n / sum(n)) %>%
                            dplyr::rename(Variable = Region) %>%
                            complete(Variable = AllBRLRegions, fill = list(n = 0, Percentage = 0)) %>%
                            dplyr::mutate(Variable = factor(Variable, levels = variable_levels, ordered = TRUE)) %>%
                            dplyr::mutate(Stats = "RegionMembers")


# Create a data frame with counts and proportions per institution ~
fulldf_StateAttendeesPerc <- subset(fulldfUltra, Data == "Attendees") %>%
                             dplyr::count(Conference, State) %>%
                             dplyr::group_by(Conference) %>%
                             dplyr::mutate(Percentage = n / sum(n)) %>%
                             dplyr::rename(Variable = State) %>%
                             tidyr::complete(Variable = AllBRLStates, fill = list(n = 0, Percentage = 0)) %>%
                             dplyr::mutate(Variable = factor(Variable, levels = variable_levels, ordered = TRUE)) %>%
                             dplyr::mutate(Stats = "StateAttendees") %>%
                             dplyr::ungroup()


# Create a data frame with counts and proportions per institution ~
fulldf_RegionAttendeesPerc <- subset(fulldfUltra, Data == "Attendees") %>%
                                     dplyr::filter(Region != "Error") %>%
                                     dplyr::count(Conference, Region) %>%
                                     dplyr::group_by(Conference) %>%
                                     dplyr::mutate(Percentage = n / sum(n)) %>%
                                     dplyr::rename(Variable = Region) %>%
                                     complete(Variable = AllBRLRegions, fill = list(n = 0, Percentage = 0)) %>%
                                     dplyr::mutate(Variable = factor(Variable, levels = variable_levels, ordered = TRUE)) %>%
                                     dplyr::mutate(Stats = "RegionAttendees")

# Slight edit ~
fulldfUltra <- fulldfUltra %>% dplyr::mutate(Stage = gsub(" \\+ Membro fundador da SBBE", "", Stage))


# Gets percentage for Gender ~
fulldf_GenderMembersPerc <- subset(fulldfUltra, Data == "Members") %>%
                                   dplyr::filter(Region != "Error") %>%
                                   dplyr::count(Gender) %>%
                                   dplyr::mutate(Percentage = n / sum(n)) %>%
                                   dplyr::rename(Variable = Gender) %>%
                                   dplyr::mutate(Variable = factor(Variable, levels = variable_levels, ordered = TRUE)) %>%
                                   dplyr::mutate(Stats = "GenderMembers")


# Gets percentage for Institutions ~
fulldf_InstitutionMembersPerc <- subset(fulldfUltra, Data == "Members") %>%
                                        dplyr::filter(Institution != "") %>%
                                        dplyr::count(Institution, Region) %>%
                                        dplyr::mutate(Percentage = n / sum(n)) %>%
                                        dplyr::rename(Variable = Institution) %>%
                                        dplyr::mutate(Stats = "InstitutionMembers") %>%
                                        dplyr::mutate(Variable = factor(Variable, levels = variable_levels, ordered = TRUE))


# Combine the data frames ~  
fulldfPlots <- bind_rows(fulldf_StageMembersPerc,
                         fulldf_StateMembersPerc,
                         fulldf_RegionMembersPerc,
                         fulldf_GenderMembersPerc,
                         fulldf_InstitutionMembersPerc)


# Expands fulldfUp by creating BarFill ~
fulldfPlots$BarFill <- ifelse(str_detect(fulldfPlots$Stats, "Members"), "#006837",
                       ifelse(str_detect(fulldfPlots$Stats, "Attendees"), "#41ab5d", "#fbb4ae"))


# Reorders Population ~
fulldfPlots$Stats <- factor(fulldfPlots$Stats, ordered = T,
                            levels = c("StageMembers",
                                       "StateMembers",
                                       "RegionMembers",
                                       "GenderMembers",
                                       "InstitutionMembers"))


# Load geom data ~
BRL_Regions <- read_region(simplified = TRUE, year = 2019)
BRL_States <- read_state(code_state = "all", simplified = TRUE, year = 2019)


# Corrects entries ~
levels(BRL_Regions$name_region <- gsub("Centro Oeste", "Centro-Oeste", BRL_Regions$name_region))
levels(BRL_States$name_region <- gsub("Centro Oeste", "Centro-Oeste", BRL_States$name_region))


# Expands BRL_Regions by creating Region ~
BRL_Regions$Region <- ifelse(BRL_Regions$name_region %in% c("Norte"), "North",
                      ifelse(BRL_Regions$name_region %in% c("Nordeste"), "Northeast",
                      ifelse(BRL_Regions$name_region %in% c("Centro-Oeste"), "Central-West",
                      ifelse(BRL_Regions$name_region %in% c("Sudeste"), "Southeast",
                      ifelse(BRL_Regions$name_region %in% c("Sul"), "South", "Error")))))


# Expands BRL_Regions by adding the SBBE24 & Abroad rows ~
BRL_Regions <- add_row(BRL_Regions, name_region = "SBBE24", Region = "SBBE24")
BRL_Regions <- add_row(BRL_Regions, name_region = "SBBE26", Region = "SBBE26")
BRL_Regions <- add_row(BRL_Regions, name_region = "Exterior", Region = "Exterior")
BRL_States <- add_row(BRL_States, abbrev_state = "Exterior", name_region = "Exterior")


# Creates a data frame with the centroids of the Brazilian regions ~
BRL_Regions_Centroids_df <- data.frame(Region = c("North", "Northeast", "Central-West", "Southeast", "South", "SBBE24", "SBBE26", "Exterior", "SP"),
                                       Longitude = c(-58, -41.25, -53.15, -47.60, -51.2, -49.271111, -43.964837, -65, -48.62),
                                       Latitude = c(-3.5, -8, -15.5, -21, -27.5, -25.429722, -19.872538, -25, -21.9))


# Merges data frame to perform the change ~
BRL_Regions <- left_join(BRL_Regions, BRL_Regions_Centroids_df, by = "Region")


# Reduces data ~ 
BRL_Regions <- BRL_Regions %>%
               dplyr::select(-code_region)
BRL_States <- BRL_States %>%
              dplyr::select(-c(code_state, code_region, name_state))


# Renames columns ~
BRL_States <- BRL_States %>%
              rename(Variable = abbrev_state)


# Joins data frames ~
fulldf_RegionMembersPerc <- fulldf_RegionMembersPerc %>%
                            rename(name_region = Variable)
fulldf_RegionAttendeesPerc <- fulldf_RegionAttendeesPerc %>%
                              rename(name_region = Variable)


# Merges data frames individually ~
RegionMembers_df <- BRL_States %>%
                    inner_join(fulldf_RegionMembersPerc, by = "name_region") %>%
                    mutate(Stats = "Members")  %>%
                    mutate(Division = "Per Region")
StateMembers_df <- BRL_States %>%
                   inner_join(fulldf_StateMembersPerc, by = "Variable") %>%
                   mutate(Stats = "Members") %>%
                   mutate(Division = "Per State")
RegionAttendees_df <- BRL_States %>%
                      inner_join(fulldf_RegionAttendeesPerc, by = "name_region", relationship = "many-to-many") %>%
                      mutate(Stats = "Attendees") %>%
                      mutate(Division = "Per Region")
StateAttendees_df <- BRL_States %>%
                     inner_join(fulldf_StateAttendeesPerc, by = "Variable") %>%
                     mutate(Stats = "Attendees") %>%
                     mutate(Division = "Per State")


# Combines data frames ~
combined_sfs <- bind_rows(StateAttendees_df, StateMembers_df, RegionAttendees_df, RegionMembers_df)


# Expands combined_sfs by adding the SBBE24 row ~
combined_sfs <- add_row(combined_sfs, name_region = "SBBE24", Division = "Per Region", Stats = "Members")
combined_sfs <- add_row(combined_sfs, name_region = "SBBE26", Division = "Per Region", Stats = "Members")
combined_sfs <- add_row(combined_sfs, name_region = "SBBE24", Division = "Per Region", Stats = "Attendees")
combined_sfs <- add_row(combined_sfs, name_region = "SBBE26", Division = "Per Region", Stats = "Attendees")


# Converts to data frames ~
combined_dfs <- as.data.frame(combined_sfs)
BRL_Regions_df <- as.data.frame(BRL_Regions)


# Merges data frame to perform the change ~
merged_dfs <- left_join(combined_dfs, BRL_Regions_df, by = "name_region", suffix = c("", ".BRL"))


# Performs the change ~
resulting_dfs <- merged_dfs %>%
                 mutate(geometry = ifelse(Division == "Per Region", geometry.BRL, geometry))


# Eliminates unnecessary column ~
resulting_dfs <- resulting_dfs %>% 
                 dplyr::select(-c(n, geometry.BRL))


# Converts data frame back to sf ~
fulldf_map <- st_as_sf(resulting_dfs, crs = st_crs(combined_sfs))


# Renames columns ~
BRL_Regions_Centroids_df <- BRL_Regions_Centroids_df %>%
                            rename(Variable = Region)


# Converts to data frames ~
combined_dfs <- as.data.frame(combined_sfs)
BRL_Regions_df <- as.data.frame(BRL_Regions)


# Merges data frame to perform the change ~
fulldf_map <- left_join(fulldf_map, BRL_Regions_Centroids_df, by = "Variable", suffix = c("", ".SP"))


# Reorders Population ~
fulldf_map$Stats <- factor(fulldf_map$Stats, ordered = TRUE,
                           levels = c("Members",
                                      "Attendees"))


# Reorders Division ~
fulldf_map$Division <- factor(fulldf_map$Division, ordered = T,
                              levels = c("Per Region",
                                         "Per State"))


# Creates the Circular data frame ~
Circular <- subset(fulldfPlots, Stats == "InstitutionMembers") %>% arrange(desc(Percentage))


# Reorders Population ~
Circular$Region <- factor(Circular$Region, ordered = TRUE,
                          levels = c("Norte",
                                     "Nordeste",
                                     "Centro-Oeste",
                                     "Sudeste",
                                     "Sul",
                                     "Exterior"))


# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 2
to_add <- data.frame(matrix(NA, empty_bar * nlevels(Circular$Region), ncol(Circular)))
colnames(to_add) <- colnames(Circular)
to_add$Region <- rep(levels(Circular$Region), each = empty_bar)
Circular <- rbind(Circular, to_add)
Circular <- Circular %>% arrange(Region)
Circular$ID <- seq(1, nrow(Circular))


# Get the name and the y position of each label
label_data_Circular <- Circular
number_of_bar <- nrow(label_data_Circular)
angle <- 90 - 360 * (label_data_Circular$ID - .5) / number_of_bar
label_data_Circular$hjust <- ifelse(angle < -90, 1, 0)
label_data_Circular$angle <- ifelse(angle < -90, angle + 180, angle)


# Prepares a data frame for base lines ~
base_data_Circular <- Circular %>% 
  group_by(Region) %>% 
  summarize(start = min(ID), 
            end = max(ID) - empty_bar, 
            N = n(), .groups = "drop") %>%
  mutate(end = ifelse(N == 1, start + 1, end)) %>%
  mutate(title = (start + end) / 2)


# Prepares a data frame for grid ~
grid_data_Circular <- base_data_Circular
grid_data_Circular$end <- grid_data_Circular$end[ c( nrow(grid_data_Circular), 1:nrow(grid_data_Circular) -1)] + 1
grid_data_Circular$start <- grid_data_Circular$start - 1
grid_data_Circular <- grid_data_Circular[-1, ]


# Reorders Population ~
fulldf_map$name_region <- factor(fulldf_map$name_region, ordered = TRUE,
                          levels = c("Norte",
                                     "Nordeste",
                                     "Centro-Oeste",
                                     "Sudeste",
                                     "Sul",
                                     "Exterior"))


# Adds English & bilingual labels  ~
fulldf_map <- fulldf_map %>%
              mutate(name_region_EN = case_when(name_region == "Norte" ~ "North",
                                                name_region == "Nordeste" ~ "Northeast",
                                                name_region == "Centro-Oeste" ~ "Central-West",
                                                name_region == "Sudeste" ~ "Southeast",
                                                name_region == "Sul" ~ "South",
                                                name_region == "Exterior" ~ "Abroad", TRUE ~ name_region),
               name_region_Bilingual = paste0("<span style='font-size:78pt; color:#000000;'>", name_region, 
                                              "<span style='font-size:78pt; color:#ffffff;'><br>", name_region_EN, "</span>"))


# Creates MiniMap ~
MiniMap_Bilingual <- ggplot() +
  geom_sf(data = subset(fulldf_map, name_region != "SBBE24"), 
          aes(fill = name_region), 
          colour = "#f7fbff") +
  geom_star(data = subset(fulldf_map, Division == "Per Region" & Region == "Exterior"),
            aes(x = Longitude, y = Latitude, fill = name_region), 
            size = 30, starshape = 8, starstroke = .3, colour = "#f7fbff") +
  scale_fill_manual(values = c("#1b9e77", "#fdb462", "#fb8072", "#bebada", "#80b1d3", "#c994c7")) +
  ggtext::geom_richtext(data = subset(fulldf_map, Division == "Per Region" & Stats == "Members" & Region != "SBBE24" & Region != "SBBE26"),
                        aes(x = Longitude, y = Latitude, label = name_region_Bilingual),
                        family = "Cormorant", fontface = "bold", size = 8, colour = "#000000", fill = NA, label.color = NA, lineheight = .55) +
  coord_sf(xlim = c(-75.75, -33), ylim = c(-35, 6.5), expand = FALSE) +
  theme_void() +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA))


# Creates Institutions bilingual plot ~
Institutions_Plot <- ggplot(Circular, aes(x = as.factor(ID), y = Percentage * 100, fill = Region)) +
                     geom_bar(stat = "identity", alpha = 1) +
                     scale_fill_manual(values = c("#1b9e77", "#fdb462", "#fb8072", "#bebada", "#80b1d3", "#c994c7")) +
                     geom_segment(data = grid_data_Circular, aes(x = end, y = 5, xend = start, yend = 5), 
                                  colour = "#000000", alpha = 1, linewidth = .25, linetype = 4, inherit.aes = FALSE) +
                     geom_segment(data = grid_data_Circular, aes(x = end, y = 10, xend = start, yend = 10), 
                                  colour = "#000000", alpha = 1, linewidth = .25, linetype = 4, inherit.aes = FALSE) +
                     geom_segment(data = grid_data_Circular, aes(x = end, y = 15, xend = start, yend = 15), 
                                  colour = "#000000", alpha = 1, linewidth = .25, linetype = 4, inherit.aes = FALSE) +
                     annotate("text", x = rep(max(Circular$ID), 3), y = c(5, 10, 15), 
                              label = c("5%", "10%", "15%"), family = "Cormorant", size = 26, fontface = "bold", 
                              color = "#000000", hjust = 1) +
                     geom_bar(stat = "identity", alpha = .5) +
                     ylim(-100, 80) +
                     labs(title = "Instituições Representadas na SBBE",
                          subtitle = "Institutions Represented in SBBE") +
                     theme(panel.background = element_rect(fill = "#ffffff"),
                           panel.grid = element_blank(),
                           panel.border = element_blank(),
                           legend.position = "none",
                           plot.title = element_text(family = "Cormorant", size = 250, face = "bold", hjust = .5, margin = margin(t = 22)),
                           plot.subtitle = element_text(family = "Cormorant", size = 250, colour = "#555555", face = "bold", hjust = .5, margin = margin(t = 12)),
                           axis.text = element_blank(),
                           axis.title = element_blank(), 
                           axis.ticks = element_blank()) +
                     coord_polar() +
                     geom_text(data = label_data_Circular, aes(x = ID, y = Percentage * 100 + 6, 
                               label = Variable, hjust = hjust), family = "Cormorant", size = 30, 
                               color = "#000000", fontface = "bold", angle = label_data_Circular$angle, inherit.aes = FALSE) +
                     geom_segment(data = base_data_Circular, aes(x = start, y = -5, xend = end, yend = -5), 
                                  colour = "#000000", alpha = 1, size = .6, inherit.aes = FALSE)
  

# Merges Institutions bilingual plot with MiniMap ~
Institutions_PlotUp <- Institutions_Plot + 
                       inset_element(MiniMap_Bilingual, left = .329, bottom = .2725, right = .329 + .35, top = .2725 + .35, align_to = "full") +
                                     plot_layout(guides = "collect")
  

# Saves Institutions bilingual plot ~
ggsave("./SBBEPlots/SBBEMembersInstitutions.png", Institutions_PlotUp, limitsize = FALSE,
       device = "png", scale = 1, width = 14, height = 15, dpi = 600)


# Gets Gender data frame ~
Gender <- fulldfPlots %>% 
          filter(Stats %in% c("GenderMembers", "StageMembers")) %>%
          droplevels() %>%
          arrange(desc(Percentage))


# Adds English & bilingual labels  ~
Gender <- Gender %>%
          mutate(Variable_EN = case_when(Variable == "Masculino" ~ "Male",
                                         Variable == "Feminino" ~ "Female",
                                         Variable == "Outro" ~ "Other",
                                         Variable == "Profissional" ~ "Professional",
                                         Variable == "Pós-graduação" ~ "Postgrad",
                                         Variable == "Graduação" ~ "Undergrad", TRUE ~ Variable),
                 Variable_Bilingual_1 = paste0("<span style='font-size:62pt; color:#000000;'>", Variable, 
                                               "<span style='font-size:62pt; color:#555555;'><br>", Variable_EN, "</span>"))


# Adds empty bars for spacing in circular plot ~
empty_bar <- 6
to_add <- data.frame(matrix(NA, empty_bar * nlevels(Gender$Stats), ncol(Gender)))
colnames(to_add) <- colnames(Gender)
to_add$Stats <- rep(levels(Gender$Stats), each = empty_bar)
Gender <- rbind(Gender, to_add)
Gender <- Gender %>% arrange(Stats)
Gender$ID <- seq(1, nrow(Gender))


# Computes base and grid data for plot ~
base_data_Gender <- Gender %>% 
  group_by(Stats) %>% 
  summarize(start = min(ID), 
            end = max(ID) - empty_bar, 
            N = n(), .groups = "drop") %>%
  mutate(end = ifelse(N == 1, start + 1, end)) %>%
  mutate(title = (start + end) / 2)
grid_data_Gender <- base_data_Gender
grid_data_Gender$end <- grid_data_Gender$end[ c(nrow(grid_data_Gender), 1:(nrow(grid_data_Gender) - 1)) ] + 1
grid_data_Gender$start <- grid_data_Gender$start - 1
grid_data_Gender <- grid_data_Gender[-1, ]


# Computes number of bars ~
number_of_bars <- nrow(Gender)
                  label_data <- Gender %>%
                  filter(Percentage > 0) %>%
                  mutate(angle = 90 - 360 * (ID - 0.5) / nrow(Gender),
                  hjust = ifelse(angle < -90, 1, 0),
                  angle = ifelse(angle < -90, angle + 180, angle),
                  label_y = ifelse(Percentage * 100 > 30, Percentage * 100 + 8, Percentage * 100 + 5))
                  

# Creates Gender plot ~
Gender_Plot <- 
 ggplot(Gender, aes(x = as.factor(ID), y = Percentage * 100, fill = Stats)) +
    geom_bar(stat = "identity", alpha = 1) +
    geom_bar(aes(x = as.factor(ID), y = Percentage * 100, fill = Region), 
             stat = "identity", alpha = 0.5) +
    geom_segment(data = grid_data_Gender, aes(x = end, y = 10, xend = start, yend = 10), 
                 colour = "#000000", linewidth = 0.25, linetype = 4, inherit.aes = FALSE) +
    geom_segment(data = grid_data_Gender, aes(x = end, y = 20, xend = start, yend = 20), 
                 colour = "#000000", linewidth = 0.25, linetype = 4, inherit.aes = FALSE) +
    geom_segment(data = grid_data_Gender, aes(x = end, y = 30, xend = start, yend = 30), 
                 colour = "#000000", linewidth = 0.25, linetype = 4, inherit.aes = FALSE) +
    geom_segment(data = grid_data_Gender, aes(x = end, y = 40, xend = start, yend = 40), 
                 colour = "#000000", linewidth = 0.25, linetype = 4, inherit.aes = FALSE) +
    geom_segment(data = grid_data_Gender, aes(x = end, y = 50, xend = start, yend = 50), 
                 colour = "#000000", linewidth = 0.25, linetype = 4, inherit.aes = FALSE) +
    geom_segment(data = base_data_Gender,
                 aes(x = start, y = -5, xend = end, yend = -5),
                 colour = "#000000", size = .6, inherit.aes = FALSE) +
    annotate("text", x = rep(max(Gender$ID), 5), y = c(10, 20, 30, 40, 50), 
             label = c("10%", "20%", "30%", "40%", "50%"), 
             family = "Cormorant", size = 20, fontface = "bold", color = "#000000", hjust = 1) +
    ggtext::geom_richtext(data = label_data, 
                          aes(x = ID, y = label_y, label = Variable_Bilingual_1, 
                              angle = angle, hjust = hjust),
                          fill = NA, label.color = NA, 
                          family = "Cormorant", 
                          size = 6,
                          fontface = "bold", 
                          color = "#000000", 
                          lineheight = .75,
                          inherit.aes = FALSE) +
    scale_fill_manual(values = c("#e5d8bd", "#fdbf6f"), na.translate = FALSE) +
    labs(title = "Membros da SBBE por Estágio Acadêmico & Gênero",
         subtitle = "SBBE Members by Academic Stage & Gender") +
    ylim(-90, 70) +
    coord_polar() +
    theme(panel.background = element_rect(fill = "#ffffff"),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          legend.position = "none",
          plot.title = element_text(family = "Cormorant", size = 100, face = "bold", hjust = .5, margin = margin(t = 10)),
          plot.subtitle = element_text(family = "Cormorant", size = 100, colour = "#555555", face = "bold", hjust = .5, margin = margin(t = 8.5)),
          axis.text = element_blank(),
          axis.title = element_blank(), 
          axis.ticks = element_blank())


# Saves Gender plot ~
ggsave("./SBBEPlots/SBBEMembersStats.png", Gender_Plot, limitsize = FALSE,
       device = "png", scale = 1, width = 8, height = 7, dpi = 600)


# Adds English & bilingual labels  ~
fulldf_map <- fulldf_map %>%
              mutate(name_region_Bilingual_2 = paste0("<span style='font-size:56pt; color:#000000;'>", name_region,
                                                      "<span style='font-size:56pt; color:#ffffff;'><br>", name_region_EN, "</span>"))


fulldf_map <- fulldf_map %>%
              mutate(Division_Bilingual = case_when(Division == "Per State" ~ "<span style='font-size:86pt;'>Por Estado</span><br><span style='font-size:86pt; color:#000000;'>Per State</span>",
                                                    Division == "Per Region" ~ "<span style='font-size:86pt;'>Por Região</span><br><span style='font-size:86pt; color:#000000;'>Per Region</span>", TRUE ~ Division))


# Creates Members Map ~
Map_Members <-
 ggplot() +
    geom_sf(data = subset(fulldf_map, Stats == "Members"), aes(fill = Percentage * 100), colour = "#f7fbff") +
    coord_sf(xlim = c(-75.75, -33), ylim = c(-35, 6.5), expand = FALSE) +
    scale_y_continuous(breaks = c(0, -10, -20, -30)) +
    geom_star(data = subset(fulldf_map, Stats == "Members" & Region == "Exterior"),
              aes(x = Longitude, y = Latitude, fill = Percentage), size = 25, starshape = 8,
              starstroke = .3, colour = "#f7fbff") +
    labs(title = "% de Membros da SBBE por Região & Estado",
         subtitle = "% of SBBE Members per Region & State") +
    scale_fill_continuous(low = "#d9d9d9", high = "#252525",
                          breaks = c(10, 20, 30, 40, 50),
                          labels = c("10%", "20%", "30%", "40%", "50%"),
                          limits = c(0, 60)) +
    ggtext::geom_richtext(data = subset(fulldf_map, Division == "Per Region" & Stats == "Members" & Region != "SBBE24" & Region != "SBBE26"),
                        aes(x = Longitude, y = Latitude, label = name_region_Bilingual_2),
                        family = "Cormorant", fontface = "bold", size = 8, colour = "#000000", fill = NA, label.color = NA, lineheight = .4) +
    facet_grid(. ~ Division, labeller = labeller(Division = ~ unique(fulldf_map$Division_Bilingual[match(.x, fulldf_map$Division)]))) +
    annotation_scale(data = subset(fulldf_map, Division == "Per Region" & Stats == "Members"),
                     text_family = "Cormorant", location = "bl", line_width = .85,
                     text_cex = 6, style = "ticks",
                     pad_x = unit(.1, "in"), pad_y = unit(.1, "in")) +
    annotation_north_arrow(data = subset(fulldf_map, Division == "Per Region" & Stats == "Members"),
                           location = "bl", which_north = "true", style = north_arrow_fancy_orienteering,
                           height = unit(1, "cm"), width = unit(1, "cm"),
                           pad_x = unit(.07125, "in"), pad_y = unit(.175, "in")) +
    theme(legend.position = "right",
          legend.margin = margin(t = 0, b = 0, r = 0, l = 15),
          legend.box.margin = margin(t = 0, b = 0, r = 0, l = 0),
          panel.background = element_rect(fill = "#ffffff"),
          panel.border = element_part_rect(side = "tlbr", colour = "#000000", linewidth = .25, fill = NA),
          panel.spacing = unit(0, "cm"),
          panel.grid = element_blank(),
          plot.margin = margin(t = 0, b = 0, r = 0, l = 0, unit = "cm"),
          plot.title = element_text(family = "Cormorant", size = 100, face = "bold", hjust = .5, margin = margin(t = 0)),
          plot.subtitle = element_text(family = "Cormorant", size = 100, colour = "#555555", face = "bold", hjust = .5, margin = margin(t = 5, b = 10)),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          strip.text = element_markdown(family = "Cormorant", size = 86, face = "bold", lineheight = .21),
          strip.background = element_rect(colour = "#000000", fill = "#d6d6d6", linewidth = .25)) +
    guides(fill = guide_colourbar(title = "", label.theme = element_text(family = "Cormorant", size = 75, face = "bold"),
                                  barwidth = 1, barheight = 8, order = 1, frame.linetype = 1,
                                  frame.colour = "#000000", ticks.colour = "#f7fbff",
                                  direction = "vertical", reverse = FALSE,
                                  draw.ulim = TRUE, draw.llim = TRUE))


# Saves Members Map ~
ggsave("./SBBEPlots/SBBEMembersMap.png", Map_Members, limitsize = FALSE,
       device = "png", scale = 1, width = 9, height = 5.5, dpi = 600)


# Creates Attendees Map ~
Map_Attendees_SBBE24 <-
 ggplot() +
    geom_sf(data = fulldf_map %>% filter(Stats == "Attendees", Conference == "SBBE24", !is.na(Conference)), aes(fill = Percentage * 100), colour = "#f7fbff") +
    coord_sf(xlim = c(-75.75, -33), ylim = c(-35, 6.5), expand = FALSE) +
    scale_y_continuous(breaks = c(0, -10, -20, -30)) + 
    geom_star(data = fulldf_map %>% filter(Division == "Per Region", Stats == "Attendees", Region == "SBBE24", !is.na(Conference)),
              aes(x = Longitude, y = Latitude), size = 2.75, starshape = 15, starstroke = .3,
              fill = "#365338", colour = "#ffffff") +
    geom_star(data = fulldf_map %>% filter(Stats == "Attendees", Region == "Exterior", !is.na(Conference)),
              aes(x = Longitude, y = Latitude, fill = Percentage), size = 25, starshape = 8,
              starstroke = .3, colour = "#f7fbff") +
    geom_star(data = fulldf_map  %>% filter(Division == "Per Region", Stats == "Attendees", Region == "SBBE24"),
              aes(x = Longitude, y = Latitude), size = 2.6, starshape = 15, starstroke = .3,
              fill = "#365237", colour = "#ffffff") +
    geom_text(data = fulldf_map  %>% filter(Division == "Per Region", Stats == "Attendees", Region == "SBBE24"),
              aes(x = Longitude, y = Latitude, label = Region),
              nudge_x = 2.73, nudge_y = -.85,
              size = 19, family = "Cormorant", fontface = "bold", colour = "#365338") +
    labs(title = "% de Participantes do SBBE24 por Região & Estado",
         subtitle = "% of SBBE24 Attendees per Region & State") +
    scale_fill_continuous(low = "#c7e9c0", high = "#365237",
                          breaks = c(10, 20, 30, 40, 50, 60),
                          labels = c("10%", "20%", "30%", "40%", "50%", "60%"),
                          limits = c(0, 70)) +
    ggtext::geom_richtext(data = fulldf_map %>% filter(Division == "Per Region", Stats == "Attendees", Region != "SBBE24",  Region != "SBBE26", !is.na(Conference)),
                          aes(x = Longitude, y = Latitude, label = name_region_Bilingual_2),
                          family = "Cormorant", fontface = "bold", size = 8, colour = "#000000", fill = NA, label.color = NA, lineheight = .4) +
    facet_grid(. ~ Division, labeller = labeller(Division = ~ unique(fulldf_map$Division_Bilingual[match(.x, fulldf_map$Division)]))) +
    annotation_scale(data = subset(fulldf_map, Division == "Per Region" & Stats == "Attendees"),
                     text_family = "Cormorant", location = "bl", line_width = .85,
                     text_cex = 6, style = "ticks",
                     pad_x = unit(.1, "in"), pad_y = unit(.1, "in")) +
    annotation_north_arrow(data = subset(fulldf_map, Division == "Per Region" & Stats == "Attendees"),
                           location = "bl", which_north = "true", style = north_arrow_fancy_orienteering,
                           height = unit(1, "cm"), width = unit(1, "cm"),
                           pad_x = unit(.07125, "in"), pad_y = unit(.175, "in")) +
    theme(legend.position = "right",
          legend.margin = margin(t = 0, b = 0, r = 0, l = 20),
          legend.box.margin = margin(t = 0, b = 0, r = 0, l = 0),
          panel.background = element_rect(fill = "#ffffff"),
          panel.border = element_rect(colour = "#000000", linewidth = .25, fill = NA),
          panel.spacing = unit(0, "cm"),
          panel.grid = element_blank(),
          plot.margin = margin(t = 0, b = 0, r = 0, l = 0, unit = "cm"),
          plot.title = element_text(family = "Cormorant", size = 100, face = "bold", hjust = .5, margin = margin(t = 0)),
          plot.subtitle = element_text(family = "Cormorant", size = 100, colour = "#555555", face = "bold", hjust = .5, margin = margin(t = 5, b = 10)),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          strip.text = element_markdown(family = "Cormorant", size = 86, face = "bold", lineheight = .21),
          strip.background = element_rect(colour = "#000000", fill = "#d6d6d6", linewidth = .25)) +
    guides(fill = guide_colourbar(title = "", label.theme = element_text(family = "Cormorant", size = 75, face = "bold"),
                                  barwidth = 1, barheight = 8, order = 1, frame.linetype = 1,
                                  frame.colour = "#000000", ticks.colour = "#f7fbff",
                                  direction = "vertical", reverse = FALSE,
                                  draw.ulim = TRUE, draw.llim = TRUE))


# Saves Attendees Map ~
ggsave("./SBBEPlots/AttendeesMap_SBBE24.png", Map_Attendees_SBBE24, limitsize = FALSE,
       device = "png", scale = 1, width = 9, height = 5.5, dpi = 600)


# Creates Attendees Map ~
Map_Attendees_SBBE26 <-
  ggplot() +
  geom_sf(data = fulldf_map %>% filter(Stats == "Attendees", Conference == "SBBE26", !is.na(Conference)), aes(fill = Percentage * 100), colour = "#f7fbff") +
  coord_sf(xlim = c(-75.75, -33), ylim = c(-35, 6.5), expand = FALSE) +
  scale_y_continuous(breaks = c(0, -10, -20, -30)) + 
  geom_star(data = fulldf_map %>% filter(Division == "Per Region", Stats == "Attendees", Region == "SBBE26", !is.na(Conference)),
            aes(x = Longitude, y = Latitude), size = 2.75, starshape = 15, starstroke = .3,
            fill = "#365338", colour = "#ffffff") +
  geom_star(data = fulldf_map %>% filter(Stats == "Attendees", Region == "Exterior", !is.na(Conference)),
            aes(x = Longitude, y = Latitude, fill = Percentage), size = 25, starshape = 8,
            starstroke = .3, colour = "#f7fbff") +
  geom_star(data = fulldf_map  %>% filter(Division == "Per Region", Stats == "Attendees", Region == "SBBE26"),
            aes(x = Longitude, y = Latitude), size = 2.6, starshape = 15, starstroke = .3,
            fill = "#e67033", colour = "#ffffff") +
  geom_text(data = fulldf_map %>% filter(Division == "Per Region", Stats == "Attendees", Region == "SBBE26", !is.na(Conference)),
            aes(x = Longitude, y = Latitude, label = Region),
            nudge_x = 3, nudge_y = -.85,
            size = 20, family = "Cormorant", fontface = "bold", colour = "#365338") +
  geom_text(data = fulldf_map  %>% filter(Division == "Per Region", Stats == "Attendees", Region == "SBBE26"),
            aes(x = Longitude, y = Latitude, label = Region),
            nudge_x = 0, nudge_y = 1.3,
            size = 19, family = "Cormorant", fontface = "bold", colour = "#e67033") +
  labs(title = "% de Participantes do SBBE26 por Região & Estado",
       subtitle = "% of SBBE26 Attendees per Region & State") +
  scale_fill_continuous(low = "#fdd0a2", high = "#6d3633",
                        breaks = c(10, 20, 30, 40, 50, 60),
                        labels = c("10%", "20%", "30%", "40%", "50%", "60%"),
                        limits = c(0, 70)) +
  ggtext::geom_richtext(data = fulldf_map %>% filter(Division == "Per Region", Stats == "Attendees", Region != "SBBE24",  Region != "SBBE26", !is.na(Conference)),
                        aes(x = Longitude, y = Latitude, label = name_region_Bilingual_2),
                        family = "Cormorant", fontface = "bold", size = 8, colour = "#000000", fill = NA, label.color = NA, lineheight = .4) +
  facet_grid(. ~ Division, labeller = labeller(Division = ~ unique(fulldf_map$Division_Bilingual[match(.x, fulldf_map$Division)]))) +
  annotation_scale(data = subset(fulldf_map, Division == "Per Region" & Stats == "Attendees"),
                   text_family = "Cormorant", location = "bl", line_width = .85,
                   text_cex = 6, style = "ticks",
                   pad_x = unit(.1, "in"), pad_y = unit(.1, "in")) +
  annotation_north_arrow(data = subset(fulldf_map, Division == "Per Region" & Stats == "Attendees"),
                         location = "bl", which_north = "true", style = north_arrow_fancy_orienteering,
                         height = unit(1, "cm"), width = unit(1, "cm"),
                         pad_x = unit(.07125, "in"), pad_y = unit(.175, "in")) +
  theme(legend.position = "right",
        legend.margin = margin(t = 0, b = 0, r = 0, l = 20),
        legend.box.margin = margin(t = 0, b = 0, r = 0, l = 0),
        panel.background = element_rect(fill = "#ffffff"),
        panel.border = element_rect(colour = "#000000", linewidth = .25, fill = NA),
        panel.spacing = unit(0, "cm"),
        panel.grid = element_blank(),
        plot.margin = margin(t = 0, b = 0, r = 0, l = 0, unit = "cm"),
        plot.title = element_text(family = "Cormorant", size = 100, face = "bold", hjust = .5, margin = margin(t = 0)),
        plot.subtitle = element_text(family = "Cormorant", size = 100, colour = "#555555", face = "bold", hjust = .5, margin = margin(t = 5, b = 10)),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_markdown(family = "Cormorant", size = 86, face = "bold", lineheight = .21),
        strip.background = element_rect(colour = "#000000", fill = "#d6d6d6", linewidth = .25)) +
  guides(fill = guide_colourbar(title = "", label.theme = element_text(family = "Cormorant", size = 75, face = "bold"),
                                barwidth = 1, barheight = 8, order = 1, frame.linetype = 1,
                                frame.colour = "#000000", ticks.colour = "#f7fbff",
                                direction = "vertical", reverse = FALSE,
                                draw.ulim = TRUE, draw.llim = TRUE))


# Saves Attendees Map ~
ggsave("./SBBEPlots/AttendeesMap_SBBE26.png", Map_Attendees_SBBE26, limitsize = FALSE,
       device = "png", scale = 1, width = 9, height = 5.5, dpi = 600)



#
##
### The END ~~~~~