### Google Searches for "cloroquina" in Brazil over time and across states

# Packages ----
packages<-c("cepespR", "gtrendsR", "dplyr", "RCurl", "ggplot2", "gganimate", "transformr") # you may need to install these
lapply(packages, require, character.only=T)

# Votes by state ----
voto_pres<-get_votes(2018, "President", regional_aggregation="State", cache=T)
head(voto_pres)

table(voto_pres$UF)
voto_pres<-voto_pres %>% filter(UF!="ZZ")

table(voto_pres$NUM_TURNO)
votos_1<-voto_pres %>% filter(NUM_TURNO==1)

votos_UF<-votos_1 %>% select(UF, QTDE_VOTOS, NUM_TURNO) %>% group_by(UF, NUM_TURNO) %>%
  summarize(total_1st=sum(QTDE_VOTOS))
votos_UF_1<-merge(votos_UF, votos_1 %>% filter(NUMERO_CANDIDATO==17) %>% select(UF, NOME_UF, QTDE_VOTOS),
                 by="UF")
head(votos_UF_1)
colnames(votos_UF_1)[4:5]<-paste0(colnames(votos_UF_1)[4:5], "_1st")

votos_2<-voto_pres %>% filter(NUM_TURNO==2)
votos_UF_2<-votos_2 %>% select(UF, QTDE_VOTOS, NUM_TURNO) %>% group_by(UF, NUM_TURNO) %>%
  summarize(total_2nd=sum(QTDE_VOTOS))
votos_UF_2<-merge(votos_UF_2, votos_2 %>% filter(NUMERO_CANDIDATO==17) %>% select(UF, NOME_UF, QTDE_VOTOS),
                  by="UF")
head(votos_UF_2)
colnames(votos_UF_2)[4:5]<-paste0(colnames(votos_UF_2)[4:5], "_2nd")

votos_UF_1$NUM_TURNO<-NULL
votos_UF_2$NUM_TURNO<-NULL

votos_UF<-merge(votos_UF_1, votos_UF_2, by="UF")
votos_UF$prop_Bolso_1<-100*votos_UF$QTDE_VOTOS_1st/votos_UF$total_1st
votos_UF$prop_Bolso_2<-100*votos_UF$QTDE_VOTOS_2nd/votos_UF$total_2nd
head(votos_UF)

#
# Searches for word "Cloroquina" on Google over time
cloro_time<-as.data.frame(gtrends(keyword=c("cloroquina"), geo ="BR", time = "2020-03-15 2020-03-30", 
        gprop ="web")$interest_over_time)
head(cloro_time)
cloro_time$hits[cloro_time$hits=="<1"]<-"0"
cloro_time$hits<-as.numeric(cloro_time$hits)
cloro_time$date<-as.Date(substr(cloro_time$date, 1, 10))

graph1<-ggplot(cloro_time, aes(x=date, y=hits)) +
  geom_line() + geom_point() +
  labs(x="", y = "Volume Relativo de Busca", title="Busca por 'Cloroquina' no Google, Brasil") +
  theme_bw() + theme(
    plot.title = element_text(hjust = .5, size=18, face = "bold"), 
    plot.subtitle = element_text(hjust="0.5"),
    legend.position = "center",
    axis.title.x = element_text(size=12, face = "bold"),
    axis.title.y = element_text(size=14, face = "bold"),
    axis.text.x = element_text(angle=0, size=12)) +
  transition_reveal(date)
graph1

#
#
# Google searches for "Cloroquina" on Google across states by periods of four days:
cloro_1518<-as.data.frame(gtrends(keyword ="cloroquina", geo ="BR", time = "2020-03-15 2020-03-18", 
                             gprop = c("web"), low_search_volume=T)$interest_by_region)
head(cloro_1518)
cloro_1518$location<-substr(cloro_1518$location, 10, nchar(cloro_1518$location))
cloro_1518$location[cloro_1518$location=="istrict"]<-"Distrito Federal"
cloro_1518$period<-"2020-03-16"

cloro_1922<-as.data.frame(gtrends(keyword ="cloroquina", geo ="BR", time = "2020-03-19 2020-03-22", 
                             gprop = c("web"), low_search_volume=T)$interest_by_region)
cloro_1922$location<-substr(cloro_1922$location, 10, nchar(cloro_1922$location))
cloro_1922$location[cloro_1922$location=="istrict"]<-"Distrito Federal"
cloro_1922$period<-"2020-03-20"

cloro_2326<-as.data.frame(gtrends(keyword ="cloroquina", geo ="BR", time = "2020-03-23 2020-03-26", 
                             gprop = c("web"), low_search_volume=T)$interest_by_region)
cloro_2326$location<-substr(cloro_2326$location, 10, nchar(cloro_2326$location))
cloro_2326$location[cloro_2326$location=="istrict"]<-"Distrito Federal"
cloro_2326$period<-"2020-03-24"

cloro_2730<-as.data.frame(gtrends(keyword ="cloroquina", geo ="BR", time = "2020-03-27 2020-03-30", 
                                  gprop = c("web"), low_search_volume=T)$interest_by_region)
cloro_2730$location<-substr(cloro_2730$location, 10, nchar(cloro_2730$location))
cloro_2730$location[cloro_2730$location=="istrict"]<-"Distrito Federal"
cloro_2730$period<-"2020-03-28"

cloro_states<-rbind(cloro_1518, cloro_1922, cloro_2326, cloro_2730)
cloro_states$period<-as.Date(cloro_states$period)

cloro_votos<-merge(votos_UF, cloro_states, by.x="NOME_UF_1st", by.y="location")
head(cloro_votos)
summary(lm(hits~prop_Bolso_1 * as.factor(period), cloro_votos))
summary(lm(hits~prop_Bolso_1, cloro_votos[cloro_votos$period=="2020-03-20",]))
summary(lm(hits~prop_Bolso_1, cloro_votos[cloro_votos$period=="2020-03-24",]))
summary(lm(hits~prop_Bolso_1, cloro_votos[cloro_votos$period=="2020-03-28",]))

summary(lm(hits~prop_Bolso_2 * as.factor(period), cloro_votos))
summary(lm(hits~prop_Bolso_2, cloro_votos[cloro_votos$period=="2020-03-20",]))
summary(lm(hits~prop_Bolso_2, cloro_votos[cloro_votos$period=="2020-03-24",]))
summary(lm(hits~prop_Bolso_2, cloro_votos[cloro_votos$period=="2020-03-28",]))

graph2<-ggplot(cloro_votos, aes(x=prop_Bolso_2, y=hits)) + geom_point(aes(size=total_2nd)) + 
  geom_smooth(method="lm", se=FALSE) + 
  ylab("Volume Relativo de Busca") + xlab(expression(paste("% de votos para Bolsonaro no 2"^o, " Turno"))) + 
  theme_bw() + theme(
    plot.title = element_text(hjust = .5,face = "bold"), 
    plot.subtitle = element_text(hjust="0.5"),
    legend.position = "center",
    axis.title.x = element_text(size=12, face = "bold"),
    axis.title.y = element_text(size=14, face = "bold"),
    axis.text.x = element_text(angle=0, size=12)) +
  ylim(0, 100) +
  transition_time(period) +
  labs(title = "Busca por 'Cloroquina' por estado, {frame_time}")
graph2

#
#
#
#
#

# Let's use "Quarentena"- another word related to the COVID-19 crisis as a control

# Busca pela palavra "corona" no Google entre os nos diferentes estados:
quare_1518<-as.data.frame(gtrends(keyword ="quarentena", geo ="BR", time = "2020-03-15 2020-03-18", 
                                  gprop = c("web"), low_search_volume=T)$interest_by_region)
head(quare_1518)
quare_1518$location<-substr(quare_1518$location, 10, nchar(quare_1518$location))
quare_1518$location[quare_1518$location=="istrict"]<-"Distrito Federal"
quare_1518$period<-"2020-03-16"

quare_1922<-as.data.frame(gtrends(keyword ="quarentena", geo ="BR", time = "2020-03-19 2020-03-22", 
                                  gprop = c("web"), low_search_volume=T)$interest_by_region)
quare_1922$location<-substr(quare_1922$location, 10, nchar(quare_1922$location))
quare_1922$location[quare_1922$location=="istrict"]<-"Distrito Federal"
quare_1922$period<-"2020-03-20"

quare_2326<-as.data.frame(gtrends(keyword ="quarentena", geo ="BR", time = "2020-03-23 2020-03-26", 
                                  gprop = c("web"), low_search_volume=T)$interest_by_region)
quare_2326$location<-substr(quare_2326$location, 10, nchar(quare_2326$location))
quare_2326$location[quare_2326$location=="istrict"]<-"Distrito Federal"
quare_2326$period<-"2020-03-24"

quare_2730<-as.data.frame(gtrends(keyword ="quarentena", geo ="BR", time = "2020-03-27 2020-03-30", 
                                  gprop = c("web"), low_search_volume=T)$interest_by_region)
quare_2730$location<-substr(quare_2730$location, 10, nchar(quare_2730$location))
quare_2730$location[quare_2730$location=="istrict"]<-"Distrito Federal"
quare_2730$period<-"2020-03-28"

quare_states<-rbind(quare_1518, quare_1922, quare_2326, quare_2730)
quare_states$period<-as.Date(quare_states$period)

quare_votos<-merge(votos_UF, quare_states, by.x="NOME_UF_1st", by.y="location")
head(quare_votos)
summary(lm(hits~prop_Bolso_1 * as.factor(period), quare_votos))
summary(lm(hits~prop_Bolso_1, quare_votos[quare_votos$period=="2020-03-20",]))
summary(lm(hits~prop_Bolso_1, quare_votos[quare_votos$period=="2020-03-24",]))
summary(lm(hits~prop_Bolso_1, quare_votos[quare_votos$period=="2020-03-28",]))

summary(lm(hits~prop_Bolso_2 * as.factor(period), quare_votos))
summary(lm(hits~prop_Bolso_2, quare_votos[quare_votos$period=="2020-03-20",]))
summary(lm(hits~prop_Bolso_2, quare_votos[quare_votos$period=="2020-03-24",]))
summary(lm(hits~prop_Bolso_2, quare_votos[quare_votos$period=="2020-03-28",]))

graph3<-ggplot(quare_votos, aes(x=prop_Bolso_2, y=hits)) + geom_point(aes(size=total_2nd)) + 
  geom_smooth(method="lm", se=FALSE) + 
  ylab("Volume Relativo de Busca") + xlab(expression(paste("% de votos para Bolsonaro no 2"^o, " Turno"))) + 
  theme_bw() + theme(
    plot.title = element_text(hjust = .5,face = "bold"), 
    plot.subtitle = element_text(hjust="0.5"),
    legend.position = "center",
    axis.title.x = element_text(size=12, face = "bold"),
    axis.title.y = element_text(size=14, face = "bold"),
    axis.text.x = element_text(angle=0, size=12)) +
  ylim(0, 100) +
  transition_time(period) +
  labs(title = "Busca por 'Quarentena' por estado, {frame_time}")
graph3
