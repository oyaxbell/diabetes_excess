## Diabetes as a cause of excess deaths in Mexico during 2020: a comparative analysis of national death registries
## Data Analysis: Omar Yaxmehen Bello-Chavolla (oyaxbell@yahoo.com.mx)
## Latest version of Analysis 22-Feb-2022
## Any question regarding analysis, please contact Omar Yaxmehen Bello-Chavolla

#### Loading datasets ####
pacman::p_load(tidyverse, lme4, lmerTest, haven, survey, flextable, officer, jtools, readxl, ggsci, rms,mediation,ggtern,cowplot,AER,reshape2,tmap,ggplotify,ggimage,gvlma,data.table,
               ggpubr, MASS, factoextra, fpc, NbClust, ggmap, RColorBrewer, rmapshaper,ggsflabel, ggthemes, tricolore, biscale, scales, ggpubr, spdep, glmmfields,DCluster)
setwd("~/OneDrive - UNIVERSIDAD NACIONAL AUTÓNOMA DE MÉXICO/Exceso Diabetes/")
setwd("C:/Users/Investigacion/OneDrive - UNIVERSIDAD NACIONAL AUTÓNOMA DE MÉXICO/Exceso Diabetes")
mort_2020 <- read_csv("Bases/conjunto_de_datos_defunciones_registradas_2020.CSV") %>% filter(anio_ocur==2020) 
mort_2020$ICD_10_1.CHAPTER<-str_extract(strsplit(mort_2020$causa_def,NULL), "[A-Z]+" )
mort_2020$ICD_10_1.CODE<-as.numeric(substr(str_extract(strsplit(mort_2020$causa_def, split = "(?<=[a-zA-Z])\\s*(?=[0-9])", perl = TRUE),"([[:digit:]]+)"),start=1,stop=2))
mort_2020$cause<-as.numeric(substr(str_extract(strsplit(mort_2020$causa_def, split = "(?<=[a-zA-Z])\\s*(?=[0-9])", perl = TRUE),"([[:digit:]]+)"),start=3,stop=4))
mort_2020$cause<-factor(mort_2020$cause, labels = c("HHS","Ketoacidosis", "Kidney complications","Ophthalmic","Neurological", "Circulatory","Unspecified","Multiple","Unspecified","No Complications"))
#mort_2020$edad<-as.numeric(substr(str_extract(strsplit(as.character(mort_2020$edad), split = "(?<=[a-zA-Z])\\s*(?=[0-9])", perl = TRUE),"([[:digit:]]+)"),start=3,stop=4))
covid0<-mort_2020 %>% filter(ICD_10_1.CHAPTER=="U") %>% filter(ICD_10_1.CODE %in% c(07))
mort_2020<- mort_2020 %>% filter(ICD_10_1.CHAPTER=="E") %>% filter(ICD_10_1.CODE %in% c(10, 11, 12, 13, 14))
mort_2020$id<-paste0(str_pad(mort_2020$ent_ocurr, 2,pad = "0"),str_pad(mort_2020$mun_ocurr,3, pad="0"))
mort_2020<-mort_2020 %>% filter(!(edad_agru %in% c("01", "02", "03", "04", "05", "06", "07", "08"))) %>%
  mutate(EDAD_QUIN=dplyr::recode(edad_agru, "09" = "20 a 24 AÑOs", "10" = "25 a 29 AÑOs",
                                 "11" = "30 a 34 AÑOs", "12" = "35 a 39 AÑOs",
                                 "13" = "40 a 44 AÑOs", "14" = "45 a 49 AÑOs",
                                 "15" = "50 a 54 AÑOs", "16" = "55 a 59 AÑOs",
                                 "17" = "60 a 64 AÑOs", "18" = "65+ AÑOs","19" = "65+ AÑOs",
                                 "20" = "65+ AÑOs","21" = "65+ AÑOs","22" = "65+ AÑOs","23" = "65+ AÑOs",
                                 "24" = "65+ AÑOs","25" = "65+ AÑOs","26" = "65+ AÑOs","27" = "65+ AÑOs",
                                 "28" = "65+ AÑOs","29" = "65+ AÑOs","30" = "65+ AÑOs"),
         EDAD_QUIN2=dplyr::recode(edad_agru, "09" = "20-24", "10" = "25-29",
                                 "11" = "30-34", "12" = "35-39",
                                 "13" = "40-44", "14" = "45-49",
                                 "15" = "50-54", "16" = "55-59",
                                 "17" = "60-64", "18" = "65-69","19" = "70-74",
                                 "20" = "75-79","21" = "80-84","22" = "85-89","23" = "90+",
                                 "24" = "90+","25" = "90+","26" = "90+","27" = "90+",
                                 "28" = "90+","29" = "90+","30" = "90+"))
mort_2019 <- read_csv("Bases/Defunciones_2019.CSV") %>% filter(anio_ocur==2019) 
mort_2019$ICD_10_1.CHAPTER<-str_extract(strsplit(mort_2019$causa_def,NULL), "[A-Z]+" )
mort_2019$ICD_10_1.CODE<-as.numeric(substr(str_extract(strsplit(mort_2019$causa_def, split = "(?<=[a-zA-Z])\\s*(?=[0-9])", perl = TRUE),"([[:digit:]]+)"),start=1,stop=2))
mort_2019$cause<-as.numeric(substr(str_extract(strsplit(mort_2019$causa_def, split = "(?<=[a-zA-Z])\\s*(?=[0-9])", perl = TRUE),"([[:digit:]]+)"),start=3,stop=4))
mort_2019$cause<-factor(mort_2019$cause, labels = c("HHS","Ketoacidosis", "Kidney complications","Ophthalmic","Neurological", "Circulatory","Unspecified","Multiple","Unspecified","No Complications"))
#mort_2019$edad<-as.numeric(substr(str_extract(strsplit(as.character(mort_2019$edad), split = "(?<=[a-zA-Z])\\s*(?=[0-9])", perl = TRUE),"([[:digit:]]+)"),start=3,stop=4))
mort_2019<- mort_2019 %>% filter(ICD_10_1.CHAPTER=="E") %>% filter(ICD_10_1.CODE %in% c(10, 11, 12, 13, 14))
mort_2019$id<-paste0(str_pad(mort_2019$ent_ocurr, 2,pad = "0"),str_pad(mort_2019$mun_ocurr,3, pad="0"))
mort_2019<-mort_2019 %>% filter(!(edad_agru %in% c("01", "02", "03", "04", "05", "06", "07", "08"))) %>%
  mutate(EDAD_QUIN=dplyr::recode(edad_agru, "09" = "20 a 24 AÑOs", "10" = "25 a 29 AÑOs",
                                 "11" = "30 a 34 AÑOs", "12" = "35 a 39 AÑOs",
                                 "13" = "40 a 44 AÑOs", "14" = "45 a 49 AÑOs",
                                 "15" = "50 a 54 AÑOs", "16" = "55 a 59 AÑOs",
                                 "17" = "60 a 64 AÑOs", "18" = "65+ AÑOs","19" = "65+ AÑOs",
                                 "20" = "65+ AÑOs","21" = "65+ AÑOs","22" = "65+ AÑOs","23" = "65+ AÑOs",
                                 "24" = "65+ AÑOs","25" = "65+ AÑOs","26" = "65+ AÑOs","27" = "65+ AÑOs",
                                 "28" = "65+ AÑOs","29" = "65+ AÑOs","30" = "65+ AÑOs"),
         EDAD_QUIN2=dplyr::recode(edad_agru, "09" = "20-24", "10" = "25-29",
                                  "11" = "30-34", "12" = "35-39",
                                  "13" = "40-44", "14" = "45-49",
                                  "15" = "50-54", "16" = "55-59",
                                  "17" = "60-64", "18" = "65-69","19" = "70-74",
                                  "20" = "75-79","21" = "80-84","22" = "85-89","23" = "90+",
                                  "24" = "90+","25" = "90+","26" = "90+","27" = "90+",
                                  "28" = "90+","29" = "90+","30" = "90+"))
mort_2018 <- read_csv("Bases/Defunciones_2018.CSV") %>% filter(ANIO_OCUR==2018) 
mort_2018$ICD_10_1.CHAPTER<-str_extract(strsplit(mort_2018$CAUSA_DEF,NULL), "[A-Z]+" )
mort_2018$ICD_10_1.CODE<-as.numeric(substr(str_extract(strsplit(mort_2018$CAUSA_DEF, split = "(?<=[a-zA-Z])\\s*(?=[0-9])", perl = TRUE),"([[:digit:]]+)"),start=1,stop=2))
mort_2018$cause<-as.numeric(substr(str_extract(strsplit(mort_2018$CAUSA_DEF, split = "(?<=[a-zA-Z])\\s*(?=[0-9])", perl = TRUE),"([[:digit:]]+)"),start=3,stop=4))
mort_2018$cause<-factor(mort_2018$cause, labels = c("HHS","Ketoacidosis", "Kidney complications","Ophthalmic","Neurological", "Circulatory","Unspecified","Multiple","Unspecified","No Complications"))
mort_2018<- mort_2018 %>% filter(ICD_10_1.CHAPTER=="E") %>% filter(ICD_10_1.CODE %in% c(10, 11, 12, 13, 14))
mort_2018$id<-paste0(str_pad(mort_2018$ENT_OCURR, 2,pad = "0"),str_pad(mort_2018$MUN_OCURR,3, pad="0"))
mort_2018<-mort_2018 %>% filter(!(EDAD_AGRU %in% c("01", "02", "03", "04", "05", "06", "07", "08"))) %>%
  mutate(EDAD_QUIN=dplyr::recode(EDAD_AGRU, "09" = "20 a 24 AÑOs", "10" = "25 a 29 AÑOs",
                                 "11" = "30 a 34 AÑOs", "12" = "35 a 39 AÑOs",
                                 "13" = "40 a 44 AÑOs", "14" = "45 a 49 AÑOs",
                                 "15" = "50 a 54 AÑOs", "16" = "55 a 59 AÑOs",
                                 "17" = "60 a 64 AÑOs", "18" = "65+ AÑOs","19" = "65+ AÑOs",
                                 "20" = "65+ AÑOs","21" = "65+ AÑOs","22" = "65+ AÑOs","23" = "65+ AÑOs",
                                 "24" = "65+ AÑOs","25" = "65+ AÑOs","26" = "65+ AÑOs","27" = "65+ AÑOs",
                                 "28" = "65+ AÑOs","29" = "65+ AÑOs","30" = "65+ AÑOs"),
         EDAD_QUIN2=dplyr::recode(EDAD_AGRU, "09" = "20-24", "10" = "25-29",
                                  "11" = "30-34", "12" = "35-39",
                                  "13" = "40-44", "14" = "45-49",
                                  "15" = "50-54", "16" = "55-59",
                                  "17" = "60-64", "18" = "65-69","19" = "70-74",
                                  "20" = "75-79","21" = "80-84","22" = "85-89","23" = "90+",
                                  "24" = "90+","25" = "90+","26" = "90+","27" = "90+",
                                  "28" = "90+","29" = "90+","30" = "90+"))
mort_2017 <- read_csv("Bases/Defunciones_2017.CSV") %>% filter(ANIO_OCUR==2017) 
mort_2017$ICD_10_1.CHAPTER<-str_extract(strsplit(mort_2017$CAUSA_DEF,NULL), "[A-Z]+" )
mort_2017$ICD_10_1.CODE<-as.numeric(substr(str_extract(strsplit(mort_2017$CAUSA_DEF, split = "(?<=[a-zA-Z])\\s*(?=[0-9])", perl = TRUE),"([[:digit:]]+)"),start=1,stop=2))
mort_2017$cause<-as.numeric(substr(str_extract(strsplit(mort_2017$CAUSA_DEF, split = "(?<=[a-zA-Z])\\s*(?=[0-9])", perl = TRUE),"([[:digit:]]+)"),start=3,stop=4))
mort_2017$cause<-factor(mort_2017$cause, labels = c("Hyperosmolarity","Ketoacidosis", "Kidney complications","Ophthalmic","Neurological", "Circulatory","Unspecified","Multiple","Unspecified","No Complications"))
mort_2017<- mort_2017 %>% filter(ICD_10_1.CHAPTER=="E") %>% filter(ICD_10_1.CODE %in% c(10, 11, 12, 13, 14))
mort_2017$id<-paste0(str_pad(mort_2017$ENT_OCURR, 2,pad = "0"),str_pad(mort_2017$MUN_OCURR,3, pad="0"))
mort_2017<-mort_2017 %>% filter(!(EDAD_AGRU %in% c("01", "02", "03", "04", "05", "06", "07", "08"))) %>%
  mutate(EDAD_QUIN=dplyr::recode(EDAD_AGRU, "09" = "20 a 24 AÑOs", "10" = "25 a 29 AÑOs",
                                 "11" = "30 a 34 AÑOs", "12" = "35 a 39 AÑOs",
                                 "13" = "40 a 44 AÑOs", "14" = "45 a 49 AÑOs",
                                 "15" = "50 a 54 AÑOs", "16" = "55 a 59 AÑOs",
                                 "17" = "60 a 64 AÑOs", "18" = "65+ AÑOs","19" = "65+ AÑOs",
                                 "20" = "65+ AÑOs","21" = "65+ AÑOs","22" = "65+ AÑOs","23" = "65+ AÑOs",
                                 "24" = "65+ AÑOs","25" = "65+ AÑOs","26" = "65+ AÑOs","27" = "65+ AÑOs",
                                 "28" = "65+ AÑOs","29" = "65+ AÑOs","30" = "65+ AÑOs"),
         EDAD_QUIN2=dplyr::recode(EDAD_AGRU, "09" = "20-24", "10" = "25-29",
                                  "11" = "30-34", "12" = "35-39",
                                  "13" = "40-44", "14" = "45-49",
                                  "15" = "50-54", "16" = "55-59",
                                  "17" = "60-64", "18" = "65-69","19" = "70-74",
                                  "20" = "75-79","21" = "80-84","22" = "85-89","23" = "90+",
                                  "24" = "90+","25" = "90+","26" = "90+","27" = "90+",
                                  "28" = "90+","29" = "90+","30" = "90+"))

ensanut<-read.csv("Bases/ensanut2020.csv")
load("Bases/df_mx.rda")
df_mx$ENTIDAD_UM<-df_mx$CLAVE_ENT
df_mx$reg[df_mx$ENTIDAD_UM %in% c(2,3,18,25,26)]<-1
df_mx$reg[df_mx$ENTIDAD_UM %in% c(5,8,19,28)]<-2
df_mx$reg[df_mx$ENTIDAD_UM %in% c(6,14,16)]<-3
df_mx$reg[df_mx$ENTIDAD_UM %in% c(1,10,11,22,24,32)]<-4
df_mx$reg[df_mx$ENTIDAD_UM %in% c(13,29,30)]<-5
df_mx$reg[df_mx$ENTIDAD_UM %in% c(9)]<-6
df_mx$reg[df_mx$ENTIDAD_UM %in% c(15)]<-7
df_mx$reg[df_mx$ENTIDAD_UM %in% c(12,17,20,21)]<-8
df_mx$reg[df_mx$ENTIDAD_UM %in% c(4,7,27,23,31)]<-9
pop_mun<-df_mx %>% filter(AÑO==2020 & !(EDAD_QUIN%in%c("pobm_00_04","pobm_05_09","pobm_10_14","pobm_15_19")))%>%group_by(reg)%>%summarise(pop=sum(POB))
pop_state<-df_mx %>% filter(AÑO==2020 & !(EDAD_QUIN%in%c("pobm_00_04","pobm_05_09","pobm_10_14","pobm_15_19")))%>%group_by(CLAVE_ENT)%>%summarise(pop=sum(POB))
pop_state_2018<-df_mx %>% filter(AÑO==2018 & !(EDAD_QUIN%in%c("pobm_00_04","pobm_05_09","pobm_10_14","pobm_15_19")))%>%group_by(CLAVE_ENT)%>%summarise(pop=sum(POB))
names(pop_state_2018)<-c("CLAVE_ENT", "pop_2018")
ensanut$diabetes<-ifelse((ensanut$H0902A.x==1 | ensanut$H0902B==1 |ensanut$H0902C==1 |ensanut$H0902D==1 |
         ensanut$H0902E==1 |ensanut$H0902F==1 |ensanut$H0902G==1 |ensanut$H0902H==1)==T, 1, 0)
ensanut$diabetes[is.na(ensanut$diabetes)]<-0

pop_mun$reg<-factor(pop_mun$reg, labels = c("Pac?fico-Norte", "Frontera", "Pac?fico-Centro", "Centro-Norte", "Centro", "CDMX", "Estado de M?xico", "Pac?fico Sur", "Pen?nsula"), ordered = T)

ensanut$HB1AC.Valor<-as.numeric(str_replace(ensanut$HB1AC.Valor, ",", "."))
ensanut$HB1AC.Valor<-as.numeric(str_replace(ensanut$HB1AC.Valor, " ", "NA"))
ensanut1<-ensanut %>% filter(!is.na(HB1AC.Valor))
ensanut1$ponde_g20.x.x1<-as.numeric(str_replace(ensanut1$ponde_g20.x.x, ",", "."))
ensanut1$diabetes2<-ifelse(ensanut1$HB1AC.Valor>=6.5, 1, 0)
ensanut1$diabetes3<-ifelse(ensanut1$valor.GLU_SUERO>=126, 1, 0); ensanut1$diabetes3[is.na(ensanut1$diabetes3)]<-0
ensanut1$diabetes_fin<-ifelse((ensanut1$diabetes==1 | ensanut1$diabetes2==1 | ensanut1$diabetes3==1)==T, 1, 0)
ensanut1$diabetes_no_dx<-ifelse((ensanut1$diabetes==0 &((ensanut1$diabetes2==1 | ensanut1$diabetes3==1)==T))==T, 1, 0)
ensanut1$diabetes_control<-ifelse(ensanut1$HB1AC.Valor>=7.5, 1, 0)
ensanut1$diabetes_poor<-ifelse(ensanut1$HB1AC.Valor>=10, 1, 0)
ensanut1$early_diabetes<-ifelse((ensanut1$diabetes_fin+(ensanut1$Edad.y<=40))==2, 1, 0)
ensanut1$atencion_cronicas<-ifelse(ensanut1$H0402 %in% c(15,16,17,18,21,23),1,0); ensanut1$atencion_cronicas[is.na(ensanut1$atencion_cronicas)]<-0
ensanut1$atencion_cronicas_2sem<-ifelse(ensanut1$atencion_cronicas & ensanut1$H0403==1,1,0); ensanut1$atencion_cronicas_2sem[is.na(ensanut1$atencion_cronicas_2sem)]<-0
ensanut1$busqueda_atencion<-ifelse((ensanut1$H0404==1 & ensanut1$atencion_cronicas_2sem==1),1,0); ensanut1$busqueda_atencion[is.na(ensanut1$busqueda_atencion)]<-0
ensanut$an01_1<-as.numeric(str_replace(ensanut$an01_1, ",", "."))
ensanut$an01_1<-as.numeric(str_replace(ensanut$an01_1, " ", "NA"))
ensanut$an04_01<-as.numeric(str_replace(ensanut$an04_01, ",", "."))
ensanut$an04_01<-as.numeric(str_replace(ensanut$an04_01, " ", "NA"))
ensanut$imc<-ensanut$an01_1/((ensanut$an04_01/100)^2)
ensanut$obesidad<-ifelse(ensanut$imc>=30, 1, 0)
### ENSANUT design data
dstrat<-svydesign(id=ensanut1$FOLIO_I.y.y,strata=ensanut1$estrato.y.y, weights=ensanut1$ponde_g20.x.x1, data=ensanut1)
options(survey.lonely.psu="adjust")

#Shapes df
geom_mx <-  sf::st_read(dsn="shapes", layer="areas_geoestadisticas_estatales")  %>% sf::st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",stringsAsFactors=FALSE)
geom_mx<-ms_simplify(geom_mx, keep = 0.01, keep_shapes = T)
geom_mx$CVE_ENT_NUM<-as.numeric(geom_mx$CVE_ENT)
geom_mx$SUBREGION[geom_mx$CVE_ENT_NUM %in% c(2,3,18,25,26)]<-1
geom_mx$SUBREGION[geom_mx$CVE_ENT_NUM %in% c(5,8,19,28)]<-2
geom_mx$SUBREGION[geom_mx$CVE_ENT_NUM %in% c(6,14,16)]<-3
geom_mx$SUBREGION[geom_mx$CVE_ENT_NUM %in% c(1,10,11,22,24,32)]<-4
geom_mx$SUBREGION[geom_mx$CVE_ENT_NUM %in% c(13,29,30)]<-5
geom_mx$SUBREGION[geom_mx$CVE_ENT_NUM %in% c(9)]<-6
geom_mx$SUBREGION[geom_mx$CVE_ENT_NUM %in% c(15)]<-7
geom_mx$SUBREGION[geom_mx$CVE_ENT_NUM %in% c(12,17,20,21)]<-8
geom_mx$SUBREGION[geom_mx$CVE_ENT_NUM %in% c(4,7,27,23,31)]<-9
geom_df.mun <-  sf::st_read(dsn="shapes", layer="areas_geoestadisticas_municipales")  %>% sf::st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",stringsAsFactors=FALSE)
geom_df.mun$id<-paste0(str_pad(geom_df.mun$CVE_ENT, 2,pad = "0"),str_pad(geom_df.mun$CVE_MUN,3, pad="0"))
st_crs(adj_mort_state2$geometry) <- "+proj=utm +zone=30 +ellps=airy +units=km"
geo_data <- poly2nb(adj_mort_state2$geometry)
### ENSANUT 2018 ###
ensanut2018<-read.csv("Bases/ensanut2018.csv")

ensanut2018$diab_7<-ifelse(ensanut2018$VALOR_HB1AC>=7.5,1,0)
ensanut2018$diab_7[is.na(ensanut2018$diab_7)]<-0
ensanut2018$diab_10<-ifelse(ensanut2018$VALOR_HB1AC>=10,1,0)
ensanut2018$diab_10[is.na(ensanut2018$diab_10)]<-0
ensanut2018$diabetes2<-ifelse(ensanut2018$VALOR_HB1AC>=6.5, 1, 0)
ensanut2018$diabetes2[is.na(ensanut2018$diabetes2)]<-0
ensanut2018$diabetes3[is.na(ensanut2018$diabetes3)]<-0
ensanut2018$diabetes3<-ifelse(ensanut2018$VALOR_GLU_SUERO>=126, 1, 0); ensanut1$diabetes3[is.na(ensanut1$diabetes3)]<-0
ensanut2018$diabetes_fin<-ifelse(((ensanut2018$P5_1.y>=8 & ensanut2018$VALOR_GLU_SUERO>=126) |  (ensanut2018$P5_1.y<8 & ensanut2018$VALOR_GLU_SUERO>=200) | (ensanut2018$P3_1==1) |  (ensanut2018$VALOR_HB1AC>=6.5))==T, 1, 0)
ensanut2018$diabetes_fin[is.na(ensanut2018$diabetes_fin)]<-0
ensanut2018$diabetes_no_dx<-ifelse((ensanut2018$P7!=1 &((ensanut2018$diabetes2==1 | ensanut2018$diabetes3==1)==T))==T, 1, 0)
ensanut2018$diabetes_no_dx[is.na(ensanut2018$diabetes_no_dx)]<-0
ensanut2018_1<-ensanut2018 %>% filter(!is.na(VALOR_HB1AC))
ensanut2018$EDAD_DIABETES<-ensanut2018$P3_2;ensanut2018$EDAD_DIABETES<-na.tools::na.replace(ensanut2018$EDAD_DIABETES, ensanut2018$EDAD)
ensanut2018$early_diabetes<-ifelse((ensanut2018$diabetes_fin+(ensanut2018$EDAD_DIABETES<=40))==2,1,0)
ensanut2018$reg[ensanut2018$ENT.x %in% c(2,3,18,25,26)]<-1
ensanut2018$reg[ensanut2018$ENT.x %in% c(5,8,19,28)]<-2
ensanut2018$reg[ensanut2018$ENT.x %in% c(6,14,16)]<-3
ensanut2018$reg[ensanut2018$ENT.x %in% c(1,10,11,22,24,32)]<-4
ensanut2018$reg[ensanut2018$ENT.x %in% c(13,29,30)]<-5
ensanut2018$reg[ensanut2018$ENT.x %in% c(9)]<-6
ensanut2018$reg[ensanut2018$ENT.x %in% c(15)]<-7
ensanut2018$reg[ensanut2018$ENT.x %in% c(12,17,20,21)]<-8
ensanut2018$reg[ensanut2018$ENT.x %in% c(4,7,27,23,31)]<-9

ENSANUT_all <- svydesign(data=ensanut2018, id=~UPM, strata=~ESTRATO.x, weights=~F_20MAS, nest=TRUE)
ENSANUT_subset1<- subset(ENSANUT_all, !is.na(ensanut2018_1$VALOR_HB1AC))

densidad<-data.frame(CLAVE_ENT=c(1:32),densidad=c(253.9,52.8,10.8,16.1,20.8,130.0,75.6,15.1,
            6163.3,14.9,201.5,55.7,148.1,106.2,760.2,81.0,
            404.1,44.4,90.2,44.1,191.9,202.6,41.6,46.2,
            52.8,16.4,97.1,44.0,336.0,112.3,58.7,21.5))
simula_lee <- function(x, y, listw, nsim = nsim, zero.policy = NULL, na.action = na.fail) {
  
  if (deparse(substitute(na.action)) == "na.pass") 
    stop ("na.pass not permitted")
  na.act <- attr(na.action(cbind(x, y)), "na.action")
  x[na.act] <- NA
  y[na.act] <- NA
  x <- na.action(x)
  y <- na.action(y)
  if (!is.null(na.act)) {
    subset <- !(1:length(listw$neighbours) %in% na.act)
    listw <- subset(listw, subset, zero.policy = zero.policy)
  }
  n <- length(listw$neighbours)
  if ((n != length(x)) | (n != length(y))) 
    stop ("objects of different length")
  gamres <- suppressWarnings(nsim > gamma(n + 1))
  if (gamres) 
    stop ("nsim too large for this number of observations")
  if (nsim < 1) 
    stop ("nsim too small")
  xy <- data.frame(x, y)
  S2 <- sum((unlist(lapply(listw$weights, sum)))^2)
  
  lee_boot <- function(var, i, ...) {
    return(spdep::lee(x = var[i, 1], y = var[i, 2], ...)$localL)
  }
  
  res <- boot::boot(xy, statistic = lee_boot, R = nsim, sim = "permutation", 
                    listw = listw, n = n, S2 = S2, zero.policy = zero.policy)
}

#### Datos COVID DGE ####
covid <- read.csv(unz("Bases/datos_abiertos_covid19_31.12.2020.zip", "201231COVID19MEXICO.csv"),header=TRUE,sep = ",", encoding = "UTF-8")
covid$id<-paste0(str_pad(covid$ENTIDAD_RES, 2,pad = "0"),str_pad(covid$MUNICIPIO_RES,3, pad="0"))
covid1<-covid[,c(14:15,18:30,32:35)]
covid<-covid[,-c(14:15,18:30,32:35)]
covid1[covid1==2]<-0
covid1[covid1==97]<-NA;covid1[covid1==98]<-NA;covid1[covid1==99]<-NA
covid<-as.data.frame(cbind(covid, covid1))
covid$EMBARAZO[is.na(covid$EMBARAZO)]<-0
covid$FECHA_DEF[covid$FECHA_DEF=="9999-99-99"]<-NA
covid$FECHA_DEF<-as.Date(covid$FECHA_DEF)
covid$TIPO_PACIENTE[covid$TIPO_PACIENTE==1]<-0;covid$TIPO_PACIENTE[covid$TIPO_PACIENTE==2]<-1
covid$covid<-NULL; covid$covid[covid$CLASIFICACION_FINAL==3]<-1;covid$covid[covid$CLASIFICACION_FINAL!=3]<-0
covid$edad60<-NULL;covid$edad60[covid$EDAD>=60]<-1;covid$edad60[covid$EDAD<60]<-0
covid$edad40<-NULL;covid$edad40[covid$EDAD>=40]<-0;covid$edad40[covid$EDAD<40]<-1
covid$Mortalidad<-NULL; covid$Mortalidad[is.na(covid$FECHA_DEF)]<-0;covid$Mortalidad[is.na(covid$FECHA_DEF)==FALSE]<-1
covid1<- covid %>% filter(covid==1)
covid1$diabetes_40<-ifelse((covid1$DIABETES+covid1$edad40)==2,1,0)

#### ENSANUT prevalences by region 2018-2020 ####
### Diabetes prevalence
total<-svytotal(~ensanut1$diabetes_fin, dstrat, deff=TRUE)
totalCI<-total[1]+c(-1,1)*1.96*sqrt(752583)

prev<-svyratio(ensanut1$diabetes_fin, rep(1, nrow(ensanut1)),dstrat, level=0.95)
total_region<-svyby(ensanut1$diabetes_fin, by=~ensanut1$region_cv.y.y,dstrat, svytotal,level=0.95)
total_region$reg<- c("Pac?fico-Norte", "Frontera", "Pac?fico-Centro", "Centro-Norte", "Centro", "CDMX", "Estado de M?xico", "Pac?fico Sur", "Pen?nsula")
names(total_region)<-c("region", "diab_tot", "se", "reg")

total_region_prev<-svyby(ensanut1$diabetes_fin, by=~ensanut1$region_cv.y.y,dstrat, svymean,level=0.95)
total_region_prev$reg<-c("Pac?fico-Norte", "Frontera", "Pac?fico-Centro", "Centro-Norte", "Centro", "CDMX", "Estado de M?xico", "Pac?fico Sur", "Pen?nsula")
names(total_region_prev)<-c("region", "diab", "se", "reg")

total_region_2018<-svyby(ensanut2018$diabetes_fin, by=~ensanut2018$reg,ENSANUT_subset1, svymean,level=0.95)
total_region_2018$reg<-c("Pac?fico-Norte", "Frontera", "Pac?fico-Centro", "Centro-Norte", "Centro", "CDMX", "Estado de M?xico", "Pac?fico Sur", "Pen?nsula")
names(total_region_2018)<-c("region", "diab_2018", "se", "reg")

prev_region<-(total_region$V1/pop_mun$pop)*100

prev.CI<-prev$ratio+c(-1,1)*1.96*sqrt(prev$var)
c(prev$ratio, prev.CI)*100

prev2<-svyratio(ensanut1$diabetes_no_dx,rep(1, nrow(ensanut1)),dstrat, level=0.95)
prev.CI2<-prev2$ratio+c(-1,1)*1.96*sqrt(prev2$var)
c(prev2$ratio, prev.CI2)*100

### Undiagnosed diabetes per region ###

prev3<-svyratio(ensanut1$diabetes_no_dx, rep(1, nrow(ensanut1)),dstrat, level=0.95)
total_region2<-svyby(ensanut1$diabetes_no_dx, by=~ensanut1$region_cv.y.y,dstrat, svymean,level=0.95)
total_region2$reg<-c("Pac?fico-Norte", "Frontera", "Pac?fico-Centro", "Centro-Norte", "Centro", "CDMX", "Estado de M?xico", "Pac?fico Sur", "Pen?nsula")
names(total_region2)<-c("region", "diab_no", "se", "reg")

total_region2_2018<-svyby(ensanut2018$diabetes_no_dx, by=~ensanut2018$reg,ENSANUT_subset1, svymean,level=0.95)
total_region2_2018$reg<-c("Pac?fico-Norte", "Frontera", "Pac?fico-Centro", "Centro-Norte", "Centro", "CDMX", "Estado de M?xico", "Pac?fico Sur", "Pen?nsula")
names(total_region2_2018)<-c("region", "diab_no_2018", "se", "reg")

### Glycemic control ###

prev4<-svyratio(ensanut1$diabetes_control, rep(1, nrow(ensanut1)),dstrat, level=0.95)
total_region3<-svyby(ensanut1$diabetes_control, by=~ensanut1$region_cv.y.y,dstrat, svymean,level=0.95)
total_region3$reg<-c("Pac?fico-Norte", "Frontera", "Pac?fico-Centro", "Centro-Norte", "Centro", "CDMX", "Estado de M?xico", "Pac?fico Sur", "Pen?nsula")
names(total_region3)<-c("region", "diab_7", "se", "reg")

total_region3_2018<-svyby(ensanut2018$diab_7, by=~ensanut2018$reg,ENSANUT_subset1, svymean,level=0.95)
total_region3_2018$reg<-c("Pac?fico-Norte", "Frontera", "Pac?fico-Centro", "Centro-Norte", "Centro", "CDMX", "Estado de M?xico", "Pac?fico Sur", "Pen?nsula")
names(total_region3_2018)<-c("region", "diab_7_2018", "se", "reg")

### Glycemic control 10 ###

prev5<-svyratio(ensanut1$diabetes_poor, rep(1, nrow(ensanut1)),dstrat, level=0.95)
total_region4<-svyby(ensanut1$diabetes_poor, by=~ensanut1$region_cv.y.y,dstrat, svymean,level=0.95)
total_region4$reg<-c("Pac?fico-Norte", "Frontera", "Pac?fico-Centro", "Centro-Norte", "Centro", "CDMX", "Estado de M?xico", "Pac?fico Sur", "Pen?nsula")
names(total_region4)<-c("region", "diab_10", "se", "reg")

total_region4_2018<-svyby(ensanut2018$diab_10, by=~ensanut2018$reg,ENSANUT_subset1, svymean,level=0.95)
total_region4_2018$reg<-c("Pac?fico-Norte", "Frontera", "Pac?fico-Centro", "Centro-Norte", "Centro", "CDMX", "Estado de M?xico", "Pac?fico Sur", "Pen?nsula")
names(total_region4_2018)<-c("region", "diab_10_2018", "se", "reg")

### Early onset diabetes ###
prev6<-svyratio(ensanut1$early_diabetes, rep(1, nrow(ensanut1)),dstrat, level=0.95)
total_region6<-svyby(ensanut1$early_diabetes, by=~ensanut1$region_cv.y.y,dstrat, svymean,level=0.95)
total_region6$reg<-c("Pac?fico-Norte", "Frontera", "Pac?fico-Centro", "Centro-Norte", "Centro", "CDMX", "Estado de M?xico", "Pac?fico Sur", "Pen?nsula")
names(total_region6)<-c("region", "diab_early", "se", "reg")

total_region5_2018<-svyby(ensanut2018$early_diabetes, by=~ensanut2018$reg,ENSANUT_subset1, svymean,level=0.95)
total_region5_2018$reg<-c("Pac?fico-Norte", "Frontera", "Pac?fico-Centro", "Centro-Norte", "Centro", "CDMX", "Estado de M?xico", "Pac?fico Sur", "Pen?nsula")
names(total_region5_2018)<-c("region", "diab_early_2018", "se", "reg")

### COVID Seroprevalence ###
ensanut2<-ensanut %>% filter(!is.na(valor))
ensanut2$ponde_g20.y<-as.numeric(str_replace(ensanut2$ponde_g20.y, ",", "."))
dstrat2<-svydesign(id=ensanut2$FOLIO_I.x.x,strata=ensanut2$estrato.x.x, weights=ensanut2$ponde_g20.y, data=ensanut2)
ensanut2$covid<-ifelse(ensanut2$valor==1, 1,0)
ensanut2$covid[is.na(ensanut2$covid)]<-0
total_covid_region<-svyby(ensanut2$covid, by=~ensanut2$region_cv.x.x,dstrat2, svymean,level=0.95)
total_covid_region$reg<-total_covid_region$`ensanut2$region_cv.x.x`
total_covid_region$reg<-c("Pac?fico-Norte", "Frontera", "Pac?fico-Centro", "Centro-Norte", "Centro", "CDMX", "Estado de M?xico", "Pac?fico Sur", "Pen?nsula")
names(total_covid_region)<-c("region", "covid", "se", "reg")

#### ENSANUT prevalences by state 2018-2020 ####
### Glycemic control State###
total_region7<-svyby(ensanut1$diabetes_control, by=~ensanut1$ENTIDAD.x.x,dstrat, svymean,level=0.95)
names(total_region7)<-c("CLAVE_ENT", "diab_7", "se")
total_region7$diab_7[total_region7$diab_7==0]<-prev2$ratio

### Undiagnosed diabetes state ###
total_region8<-svyby(ensanut1$diabetes_no_dx, by=~ensanut1$ENTIDAD.x.x,dstrat, svymean,level=0.95)
names(total_region8)<-c("CLAVE_ENT", "undx_diab", "se")
total_region8$undx_diab[total_region8$undx_diab==0]<-prev3$ratio
### Glycemic control State ???10###
total_region9<-svyby(ensanut1$diabetes_poor, by=~ensanut1$ENTIDAD.x.x,dstrat, svymean,level=0.95)
names(total_region9)<-c("CLAVE_ENT", "diab_10", "se")
total_region9$diab_10[total_region9$diab_10==0]<-prev5$ratio


dstrat<-svydesign(id=ensanut1$FOLIO_I.y.y,strata=ensanut1$estrato.y.y, weights=ensanut1$ponde_g20.x.x1, data=ensanut1)
diab<-subset(dstrat, diabetes_fin==1)
hba1c_median<-svyby(diab$variables$HB1AC.Valor, by=~diab$variables$ENTIDAD.x.x,design=diab, svymean,level=0.95)
names(hba1c_median)<-c("CLAVE_ENT", "hba1c_med", "se")

### COVID Seroprevalence ###
ensanut2<-ensanut %>% filter(!is.na(valor))
ensanut2$ponde_g20.y<-as.numeric(str_replace(ensanut2$ponde_g20.y, ",", "."))
dstrat2<-svydesign(id=ensanut2$FOLIO_I.x.x,strata=ensanut2$estrato.x.x, weights=ensanut2$ponde_g20.y, data=ensanut2)
ensanut2$covid<-ifelse(ensanut2$valor==1, 1,0)
ensanut2$covid[is.na(ensanut2$covid)]<-0
total_covid_state<-svyby(ensanut2$covid, by=~ensanut2$ENTIDAD.x.x,dstrat2, svymean,level=0.95)
names(total_covid_state)<-c("CLAVE_ENT", "covid", "se")

### Glycemic control State 2018###
total_2018<-svyby(ensanut2018$diab_7, by=~ensanut2018$ENT.y,ENSANUT_subset1, svymean,level=0.95)
names(total_2018)<-c("CLAVE_ENT", "diab_7_2018", "se")

### Glycemic control 10 State 2018 ###
total_2018_10<-svyby(ensanut2018$diab_10, by=~ensanut2018$ENT.y,ENSANUT_subset1, svymean,level=0.95)
names(total_2018_10)<-c("CLAVE_ENT", "diab_10_2018", "se")

### Diabetes prevalence 2018 ##
prev_diab_2018<-svymean(ensanut2018$diabetes_fin,ENSANUT_subset1, level=0.95)
total_2018_no<-svyby(ensanut2018$diabetes_no_dx, by=~ENT.y,ENSANUT_subset1, svymean,level=0.95)
names(total_2018_no)<-c("CLAVE_ENT", "diab_2018_no", "se")

total_2020_no<-svyby(ensanut1$diabetes_no_dx, by=~ensanut1$ENTIDAD.x.x,dstrat, svymean,level=0.95)
names(total_2020_no)<-c("CLAVE_ENT", "diab_2020_no", "se")
total_2020_no$diab_2020_no[total_2020_no$diab_2020_no==0]<-prev3$ratio

total_2018_diab<-svyby(ensanut2018$diabetes_fin, by=~ENT.y,ENSANUT_subset1, svymean,level=0.95)
names(total_2018_diab)<-c("CLAVE_ENT", "diab_2018", "se")

#### Excess mortality dataset ####
#Population datasets
Pob_conapo1<- readxl::read_xlsx("Bases/base_municipios_final_datos_01.xlsx")
Pob_conapo2<- readxl::read_xlsx("Bases/base_municipios_final_datos_02.xlsx")
Pob_conapo<- rbind(Pob_conapo1, Pob_conapo2)
Pob_conapo$id<-str_pad(Pob_conapo$CLAVE,5, pad="0")
Pob_conapo.2<-Pob_conapo
Pob_conapo<-Pob_conapo
Pob_conapo$reg[Pob_conapo$CLAVE_ENT %in% c(2,3,18,25,26)]<-1
Pob_conapo$reg[Pob_conapo$CLAVE_ENT %in% c(5,8,19,28)]<-2
Pob_conapo$reg[Pob_conapo$CLAVE_ENT %in% c(6,14,16)]<-3
Pob_conapo$reg[Pob_conapo$CLAVE_ENT %in% c(1,10,11,22,24,32)]<-4
Pob_conapo$reg[Pob_conapo$CLAVE_ENT %in% c(13,29,30)]<-5
Pob_conapo$reg[Pob_conapo$CLAVE_ENT %in% c(9)]<-6
Pob_conapo$reg[Pob_conapo$CLAVE_ENT %in% c(15)]<-7
Pob_conapo$reg[Pob_conapo$CLAVE_ENT %in% c(12,17,20,21)]<-8
Pob_conapo$reg[Pob_conapo$CLAVE_ENT %in% c(4,7,27,23,31)]<-9
Pob_conapo$reg<-factor(Pob_conapo$reg, labels = c("Pac?fico-Norte", "Frontera", "Pac?fico-Centro", "Centro-Norte", "Centro", "CDMX", "Estado de M?xico", "Pac?fico Sur", "Pen?nsula"), ordered=T)
Pob_conapo$reg<-as.character(Pob_conapo$reg)
Pob_conapo<- Pob_conapo %>% filter(!(EDAD_QUIN %in% c("pobm_00_04", "pobm_05_09", "pobm_10_14", "pobm_15_19"))) %>%
  mutate(EDAD_QUIN=dplyr::recode(EDAD_QUIN, "pobm_20_24" = "20 a 24 AÑOs", "pobm_25_29" = "25 a 29 AÑOs",
                                 "pobm_30_34" = "30 a 34 AÑOs", "pobm_35_39" = "35 a 39 AÑOs",
                                 "pobm_40_44" = "40 a 44 AÑOs", "pobm_45_49" = "45 a 49 AÑOs",
                                 "pobm_50_54" = "50 a 54 AÑOs", "pobm_55_59" = "55 a 59 AÑOs",
                                 "pobm_60_64" = "60 a 64 AÑOs", "pobm_65_mm" = "65+ AÑOs"))

pop2020<-Pob_conapo %>% filter(Year==2020) %>% summarise(pop=sum(POB))
pop2019<-Pob_conapo %>% filter(Year==2019) %>% summarise(pop=sum(POB))
pop2018<-Pob_conapo %>% filter(Year==2018) %>% summarise(pop=sum(POB))
pop2017<-Pob_conapo %>% filter(Year==2017) %>% summarise(pop=sum(POB))

mort2020<-mort_2020 %>% filter(mes_ocurr!=99 & edad_agru<9 & ent_resid<33) %>%
  group_by(ent_resid, EDAD_QUIN)%>%summarise(Total_2020=n()) 
mort2019<-mort_2019 %>% filter(mes_ocurr!=99 & edad_agru<9 & ent_resid<33) %>%
  group_by(ent_resid, EDAD_QUIN)%>%summarise(Total_2019=n()) 
mort2018<-mort_2018 %>% filter(MES_OCURR!=99 & EDAD_AGRU<9 & ENT_RESID<33) %>%
  group_by(ENT_RESID, EDAD_QUIN)%>%summarise(Total_2018=n()) 
names(mort2018)<-c("ent_resid",  "EDAD_QUIN", "Total_2018")
mort2017<-mort_2017 %>% filter(MES_OCURR!=99 & EDAD_AGRU<9 & ENT_RESID<33) %>%
  group_by(ENT_RESID, EDAD_QUIN)%>%summarise(Total_2017=n()) 
names(mort2017)<-c("ent_resid", "EDAD_QUIN", "Total_2017")
mort1<-mort2020 %>% left_join(mort2019, by=c("ent_resid", "EDAD_QUIN")) %>%
  left_join(mort2018, by=c("ent_resid", "EDAD_QUIN")) %>% left_join(mort2017, by=c("ent_resid", "EDAD_QUIN"))
mort1$ent_resid<-as.numeric(mort1$ent_resid)
mort1$reg[mort1$ent_resid %in% c(2,3,18,25,26)]<-1
mort1$reg[mort1$ent_resid %in% c(5,8,19,28)]<-2
mort1$reg[mort1$ent_resid %in% c(6,14,16)]<-3
mort1$reg[mort1$ent_resid %in% c(1,10,11,22,24,32)]<-4
mort1$reg[mort1$ent_resid %in% c(13,29,30)]<-5
mort1$reg[mort1$ent_resid %in% c(9)]<-6
mort1$reg[mort1$ent_resid %in% c(15)]<-7
mort1$reg[mort1$ent_resid %in% c(12,17,20,21)]<-8
mort1$reg[mort1$ent_resid %in% c(4,7,27,23,31)]<-9
mort1$reg<-factor(mort1$reg, labels = c("Pac?fico-Norte", "Frontera", "Pac?fico-Centro", "Centro-Norte", "Centro", "CDMX", "Estado de M?xico", "Pac?fico Sur", "Pen?nsula"), ordered = T)
mort1$reg<-as.character(mort1$reg)
names(mort1)<-c("CLAVE_ENT", "EDAD_QUIN", "Total_2020", "Total_2019", "Total_2018", "Total_2017", "reg")
names(mort1)
#### Rates of excess diabetes mortality in Mexico ####
### By year ###
mort_0<- mort_2020 %>% dplyr::select(mes_ocurr, anio_ocur,ICD_10_1.CODE) %>%  filter(mes_ocurr!=99) %>% 
  mutate(ICD_10_1.CODE=factor(ICD_10_1.CODE, labels = c("Type 1 diabetes", "Type 2 diabetes", "Other diabetes types","Other diabetes types","Other diabetes types")))%>%
  summarise(deaths=(n()/83757486)*100000, deaths_n=n()) 
mort_1<- mort_2019 %>% dplyr::select(mes_ocurr, anio_ocur,ICD_10_1.CODE)%>%  filter(mes_ocurr!=99) %>% 
  mutate(ICD_10_1.CODE=factor(ICD_10_1.CODE, labels = c("Type 1 diabetes", "Type 2 diabetes", "Other diabetes types","Other diabetes types","Other diabetes types")))%>%
  summarise(deaths=(n()/82418624)*100000, deaths_n=n())
mort_2<- mort_2018 %>% dplyr::select(MES_OCURR, ANIO_OCUR,ICD_10_1.CODE)%>%  filter(MES_OCURR!=99) %>% 
  mutate(ICD_10_1.CODE=factor(ICD_10_1.CODE, labels = c("Type 1 diabetes", "Type 2 diabetes", "Other diabetes types","Other diabetes types","Other diabetes types")))%>%
  summarise(deaths=(n()/81060179)*100000, deaths_n=n())
mort_3<- mort_2017 %>% dplyr::select(MES_OCURR, ANIO_OCUR,ICD_10_1.CODE)%>%  filter(MES_OCURR!=99) %>% 
  mutate(ICD_10_1.CODE=factor(ICD_10_1.CODE, labels = c("Type 1 diabetes", "Type 2 diabetes", "Other diabetes types","Other diabetes types","Other diabetes types")))%>%
  summarise(deaths=(n()/79678254)*100000, deaths_n=n())
mort_fin<-rbind(mort_0, mort_1, mort_2, mort_3)
mort_fin

mort_prom<-rbind(mort_1, mort_2, mort_3)
mort_prom<-cbind(deaths=mean(mort_prom$deaths), deaths_n=mean(mort_prom$deaths_n))
mort_prom<-rbind(mort_0, mort_prom)

### By cause ###
mort_0<- mort_2020 %>% dplyr::select(mes_ocurr, anio_ocur,ICD_10_1.CODE) %>%  filter(mes_ocurr!=99) %>% 
  mutate(ICD_10_1.CODE=factor(ICD_10_1.CODE, labels = c("Type 1 diabetes", "Type 2 diabetes", "Other diabetes types","Other diabetes types","Other diabetes types")))%>%
  group_by(ICD_10_1.CODE)%>%summarise(deaths=(n()/83757486)*100000, deaths_n=n()) 
mort_1<- mort_2019 %>% dplyr::select(mes_ocurr, anio_ocur,ICD_10_1.CODE)%>%  filter(mes_ocurr!=99) %>% 
  mutate(ICD_10_1.CODE=factor(ICD_10_1.CODE, labels = c("Type 1 diabetes", "Type 2 diabetes", "Other diabetes types","Other diabetes types","Other diabetes types")))%>%
  group_by(ICD_10_1.CODE)%>%summarise(deaths=(n()/82418624)*100000, deaths_n=n())
mort_2<- mort_2018 %>% dplyr::select(MES_OCURR, ANIO_OCUR,ICD_10_1.CODE)%>%  filter(MES_OCURR!=99) %>% 
  mutate(ICD_10_1.CODE=factor(ICD_10_1.CODE, labels = c("Type 1 diabetes", "Type 2 diabetes", "Other diabetes types","Other diabetes types","Other diabetes types")))%>%
  group_by(ICD_10_1.CODE)%>%summarise(deaths=(n()/81060179)*100000, deaths_n=n())
names(mort_2)<-c("ICD_10_1.CODE","deaths", "deaths_n")
mort_3<- mort_2017 %>% dplyr::select(MES_OCURR, ANIO_OCUR,ICD_10_1.CODE)%>%  filter(MES_OCURR!=99) %>% 
  mutate(ICD_10_1.CODE=factor(ICD_10_1.CODE, labels = c("Type 1 diabetes", "Type 2 diabetes", "Other diabetes types","Other diabetes types","Other diabetes types")))%>%
  group_by(ICD_10_1.CODE)%>%summarise(deaths=(n()/79678254)*100000, deaths_n=n())
names(mort3)<-c("ICD_10_1.CODE","deaths", "deaths_n")
mort_fin<-rbind(mort_0, mort_1, mort_2, mort_3)
mort_fin

mort_prom<-rbind(mort_1, mort_2, mort_3)
mort_prom<-mort_prom %>% group_by(ICD_10_1.CODE) %>%
  summarise(deaths=mean(deaths), deaths_n=mean(deaths_n))
mort_prom<-rbind(mort_0, mort_prom)
## By secondary cause ###

mort_0<- mort_2020 %>% dplyr::select(mes_ocurr, anio_ocur,cause) %>%  filter(mes_ocurr!=99) %>% 
  group_by(cause)%>%summarise(deaths=(n()/83757486)*100000, deaths_n=n()) 
mort_1<- mort_2019 %>% dplyr::select(mes_ocurr, anio_ocur,cause)%>%  filter(mes_ocurr!=99) %>% 
  group_by(cause)%>%summarise(deaths=(n()/82418624)*100000, deaths_n=n())
names(mort_1)<-c("cause2","deaths2", "deaths_n2")
mort_2<- mort_2018 %>% dplyr::select(MES_OCURR, ANIO_OCUR,cause)%>%  filter(MES_OCURR!=99) %>% 
  group_by(cause)%>%summarise(deaths=(n()/81060179)*100000, deaths_n=n())
names(mort_2)<-c("cause3","deaths3", "deaths_n3")
mort_fin<-cbind(mort_0, mort_1, mort_2)
mort_fin$excess<-mort_fin$deaths-((mort_fin$deaths2+mort_fin$deaths3)/2)
mort_fin$excess_prc<-(mort_fin$deaths-((mort_fin$deaths2+mort_fin$deaths3)/2))/((mort_fin$deaths2+mort_fin$deaths3)/2)

## By place of death
mort_0<- mort_2020 %>% filter(sitio_ocur!=99)%>%
  mutate(amb=ifelse(sitio_ocur %in% c(10:12),1,0))%>%
  group_by(amb)%>%
  summarise(deaths=(n()/83757486)*100000, deaths_n=n()) 
mort_1<- mort_2019 %>%  filter(sitio_ocur!=99)%>%
  mutate(amb=ifelse(sitio_ocur %in% c(10:12),1,0))%>%
  group_by(amb)%>%
  summarise(deaths=(n()/82418624)*100000, deaths_n=n())
mort_2<- mort_2018 %>%  filter(SITIO_OCUR!=99)%>%
  mutate(amb=ifelse(SITIO_OCUR %in% c(10:12),1,0))%>%
  group_by(amb)%>%
  summarise(deaths=(n()/81060179)*100000, deaths_n=n())
mort_3<- mort_2017 %>%  filter(SITIO_OCUR!=99)%>%
  mutate(amb=ifelse(SITIO_OCUR %in% c(10:12),1,0))%>%
  group_by(amb)%>%
  summarise(deaths=(n()/79678254)*100000, deaths_n=n())

mort_prom<-rbind(mort_1, mort_2, mort_3)
mort_prom<-mort_prom %>% group_by(amb) %>% 
  summarise(deaths=mean(deaths), deaths_n=mean(deaths_n))
mort_prom$year<-c("2017-2019")
mort_0$year<-c("2020")
mort_prom<-rbind(mort_0, mort_prom)
mort_prom$amb<-factor(mort_prom$amb, labels = c("In-hospital", "Out-of-hospital"))
mort_prom


#### Age adjusted mortality per year ####
#Estimadores poblacionales por edad
names(Pob_conapo)[7]<-c("Year")
Pob.grupos.edad.estado.2017<- Pob_conapo %>% 
  filter(Year == "2017") %>%
  group_by(reg, EDAD_QUIN) %>%
  tally(POB) %>% 
  rename(POB = n) %>% 
  group_by(reg, EDAD_QUIN) %>% 
  summarise(POB = sum(POB))

Pob.grupos.edad.total.2017<- Pob_conapo %>% 
  filter(Year == 2017) %>% 
  group_by(EDAD_QUIN) %>% 
  tally(POB) %>%
  rename(POB = n)

Pob.grupos.edad.total.2018<- Pob_conapo %>% 
  filter(Year == 2018) %>% 
  group_by(EDAD_QUIN) %>% 
  tally(POB) %>%
  rename(POB = n)

Pob.grupos.edad.estado.2018<- Pob_conapo %>% 
  filter(Year == 2018) %>%
  group_by(reg, EDAD_QUIN) %>%
  tally(POB) %>% 
  rename(POB = n) %>% 
  group_by(reg, EDAD_QUIN) %>% 
  summarise(POB = sum(POB))

Pob.grupos.edad.total.2019<- Pob_conapo %>% 
  filter(Year == 2019) %>% 
  group_by(EDAD_QUIN) %>% 
  tally(POB) %>%
  rename(POB = n)

Pob.grupos.edad.estado.2019<- Pob_conapo %>% 
  filter(Year == 2019) %>%
  group_by(reg, EDAD_QUIN) %>%
  tally(POB) %>% 
  rename(POB = n) %>% 
  group_by(reg, EDAD_QUIN) %>% 
  summarise(POB = sum(POB))

Pob.grupos.edad.estado.2020<- Pob_conapo %>% 
  filter(Year == 2020) %>%
  group_by(reg, EDAD_QUIN) %>%
  tally(POB) %>% 
  rename(POB = n) %>% 
  group_by(reg, EDAD_QUIN) %>% 
  summarise(POB = sum(POB))

Pob.grupos.edad.total.2020<- Pob_conapo %>% 
  filter(Year == 2020) %>% 
  group_by(EDAD_QUIN) %>% 
  tally(POB) %>%
  rename(POB = n)

adj_mort_2017<-mort1 %>% group_by(reg, EDAD_QUIN) %>% 
  summarise(total_20_17=sum(Total_2017)) %>%
  left_join(Pob.grupos.edad.estado.2017, by =c("reg","EDAD_QUIN"))%>%
  left_join(Pob.grupos.edad.total.2017,by=c("EDAD_QUIN"))%>% 
  mutate(DEF_DIAB = (total_20_17/POB.x)*POB.y)%>% 
  group_by(reg) %>% 
  summarise(DEF_ESPER_DIAB_17=sum(DEF_DIAB,na.rm = T))%>%
  group_by(reg)%>% 
  mutate(DIAB_DEATHS_17 = (DEF_ESPER_DIAB_17/79678254)*100000)

adj_mort_2018<-mort1 %>% group_by(reg, EDAD_QUIN) %>% 
  summarise(total_20_2018=sum(Total_2018)) %>%
  left_join(Pob.grupos.edad.estado.2018, by =c("reg","EDAD_QUIN"))%>%
  left_join(Pob.grupos.edad.total.2018,by=c("EDAD_QUIN"))%>% 
  mutate(DEF_DIAB = (total_20_2018/POB.x)*POB.y)%>% 
  group_by(reg) %>% 
  summarise(DEF_ESPER_DIAB_18=sum(DEF_DIAB,na.rm = T))%>%
  group_by(reg)%>% 
  mutate(DIAB_DEATHS_18 = (DEF_ESPER_DIAB_18/81060179)*100000)

adj_mort_2019<-mort1 %>% group_by(reg, EDAD_QUIN) %>% 
  summarise(total_20_2019=sum(Total_2019)) %>%
  left_join(Pob.grupos.edad.estado.2019, by =c("reg","EDAD_QUIN"))%>%
  left_join(Pob.grupos.edad.total.2019,by=c("EDAD_QUIN"))%>% 
  mutate(DEF_DIAB = (total_20_2019/POB.x)*POB.y)%>% 
  group_by(reg) %>% 
  summarise(DEF_ESPER_DIAB_19=sum(DEF_DIAB,na.rm = T))%>%
  group_by(reg)%>% 
  mutate(DIAB_DEATHS_19 = (DEF_ESPER_DIAB_19/82418624)*100000)

adj_mort_2020<-mort1 %>% group_by(reg, EDAD_QUIN) %>% 
  summarise(total_20_2020=sum(Total_2020)) %>%
  left_join(Pob.grupos.edad.estado.2020, by =c("reg","EDAD_QUIN"))%>%
  left_join(Pob.grupos.edad.total.2020,by=c("EDAD_QUIN"))%>% 
  mutate(DEF_DIAB = (total_20_2020/POB.x)*POB.y)%>% 
  group_by(reg) %>% 
  summarise(DEF_ESPER_DIAB_20=sum(DEF_DIAB,na.rm = T))%>%
  group_by(reg)%>% 
  mutate(DIAB_DEATHS_20 = (DEF_ESPER_DIAB_20/83757486)*100000)

### By state ###

#Estimadores poblacionales por edad
Pob.grupos.edad.estado.2017<- Pob_conapo %>% 
  filter(Year == 2017) %>%
  group_by(CLAVE_ENT, EDAD_QUIN) %>%
  tally(POB) %>% 
  rename(POB = n) %>% 
  group_by(CLAVE_ENT, EDAD_QUIN) %>% 
  summarise(POB = sum(POB))

Pob.grupos.edad.total.2017<- Pob_conapo %>% 
  filter(Year == 2017) %>% 
  group_by(EDAD_QUIN) %>% 
  tally(POB) %>%
  rename(POB = n)

Pob.grupos.edad.total.2018<- Pob_conapo %>% 
  filter(Year == 2018) %>% 
  group_by(EDAD_QUIN) %>% 
  tally(POB) %>%
  rename(POB = n)

Pob.grupos.edad.estado.2018<- Pob_conapo %>% 
  filter(Year == 2018) %>%
  group_by(CLAVE_ENT, EDAD_QUIN) %>%
  tally(POB) %>% 
  rename(POB = n) %>% 
  group_by(CLAVE_ENT, EDAD_QUIN) %>% 
  summarise(POB = sum(POB))

Pob.grupos.edad.total.2019<- Pob_conapo %>% 
  filter(Year == 2019) %>% 
  group_by(EDAD_QUIN) %>% 
  tally(POB) %>%
  rename(POB = n)

Pob.grupos.edad.estado.2019<- Pob_conapo %>% 
  filter(Year == 2019) %>%
  group_by(CLAVE_ENT, EDAD_QUIN) %>%
  tally(POB) %>% 
  rename(POB = n) %>% 
  group_by(CLAVE_ENT, EDAD_QUIN) %>% 
  summarise(POB = sum(POB))

Pob.grupos.edad.estado.2020<- Pob_conapo %>% 
  filter(Year == 2020) %>%
  group_by(CLAVE_ENT, EDAD_QUIN) %>%
  tally(POB) %>% 
  rename(POB = n) %>% 
  group_by(CLAVE_ENT, EDAD_QUIN) %>% 
  summarise(POB = sum(POB))

Pob.grupos.edad.total.2020<- Pob_conapo %>% 
  filter(Year == 2020) %>% 
  group_by(EDAD_QUIN) %>% 
  tally(POB) %>%
  rename(POB = n)

adj_mort_2017_state<-mort1 %>% group_by(CLAVE_ENT, EDAD_QUIN) %>% 
  summarise(total_20_17=sum(Total_2017)) %>%
  left_join(Pob.grupos.edad.estado.2017, by =c("CLAVE_ENT","EDAD_QUIN"))%>%
  left_join(Pob.grupos.edad.total.2017,by=c("EDAD_QUIN"))%>% 
  mutate(DEF_DIAB = (total_20_17/POB.x)*(POB.y))%>% 
  group_by(CLAVE_ENT) %>% 
  summarise(DEF_ESPER_DIAB_17=sum(DEF_DIAB,na.rm = T))%>%
  group_by(CLAVE_ENT)%>% 
  mutate(DIAB_DEATHS_17 = (DEF_ESPER_DIAB_17/79678254)*100000)

adj_mort_2018_state<-mort1 %>%  group_by(CLAVE_ENT, EDAD_QUIN) %>% 
  summarise(total_20_2018=sum(Total_2018)) %>%
  left_join(Pob.grupos.edad.estado.2018, by =c("CLAVE_ENT","EDAD_QUIN"))%>%
  left_join(Pob.grupos.edad.total.2018,by=c("EDAD_QUIN"))%>% 
  mutate(DEF_DIAB = (total_20_2018/POB.x)*POB.y)%>% 
  group_by(CLAVE_ENT) %>% 
  summarise(DEF_ESPER_DIAB_18=sum(DEF_DIAB,na.rm = T))%>%
  group_by(CLAVE_ENT)%>% 
  mutate(DIAB_DEATHS_18 = (DEF_ESPER_DIAB_18/81060179)*100000)

adj_mort_2019_state<-mort1 %>%  group_by(CLAVE_ENT, EDAD_QUIN) %>% 
  summarise(total_20_2019=sum(Total_2019)) %>%
  left_join(Pob.grupos.edad.estado.2019, by =c("CLAVE_ENT","EDAD_QUIN"))%>%
  left_join(Pob.grupos.edad.total.2019,by=c("EDAD_QUIN"))%>% 
  mutate(DEF_DIAB = (total_20_2019/POB.x)*POB.y)%>% 
  group_by(CLAVE_ENT) %>% 
  summarise(DEF_ESPER_DIAB_19=sum(DEF_DIAB,na.rm = T))%>%
  group_by(CLAVE_ENT)%>% 
  mutate(DIAB_DEATHS_19 = (DEF_ESPER_DIAB_19/82418624)*100000)

adj_mort_2020_state<-mort1 %>%  group_by(CLAVE_ENT, EDAD_QUIN) %>% 
  summarise(total_20_2020=sum(Total_2020)) %>%
  left_join(Pob.grupos.edad.estado.2020, by =c("CLAVE_ENT","EDAD_QUIN"))%>%
  left_join(Pob.grupos.edad.total.2020,by=c("EDAD_QUIN"))%>% 
  mutate(DEF_DIAB = (total_20_2020/POB.x)*POB.y)%>% 
  group_by(CLAVE_ENT) %>% 
  summarise(DEF_ESPER_DIAB_20=sum(DEF_DIAB,na.rm = T))%>%
  group_by(CLAVE_ENT)%>% 
  mutate(DIAB_DEATHS_20 = (DEF_ESPER_DIAB_20/83757486)*100000)

#### Age adjusted excess mortality dataset per state ####

irs<-read_xlsx("Bases/irs.xlsx") %>% dplyr::select(id, IRS, IRS_cat) %>% mutate(CLAVE_ENT=as.numeric(id))
covid1$DIABETES[is.na(covid1$DIABETES)]<-0
covid1$mort_diab<-ifelse((covid1$DIABETES+covid1$Mortalidad)==2,1,0)
covid1$mort_diab_40<-ifelse((covid1$diabetes_40+covid1$Mortalidad)==2,1,0)
covid1$hosp_diab<-ifelse((covid1$DIABETES+covid1$TIPO_PACIENTE)==2,1,0)
cases<- covid1 %>% group_by(ENTIDAD_RES) %>%
  summarise(covid_hosp=sum(TIPO_PACIENTE, na.rm=T), covid_diab=sum(DIABETES,na.rm=T),covid_mort_diab=sum(mort_diab, na.rm=T), covid_diab_40=sum(mort_diab_40, na.rm=T)) %>% 
  mutate(CLAVE_ENT=ENTIDAD_RES)%>% dplyr::select(CLAVE_ENT, covid_hosp, covid_diab,covid_mort_diab,covid_diab_40)
total_state<-svyby(ensanut1$diabetes_fin, by=~ensanut1$ENTIDAD.x.x,dstrat, svytotal,level=0.95)
names(total_state)<-c("CLAVE_ENT", "diab", "se")

adj_mort_state<-adj_mort_2017_state %>% left_join(adj_mort_2018_state, by="CLAVE_ENT") %>%
  left_join(adj_mort_2019_state, by="CLAVE_ENT") %>%left_join(adj_mort_2020_state, by="CLAVE_ENT") %>%
  mutate(avg=(DIAB_DEATHS_17+DIAB_DEATHS_18+DIAB_DEATHS_19)/3)%>%
  mutate(excess=DIAB_DEATHS_20-avg)%>% left_join(pop_state, by="CLAVE_ENT") %>% left_join(total_state, by="CLAVE_ENT") %>%
  mutate(prev_diab=round((diab/pop)*100,2)) %>%
  left_join(irs %>% dplyr::select(CLAVE_ENT, IRS, IRS_cat), by="CLAVE_ENT")  %>% left_join(cases, by="CLAVE_ENT") %>% 
  mutate(covid_diab=round((covid_diab/diab*100000),2), hosp=(covid_hosp/pop)*100000, mort_covid_diab=(covid_mort_diab/diab)*100000, mort_diab_40=(covid_diab_40/diab)*100000) %>%
  left_join(total_region7%>%dplyr::select(CLAVE_ENT, diab_7), by="CLAVE_ENT") %>%mutate(diab_7_prev=diab_7*100)%>%
  left_join(total_region9%>%dplyr::select(CLAVE_ENT, diab_10), by="CLAVE_ENT") %>%mutate(diab_10_prev=diab_10*100)%>%
  left_join(total_region8%>%dplyr::select(CLAVE_ENT, undx_diab), by="CLAVE_ENT") %>%mutate(diab_no=undx_diab*100)%>%
  left_join(total_2018%>%dplyr::select(CLAVE_ENT, diab_7_2018), by="CLAVE_ENT") %>%mutate(diab_7_prev_2018=diab_7_2018*100)%>%
  left_join(total_2018_10%>%dplyr::select(CLAVE_ENT, diab_10_2018), by="CLAVE_ENT") %>%mutate(diab_10_prev_2018=diab_10_2018*100)%>%
  left_join(total_2018_no%>%dplyr::select(CLAVE_ENT, diab_2018_no), by="CLAVE_ENT") %>%mutate(diab_2018_undx=diab_2018_no*100)%>%
  left_join(total_2020_no%>%dplyr::select(CLAVE_ENT, diab_2020_no), by="CLAVE_ENT") %>%mutate(diab_2020_undx=diab_2020_no*100)%>%
  left_join(total_2018_diab%>%dplyr::select(CLAVE_ENT, diab_2018), by="CLAVE_ENT") %>%mutate(diab_2018_prev=diab_2018*100)%>%
  left_join(total_covid_state%>%dplyr::select(CLAVE_ENT, covid), by="CLAVE_ENT") %>%mutate(covid_rate=covid*100)%>%
  left_join(hba1c_median%>%dplyr::select(CLAVE_ENT, hba1c_med), by="CLAVE_ENT") %>%
  left_join(densidad%>%dplyr::select(CLAVE_ENT, densidad), by="CLAVE_ENT") %>%
  mutate(delta_diab_7=diab_7_prev-diab_7_prev_2018, delta_diab_10=diab_10_prev-diab_10_prev_2018, delta_undx=diab_2020_undx-diab_2018_undx, delta_diab=prev_diab-diab_2018_prev) %>%
  dplyr::select(CLAVE_ENT, avg,prev_diab,excess,diab, pop,covid_diab,diab_7_prev,diab_7_prev_2018,diab_10_prev, diab_10_prev_2018,delta_diab_7,delta_diab_10,delta_undx,
                hosp,mort_covid_diab, diab_7_prev,diab_no, IRS,mort_diab_40,diab_2020_undx,delta_diab,covid_rate, densidad, covid_mort_diab, covid_hosp, IRS_cat, hba1c_med)

l0<-lm(IRS~log(densidad), data=adj_mort_state)
gvlma::gvlma(l0)
adj_mort_state$disli<-l0$residuals

adj_mort_state$expected=adj_mort_state$pop*(sum(adj_mort_state$excess)/sum(adj_mort_state$pop))
smth<-empbaysmooth(adj_mort_state$excess, adj_mort_state$expected)
adj_mort_state$excess_smooth<-smth$smthrr

adj_mort_state2<-adj_mort_state%>%mutate(CVE_ENT=paste0(str_pad(CLAVE_ENT, 2,pad = "0")))%>%
  left_join(geom_mx,by="CVE_ENT")

#### Figure 1: Global excess mortality per cause ####
mort_0<- mort_2020 %>% summarise(deaths=(n()/83757486)*100000, deaths_n=n()) 
mort_1<- mort_2019 %>% summarise(deaths=(n()/82418624)*100000, deaths_n=n())
mort_2<- mort_2018 %>% summarise(deaths=(n()/81060179)*100000, deaths_n=n())
mort_3<- mort_2017 %>% summarise(deaths=(n()/79678254)*100000, deaths_n=n())
mort_prom<-rbind(mort_1, mort_2, mort_3)
mort_prom<-mort_prom %>% summarise(deaths=mean(deaths), deaths_n=mean(deaths_n))
mort_prom$year<-c("2017-2019")
mort_0$year<-c("2020")
mort_prom1<-rbind(mort_0, mort_prom)
mort_prom1$ICD_10_1.CODE<-c("Diabetes overall")

## By place of death ##
mort_0<- mort_2020 %>% mutate(ICD_10_1.CODE=factor(ICD_10_1.CODE, labels = c("Type 1 diabetes", "Type 2 diabetes", "Other diabetes types","Other diabetes types","Other diabetes types")))%>%
  group_by(ICD_10_1.CODE)%>%
  summarise(deaths=(n()/83757486)*100000, deaths_n=n()) 
mort_1<- mort_2019 %>% mutate(ICD_10_1.CODE=factor(ICD_10_1.CODE, labels = c("Type 1 diabetes", "Type 2 diabetes", "Other diabetes types","Other diabetes types","Other diabetes types")))%>%
  group_by(ICD_10_1.CODE)%>%
  summarise(deaths=(n()/82418624)*100000, deaths_n=n())
mort_2<- mort_2018 %>% mutate(ICD_10_1.CODE=factor(ICD_10_1.CODE, labels = c("Type 1 diabetes", "Type 2 diabetes", "Other diabetes types","Other diabetes types","Other diabetes types")))%>%
  group_by(ICD_10_1.CODE)%>%
  summarise(deaths=(n()/81060179)*100000, deaths_n=n())
mort_3<- mort_2017 %>% mutate(ICD_10_1.CODE=factor(ICD_10_1.CODE, labels = c("Type 1 diabetes", "Type 2 diabetes", "Other diabetes types","Other diabetes types","Other diabetes types")))%>%
  group_by(ICD_10_1.CODE)%>%
  summarise(deaths=(n()/79678254)*100000, deaths_n=n())

mort_prom<-rbind(mort_1, mort_2, mort_3)
mort_prom<-mort_prom %>% group_by(ICD_10_1.CODE) %>% 
  summarise(deaths=mean(deaths), deaths_n=mean(deaths_n))
mort_prom$year<-c("2017-2019")
mort_0$year<-c("2020")
mort_prom<-rbind(mort_0, mort_prom)

mort_fin<-rbind(mort_prom1, mort_prom)

mort_fin1<-mort_fin %>%filter(ICD_10_1.CODE!="Other diabetes types")
mort_fin0<-mort_fin %>%filter(ICD_10_1.CODE=="Other diabetes types")
mort_fin1<-data.table(mort_fin1)

mort_fin1[mort_fin1$ICD_10_1.CODE %in% c("Diabetes overall", "Type 2 diabetes"),y_min := 0]
mort_fin1[mort_fin1$ICD_10_1.CODE %in% c("Diabetes overall", "Type 2 diabetes"), y_max :=200]
mort_fin1[mort_fin1$ICD_10_1.CODE %in% c("Type 1 diabetes"), y_min := 0]
mort_fin1[mort_fin1$ICD_10_1.CODE %in% c("Type 1 diabetes"), y_max :=10]

k1<-mort_fin1 %>% 
  mutate(across(ICD_10_1.CODE, factor, levels=c("Diabetes overall","Type 2 diabetes","Type 1 diabetes"))) %>%
  ggplot(aes(y=deaths, x=year,fill=year)) + geom_bar(stat="identity", position='dodge', color="black", width=0.6)+
  theme_pubclean()+ylab("Frequency (%)")+xlab("")+
  theme(axis.ticks.x = element_blank())+
  geom_text(aes(label=round(deaths,3)), vjust=2, color="white", size=3, position = position_dodge(0.9))+
  labs(fill="Period")+facet_wrap(~ICD_10_1.CODE, nrow = 1, ncol=4, scales="free_y")+scale_fill_jama()+
  ylab("Diabetes deaths per 100,000 inhabitants")+
  geom_blank(aes(y = y_min)) +
  geom_blank(aes(y = y_max))

o1<-mort_fin0 %>% 
  mutate(across(ICD_10_1.CODE, factor, levels=c("Diabetes overall","Type 2 diabetes","Type 1 diabetes", "Other diabetes types"))) %>%
  ggplot(aes(y=deaths, x=year,fill=year)) + geom_bar(stat="identity", position='dodge', color="black", width=0.6)+
  theme_pubclean()+ylab("Frequency (%)")+xlab("")+
  theme(axis.ticks.x = element_blank())+
  geom_text(aes(label=round(deaths,3)), vjust=2, color="white", size=3, position = position_dodge(0.9))+
  labs(fill="Period")+facet_wrap(~ICD_10_1.CODE, nrow = 1, ncol=4)+scale_fill_jama()+
  ylab("Diabetes deaths per 100,000 inhabitants")+scale_y_log10()


## By diabetes type ##
mort_0<- mort_2020 %>% dplyr::select(mes_ocurr, anio_ocur) %>%  filter(mes_ocurr!=99) %>% 
  group_by(anio_ocur,mes_ocurr)%>%summarise(deaths=(n()/83757486)*100000, deaths_n=n()) 
mort_1<- mort_2019 %>% dplyr::select(mes_ocurr, anio_ocur)%>%  filter(mes_ocurr!=99) %>% 
  group_by(anio_ocur,mes_ocurr)%>%summarise(deaths=(n()/82418624)*100000, deaths_n=n())
mort_2<- mort_2018 %>% dplyr::select(MES_OCURR, ANIO_OCUR)%>%  filter(MES_OCURR!=99) %>% 
  group_by(ANIO_OCUR,MES_OCURR)%>%summarise(deaths=(n()/81060179)*100000, deaths_n=n())
names(mort_2)<-c("anio_ocur","mes_ocurr","deaths", "deaths_n")
mort_3<- mort_2017 %>% dplyr::select(MES_OCURR, ANIO_OCUR)%>%  filter(MES_OCURR!=99) %>% 
  group_by(ANIO_OCUR,MES_OCURR)%>%summarise(deaths=(n()/79678254)*100000, deaths_n=n())
names(mort_3)<-c("anio_ocur","mes_ocurr","deaths", "deaths_n")
mort_ov<-rbind(mort_0, mort_1, mort_2, mort_3)
mort_ov$ICD_10_1.CODE<-c("Diabetes overall")


mort_0<- mort_2020 %>% dplyr::select(mes_ocurr, anio_ocur,ICD_10_1.CODE) %>%  filter(mes_ocurr!=99) %>% 
  mutate(ICD_10_1.CODE=factor(ICD_10_1.CODE, labels = c("Type 1 diabetes", "Type 2 diabetes", "Other diabetes types","Other diabetes types","Other diabetes types")))%>%
  group_by(anio_ocur,mes_ocurr, ICD_10_1.CODE)%>%summarise(deaths=(n()/83757486)*100000, deaths_n=n()) 
mort_1<- mort_2019 %>% dplyr::select(mes_ocurr, anio_ocur,ICD_10_1.CODE)%>%  filter(mes_ocurr!=99) %>% 
  mutate(ICD_10_1.CODE=factor(ICD_10_1.CODE, labels = c("Type 1 diabetes", "Type 2 diabetes", "Other diabetes types","Other diabetes types","Other diabetes types")))%>%
  group_by(anio_ocur,mes_ocurr,ICD_10_1.CODE)%>%summarise(deaths=(n()/82418624)*100000, deaths_n=n())
mort_2<- mort_2018 %>% dplyr::select(MES_OCURR, ANIO_OCUR,ICD_10_1.CODE)%>%  filter(MES_OCURR!=99) %>% 
  mutate(ICD_10_1.CODE=factor(ICD_10_1.CODE, labels = c("Type 1 diabetes", "Type 2 diabetes", "Other diabetes types","Other diabetes types","Other diabetes types")))%>%
  group_by(ANIO_OCUR,MES_OCURR,ICD_10_1.CODE)%>%summarise(deaths=(n()/81060179)*100000, deaths_n=n())
names(mort_2)<-c("anio_ocur","mes_ocurr", "ICD_10_1.CODE","deaths", "deaths_n")
mort_3<- mort_2017 %>% dplyr::select(MES_OCURR, ANIO_OCUR,ICD_10_1.CODE)%>%  filter(MES_OCURR!=99) %>% 
  mutate(ICD_10_1.CODE=factor(ICD_10_1.CODE, labels = c("Type 1 diabetes", "Type 2 diabetes", "Other diabetes types","Other diabetes types","Other diabetes types")))%>%
  group_by(ANIO_OCUR,MES_OCURR,ICD_10_1.CODE)%>%summarise(deaths=(n()/79678254)*100000, deaths_n=n())
names(mort_3)<-c("anio_ocur","mes_ocurr", "ICD_10_1.CODE","deaths", "deaths_n")
mort_fin<-rbind(mort_ov,mort_0, mort_1, mort_2, mort_3)

mort_fin1<-mort_fin %>% filter(ICD_10_1.CODE!="Other diabetes types")
mort_fin0<-mort_fin %>% filter(ICD_10_1.CODE=="Other diabetes types")

n1<-mort_fin1 %>% 
  mutate(across(ICD_10_1.CODE, factor, levels=c("Diabetes overall","Type 2 diabetes","Type 1 diabetes", "Other diabetes types"))) %>%
  ggplot(aes(x=mes_ocurr, y=deaths, col=factor(anio_ocur)))+
  geom_point()+geom_line(size=1)+theme_pubclean()+ylab("Diabetes deaths per 100,000 inhabitants")+
  xlab("Month of occurence")+labs(col="Year")+facet_wrap(~ICD_10_1.CODE, scales = "free", nrow=1, ncol=4)+scale_color_jama()+
  scale_x_continuous(breaks=seq(1,12,1),labels=paste0(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")))+
  theme(legend.position="top",axis.text.x=element_text(angle=90))

o2<-mort_fin0 %>% 
  mutate(across(ICD_10_1.CODE, factor, levels=c("Diabetes overall","Type 2 diabetes","Type 1 diabetes", "Other diabetes types"))) %>%
  ggplot(aes(x=mes_ocurr, y=deaths, col=factor(anio_ocur)))+
  geom_point()+geom_line(size=1)+theme_pubclean()+ylab("Diabetes deaths per 100,000 inhabitants")+
  xlab("Month of occurence")+labs(col="Year")+facet_wrap(~ICD_10_1.CODE, scales = "free", nrow=1, ncol=4)+scale_color_jama()+
  scale_x_continuous(breaks=seq(1,12,1),labels=paste0(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")))+
  theme(legend.position="top",axis.text.x=element_text(angle=90))

##By age group and diabetes type ##
mort_0<- mort_2020 %>% dplyr::select(mes_ocurr, anio_ocur, EDAD_QUIN2) %>%  filter(mes_ocurr!=99) %>% 
  group_by(anio_ocur,EDAD_QUIN2)%>%dplyr::summarise(deaths_n=n()) %>%mutate(year=rep("2020", n()))
mort_1<- mort_2019 %>% dplyr::select(mes_ocurr, anio_ocur, EDAD_QUIN2) %>%  filter(mes_ocurr!=99) %>% 
  group_by(anio_ocur,EDAD_QUIN2)%>%dplyr::summarise(deaths_n=n()) 
mort_2<- mort_2018 %>% dplyr::select(MES_OCURR, ANIO_OCUR,EDAD_QUIN2)%>%  filter(MES_OCURR!=99) %>% 
  group_by(ANIO_OCUR,EDAD_QUIN2)%>%summarise(deaths_n=n())
names(mort_2)<-c("anio_ocur","EDAD_QUIN2","deaths_n")
mort_3<- mort_2017 %>% dplyr::select(MES_OCURR, ANIO_OCUR,EDAD_QUIN2)%>%  filter(MES_OCURR!=99) %>% 
  group_by(ANIO_OCUR,EDAD_QUIN2)%>%summarise(deaths_n=n())
names(mort_3)<-c("anio_ocur","EDAD_QUIN2","deaths_n")

mort_prom<-rbind(mort_1, mort_2, mort_3)
mort_ov<-mort_prom %>% group_by(EDAD_QUIN2) %>%
  summarise(deaths_n=mean(deaths_n, na.rm=T))%>%mutate(year=rep("2017-2019", n()))
mort_ov<-rbind(mort_ov, mort_0[,-c(1)])
mort_ov$ICD_10_1.CODE<-c("Diabetes overall")

mort_0<- mort_2020 %>% dplyr::select(mes_ocurr, anio_ocur, EDAD_QUIN2,ICD_10_1.CODE) %>%  filter(mes_ocurr!=99) %>% 
  mutate(ICD_10_1.CODE=factor(ICD_10_1.CODE, labels = c("Type 1 diabetes", "Type 2 diabetes", "Other diabetes types","Other diabetes types","Other diabetes types")))%>%
  group_by(anio_ocur,ICD_10_1.CODE,EDAD_QUIN2)%>%dplyr::summarise(deaths_n=n()) %>%mutate(year=rep("2020", n()))
mort_1<- mort_2019 %>% dplyr::select(mes_ocurr, anio_ocur, EDAD_QUIN2,ICD_10_1.CODE) %>%  filter(mes_ocurr!=99) %>% 
  mutate(ICD_10_1.CODE=factor(ICD_10_1.CODE, labels = c("Type 1 diabetes", "Type 2 diabetes", "Other diabetes types","Other diabetes types","Other diabetes types")))%>%
  group_by(anio_ocur,ICD_10_1.CODE,EDAD_QUIN2)%>%dplyr::summarise(deaths_n=n()) 
mort_2<- mort_2018 %>% dplyr::select(MES_OCURR, ANIO_OCUR,EDAD_QUIN2,ICD_10_1.CODE)%>%  filter(MES_OCURR!=99) %>% 
  mutate(ICD_10_1.CODE=factor(ICD_10_1.CODE, labels = c("Type 1 diabetes", "Type 2 diabetes", "Other diabetes types","Other diabetes types","Other diabetes types")))%>%
  group_by(ANIO_OCUR,ICD_10_1.CODE,EDAD_QUIN2)%>%summarise(deaths_n=n())
names(mort_2)<-c("anio_ocur","ICD_10_1.CODE","EDAD_QUIN2","deaths_n")
mort_3<- mort_2017 %>% dplyr::select(MES_OCURR, ANIO_OCUR,EDAD_QUIN2,ICD_10_1.CODE)%>%  filter(MES_OCURR!=99) %>% 
  mutate(ICD_10_1.CODE=factor(ICD_10_1.CODE, labels = c("Type 1 diabetes", "Type 2 diabetes", "Other diabetes types","Other diabetes types","Other diabetes types")))%>%
  group_by(ANIO_OCUR,ICD_10_1.CODE,EDAD_QUIN2)%>%summarise(deaths_n=n())
names(mort_3)<-c("anio_ocur","ICD_10_1.CODE","EDAD_QUIN2","deaths_n")

mort_prom<-rbind(mort_1, mort_2, mort_3)
mort_prom2<-mort_prom %>% group_by(ICD_10_1.CODE, EDAD_QUIN2) %>%
  summarise(deaths_n=mean(deaths_n, na.rm=T))%>%mutate(year=rep("2017-2019", n()))

mort_age<-rbind(mort_prom2, mort_0[,-c(1)])
mort_age<-rbind(mort_ov,mort_age)
mort_age1<- mort_age %>% filter(ICD_10_1.CODE!="Other diabetes types")
mort_age0<- mort_age %>% filter(ICD_10_1.CODE=="Other diabetes types")
mort_age1<-data.table(mort_age1)

mort_age1[mort_age1$ICD_10_1.CODE %in% c("Diabetes overall", "Type 2 diabetes"),y_min := 0]
mort_age1[mort_age1$ICD_10_1.CODE %in% c("Diabetes overall", "Type 2 diabetes"), y_max :=21000]
mort_age1[mort_age1$ICD_10_1.CODE %in% c("Type 1 diabetes"), y_min := 0]
mort_age1[mort_age1$ICD_10_1.CODE %in% c("Type 1 diabetes"), y_max :=400]


n2 <- mort_age1 %>%
  mutate(across(ICD_10_1.CODE, factor, levels=c("Diabetes overall","Type 2 diabetes","Type 1 diabetes", "Other diabetes types"))) %>%
  arrange((EDAD_QUIN2))%>%
  ggplot(aes(x = EDAD_QUIN2, y = deaths_n, fill = year)) + 
  geom_bar(stat = "identity", position = "dodge") +
  theme_pubclean()+xlab("Age group (years)")+ylab("Overall number of diabetes deaths")+labs(fill="Period")+
  facet_wrap(~ICD_10_1.CODE, scales = "free", nrow = 1, ncol=4)+scale_fill_jama()+
  theme(axis.text.x=element_text(angle=90))+
  geom_blank(aes(y = y_min)) +
  geom_blank(aes(y = y_max))

o3 <- mort_age0 %>%
  mutate(across(ICD_10_1.CODE, factor, levels=c("Diabetes overall","Type 2 diabetes","Type 1 diabetes", "Other diabetes types"))) %>%
  arrange((EDAD_QUIN2))%>%
  ggplot(aes(x = EDAD_QUIN2, y = deaths_n, fill = year)) + 
  geom_bar(stat = "identity", position = "dodge") +
  theme_pubclean()+xlab("Age group (years)")+ylab("Overall number of diabetes deaths")+labs(fill="Period")+
  facet_wrap(~ICD_10_1.CODE, scales = "free", nrow = 1, ncol=4)+scale_fill_jama()+
  theme(axis.text.x=element_text(angle=90))

## By place of death ##
mort_0<- mort_2020 %>% filter(sitio_ocur!=99)%>%
  mutate(amb=ifelse(sitio_ocur %in% c(10:12),1,0))%>%
  group_by(amb)%>%
  summarise(deaths=(n()/83757486)*100000, deaths_n=n()) 
mort_1<- mort_2019 %>% filter(sitio_ocur!=99)%>%
  mutate(amb=ifelse(sitio_ocur %in% c(10:12),1,0))%>%
  group_by(amb)%>%
  summarise(deaths=(n()/82418624)*100000, deaths_n=n())
mort_2<- mort_2018 %>% filter(SITIO_OCUR!=99)%>%
  mutate(amb=ifelse(SITIO_OCUR %in% c(10:12),1,0))%>%
  group_by(amb)%>%
  summarise(deaths=(n()/81060179)*100000, deaths_n=n())
mort_3<- mort_2017 %>% filter(SITIO_OCUR!=99)%>%
  mutate(amb=ifelse(SITIO_OCUR %in% c(10:12),1,0))%>%
  group_by(amb)%>%
  summarise(deaths=(n()/79678254)*100000, deaths_n=n())

mort_prom<-rbind(mort_1, mort_2, mort_3)
mort_prom<-mort_prom %>% group_by(amb) %>% 
  summarise(deaths=mean(deaths), deaths_n=mean(deaths_n))
mort_prom$year<-c("2017-2019")
mort_0$year<-c("2020")
mort_prom<-rbind(mort_0, mort_prom)
mort_ov<-mort_prom%>%mutate(amb=factor(amb, labels = c("In-hospital", "Out-of-hospital")))
mort_ov$ICD_10_1.CODE<-c("Diabetes overall")

mort_0<- mort_2020 %>% filter(sitio_ocur!=99)%>%
  mutate(ICD_10_1.CODE=factor(ICD_10_1.CODE, labels = c("Type 1 diabetes", "Type 2 diabetes", "Other diabetes types","Other diabetes types","Other diabetes types")))%>%
  mutate(amb=ifelse(sitio_ocur %in% c(10:12),1,0))%>%
  group_by(ICD_10_1.CODE,amb)%>%
  summarise(deaths=(n()/83757486)*100000, deaths_n=n()) 
mort_1<- mort_2019 %>% filter(sitio_ocur!=99)%>%
  mutate(ICD_10_1.CODE=factor(ICD_10_1.CODE, labels = c("Type 1 diabetes", "Type 2 diabetes", "Other diabetes types","Other diabetes types","Other diabetes types")))%>%
  mutate(amb=ifelse(sitio_ocur %in% c(10:12),1,0))%>%
  group_by(ICD_10_1.CODE,amb)%>%
  summarise(deaths=(n()/82418624)*100000, deaths_n=n())
mort_2<- mort_2018 %>% filter(SITIO_OCUR!=99)%>%
  mutate(ICD_10_1.CODE=factor(ICD_10_1.CODE, labels = c("Type 1 diabetes", "Type 2 diabetes", "Other diabetes types","Other diabetes types","Other diabetes types")))%>%
  mutate(amb=ifelse(SITIO_OCUR %in% c(10:12),1,0))%>%
  group_by(ICD_10_1.CODE,amb)%>%
  summarise(deaths=(n()/81060179)*100000, deaths_n=n())
mort_3<- mort_2017 %>% filter(SITIO_OCUR!=99)%>%
  mutate(ICD_10_1.CODE=factor(ICD_10_1.CODE, labels = c("Type 1 diabetes", "Type 2 diabetes", "Other diabetes types","Other diabetes types","Other diabetes types")))%>%
  mutate(amb=ifelse(SITIO_OCUR %in% c(10:12),1,0))%>%
  group_by(ICD_10_1.CODE,amb)%>%
  summarise(deaths=(n()/79678254)*100000, deaths_n=n())

mort_prom<-rbind(mort_1, mort_2, mort_3)
mort_prom<-mort_prom %>% group_by(ICD_10_1.CODE, amb) %>% 
  summarise(deaths=mean(deaths), deaths_n=mean(deaths_n))
mort_prom$year<-c("2017-2019")
mort_0$year<-c("2020")
mort_prom<-rbind(mort_0, mort_prom)
age_adj_place<-mort_prom%>%mutate(amb=factor(amb, labels = c("In-hospital", "Out-of-hospital")))
age_adj_place<-rbind(mort_ov, age_adj_place)

age_adj_place1<-age_adj_place %>% filter(ICD_10_1.CODE!="Other diabetes types")
age_adj_place0<-age_adj_place %>% filter(ICD_10_1.CODE=="Other diabetes types")

age_adj_place1<-data.table(age_adj_place1)

age_adj_place1[age_adj_place1$ICD_10_1.CODE %in% c("Diabetes overall", "Type 2 diabetes"),y_min := 0]
age_adj_place1[age_adj_place1$ICD_10_1.CODE %in% c("Diabetes overall", "Type 2 diabetes"), y_max :=150]
age_adj_place1[age_adj_place1$ICD_10_1.CODE %in% c("Type 1 diabetes"), y_min := 0]
age_adj_place1[age_adj_place1$ICD_10_1.CODE %in% c("Type 1 diabetes"), y_max :=3]


k2<-age_adj_place1 %>% 
  mutate(across(ICD_10_1.CODE, factor, levels=c("Diabetes overall","Type 2 diabetes","Type 1 diabetes", "Other diabetes types"))) %>%
  ggplot(aes(y=deaths, x=amb,fill=year)) + geom_bar(stat="identity", position='dodge', color="black", width=0.7)+
  theme_pubclean()+ylab("Frequency (%)")+xlab("")+
  theme(axis.ticks.x = element_blank())+
  geom_text(aes(label=formatC(round(deaths,1),1,format="f")), vjust=1.4, color="white", size=3, position = position_dodge(0.7))+
  labs(fill="Period")+facet_wrap(~ICD_10_1.CODE, nrow = 1, ncol=4, scales = "free")+scale_fill_jama()+
  ylab("Diabetes deaths per 100,000 inhabitants")+
  geom_blank(aes(y = y_min)) +
  geom_blank(aes(y = y_max))

o4<-age_adj_place0 %>% 
  mutate(across(ICD_10_1.CODE, factor, levels=c("Diabetes overall","Type 2 diabetes","Type 1 diabetes", "Other diabetes types"))) %>%
  ggplot(aes(y=deaths, x=amb,fill=year)) + geom_bar(stat="identity", position='dodge', color="black", width=0.7)+
  theme_pubclean()+ylab("Frequency (%)")+xlab("")+
  theme(axis.ticks.x = element_blank())+
  geom_text(aes(label=formatC(round(deaths,1),1,format="f")), vjust=1.4, color="white", size=3, position = position_dodge(0.7))+
  labs(fill="Period")+facet_wrap(~ICD_10_1.CODE, nrow = 1, ncol=4, scales = "free")+scale_fill_jama()+
  ylab("Diabetes deaths per 100,000 inhabitants")

## Build figure

fig1a<-ggarrange(k1,n2, k2, labels=c("B", "C", "D"), nrow=3, ncol = 1, common.legend = T)
fig1<-ggarrange(n1, fig1a, labels=c("A", ""), nrow=2, ncol = 1, heights = c(1,3))

ggsave(fig1,filename = "Figure1.jpg", 
       width = 25, 
       height = 40,
       units=c("cm"),
       dpi = 300,
       limitsize = FALSE)

suppfig3<-ggarrange(o1,o2, o3, o4, labels=c("A","B", "C", "D"), nrow=2, ncol = 2, common.legend = T)

ggsave(suppfig3,filename = "SuppFigure1.jpg", 
       width = 30, 
       height = 30,
       units=c("cm"),
       dpi = 300,
       limitsize = FALSE)

#### Figure 2: Diabetes-related excess deaths by cause ####
### Rates per cause ###
mort2020<-mort_2020 %>% filter(mes_ocurr!=99 & edad_agru<9 & ent_resid<33) %>%
  group_by(cause)%>%summarise(deaths=n()/81060179*100000)
mort2020$year<-rep(2020, nrow(mort2020))
mort2019<-mort_2019 %>% filter(mes_ocurr!=99 & edad_agru<9 & ent_resid<33) %>%
  group_by(cause)%>%summarise(deaths=n()/82418624*100000) 
mort2019$year<-rep(2019, nrow(mort2019))
mort2018<-mort_2018 %>% filter(MES_OCURR!=99 & EDAD_AGRU<9 & ENT_RESID<33) %>%
  group_by(cause)%>%summarise(deaths=n()/83757486*100000) 
mort2018$year<-rep(2018, nrow(mort2018))

cause_fin<-rbind(mort2020,mort2019, mort2018)
n3<-cause_fin %>% 
  ggplot(aes(y=deaths, x=factor(year),fill=factor(year))) + geom_bar(stat="identity", position='dodge', color="black", width=0.8)+
  theme_pubclean()+ylab("Frequency (%)")+xlab("")+
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+
  geom_text(aes(label=round(deaths,3)), vjust=1.4, color="white", size=3)+
  labs(fill="Year")+facet_wrap(~cause, scales = "free", nrow = 3, ncol=3)+scale_fill_jama()+
  ylab("Diabetes deaths per 100,000 inhabitants")

### Rates per cause ###
mort2020<-mort_2020 %>% filter(mes_ocurr!=99 & edad_agru<9 & ent_resid<33) %>% 
  filter(sitio_ocur!=99)%>%
  mutate(ICD_10_1.CODE=factor(ICD_10_1.CODE, labels = c("Type 1 diabetes", "Type 2 diabetes", "Other diabetes types","Other diabetes types","Other diabetes types")))%>%
  mutate(amb=ifelse(sitio_ocur %in% c(10:12),1,0))%>%
  group_by(amb,cause)%>%summarise(deaths=n()/81060179*100000)
mort2020$year<-rep(2020, nrow(mort2020))
mort2019<-mort_2019 %>% filter(mes_ocurr!=99 & edad_agru<9 & ent_resid<33) %>%
  filter(sitio_ocur!=99)%>%
  mutate(ICD_10_1.CODE=factor(ICD_10_1.CODE, labels = c("Type 1 diabetes", "Type 2 diabetes", "Other diabetes types","Other diabetes types","Other diabetes types")))%>%
  mutate(amb=ifelse(sitio_ocur %in% c(10:12),1,0))%>%
  group_by(amb,cause)%>%summarise(deaths=n()/82418624*100000) 
mort2019$year<-rep(2019, nrow(mort2019))
mort2018<-mort_2018 %>% filter(MES_OCURR!=99 & EDAD_AGRU<9 & ENT_RESID<33) %>%
  filter(SITIO_OCUR!=99)%>%
  mutate(ICD_10_1.CODE=factor(ICD_10_1.CODE, labels = c("Type 1 diabetes", "Type 2 diabetes", "Other diabetes types","Other diabetes types","Other diabetes types")))%>%
  mutate(amb=ifelse(SITIO_OCUR %in% c(10:12),1,0))%>%
  group_by(amb,cause)%>%summarise(deaths=n()/83757486*100000) 
mort2018$year<-rep(2018, nrow(mort2018))

cause_fin<-rbind(mort2020,mort2019, mort2018)
cause_fin$amb<-factor(cause_fin$amb, labels = c("In-hospital", "Out-of-hospital"))
n4<-cause_fin %>% filter(cause %in% c("HHS", "Ketoacidosis", "Kidney complications"))%>%
  ggplot(aes(y=deaths, x=amb,fill=factor(year))) + geom_bar(stat="identity", position='dodge', color="black")+
  theme_pubclean()+ylab("Frequency (%)")+xlab("")+
  theme(axis.ticks.x = element_blank())+
  geom_text(aes(label=round(deaths,2)), vjust=1.4, color="white", size=3, position = position_dodge(0.95))+
  labs(fill="Year")+facet_wrap(~cause, scales = "free")+scale_fill_jama()+
  ylab("Diabetes deaths per 100,000 inhabitants")+theme_hc()


fig2<-ggarrange(n3, n4, labels = c("A", "B"), common.legend = T, nrow = 2,ncol=1, heights  = c(1.5,1))

## Build figure

ggsave(fig2,filename = "Figure2.jpg", 
       width = 25, 
       height = 30,
       units=c("cm"),
       dpi = 300,
       limitsize = FALSE)
#### Supplementary Figure 2: COVID-19 super-imposed graph ####
## By diabetes type ##
mort_0<- mort_2020 %>% dplyr::select(mes_ocurr, anio_ocur) %>%  filter(mes_ocurr!=99) %>% 
  group_by(anio_ocur,mes_ocurr)%>%summarise(deaths=(n()/83757486)*100000, deaths_n=n()) 
mort_1<- mort_2019 %>% dplyr::select(mes_ocurr, anio_ocur)%>%  filter(mes_ocurr!=99) %>% 
  group_by(anio_ocur,mes_ocurr)%>%summarise(deaths=(n()/82418624)*100000, deaths_n=n())
mort_2<- mort_2018 %>% dplyr::select(MES_OCURR, ANIO_OCUR)%>%  filter(MES_OCURR!=99) %>% 
  group_by(ANIO_OCUR,MES_OCURR)%>%summarise(deaths=(n()/81060179)*100000, deaths_n=n())
names(mort_2)<-c("anio_ocur","mes_ocurr","deaths", "deaths_n")
mort_3<- mort_2017 %>% dplyr::select(MES_OCURR, ANIO_OCUR)%>%  filter(MES_OCURR!=99) %>% 
  group_by(ANIO_OCUR,MES_OCURR)%>%summarise(deaths=(n()/79678254)*100000, deaths_n=n())
names(mort_3)<-c("anio_ocur","mes_ocurr","deaths", "deaths_n")
mort_ov<-rbind(mort_0, mort_1, mort_2, mort_3)
mort_ov$anio_ocur<-as.character(mort_ov$anio_ocur)

covid_time<- covid %>% dplyr::select(mes_ocurr, anio_ocur) %>%  filter(mes_ocurr!=99) %>% 
  group_by(anio_ocur,mes_ocurr)%>%summarise(deaths=(n()/83757486)*100000, deaths_n=n()) 
covid_time$anio_ocur<-as.character(covid_time$anio_ocur)
covid_time$anio_ocur<-"COVID-19 in 2020"

mort_ov<-rbind(mort_ov, covid_time)

n1<-mort_ov %>% 
  ggplot(aes(x=mes_ocurr, y=deaths, col=factor(anio_ocur)))+
  geom_point()+geom_line(size=1)+theme_pubclean()+ylab("Diabetes or COVID-19 deaths per 100,000 inhabitants")+
  xlab("Month of occurence")+labs(col="Year")+scale_color_jama()+
  scale_x_continuous(breaks=seq(1,12,1),labels=paste0(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")))+
  theme(legend.position="top",axis.text.x=element_text(angle=90))

#### Supplementary Figure 3: Diabetes-related excess deaths by type and cause ####
### Rates per cause ###
mort2020<-mort_2020 %>% filter(mes_ocurr!=99 & edad_agru<9 & ent_resid<33) %>%
  mutate(ICD_10_1.CODE=factor(ICD_10_1.CODE, labels = c("Type 1 diabetes", "Type 2 diabetes", "Other diabetes types","Other diabetes types","Other diabetes types")))%>%
  group_by(ICD_10_1.CODE,cause)%>%summarise(deaths=n()/81060179*100000)
mort2020$year<-rep(2020, nrow(mort2020))
mort2019<-mort_2019 %>% filter(mes_ocurr!=99 & edad_agru<9 & ent_resid<33) %>%
  mutate(ICD_10_1.CODE=factor(ICD_10_1.CODE, labels = c("Type 1 diabetes", "Type 2 diabetes", "Other diabetes types","Other diabetes types","Other diabetes types")))%>%
  group_by(ICD_10_1.CODE,cause)%>%summarise(deaths=n()/82418624*100000) 
mort2019$year<-rep(2019, nrow(mort2019))
mort2018<-mort_2018 %>% filter(MES_OCURR!=99 & EDAD_AGRU<9 & ENT_RESID<33) %>%
  mutate(ICD_10_1.CODE=factor(ICD_10_1.CODE, labels = c("Type 1 diabetes", "Type 2 diabetes", "Other diabetes types","Other diabetes types","Other diabetes types")))%>%
  group_by(ICD_10_1.CODE,cause)%>%summarise(deaths=n()/83757486*100000) 
mort2018$year<-rep(2018, nrow(mort2018))

cause_fin<-rbind(mort2020,mort2019, mort2018)
n3<-cause_fin %>% 
  ggplot(aes(y=deaths, x=factor(year),fill=factor(year))) + geom_bar(stat="identity", position='dodge', color="black")+
  theme_pubclean()+ylab("Frequency (%)")+xlab("")+
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank())+
  geom_text(aes(label=round(deaths,3)), vjust=1.4, color="white", size=3)+
  labs(fill="Year")+facet_wrap(~ICD_10_1.CODE+cause, scales = "free", ncol=7, nrow=4)+scale_fill_jama()+
  ylab("Diabetes-related deaths per 100,000 inhabitants")

## Build figure
ggsave(n3,filename = "SuppFigure2.jpg", 
       width = 35, 
       height = 20,
       units=c("cm"),
       dpi = 300,
       limitsize = FALSE)

#### Supplementary Table 1: Age-adjusted excess mortality per region based on ENSANUT 2018-2020 ####
adj_mort<-adj_mort_2017 %>% left_join(adj_mort_2018, by="reg") %>%
  left_join(adj_mort_2019, by="reg") %>%left_join(adj_mort_2020, by="reg") %>%
  mutate(avg=round((DIAB_DEATHS_17+DIAB_DEATHS_18+DIAB_DEATHS_19)/3,2))%>%
  mutate(excess=round(DIAB_DEATHS_20-avg,2)) %>% left_join(pop_mun, by="reg") %>% 
  left_join(total_region_prev %>% dplyr::select(reg, diab), by="reg") %>%left_join(total_region %>% dplyr::select(reg, diab_tot), by="reg") %>% 
  mutate(prev_diab=round(diab*100,2)) %>%
  left_join(total_covid_region%>% dplyr::select(reg, covid), by="reg") %>% mutate(covid_rate=round(covid*100,2)) %>% left_join(total_region2%>% dplyr::select(reg, diab_no), by="reg") %>%
  mutate(undx_diab=round(diab_no*100,2)) %>%left_join(total_region3%>% dplyr::select(reg, diab_7), by="reg") %>%
  mutate(diab_7_prev=round((diab_7*100),2)) %>%left_join(total_region4%>% dplyr::select(reg, diab_10), by="reg") %>%
  mutate(diab_10_prev=round((diab_10*100),2)) %>%left_join(total_region6%>% dplyr::select(reg, diab_early), by="reg") %>%
  mutate(early_diab=round((diab_early*100),2)) %>% left_join(total_region_2018%>% dplyr::select(reg, diab_2018), by="reg") %>%
  mutate(prev_diab_2018=round((diab_2018*100),2))%>%left_join(total_region2_2018%>% dplyr::select(reg, diab_no_2018), by="reg") %>%
  mutate(undx_diab_2018=round((diab_no_2018*100),2))%>%left_join(total_region3_2018%>% dplyr::select(reg, diab_7_2018), by="reg") %>%
  mutate(diab_7_prev_2018=round((diab_7_2018*100),2))%>%left_join(total_region4_2018%>% dplyr::select(reg, diab_10_2018), by="reg") %>%
  mutate(diab_10_prev_2018=round((diab_10_2018*100),2))%>%left_join(total_region5_2018%>% dplyr::select(reg, diab_early_2018), by="reg") %>%
  mutate(early_diab_2018=round((diab_early_2018*100),2))%>%
  dplyr::select(reg, avg,DIAB_DEATHS_20,excess,diab_tot,prev_diab, undx_diab, diab_7_prev,covid_rate)
adj_mort$DIAB_DEATHS_20<-round(adj_mort$DIAB_DEATHS_20,2)
totales0<-adj_mort 
names(totales0)<-c("Region", "Average Diabetes mortlity rate 2017-2019", "Diabetes mortality rate 2020", "Excess mortality rate", "Diabetes cases 2020", "Diabetes prevalence", "Undiagnosed diabetes prevalence","HbA1c >7.5% prevalence","COVID-19 seroprevalence")
tab1 <-align(flextable(totales0,cwidth = c(1.5,1,1,1,1,1,1,1,1)),align = "center",part = "all")
sect_properties <- prop_section(
  page_size = page_size(orient = "landscape",
                        width = 8.3, height = 11.7),
  type = "continuous",
  page_margins = page_mar()
)
save_as_docx(tab1,path="tabla1.docx", pr_section = sect_properties)
#### Figure 3: Maps of age-adjusted excess mortality per diabetes type and determinants of age-adjusted excess mortality ####
mort2020<-mort_2020 %>% filter(mes_ocurr!=99 & edad_agru<9 & ent_resid<33) %>%
  mutate(ICD_10_1.CODE=factor(ICD_10_1.CODE, labels = c("Type 1 diabetes", "Type 2 diabetes", "Other diabetes types","Other diabetes types","Other diabetes types")))%>%
  group_by(ent_resid, ICD_10_1.CODE,EDAD_QUIN)%>%summarise(Total_2020=n()) 
mort2019<-mort_2019 %>% filter(mes_ocurr!=99 & edad_agru<9 & ent_resid<33) %>%
  mutate(ICD_10_1.CODE=factor(ICD_10_1.CODE, labels = c("Type 1 diabetes", "Type 2 diabetes", "Other diabetes types","Other diabetes types","Other diabetes types")))%>%
  group_by(ent_resid, ICD_10_1.CODE,EDAD_QUIN)%>%summarise(Total_2019=n()) 
mort2018<-mort_2018 %>% filter(MES_OCURR!=99 & EDAD_AGRU<9 & ENT_RESID<33) %>%
  mutate(ICD_10_1.CODE=factor(ICD_10_1.CODE, labels = c("Type 1 diabetes", "Type 2 diabetes", "Other diabetes types","Other diabetes types","Other diabetes types")))%>%
  group_by(ENT_RESID, ICD_10_1.CODE,EDAD_QUIN)%>%summarise(Total_2018=n()) 
names(mort2018)<-c("ent_resid",  "ICD_10_1.CODE","EDAD_QUIN", "Total_2018")
mort2017<-mort_2017 %>% filter(MES_OCURR!=99 & EDAD_AGRU<9 & ENT_RESID<33) %>%
  mutate(ICD_10_1.CODE=factor(ICD_10_1.CODE, labels = c("Type 1 diabetes", "Type 2 diabetes", "Other diabetes types","Other diabetes types","Other diabetes types")))%>%
  group_by(ENT_RESID, ICD_10_1.CODE,EDAD_QUIN)%>%summarise(Total_2017=n()) 
names(mort2017)<-c("ent_resid", "ICD_10_1.CODE","EDAD_QUIN", "Total_2017")
mort2<-mort2020 %>% left_join(mort2019, by=c("ent_resid", "ICD_10_1.CODE","EDAD_QUIN")) %>%
  left_join(mort2018, by=c("ent_resid", "ICD_10_1.CODE","EDAD_QUIN")) %>% left_join(mort2017, by=c("ent_resid", "ICD_10_1.CODE","EDAD_QUIN"))
names(mort2)<-c("CLAVE_ENT", "ICD_10_1.CODE","EDAD_QUIN", "Total_2020", "Total_2019", "Total_2018", "Total_2017")
mort2$CLAVE_ENT<-as.numeric(mort2$CLAVE_ENT)

adj_mort_2017_state2<-mort2 %>%group_by(CLAVE_ENT, ICD_10_1.CODE,EDAD_QUIN) %>% 
  summarise(total_20_17=sum(Total_2017)) %>%
  left_join(Pob.grupos.edad.estado.2017, by =c("CLAVE_ENT","EDAD_QUIN"))%>%
  left_join(Pob.grupos.edad.total.2017,by=c("EDAD_QUIN"))%>% 
  mutate(DEF_DIAB = (total_20_17/POB.x)*(POB.y))%>% 
  group_by(CLAVE_ENT, ICD_10_1.CODE) %>% 
  summarise(DEF_ESPER_DIAB_17=sum(DEF_DIAB,na.rm = T))%>%
  group_by(CLAVE_ENT,ICD_10_1.CODE)%>% 
  mutate(DIAB_DEATHS_17 = (DEF_ESPER_DIAB_17/79678254)*100000)

adj_mort_2018_state2<-mort2 %>%  group_by(CLAVE_ENT, ICD_10_1.CODE,EDAD_QUIN) %>% 
  summarise(total_20_2018=sum(Total_2018)) %>%
  left_join(Pob.grupos.edad.estado.2018, by =c("CLAVE_ENT","EDAD_QUIN"))%>%
  left_join(Pob.grupos.edad.total.2018,by=c("EDAD_QUIN"))%>% 
  mutate(DEF_DIAB = (total_20_2018/POB.x)*POB.y)%>% 
  group_by(CLAVE_ENT,ICD_10_1.CODE) %>% 
  summarise(DEF_ESPER_DIAB_18=sum(DEF_DIAB,na.rm = T))%>%
  group_by(CLAVE_ENT,ICD_10_1.CODE)%>% 
  mutate(DIAB_DEATHS_18 = (DEF_ESPER_DIAB_18/81060179)*100000)

adj_mort_2019_state2<-mort2 %>%  group_by(CLAVE_ENT, ICD_10_1.CODE,EDAD_QUIN) %>% 
  summarise(total_20_2019=sum(Total_2019)) %>%
  left_join(Pob.grupos.edad.estado.2019, by =c("CLAVE_ENT","EDAD_QUIN"))%>%
  left_join(Pob.grupos.edad.total.2019,by=c("EDAD_QUIN"))%>% 
  mutate(DEF_DIAB = (total_20_2019/POB.x)*POB.y)%>% 
  group_by(CLAVE_ENT,ICD_10_1.CODE) %>% 
  summarise(DEF_ESPER_DIAB_19=sum(DEF_DIAB,na.rm = T))%>%
  group_by(CLAVE_ENT,ICD_10_1.CODE)%>% 
  mutate(DIAB_DEATHS_19 = (DEF_ESPER_DIAB_19/82418624)*100000)

adj_mort_2020_state2<-mort2 %>%  group_by(CLAVE_ENT, ICD_10_1.CODE,EDAD_QUIN) %>% 
  summarise(total_20_2020=sum(Total_2020)) %>%
  left_join(Pob.grupos.edad.estado.2020, by =c("CLAVE_ENT","EDAD_QUIN"))%>%
  left_join(Pob.grupos.edad.total.2020,by=c("EDAD_QUIN"))%>% 
  mutate(DEF_DIAB = (total_20_2020/POB.x)*POB.y)%>% 
  group_by(CLAVE_ENT,ICD_10_1.CODE) %>% 
  summarise(DEF_ESPER_DIAB_20=sum(DEF_DIAB,na.rm = T))%>%
  group_by(CLAVE_ENT,ICD_10_1.CODE)%>% 
  mutate(DIAB_DEATHS_20 = (DEF_ESPER_DIAB_20/83757486)*100000)

age_adj_type<-adj_mort_2017_state2 %>% left_join(adj_mort_2018_state2, by=c("CLAVE_ENT", "ICD_10_1.CODE")) %>%
  left_join(adj_mort_2019_state2, by=c("CLAVE_ENT", "ICD_10_1.CODE")) %>% left_join(adj_mort_2020_state2, by=c("CLAVE_ENT", "ICD_10_1.CODE")) 

type1<-adj_mort_state2 %>% left_join(age_adj_type %>% filter(ICD_10_1.CODE=="Type 1 diabetes"), by="CLAVE_ENT") %>%
  mutate(avg2=(DIAB_DEATHS_17+DIAB_DEATHS_18+DIAB_DEATHS_19)/3)%>%mutate(excess2=DIAB_DEATHS_20-avg2)

type2<-adj_mort_state2 %>% left_join(age_adj_type %>% filter(ICD_10_1.CODE=="Type 2 diabetes"), by="CLAVE_ENT")%>%
  mutate(avg2=(DIAB_DEATHS_17+DIAB_DEATHS_18+DIAB_DEATHS_19)/3)%>%mutate(excess2=DIAB_DEATHS_20-avg2)


f2a<-adj_mort_state2 %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = excess, geometry=geometry), color = "white", size = 0.1, show.legend = TRUE) +
  scale_fill_distiller(palette = "BrBG", direction = -1)+ ggtitle("Age-adjusted overall excess diabetes-related mortality") +
  theme_map()+labs(fill="Excess per \n100K/hab")

f2b<-type2 %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = excess2, geometry=geometry), color = "white", size = 0.1, show.legend = TRUE) +
  scale_fill_distiller(palette = "BrBG", direction = -1)+ ggtitle("Age-adjusted excess mortality in type 2 diabetes") +
  theme_map()+labs(fill="Excess per \n100K/hab")

f2c<-type1 %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = excess2, geometry=geometry), color = "white", size = 0.1, show.legend = TRUE) +
  scale_fill_distiller(palette = "BrBG")+ ggtitle("Age-adjusted excess mortality in type 1 diabetes") +
  theme_map()+labs(fill="Excess per \n100K/hab")

fig3a<-ggarrange(f2b, f2c,labels=LETTERS[2:3], nrow=2, ncol=1)

fig3<-ggarrange(f2a, fig3a,labels=c("A", ""), nrow=1, ncol=2, widths = c(3:1))

ggsave(fig3,filename = "Figure3.jpg", 
       width = 30, 
       height = 18,
       units=c("cm"),
       dpi = 300,
       limitsize = FALSE)


#### Figure 4: Bivariate relationships ####
### Bivariate chloropleth maps ###
adj_mort_state2$log_hosp<-log(adj_mort_state2$hosp)
excess_1<-bi_class(adj_mort_state2, y = diab_7_prev, x = excess, style = "quantile", dim = 2)
excess_2<-bi_class(adj_mort_state2, y = covid_diab, x = excess, style = "quantile", dim = 2)
excess_3<-bi_class(adj_mort_state2, y = log_hosp, x = excess, style = "quantile", dim = 2)
excess_4<-bi_class(adj_mort_state2, y = mort_covid_diab, x = excess, style = "quantile", dim = 2)
excess_5<-bi_class(adj_mort_state2, y = diab_2020_undx, x = excess, style = "quantile", dim = 2)
excess_6<-bi_class(adj_mort_state2, y = prev_diab, x = excess, style = "quantile", dim = 2)
excess_7<-bi_class(adj_mort_state2, y = covid_rate, x = excess, style = "quantile", dim = 2)
excess_8<-bi_class(adj_mort_state2, y = disli, x = excess, style = "quantile", dim = 2)

Fig3A<-ggdraw() +
  draw_plot(ggplot() +
              geom_sf(data = excess_1, mapping = aes(fill = bi_class, geometry=geometry), color = "white", size = 0.1, show.legend = FALSE) +
              bi_scale_fill(pal = "DkBlue", dim = 2)+ ggtitle("Diabetes excess mortality vs. HbA1c >7.5% prevalence in 2020") +
              theme_map(), 0, 0, 1, 1) +
  draw_plot(bi_legend(pal = "DkBlue",
                      dim = 2,
                      xlab = "Higher T2D excess \n mortality per 100K hab",
                      ylab = "Higher HbA1c >7.5% \n prevalence (%)",
                      size = 7), 0.7, .5, 0.25, 0.25)

Fig3B<-ggdraw() +
  draw_plot(ggplot() +
              geom_sf(data = excess_6, mapping = aes(fill = bi_class, geometry=geometry), color = "white", size = 0.1, show.legend = FALSE) +
              bi_scale_fill(pal = "DkBlue", dim = 2)+ ggtitle("Diabetes excess mortality vs. overall diabetes prevalence 2020") +
              theme_map(), 0, 0, 1, 1) +
  draw_plot(bi_legend(pal = "DkBlue",
                      dim = 2,
                      xlab = "Higher T2D excess \n mortality per 100K hab",
                      ylab = "Higher diabetes \n prevalence (%)",
                      size = 7), 0.7, .5, 0.25, 0.25)

Fig3C<-ggdraw() +
  draw_plot(ggplot() +
              geom_sf(data = excess_5, mapping = aes(fill = bi_class, geometry=geometry), color = "white", size = 0.1, show.legend = FALSE) +
              bi_scale_fill(pal = "DkBlue", dim = 2)+ ggtitle("Diabetes excess mortality vs. undiagnosed diabetes prevalence 2020") +
              theme_map(), 0, 0, 1, 1) +
  draw_plot(bi_legend(pal = "DkBlue",
                      dim = 2,
                      xlab = "Higher T2D excess \n mortality per 100K hab",
                      ylab = "Higher undiagnosed \n diabetes prevalence (%)",
                      size = 7), 0.7, .5, 0.25, 0.25)

Fig3D<-ggdraw() +
  draw_plot(ggplot() +
              geom_sf(data = excess_8, mapping = aes(fill = bi_class, geometry=geometry), color = "white", size = 0.1, show.legend = FALSE) +
              bi_scale_fill(pal = "DkBlue", dim = 2)+ ggtitle("Diabetes excess mortality vs. population-density independent Social Lag Index 2020") +
              theme_map(), 0, 0, 1, 1) +
  draw_plot(bi_legend(pal = "DkBlue",
                      dim = 2,
                      xlab = "Higher T2D excess \n mortality per 100K hab",
                      ylab = "Higher DISLI",
                      size = 7), 0.7, .5, 0.25, 0.25)

fig3<-ggarrange(Fig3A, Fig3B, Fig3C,Fig3D,labels=LETTERS[1:4], nrow=2, ncol=2)

ggsave(fig3,filename = "Figure4.jpg", 
       width = 45, 
       height = 30,
       units=c("cm"),
       dpi = 300,
       limitsize = FALSE)

#### Extra Figure: Age-adjusted excess mortality per diabetes complication ####
mort2020<-mort_2020 %>% filter(mes_ocurr!=99 & edad_agru<9 & ent_resid<33) %>%
  group_by(ent_resid, cause,EDAD_QUIN)%>%summarise(Total_2020=n()) 
mort2019<-mort_2019 %>% filter(mes_ocurr!=99 & edad_agru<9 & ent_resid<33) %>%
  group_by(ent_resid, cause,EDAD_QUIN)%>%summarise(Total_2019=n()) 
mort2018<-mort_2018 %>% filter(MES_OCURR!=99 & EDAD_AGRU<9 & ENT_RESID<33) %>%
  group_by(ENT_RESID, cause,EDAD_QUIN)%>%summarise(Total_2018=n()) 
names(mort2018)<-c("ent_resid",  "cause","EDAD_QUIN", "Total_2018")
unique(mort2019$cause)
mort2<-mort2020 %>% left_join(mort2019, by=c("ent_resid", "cause","EDAD_QUIN")) %>%
  left_join(mort2018, by=c("ent_resid", "cause","EDAD_QUIN")) 
names(mort2)<-c("CLAVE_ENT", "cause","EDAD_QUIN", "Total_2020", "Total_2019", "Total_2018")
mort2$CLAVE_ENT<-as.numeric(mort2$CLAVE_ENT)


adj_mort_2018_state2<-mort2 %>%  group_by(CLAVE_ENT, cause,EDAD_QUIN) %>% 
  summarise(total_20_2018=sum(Total_2018)) %>%
  left_join(Pob.grupos.edad.estado.2018, by =c("CLAVE_ENT","EDAD_QUIN"))%>%
  left_join(Pob.grupos.edad.total.2018,by=c("EDAD_QUIN"))%>% 
  mutate(DEF_DIAB = (total_20_2018/POB.x)*POB.y)%>% 
  group_by(CLAVE_ENT,cause) %>% 
  summarise(DEF_ESPER_DIAB_18=sum(DEF_DIAB,na.rm = T))%>%
  group_by(CLAVE_ENT,cause)%>% 
  mutate(DIAB_DEATHS_18 = (DEF_ESPER_DIAB_18/81060179)*100000)

adj_mort_2019_state2<-mort2 %>%  group_by(CLAVE_ENT, cause,EDAD_QUIN) %>% 
  summarise(total_20_2019=sum(Total_2019)) %>%
  left_join(Pob.grupos.edad.estado.2019, by =c("CLAVE_ENT","EDAD_QUIN"))%>%
  left_join(Pob.grupos.edad.total.2019,by=c("EDAD_QUIN"))%>% 
  mutate(DEF_DIAB = (total_20_2019/POB.x)*POB.y)%>% 
  group_by(CLAVE_ENT,cause) %>% 
  summarise(DEF_ESPER_DIAB_19=sum(DEF_DIAB,na.rm = T))%>%
  group_by(CLAVE_ENT,cause)%>% 
  mutate(DIAB_DEATHS_19 = (DEF_ESPER_DIAB_19/82418624)*100000)

adj_mort_2020_state2<-mort2 %>%  group_by(CLAVE_ENT, cause,EDAD_QUIN) %>% 
  summarise(total_20_2020=sum(Total_2020)) %>%
  left_join(Pob.grupos.edad.estado.2020, by =c("CLAVE_ENT","EDAD_QUIN"))%>%
  left_join(Pob.grupos.edad.total.2020,by=c("EDAD_QUIN"))%>% 
  mutate(DEF_DIAB = (total_20_2020/POB.x)*POB.y)%>% 
  group_by(CLAVE_ENT,cause) %>% 
  summarise(DEF_ESPER_DIAB_20=sum(DEF_DIAB,na.rm = T))%>%
  group_by(CLAVE_ENT,cause)%>% 
  mutate(DIAB_DEATHS_20 = (DEF_ESPER_DIAB_20/83757486)*100000)

age_adj_type<-adj_mort_2018_state2 %>% 
  left_join(adj_mort_2019_state2, by=c("CLAVE_ENT", "cause")) %>% left_join(adj_mort_2020_state2, by=c("CLAVE_ENT", "cause")) 

coma<-adj_mort_state2 %>% left_join(age_adj_type %>% filter(cause=="Diabetic coma"), by="CLAVE_ENT") %>%
  mutate(avg2=(DIAB_DEATHS_18+DIAB_DEATHS_19)/2)%>%mutate(excess2=DIAB_DEATHS_20-avg2)
keto<-adj_mort_state2 %>% left_join(age_adj_type %>% filter(cause=="Ketoacidosis"), by="CLAVE_ENT") %>%
  mutate(avg2=(DIAB_DEATHS_18+DIAB_DEATHS_19)/2)%>%mutate(excess2=DIAB_DEATHS_20-avg2)
kidney<-adj_mort_state2 %>% left_join(age_adj_type %>% filter(cause=="Kidney complications"), by="CLAVE_ENT")%>%
  mutate(avg2=(DIAB_DEATHS_18+DIAB_DEATHS_19)/2)%>%mutate(excess2=DIAB_DEATHS_20-avg2)
circ<-adj_mort_state2 %>% left_join(age_adj_type %>% filter(cause=="Circulatory"), by="CLAVE_ENT")%>%
  mutate(avg2=(DIAB_DEATHS_18+DIAB_DEATHS_19)/2)%>%mutate(excess2=DIAB_DEATHS_20-avg2)
neuro<-adj_mort_state2 %>% left_join(age_adj_type %>% filter(cause=="Neurological"), by="CLAVE_ENT")%>%
  mutate(avg2=(DIAB_DEATHS_18+DIAB_DEATHS_19)/2)%>%mutate(excess2=DIAB_DEATHS_20-avg2)
opht<-adj_mort_state2 %>% left_join(age_adj_type %>% filter(cause=="Ophtalmic"), by="CLAVE_ENT")%>%
  mutate(avg2=(DIAB_DEATHS_18+DIAB_DEATHS_19)/2)%>%mutate(excess2=DIAB_DEATHS_20-avg2)
unspecified<-adj_mort_state2 %>% left_join(age_adj_type %>% filter(cause=="Unspecified"), by="CLAVE_ENT")%>%
  mutate(avg2=(DIAB_DEATHS_18+DIAB_DEATHS_19)/2)%>%mutate(excess2=DIAB_DEATHS_20-avg2)
no_comp<-adj_mort_state2 %>% left_join(age_adj_type %>% filter(cause=="No Complications"), by="CLAVE_ENT")%>%
  mutate(avg2=(DIAB_DEATHS_18+DIAB_DEATHS_19)/2)%>%mutate(excess2=DIAB_DEATHS_20-avg2)
multiple<-adj_mort_state2 %>% left_join(age_adj_type %>% filter(cause=="Multiple"), by="CLAVE_ENT")%>%
  mutate(avg2=(DIAB_DEATHS_18+DIAB_DEATHS_19)/2)%>%mutate(excess2=DIAB_DEATHS_20-avg2)
coma$excess2[coma$excess2==min(coma$excess2)]<-0.01
s2a<-coma %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = excess2, geometry=geometry), color = "white", size = 0.1, show.legend = TRUE) +
  scale_fill_distiller(palette = "BrBG")+ ggtitle("Age-adjusted excess mortality due to diabetic coma") +
  theme_map()+labs(fill="Excess per \n100K/hab")

s2b<-keto %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = excess2, geometry=geometry), color = "white", size = 0.1, show.legend = TRUE) +
  scale_fill_distiller(palette = "BrBG", trans="log10")+ ggtitle("Age-adjusted excess mortality due to ketoacidosis") +
  theme_map()+labs(fill="Excess per \n100K/hab")

s2c<-kidney %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = excess2, geometry=geometry), color = "white", size = 0.1, show.legend = TRUE) +
  scale_fill_distiller(palette = "BrBG", direction = -1)+ ggtitle("Age-adjusted excess mortality due to kidney complications") +
  theme_map()+labs(fill="Excess per \n100K/hab")

s2d<-circ %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = excess2, geometry=geometry), color = "white", size = 0.1, show.legend = TRUE) +
  scale_fill_distiller(palette = "BrBG")+ ggtitle("Age-adjusted excess mortality due to peripheral circulatory complications") +
  theme_map()+labs(fill="Excess per \n100K/hab")

s2e<-neuro %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = excess2, geometry=geometry), color = "white", size = 0.1, show.legend = TRUE) +
  scale_fill_distiller(palette = "BrBG")+ ggtitle("Age-adjusted excess mortality due to neurological complications") +
  theme_map()+labs(fill="Excess per \n100K/hab")

s2f<-opht %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = excess2, geometry=geometry), color = "white", size = 0.1, show.legend = TRUE) +
  scale_fill_distiller(palette = "BrBG")+ ggtitle("Age-adjusted excess mortality due to ophtalmic complications") +
  theme_map()+labs(fill="Excess per \n100K/hab")

s2g<-multiple %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = excess2, geometry=geometry), color = "white", size = 0.1, show.legend = TRUE) +
  scale_fill_distiller(palette = "BrBG")+ ggtitle("Age-adjusted excess mortality due to multiple complications") +
  theme_map()+labs(fill="Excess per \n100K/hab")

s2h<-no_comp %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = excess2, geometry=geometry), color = "white", size = 0.1, show.legend = TRUE) +
  scale_fill_distiller(palette = "BrBG")+ ggtitle("Age-adjusted excess mortality without complications") +
  theme_map()+labs(fill="Excess per \n100K/hab")

s2i<-unspecified %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = excess2, geometry=geometry), color = "white", size = 0.1, show.legend = TRUE) +
  scale_fill_distiller(palette = "BrBG")+ ggtitle("Age-adjusted excess mortality due to unspecified complications") +
  theme_map()+labs(fill="Excess per \n100K/hab")

figsup2<-ggarrange(s2a, s2b, s2c, s2d,s2e,s2f,s2g,s2h,s2i,labels=LETTERS[1:9], ncol=3, nrow=3)

ggsave(figsup2,filename = "SuppFigure3.jpg", 
       width = 45, 
       height = 22,
       units=c("cm"),
       dpi = 300,
       limitsize = FALSE)

#### Supplementary Figure 4: Analyses of Spatial autocorrelation ####
s3a<-as.ggplot(~moran.plot(adj_mort_state2$excess, nb2listw(geo_data), pch=19,
                           xlim=c(0,100), ylim=c(0,75), ylab = c("Spatially lagged age-adjusted excess mortality"),
                           xlab=c("Age-adjusted excess diabetes-related mortality")))

moran.test(adj_mort_state2$excess, nb2listw(geo_data))

local <- (localmoran(adj_mort_state2$excess, nb2listw(geo_data)))

moran.map <- st_as_sf(adj_mort_state2$geometry, local)

s3b<-tmap_grob(tm_shape(moran.map) +
                 tm_fill(col = "Ii",
                         style = "quantile",
                         title = "Local Moran statistic"))

quadrant <- vector(mode="numeric",length=nrow(local))

# centers the variable of interest around its mean
m.excess <- adj_mort_state2$excess - mean(adj_mort_state2$excess)     

# centers the local Moran's around the mean
m.local <- local[,1] - mean(local[,1])    

# significance threshold
signif <- 0.05

# builds a data quadrant
quadrant[m.excess >0 & m.local>0] <- 4  
quadrant[m.excess <0 & m.local<0] <- 1      
quadrant[m.excess <0 & m.local>0] <- 2
quadrant[m.excess >0 & m.local<0] <- 3
quadrant[local[,5]>signif] <- 0   

# plot in r
brks <- c(0,1,2,3,4)
colors <- c("white","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4),"red")
plot(moran.map[1],border="lightgray",col=colors[findInterval(quadrant,brks,all.inside=FALSE)], main="LISA for age-adjusted excess diabetes-related mortality")
legend("bottomleft", legend = c("Non-significant","Low-low","Low-high","High-low","High-high"),
       fill=colors,bty="n")

local_g <- localG(adj_mort_state2$excess, nb2listw(geo_data))
g.map <- st_as_sf(adj_mort_state2$geometry, as.matrix(local_g))
names(g.map)[1] <- "gstat"

s3c<-g.map %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = gstat, geometry=x), color = "white", size = 0.1, show.legend = TRUE) +
  scale_fill_distiller(palette = "RdBu")+ ggtitle("Gi-statistic for age-adjusted diabetes-related excess mortality") +
  theme_map()+labs(fill="Gi-statistic")

s3d<-moran.map %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = Ii, geometry=x), color = "white", size = 0.1, show.legend = TRUE) +
  scale_fill_distiller(palette = "RdBu", )+ ggtitle("Moran's I-statistic for age-adjusted diabetes-related excess mortality") +
  theme_map()+labs(fill="Moran's Istatistic")

figs3<-ggarrange(s3a, s3b, "",s3c,labels=LETTERS[1:4])

ggsave(figs3,filename = "SuppFigure3.jpg", 
       width = 50, 
       height = 35,
       units=c("cm"),
       dpi = 300,
       limitsize = FALSE)


#### Supplementary Figure 5: Maps of Out-of-hospital to in-hospital deaths per diabetes type ####
mort_0<- mort_2020 %>% 
  mutate(amb=ifelse(sitio_ocur %in% c(10:12),1,0))%>%
  group_by(ent_resid,amb)%>%
  summarise(deaths=(n()/83757486)*100000) 
mort_1<- mort_2019 %>% 
  mutate(amb=ifelse(sitio_ocur %in% c(10:12),1,0))%>%
  group_by(ent_resid,amb)%>%
  summarise(deaths=(n()/82418624)*100000)
mort_2<- mort_2018 %>% 
  mutate(amb=ifelse(SITIO_OCUR %in% c(10:12),1,0))%>%
  group_by(ENT_RESID,amb)%>%
  summarise(deaths=(n()/81060179)*100000)
names(mort_2)<-c("ent_resid","amb", "deaths")
mort_3<- mort_2017 %>% 
  mutate(amb=ifelse(SITIO_OCUR %in% c(10:12),1,0))%>%
  group_by(ENT_RESID,amb)%>%
  summarise(deaths=(n()/79678254)*100000)
names(mort_3)<-c("ent_resid","amb", "deaths")


mort_prom<-rbind(mort_1, mort_2, mort_3)
mort_prom<-mort_prom %>% group_by(ent_resid, amb) %>% summarise(deaths=mean(deaths))
mort_prom$year<-c("2017-2019")
mort_0$year<-c("2020")
mort_prom<-rbind(mort_0, mort_prom)
names(mort_prom)<-c("CLAVE_ENT", "amb", "deaths", "year")
mort_prom$amb<-factor(mort_prom$amb, labels = c("In-hospital", "Out-of-hospital"))
age_adj_place2 <-mort_prom %>% spread(amb,deaths) %>% mutate(ratio = `Out-of-hospital`/`In-hospital`) %>% dplyr::select("CLAVE_ENT", "year", "ratio")%>%
  spread(year, ratio) %>%mutate(ratio_change=`2020`-`2017-2019`) %>% dplyr::select("CLAVE_ENT", "ratio_change")
age_adj_place2$CLAVE_ENT<-as.double(age_adj_place2$CLAVE_ENT)

adj_mort_state2<-adj_mort_state2 %>% left_join(age_adj_place2,by=c("CLAVE_ENT"))

#### Per type ###
mort_0<- mort_2020 %>% 
  mutate(ICD_10_1.CODE=factor(ICD_10_1.CODE, labels = c("Type 1 diabetes", "Type 2 diabetes", "Other diabetes types","Other diabetes types","Other diabetes types")))%>%
  mutate(amb=ifelse(sitio_ocur %in% c(10:12),1,0))%>%
  group_by(ent_resid,ICD_10_1.CODE,amb)%>%
  summarise(deaths=(n()/83757486)*100000, deaths_n=n()) 
mort_1<- mort_2019 %>% 
  mutate(ICD_10_1.CODE=factor(ICD_10_1.CODE, labels = c("Type 1 diabetes", "Type 2 diabetes", "Other diabetes types","Other diabetes types","Other diabetes types")))%>%
  mutate(amb=ifelse(sitio_ocur %in% c(10:12),1,0))%>%
  group_by(ent_resid,ICD_10_1.CODE,amb)%>%
  summarise(deaths=(n()/82418624)*100000, deaths_n=n())
mort_2<- mort_2018 %>% 
  mutate(ICD_10_1.CODE=factor(ICD_10_1.CODE, labels = c("Type 1 diabetes", "Type 2 diabetes", "Other diabetes types","Other diabetes types","Other diabetes types")))%>%
  mutate(amb=ifelse(SITIO_OCUR %in% c(10:12),1,0))%>%
  group_by(ENT_RESID,ICD_10_1.CODE,amb)%>%
  summarise(deaths=(n()/81060179)*100000, deaths_n=n())
names(mort_2)<-c("ent_resid",  "ICD_10_1.CODE","amb", "deaths", "deaths_n")
mort_3<- mort_2017 %>% 
  mutate(ICD_10_1.CODE=factor(ICD_10_1.CODE, labels = c("Type 1 diabetes", "Type 2 diabetes", "Other diabetes types","Other diabetes types","Other diabetes types")))%>%
  mutate(amb=ifelse(SITIO_OCUR %in% c(10:12),1,0))%>%
  group_by(ENT_RESID,ICD_10_1.CODE,amb)%>%
  summarise(deaths=(n()/79678254)*100000, deaths_n=n())
names(mort_3)<-c("ent_resid",  "ICD_10_1.CODE","amb", "deaths", "deaths_n")


mort_prom<-rbind(mort_1, mort_2, mort_3)
mort_prom<-mort_prom %>% group_by(ent_resid, ICD_10_1.CODE, amb) %>% 
  summarise(deaths=mean(deaths), deaths_n=mean(deaths_n))
mort_prom$year<-c("2017-2019")
mort_0$year<-c("2020")
mort_prom<-rbind(mort_0, mort_prom)
names(mort_prom)<-c("CLAVE_ENT", "ICD_10_1.CODE","amb", "deaths", "deaths_n", "year")
mort_prom$amb<-factor(mort_prom$amb, labels = c("In-hospital", "Out-of-hospital"))
age_adj_place<-mort_prom%>%dplyr::select("CLAVE_ENT","ICD_10_1.CODE", "year", "amb","deaths") %>% spread(amb,deaths) %>% mutate(ratio = `Out-of-hospital`/`In-hospital`) %>%
  dplyr::select("CLAVE_ENT", "ICD_10_1.CODE", "year","ratio")%>%
  spread(year, ratio) %>%mutate(ratio_change2=`2020`-`2017-2019`) %>% dplyr::select("CLAVE_ENT", "ICD_10_1.CODE","ratio_change2")
age_adj_place$CLAVE_ENT<-as.double(age_adj_place$CLAVE_ENT)

type1<-adj_mort_state2 %>% left_join(age_adj_place %>% filter(ICD_10_1.CODE=="Type 1 diabetes"), by="CLAVE_ENT") 
type2<-adj_mort_state2 %>% left_join(age_adj_place %>% filter(ICD_10_1.CODE=="Type 2 diabetes"), by="CLAVE_ENT")
unspecified<-adj_mort_state2 %>% left_join(age_adj_place %>% filter(ICD_10_1.CODE=="Other diabetes types"), by="CLAVE_ENT")

f2a<-adj_mort_state2 %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = ratio_change, geometry=geometry), color = "white", size = 0.1, show.legend = TRUE) +
  scale_fill_distiller(palette = "BrBG", direction = -1)+ ggtitle("Change in diabetes-related mortality ratio in 2020 compared to 2017-2019") +
  theme_map()+labs(fill="Out-of-hospital to in-hospital\nmortality ratio")
moran.test(adj_mort_state2$ratio_change, nb2listw(geo_data))

f2b<-type2 %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = ratio_change2, geometry=geometry), color = "white", size = 0.1, show.legend = TRUE) +
  scale_fill_distiller(palette = "BrBG", direction = -1)+ ggtitle("Change in diabetes-related mortality ratio in 2020 compared to 2017-2019 for type 2 diabetes") +
  theme_map()+labs(fill="Out-of-hospital to in-hospital\nmortality ratio")
moran.test(type1$ratio_change2, nb2listw(geo_data))

f2c<-type1 %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = ratio_change2, geometry=geometry), color = "white", size = 0.1, show.legend = TRUE) +
  scale_fill_distiller(palette = "BrBG", direction = -1)+ ggtitle("Change in diabetes-related mortality ratio in 2020 compared to 2017-2019 for type 1 diabetes") +
  theme_map()+labs(fill="Out-of-hospital to in-hospital\nmortality ratio")

f2d<-unspecified %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = ratio_change2, geometry=geometry), color = "white", size = 0.1, show.legend = TRUE) +
  scale_fill_distiller(palette = "BrBG", direction = -1)+ ggtitle("Change in diabetes-related mortality ratio in 2020 compared to 2017-2019 for Other diabetes types") +
  theme_map()+labs(fill="Out-of-hospital to in-hospital\nmortality ratio")

fig_x<-ggarrange(f2a, f2b, f2c, f2d,labels=LETTERS[1:4], ncol=2, nrow=2)

ggsave(fig_x,filename = "SuppFigure5.jpg", 
       width = 40, 
       height = 20,
       units=c("cm"),
       dpi = 300,
       limitsize = FALSE)


#### Supplementary Figure 6: COVID-19 and excess diabetes-related mortality ####
Fig3E<-ggdraw() +
  draw_plot(ggplot() +
              geom_sf(data = excess_7, mapping = aes(fill = bi_class, geometry=geometry), color = "white", size = 0.1, show.legend = FALSE) +
              bi_scale_fill(pal = "DkBlue", dim = 2)+ ggtitle("COVID-19 seroprevalence") +
              theme_map(), 0, 0, 1, 1) +
  draw_plot(bi_legend(pal = "DkBlue",
                      dim = 2,
                      xlab = "Higher T2D excess \n mortality per 100K hab",
                      ylab = "Higher COVID-19  \n seroprevalence (%)",
                      size = 7), 0.7, .5, 0.25, 0.25)

Fig3F<-ggdraw() +
  draw_plot(ggplot() +
              geom_sf(data = excess_2, mapping = aes(fill = bi_class, geometry=geometry), color = "white", size = 0.1, show.legend = FALSE) +
              bi_scale_fill(pal = "DkBlue", dim =2)+ ggtitle("COVID-19 incidence in diabetes") +
              theme_map(), 0, 0, 1, 1) +
  draw_plot(bi_legend(pal = "DkBlue",
                      dim = 2,
                      xlab = "Higher T2D excess \n mortality per 100K hab",
                      ylab = "Higher COVID-19 incidence \n per 100K diab",
                      size = 7), 0.7, .5, 0.25, 0.25)
Fig3G<-ggdraw() +
  draw_plot(ggplot() +
              geom_sf(data = excess_3, mapping = aes(fill = bi_class, geometry=geometry), color = "white", size = 0.1, show.legend = FALSE) +
              bi_scale_fill(pal = "DkBlue", dim = 2)+ ggtitle("COVID-19 hospitalizations") +
              theme_map(), 0, 0, 1, 1) +
  draw_plot(bi_legend(pal = "DkBlue",
                      dim = 2,
                      xlab = "Higher T2D excess \n mortality per 100K hab",
                      ylab = "Higher COVID-19 hosp. \n per 100K hab",
                      size = 7), 0.7, .5, 0.25, 0.25)
Fig3H<-ggdraw() +
  draw_plot(ggplot() +
              geom_sf(data = excess_4, mapping = aes(fill = bi_class, geometry=geometry), color = "white", size = 0.1, show.legend = FALSE) +
              bi_scale_fill(pal = "DkBlue", dim = 2)+ ggtitle("COVID-19 deaths in diabetes") +
              theme_map(), 0, 0, 1, 1) +
  draw_plot(bi_legend(pal = "DkBlue",
                      dim =2,
                      xlab = "Higher T2D excess \n mortality per 100K hab",
                      ylab = "Higher T2D COVID-19 \n mortality per 100K diab",
                      size = 7), 0.7, .5, 0.25, 0.25)

suppfig4<-ggarrange(Fig3E, Fig3F,Fig3G,Fig3H,labels=LETTERS[1:4], nrow=2, ncol=2)

ggsave(suppfig4,filename = "SuppFigure6.jpg", 
       width = 55, 
       height = 30,
       units=c("cm"),
       dpi = 300,
       limitsize = FALSE)

#### Supplementary Figure 7: Chloropleth maps of Out-of-hospital to in-hospital mortality ratio ####
excess_1_1<-bi_class(adj_mort_state2, y = diab_7_prev, x = ratio_change, style = "quantile", dim = 2)
excess_2_1<-bi_class(adj_mort_state2, y = prev_diab, x = ratio_change, style = "quantile", dim = 2)
excess_3_1<-bi_class(adj_mort_state2, y = diab_2020_undx, x = ratio_change, style = "quantile", dim = 2)
excess_4_1<-bi_class(adj_mort_state2, y = disli, x = ratio_change, style = "quantile", dim = 2)

Fig4A<-ggdraw() +
  draw_plot(ggplot() +
              geom_sf(data = excess_1_1, mapping = aes(fill = bi_class, geometry=geometry), color = "white", size = 0.1, show.legend = FALSE) +
              bi_scale_fill(pal = "DkBlue", dim = 2)+ ggtitle("HbA1c >7.5% prevalence in 2020") +
              theme_map(), 0, 0, 1, 1) +
  draw_plot(bi_legend(pal = "DkBlue",
                      dim = 2,
                      xlab = "Higher Out-of-hospital \n to in-hospital mortality ratio",
                      ylab = "Higher HbA1c >7.5% \n prevalence (%)",
                      size = 7), 0.7, .5, 0.25, 0.25)
Fig4B<-ggdraw() +
  draw_plot(ggplot() +
              geom_sf(data = excess_2_1, mapping = aes(fill = bi_class, geometry=geometry), color = "white", size = 0.1, show.legend = FALSE) +
              bi_scale_fill(pal = "DkBlue", dim = 2)+ ggtitle("Undiagnosed diabetes prevalence in 2020") +
              theme_map(), 0, 0, 1, 1) +
  draw_plot(bi_legend(pal = "DkBlue",
                      dim = 2,
                      xlab = "Higher Out-of-hospital \n to in-hospital mortality ratio",
                      ylab = "Higher undiagnosed  \n  diabetes prevalence (%)",
                      size = 7), 0.7, .5, 0.25, 0.25)
Fig4C<-ggdraw() +
  draw_plot(ggplot() +
              geom_sf(data = excess_3_1, mapping = aes(fill = bi_class, geometry=geometry), color = "white", size = 0.1, show.legend = FALSE) +
              bi_scale_fill(pal = "DkBlue", dim = 2)+ ggtitle("Overall diabetes prevalence in 2020") +
              theme_map(), 0, 0, 1, 1) +
  draw_plot(bi_legend(pal = "DkBlue",
                      dim = 2,
                      xlab = "Higher Out-of-hospital \n to in-hospital mortality ratio",
                      ylab = "Higher diabetes \n prevalence (%)",
                      size = 7), 0.7, .5, 0.25, 0.25)
Fig4D<-ggdraw() +
  draw_plot(ggplot() +
              geom_sf(data = excess_4_1, mapping = aes(fill = bi_class, geometry=geometry), color = "white", size = 0.1, show.legend = FALSE) +
              bi_scale_fill(pal = "DkBlue", dim = 2)+ ggtitle("Population-density independent Social Lag Index 2020") +
              theme_map(), 0, 0, 1, 1) +
  draw_plot(bi_legend(pal = "DkBlue",
                      dim = 2,
                      xlab = "Higher Out-of-hospital \n to in-hospital mortality ratio",
                      ylab = "Higher DISLI",
                      size = 7), 0.7, .5, 0.25, 0.25)

fig4<-ggarrange(Fig4A, Fig4C, Fig4B,Fig4D,labels=LETTERS[1:4], nrow=2, ncol=2)

ggsave(fig4,filename = "SuppFigure7.jpg", 
       width = 55, 
       height = 30,
       units=c("cm"),
       dpi = 300,
       limitsize = FALSE)

#### Analyses of Age-adjusted excess mortality per subtype ####
mort2020<-mort_2020 %>% filter(mes_ocurr!=99 & edad_agru<9 & ent_resid<33) %>%
  mutate(ICD_10_1.CODE=factor(ICD_10_1.CODE, labels = c("Type 1 diabetes", "Type 2 diabetes", "Other diabetes types","Other diabetes types","Other diabetes types")))%>%
  group_by(ent_resid, ICD_10_1.CODE,EDAD_QUIN)%>%summarise(Total_2020=n()) 
mort2019<-mort_2019 %>% filter(mes_ocurr!=99 & edad_agru<9 & ent_resid<33) %>%
  mutate(ICD_10_1.CODE=factor(ICD_10_1.CODE, labels = c("Type 1 diabetes", "Type 2 diabetes", "Other diabetes types","Other diabetes types","Other diabetes types")))%>%
  group_by(ent_resid, ICD_10_1.CODE,EDAD_QUIN)%>%summarise(Total_2019=n()) 
mort2018<-mort_2018 %>% filter(MES_OCURR!=99 & EDAD_AGRU<9 & ENT_RESID<33) %>%
  mutate(ICD_10_1.CODE=factor(ICD_10_1.CODE, labels = c("Type 1 diabetes", "Type 2 diabetes", "Other diabetes types","Other diabetes types","Other diabetes types")))%>%
  group_by(ENT_RESID, ICD_10_1.CODE,EDAD_QUIN)%>%summarise(Total_2018=n()) 
names(mort2018)<-c("ent_resid",  "ICD_10_1.CODE","EDAD_QUIN", "Total_2018")
mort2017<-mort_2017 %>% filter(MES_OCURR!=99 & EDAD_AGRU<9 & ENT_RESID<33) %>%
  mutate(ICD_10_1.CODE=factor(ICD_10_1.CODE, labels = c("Type 1 diabetes", "Type 2 diabetes", "Other diabetes types","Other diabetes types","Other diabetes types")))%>%
  group_by(ENT_RESID, ICD_10_1.CODE,EDAD_QUIN)%>%summarise(Total_2017=n()) 
names(mort2017)<-c("ent_resid", "ICD_10_1.CODE","EDAD_QUIN", "Total_2017")
mort2<-mort2020 %>% left_join(mort2019, by=c("ent_resid", "ICD_10_1.CODE","EDAD_QUIN")) %>%
  left_join(mort2018, by=c("ent_resid", "ICD_10_1.CODE","EDAD_QUIN")) %>% left_join(mort2017, by=c("ent_resid", "ICD_10_1.CODE","EDAD_QUIN"))
names(mort2)<-c("CLAVE_ENT", "ICD_10_1.CODE","EDAD_QUIN", "Total_2020", "Total_2019", "Total_2018", "Total_2017")
mort2$CLAVE_ENT<-as.numeric(mort2$CLAVE_ENT)

adj_mort_2017_state2<-mort2 %>%group_by(CLAVE_ENT, ICD_10_1.CODE,EDAD_QUIN) %>% 
  summarise(total_20_17=sum(Total_2017)) %>%
  left_join(Pob.grupos.edad.estado.2017, by =c("CLAVE_ENT","EDAD_QUIN"))%>%
  left_join(Pob.grupos.edad.total.2017,by=c("EDAD_QUIN"))%>% 
  mutate(DEF_DIAB = (total_20_17/POB.x)*(POB.y))%>% 
  group_by(CLAVE_ENT, ICD_10_1.CODE) %>% 
  summarise(DEF_ESPER_DIAB_17=sum(DEF_DIAB,na.rm = T))%>%
  group_by(CLAVE_ENT,ICD_10_1.CODE)%>% 
  mutate(DIAB_DEATHS_17 = (DEF_ESPER_DIAB_17/79678254)*100000)

adj_mort_2018_state2<-mort2 %>%  group_by(CLAVE_ENT, ICD_10_1.CODE,EDAD_QUIN) %>% 
  summarise(total_20_2018=sum(Total_2018)) %>%
  left_join(Pob.grupos.edad.estado.2018, by =c("CLAVE_ENT","EDAD_QUIN"))%>%
  left_join(Pob.grupos.edad.total.2018,by=c("EDAD_QUIN"))%>% 
  mutate(DEF_DIAB = (total_20_2018/POB.x)*POB.y)%>% 
  group_by(CLAVE_ENT,ICD_10_1.CODE) %>% 
  summarise(DEF_ESPER_DIAB_18=sum(DEF_DIAB,na.rm = T))%>%
  group_by(CLAVE_ENT,ICD_10_1.CODE)%>% 
  mutate(DIAB_DEATHS_18 = (DEF_ESPER_DIAB_18/81060179)*100000)

adj_mort_2019_state2<-mort2 %>%  group_by(CLAVE_ENT, ICD_10_1.CODE,EDAD_QUIN) %>% 
  summarise(total_20_2019=sum(Total_2019)) %>%
  left_join(Pob.grupos.edad.estado.2019, by =c("CLAVE_ENT","EDAD_QUIN"))%>%
  left_join(Pob.grupos.edad.total.2019,by=c("EDAD_QUIN"))%>% 
  mutate(DEF_DIAB = (total_20_2019/POB.x)*POB.y)%>% 
  group_by(CLAVE_ENT,ICD_10_1.CODE) %>% 
  summarise(DEF_ESPER_DIAB_19=sum(DEF_DIAB,na.rm = T))%>%
  group_by(CLAVE_ENT,ICD_10_1.CODE)%>% 
  mutate(DIAB_DEATHS_19 = (DEF_ESPER_DIAB_19/82418624)*100000)

adj_mort_2020_state2<-mort2 %>%  group_by(CLAVE_ENT, ICD_10_1.CODE,EDAD_QUIN) %>% 
  summarise(total_20_2020=sum(Total_2020)) %>%
  left_join(Pob.grupos.edad.estado.2020, by =c("CLAVE_ENT","EDAD_QUIN"))%>%
  left_join(Pob.grupos.edad.total.2020,by=c("EDAD_QUIN"))%>% 
  mutate(DEF_DIAB = (total_20_2020/POB.x)*POB.y)%>% 
  group_by(CLAVE_ENT,ICD_10_1.CODE) %>% 
  summarise(DEF_ESPER_DIAB_20=sum(DEF_DIAB,na.rm = T))%>%
  group_by(CLAVE_ENT,ICD_10_1.CODE)%>% 
  mutate(DIAB_DEATHS_20 = (DEF_ESPER_DIAB_20/83757486)*100000)

age_adj_type<-adj_mort_2017_state2 %>% left_join(adj_mort_2018_state2, by=c("CLAVE_ENT", "ICD_10_1.CODE")) %>%
  left_join(adj_mort_2019_state2, by=c("CLAVE_ENT", "ICD_10_1.CODE")) %>% left_join(adj_mort_2020_state2, by=c("CLAVE_ENT", "ICD_10_1.CODE")) 

type1<-adj_mort_state2 %>% left_join(age_adj_type %>% filter(ICD_10_1.CODE=="Type 1 diabetes"), by="CLAVE_ENT") %>%
  mutate(avg2=(DIAB_DEATHS_17+DIAB_DEATHS_18+DIAB_DEATHS_19)/3)%>%mutate(excess2=DIAB_DEATHS_20-avg2)
type2<-adj_mort_state2 %>% left_join(age_adj_type %>% filter(ICD_10_1.CODE=="Type 2 diabetes"), by="CLAVE_ENT")%>%
  mutate(avg2=(DIAB_DEATHS_17+DIAB_DEATHS_18+DIAB_DEATHS_19)/3)%>%mutate(excess2=DIAB_DEATHS_20-avg2)
unspecified<-adj_mort_state2 %>% left_join(age_adj_type %>% filter(ICD_10_1.CODE=="Other diabetes types"), by="CLAVE_ENT")%>%
  mutate(avg2=(DIAB_DEATHS_17+DIAB_DEATHS_18+DIAB_DEATHS_19)/3)%>%mutate(excess2=DIAB_DEATHS_20-avg2)


adj_mort_state2 %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = excess, geometry=geometry), color = "white", size = 0.1, show.legend = TRUE) +
  scale_fill_distiller(palette = "BrBG", direction = -1)+ ggtitle("Age-adjusted overall excess diabetes-related mortality") +
  theme_map()+labs(fill="Excess per \n100K/hab")

type2 %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = excess2, geometry=geometry), color = "white", size = 0.1, show.legend = TRUE) +
  scale_fill_distiller(palette = "BrBG", direction = -1)+ ggtitle("Age-adjusted excess mortality in type 2 diabetes") +
  theme_map()+labs(fill="Excess per \n100K/hab")

type1 %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = excess2, geometry=geometry), color = "white", size = 0.1, show.legend = TRUE) +
  scale_fill_distiller(palette = "BrBG")+ ggtitle("Age-adjusted excess mortality in type 1 diabetes") +
  theme_map()+labs(fill="Excess per \n100K/hab")

unspecified %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = excess2, geometry=geometry), color = "white", size = 0.1, show.legend = TRUE) +
  scale_fill_distiller(palette = "BrBG")+ ggtitle("Age-adjusted excess mortality in Other diabetes types") +
  theme_map()+labs(fill="Excess per \n100K/hab")

#### Lee's L correlation for determinants of excess mortality ####
# Adjacency Matrix
lw <- spdep::nb2listw(geo_data, style = "B", zero.policy = T)
W  <- as(lw, "symmetricMatrix")
W  <- as.matrix(W / Matrix::rowSums(W))
W[which(is.na(W))] <- 0
Lxy <-lee(adj_mort_state2$excess, adj_mort_state2$disli, 
          nb2listw(geo_data), length(adj_mort_state2$excess), zero.policy=TRUE, NAOK = TRUE)
LMCxy <- lee.mc(adj_mort_state2$excess, adj_mort_state2$disli, 
                nsim=10000, nb2listw(geo_data), zero.policy=TRUE)


# Global Lee's L for all covariates

spdep::lee.test(adj_mort_state2$excess, adj_mort_state2$disli, 
                nb2listw(geo_data),
                zero.policy = TRUE, alternative = "two.sided", na.action = na.omit)
spdep::lee.test(adj_mort_state2$excess, adj_mort_state2$covid_rate, 
                nb2listw(geo_data),
                zero.policy = TRUE, alternative = "two.sided", na.action = na.omit)
spdep::lee.test(adj_mort_state2$excess, adj_mort_state2$hosp, 
                nb2listw(geo_data),
                zero.policy = TRUE, alternative = "two.sided", na.action = na.omit)
spdep::lee.test(adj_mort_state2$excess, adj_mort_state2$covid_diab, 
                nb2listw(geo_data),
                zero.policy = TRUE, alternative = "two.sided", na.action = na.omit)
spdep::lee.test(adj_mort_state2$excess, log(adj_mort_state2$hosp), 
                nb2listw(geo_data),
                zero.policy = TRUE, alternative = "two.sided", na.action = na.omit)
spdep::lee.test(adj_mort_state2$excess, adj_mort_state2$diab_no, 
                nb2listw(geo_data),
                zero.policy = TRUE, alternative = "two.sided", na.action = na.omit)
spdep::lee.test(adj_mort_state2$disli, adj_mort_state2$diab_7_prev, 
                nb2listw(geo_data),
                zero.policy = TRUE, alternative = "two.sided", na.action = na.omit)
spdep::lee.test(adj_mort_state2$excess, adj_mort_state2$hosp, 
                nb2listw(geo_data),
                zero.policy = TRUE, alternative = "two.sided", na.action = na.omit)
spdep::lee.test(adj_mort_state2$ratio_change, adj_mort_state2$disli, 
                nb2listw(geo_data),
                zero.policy = TRUE, alternative = "two.sided", na.action = na.omit)
spdep::lee.test(adj_mort_state2$ratio_change, adj_mort_state2$diab_7_prev, 
                nb2listw(geo_data),
                zero.policy = TRUE, alternative = "two.sided", na.action = na.omit)
spdep::lee.test(adj_mort_state2$ratio_change, log(adj_mort_state2$covid_hosp), 
                nb2listw(geo_data),
                zero.policy = TRUE, alternative = "two.sided", na.action = na.omit)
spdep::lee.test(adj_mort_state2$ratio_change, adj_mort_state2$diab_2020_undx, 
                nb2listw(geo_data),
                zero.policy = TRUE, alternative = "two.sided", na.action = na.omit)


# Local Lee's L simulations # 1

set.seed(123);local_sims <- simula_lee(adj_mort_state2$excess, adj_mort_state2$disli, 
                         nb2listw(geo_data),
                         nsim = 10000,
                         zero.policy = TRUE,
                         na.action = na.omit)

m_i <- Lxy[[2]]  # local values

# Identify the significant values 
alpha <- 0.05  # for a 95% confidence interval
probs <- c(alpha/2, 1-alpha/2)
intervals <- t(apply(t(local_sims[[2]]), 1, function(x) quantile(x, probs = probs)))
sig <- ( m_i < intervals[ , 1] ) | ( m_i > intervals[ , 2] )

# ----------------------------------------------------- #
# Prepare for plotting
adj_mort_state2$sig <- sig

# Identify the Lee's L clusters
Xp <- scale(adj_mort_state2$excess)[ , 1]
Yp <- scale(adj_mort_state2$disli)[ , 1]

patterns <- as.character(interaction(Xp > 0, W %*% Yp > 0)) 
patterns <- patterns %>% 
  stringr::str_replace_all("TRUE","High") %>% 
  stringr::str_replace_all("FALSE","Low")
patterns[adj_mort_state2$sig == 0] <- "Not significant"
adj_mort_state2$patterns <- patterns

# Rename Lee's L clusters
adj_mort_state2$patterns2 <- factor(adj_mort_state2$patterns,
                           levels = c("High.High", "High.Low", "Low.High", "Low.Low", "Not significant"),
                           labels = c("High excess - High DISLI", "High excess - Low DISLI", "Low excess - High DISLI","Low excess - Low DISLI", "Not significant"))

# ----------------------------------------------------- #
# Plot
ggplot() +
  geom_sf(data = adj_mort_state2, aes(fill = patterns2,geometry=geometry)) +
  theme_map()+labs(fill="Lee's L")+scale_fill_jama()

set.seed(123);local_sims <- simula_lee(adj_mort_state2$disli, adj_mort_state2$diab_7_prev, 
                                       nb2listw(geo_data),
                                       nsim = 10000,
                                       zero.policy = TRUE,
                                       na.action = na.omit)

m_i <- Lxy[[2]]  # local values

# Identify the significant values 
alpha <- 0.05  # for a 95% confidence interval
probs <- c(alpha/2, 1-alpha/2)
intervals <- t(apply(t(local_sims[[2]]), 1, function(x) quantile(x, probs = probs)))
sig <- ( m_i < intervals[ , 1] ) | ( m_i > intervals[ , 2] )

# ----------------------------------------------------- #
# Prepare for plotting
adj_mort_state2$sig <- sig

# Identify the Lee's L clusters
Xp <- scale(adj_mort_state2$disli)[ , 1]
Yp <- scale(adj_mort_state2$diab_7_prev)[ , 1]

patterns <- as.character(interaction(Xp > 0, W %*% Yp > 0)) 
patterns <- patterns %>% 
  stringr::str_replace_all("TRUE","High") %>% 
  stringr::str_replace_all("FALSE","Low")
patterns[adj_mort_state2$sig == 0] <- "Not significant"
adj_mort_state2$patterns <- patterns

# Rename Lee's L clusters
adj_mort_state2$patterns2 <- factor(adj_mort_state2$patterns,
                                    levels = c("High.High", "High.Low", "Low.High", "Low.Low", "Not significant"),
                                    labels = c("High DISLI - High HbA1c >7.5%", "High DISLI - Low HbA1c >7.5%", "Low DISLI - High HbA1c >7.5%","Low DISLI - Low HbA1c >7.5%", "Not significant"))

# ----------------------------------------------------- #
# Plot
ggplot() +
  geom_sf(data = adj_mort_state2, aes(fill = patterns2,geometry=geometry)) +
  theme_map()+labs(fill="Lee's L")+scale_fill_jama()

#### Regression models for excess mortality ####
m1 <- glm.nb(ratio_change~log(covid_hosp),data=adj_mort_state2, link = "log")
summary(m1)
summ(m1, exp=T, confint=T)
lm.morantest(m1, listw = nb2listw(geo_data))
vif(m1)

moran.test(m1, )

BIC(m1)

m2 <- glm.nb(excess2+1~disli+log(covid_hosp),data=type2, link = "log")
summary(m2)
summ(m2, exp=T, confint=T)
lm.morantest(m2, listw = nb2listw(geo_data))

m3 <- glm.nb(excess2~hba1c_med,data=type1, link = "log")
summary(m3)
summ(m3, exp=T, confint=T)
lm.morantest(m3, listw = nb2listw(geo_data))

m4 <- glm.nb(excess2+4~unspecified$disli+unspecified$prev_diab,data=unspecified, link = "log")
summary(m4)
summ(m4, exp=T, confint=T)
lm.morantest(m4, listw = nb2listw(geo_data))

m5 <- glm.nb(ratio_change~log(covid_hosp),data=adj_mort_state2, link = "log")
summary(m5)
summ(m1, exp=T, confint=T)
lm.morantest(m1, listw = nb2listw(geo_data))
vif(m1)

BIC(m1)

