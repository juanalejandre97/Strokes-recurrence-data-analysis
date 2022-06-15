library(readxl)
library(dplyr)

#Se va a crear el conjunto de datos con todos los registros y todos los c?digos sean ICTUS o no, con la codificaci?n CIE9


#insertamos ambos ficheros ofrecidos por el hospital 
#virgen del Roc?o

#Mapeos_cie9_cie10 <- read_excel("Mapeos_cie9-cie10.xlsx", sheet = "9mc-10mc")
################### DATOS CIE9#######################
SHA512_CIE9 <- read_excel("SHA512_CIE9.xlsx")[-1]

SHA512_CIE9<-unique(SHA512_CIE9)#baja de dimensi?n por lo tanto hay elementos repetidos
# SIDCA_OWN.AR_DIAGNOSTICO
SHA512_CIE9$DIAGNOSTICO<-gsub("SIDCA_OWN.AR_DIAGNOSTICO","",SHA512_CIE9$DIAGNOSTICO,ignore.case = FALSE)
SHA512_CIE9$DIAGNOSTICO<-gsub("[(]","",SHA512_CIE9$DIAGNOSTICO)
SHA512_CIE9$DIAGNOSTICO<-gsub(")","",SHA512_CIE9$DIAGNOSTICO)
SHA512_CIE9$DIAGNOSTICO<-gsub("'","",SHA512_CIE9$DIAGNOSTICO)
SHA512_CIE9$DIAGNOSTICO<-ifelse(SHA512_CIE9$DIAGNOSTICO=="",NA,SHA512_CIE9$DIAGNOSTICO)
SHA512_CIE9$DIAGNOSTICO<-gsub(", ",",",SHA512_CIE9$DIAGNOSTICO)


#Eliminamos aquellos elementos que tengan los diagn?sticos vac?os
SHA512_CIE9<-SHA512_CIE9 %>% 
  filter(is.na(DIAGNOSTICO)==F) #68769

#Se va a eliminar la hora tanto de fecha de ingreso como de alta
SHA512_CIE9$FCH_INGRESO<-as.character(SHA512_CIE9$FCH_INGRESO)
SHA512_CIE9$FCH_ALTA<-as.character(SHA512_CIE9$FCH_ALTA)
SHA512_CIE9$FCH_INGRESO<-substr(SHA512_CIE9$FCH_INGRESO,1,10)
SHA512_CIE9$FCH_ALTA<-substr(SHA512_CIE9$FCH_ALTA,1,10)
SHA512_CIE9$FCH_INGRESO<-as.Date(SHA512_CIE9$FCH_INGRESO)
SHA512_CIE9$FCH_ALTA<-as.Date(SHA512_CIE9$FCH_ALTA)

SHA512_CIE9<-SHA512_CIE9 %>% 
  select(-POA)
####

library(splitstackshape)
SHA512_CIE9<-cSplit(SHA512_CIE9,"DIAGNOSTICO",",",drop=F)
colnames(SHA512_CIE9)<-c("Id","DIAGNOSTICO","FCH_INGRESO","FCH_ALTA",paste0("cod","_",seq(1:25)))


SHA512_CIE9$cod_1<-as.character(SHA512_CIE9$cod_1)
SHA512_CIE9$cod_2<-as.character(SHA512_CIE9$cod_2)
SHA512_CIE9$cod_3<-as.character(SHA512_CIE9$cod_3)
SHA512_CIE9$cod_4<-as.character(SHA512_CIE9$cod_4)
SHA512_CIE9$cod_5<-as.character(SHA512_CIE9$cod_5)
SHA512_CIE9$cod_6<-as.character(SHA512_CIE9$cod_6)
SHA512_CIE9$cod_7<-as.character(SHA512_CIE9$cod_7)
SHA512_CIE9$cod_8<-as.character(SHA512_CIE9$cod_8)
SHA512_CIE9$cod_9<-as.character(SHA512_CIE9$cod_9)
SHA512_CIE9$cod_10<-as.character(SHA512_CIE9$cod_10)
SHA512_CIE9$cod_11<-as.character(SHA512_CIE9$cod_11)
SHA512_CIE9$cod_12<-as.character(SHA512_CIE9$cod_12)
SHA512_CIE9$cod_13<-as.character(SHA512_CIE9$cod_13)
SHA512_CIE9$cod_14<-as.character(SHA512_CIE9$cod_14)
SHA512_CIE9$cod_15<-as.character(SHA512_CIE9$cod_15)
SHA512_CIE9$cod_16<-as.character(SHA512_CIE9$cod_16)
SHA512_CIE9$cod_17<-as.character(SHA512_CIE9$cod_17)
SHA512_CIE9$cod_18<-as.character(SHA512_CIE9$cod_18)
SHA512_CIE9$cod_19<-as.character(SHA512_CIE9$cod_19)
SHA512_CIE9$cod_20<-as.character(SHA512_CIE9$cod_20)
SHA512_CIE9$cod_21<-as.character(SHA512_CIE9$cod_21)
SHA512_CIE9$cod_22<-as.character(SHA512_CIE9$cod_22)
SHA512_CIE9$cod_23<-as.character(SHA512_CIE9$cod_23)
SHA512_CIE9$cod_24<-as.character(SHA512_CIE9$cod_24)
SHA512_CIE9$cod_25<-as.character(SHA512_CIE9$cod_25)
SHA512_CIE9<-SHA512_CIE9 %>% 
  select(Id,DIAGNOSTICO,cod_1:cod_25,FCH_INGRESO,FCH_ALTA)

#################################################################

#Filtramos aquellos pacientes que al menos hayan tenido un c?digo ICTUS registrado 
#en los tres primeros codigos

codigos_ictus_rocio<-c("431","432.0","432.1","432.9","433.00","433.01","433.10","433.11","433.20",
                       "433.21","433.30","433.31","433.80","433.81","433.90","433.91","434.00","434.01","434.10","434.11","434.90","434.91",
                       "435.0","435.1","435.2","435.3","435.8","435.9","436","437.0",
                       "437.1","437.2","437.3","437.4","437.5","437.6","437.8","437.9")

codigos_ictus_juan <-c("434.00","434.01","434.10","434.11","434.90","434.91","432.0","432.1","432.9","435.0","435.1","435.2","435.3","435.8","435.9","436","431")

"SHA512_CIE9_ICTUS<-SHA512_CIE9 %>% 
  dplyr::filter(cod_1 %in% codigos_ictus_rocio | cod_2 %in% codigos_ictus_rocio |
                  cod_3 %in% codigos_ictus_rocio | cod_4 %in% codigos_ictus_rocio |
                  cod_5 %in% codigos_ictus_rocio | cod_6 %in% codigos_ictus_rocio |
                  cod_7 %in% codigos_ictus_rocio | cod_8 %in% codigos_ictus_rocio |
                  cod_9 %in% codigos_ictus_rocio | cod_10 %in% codigos_ictus_rocio |
                  cod_11 %in% codigos_ictus_rocio | cod_12 %in% codigos_ictus_rocio |
                  cod_13 %in% codigos_ictus_rocio | cod_14 %in% codigos_ictus_rocio |
                  cod_15 %in% codigos_ictus_rocio | cod_16 %in% codigos_ictus_rocio |
                  cod_17 %in% codigos_ictus_rocio | cod_18 %in% codigos_ictus_rocio |
                  cod_19 %in% codigos_ictus_rocio | cod_20 %in% codigos_ictus_rocio |
                  cod_21 %in% codigos_ictus_rocio | cod_22 %in% codigos_ictus_rocio |
                  cod_23 %in% codigos_ictus_rocio | cod_24 %in% codigos_ictus_rocio |
                  cod_25 %in% codigos_ictus_rocio, FALSE )" #24918 REGISTROS DIFERENTES

SHA512_CIE9_ICTUS<-SHA512_CIE9 %>%
  dplyr::filter(cod_1 %in% codigos_ictus_juan | cod_2 %in% codigos_ictus_juan | cod_3 %in% codigos_ictus_juan)


########################################################

########################################################
#Sacamos los FR m?s frecuentes.Para ello pasamos los c?digos a formato largo y hacemos un conteo.
#Posteriormente veremos que enfermedades se van a tratar coomo un factor de riesgo en el ICTUS.

PACIENTES_DIF<-length(unique(SHA512_CIE9_ICTUS$Id)) #18669 pacientes diferentes con ICTUs

library(tidyr)
CODIGOS_LARGO<-gather(SHA512_CIE9_ICTUS,"orden","codigo",c(3:27))
CODIGOS_LARGO<-CODIGOS_LARGO %>% 
  filter(is.na(codigo)==F)

CODIGOS_LARGO_SIN_ICTUS<-CODIGOS_LARGO %>% 
  mutate(codigo=ifelse(codigo %in% codigos_ictus_rocio,NA,codigo))
CODIGOS_LARGO_SIN_ICTUS<-CODIGOS_LARGO_SIN_ICTUS %>% 
  filter(is.na(codigo)==F)

CODIGOS_FREC_PACIENTE<-CODIGOS_LARGO_SIN_ICTUS %>%
  select(Id,codigo) %>%
  group_by(Id,codigo) %>%
  count() %>% 
  arrange(desc(n))#aqu? se ve que hay pacientes que tienen registrado el mismo c?digo en varias ocasiones
Enfermedades_frecuentes<-CODIGOS_FREC_PACIENTE %>% group_by(codigo) %>% count() %>% arrange(desc(n)) %>% 
  mutate(porc=n/PACIENTES_DIF)

#Filtramos aquellas enfermedades que presentan al menos un 10% de los pacientes

Enfermedades_frecuentes<-Enfermedades_frecuentes %>% 
  filter(porc > 0.10)


#Mapeos_cie9_cie10<-data.frame(Mapeos_cie9_cie10)
#nombres_cie9<-Mapeos_cie9_cie10 %>% select(descripcion.cie9mc,cie9mc)

#mapeamos

#Enfermedades_frecuentes_nombres<-inner_join(Enfermedades_frecuentes,nombres_cie9,by=c("codigo"="cie9mc"))
#Enfermedades_frecuentes_nombres<-unique(Enfermedades_frecuentes_nombres)
# 401.9 Hipertensi?n arterial
# 250.00 Diabetes
#272.4 Hiperlipidemia
#305.1 y v15.82 Trastorno por tabaco
#427.31 Fibrilaci?n auricular



###################################################################################################
#A continuaci?n, seleccionamos de la base de datos en cie10, aquellos pacientes que tienen al menos un c?digo ICTUS
#de los c?digos que no sean ICTUS, nos vamos a quedar solo con aquellos que han salido enfermedades frecuentesd
#en la base de datos anterior.

SHA512_CIE_10 <- read_excel("SHA512_CIE_10_Nepisodios.xlsx") #65404 
#colnames(SHA512_CIE_10)
SHA512_CIE_10<-SHA512_CIE_10[-1]
# 
# #Eliminamos los elementos duplicados
#SHA512_CIE_10<-unique(SHA512_CIE_10)
SHA512_CIE_10<-SHA512_CIE_10 %>% 
  select(Id,FCH_INGRESO,FCH_ALTA,CODIGO)
SHA512_CIE_10<-unique(SHA512_CIE_10)
###

codigos_interes_rocio_cie10<-c("I62.00","I62.1","I62.9",
                               "I63.019","I63.119","I63.139","I63.20","I63.219","I63.22","I63.239","I63.30","I63.40","I63.50", "I63.59",
                               "I65.09","I65.1","I65.29","I65.8","I65.9","I66.09","I66.19","I66.29","I66.9",
                               "I67.1","I67.2","I67.4","I67.5","I67.7","I67.81","I67.82",
                               "I67.83","I67.848","I67.89","I10","I16.9","E11.9","I48.91","E78.4" , "E78.5","F17.200","Z87.891")



# Quiero que los c?digos que no sean codigos_interes_rocio_cie10, me ponga NA

SHA512_CIE_10<-SHA512_CIE_10 %>% 
  mutate(CODIGO = ifelse(CODIGO %in% codigos_interes_rocio_cie10,CODIGO,NA))

SHA512_CIE_10<-SHA512_CIE_10 %>% 
  filter(is.na(CODIGO)==F) #11528 
#SHA512_CIE_10<-unique(SHA512_CIE_10)
transf_cie10<-c("432.1","432.0","432.9",
                "433.21","433.21","433.11","433.91","433.21","433.01","433.11","434.01","434.11","434.91", "433.81",
                "433.20","433.00","433.10","433.30","433.90","434.00","434.00","434.00","434.10",
                "437.3","437.0","437.2","437.5","437.4","437.1","437.1",
                "348.39","435.9","436","401.9","401.9","250.00","427.31","272.4","272.4","305.1","v15.82")
# 401.9 Hipertensi?n arterial (conversi?n CIE10 : "I10","I16.9")
# 250.00 Diabetes (E11.9)
#427.31 Fibrilaci?n auricular  (I48.91)
#272.4 Hiperlipidemia (E78.4 Y E78.5)
#305.1 y v15.82 Trastorno por tabaco F17.200 y Z87.891



# 
MAPEO<-data.frame(cie10=codigos_interes_rocio_cie10,cie9=transf_cie10)

#SHA512_CIE_10<-SHA512_CIE_10 %>%
# filter(CODIGO %in%codigos_interes_rocio_cie10) #11528
#SHA512_CIE_10<-unique(SHA512_CIE_10)

#Se va a comprobar que son pacientes distintos 
SHA512_CIE_10$FCH_ALTA<-as.character(SHA512_CIE_10$FCH_ALTA)
SHA512_CIE_10$FCH_INGRESO<-as.character(SHA512_CIE_10$FCH_INGRESO)
# 
SHA512_CIE_10<-SHA512_CIE_10 %>%
  mutate(FCH_INGRESO=substr(FCH_INGRESO,1,10),
         FCH_ALTA=substr(FCH_ALTA,1,10))
SHA512_CIE_10$FCH_ALTA<-as.Date(SHA512_CIE_10$FCH_ALTA)
SHA512_CIE_10$FCH_INGRESO<-as.Date(SHA512_CIE_10$FCH_INGRESO)

SHA512_CIE_10<-SHA512_CIE_10 %>%
  inner_join(MAPEO,by=c("CODIGO" = "cie10"))

SHA512_CIE_10<-SHA512_CIE_10 %>% 
  select(Id,FCH_INGRESO,FCH_ALTA,cie9)
library(splitstackshape)
colnames(SHA512_CIE_10)<-c("Id","FCH_INGRESO","FCH_ALTA","CODIGO")

CIE10_ANCHO<-SHA512_CIE_10 %>% 
  arrange(Id,FCH_ALTA) %>% 
  group_by(Id,FCH_INGRESO,FCH_ALTA) %>% 
  mutate(CODIGO=paste(CODIGO,collapse=","))
CIE10_ANCHO<-unique(CIE10_ANCHO)
CIE10_ANCHO<-cSplit(CIE10_ANCHO,"CODIGO",drop=F)

CIE10_ANCHO$CODIGO_1<-as.character(CIE10_ANCHO$CODIGO_1)
CIE10_ANCHO$CODIGO_2<-as.character(CIE10_ANCHO$CODIGO_2)
CIE10_ANCHO$CODIGO_3<-as.character(CIE10_ANCHO$CODIGO_3)
CIE10_ANCHO$CODIGO_4<-as.character(CIE10_ANCHO$CODIGO_4)
CIE10_ANCHO$CODIGO_5<-as.character(CIE10_ANCHO$CODIGO_5)
CIE10_ANCHO$CODIGO_6<-as.character(CIE10_ANCHO$CODIGO_6)
#Estas no se porque se pusieron hay solo 6 codigos diferentes
#CIE10_ANCHO$CODIGO_7<-as.character(CIE10_ANCHO$CODIGO_7)
#CIE10_ANCHO$CODIGO_8<-as.character(CIE10_ANCHO$CODIGO_8)
#CIE10_ANCHO$CODIGO_9<-as.character(CIE10_ANCHO$CODIGO_9)

colnames(CIE10_ANCHO)<-c("Id","FCH_INGRESO","FCH_ALTA","DIAGNOSTICO",paste0("cod","_",seq(1:6)))

CIE10<-CIE10_ANCHO %>% 
  select(Id,DIAGNOSTICO,cod_1:cod_6,FCH_INGRESO,FCH_ALTA)

#De los CIE10, vamos a imponer que al menos uno de los c?dios registrados durante el ingreso sea un c?digo ICTUS
CIE10_ictus<-CIE10 %>% 
  dplyr::filter(cod_1 %in% codigos_ictus_juan | cod_2 %in% codigos_ictus_juan |
                  cod_3 %in% codigos_ictus_juan)
CIE10_ictus$cod_7<-NA
CIE10_ictus$cod_8<-NA
CIE10_ictus$cod_9<-NA
CIE10_ictus$cod_10<-NA
CIE10_ictus$cod_11<-NA
CIE10_ictus$cod_12<-NA
CIE10_ictus$cod_13<-NA
CIE10_ictus$cod_14<-NA
CIE10_ictus$cod_15<-NA
CIE10_ictus$cod_16<-NA
CIE10_ictus$cod_17<-NA
CIE10_ictus$cod_18<-NA
CIE10_ictus$cod_19<-NA
CIE10_ictus$cod_20<-NA
CIE10_ictus$cod_21<-NA
CIE10_ictus$cod_22<-NA
CIE10_ictus$cod_23<-NA
CIE10_ictus$cod_24<-NA
CIE10_ictus$cod_25<-NA


#1345 registros de #5333 que hab?a
##cargar hasta aqui

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
ictus_s_e<- read_excel("ICTUS_FINAL_LARGO_EDAD_SEX.xlsx")
codigos_ictus_TODOS<-union_all(SHA512_CIE9_ICTUS,CIE10) %>% arrange(FCH_INGRESO)
codigos_ictus_TODOS<-left_join(codigos_ictus_TODOS,ictus_s_e,by="Id")
codigos_ictus_TODOS<-codigos_ictus_TODOS %>%
  select(Id,DIAGNOSTICO.x,cod_1,cod_2,cod_3,cod_4,cod_5,cod_6,cod_7,cod_8,cod_9,cod_10,cod_11,cod_12,cod_13,cod_14,cod_15,cod_16,cod_17,cod_18,cod_19,cod_20,cod_21,cod_22,cod_23,cod_24,cod_25,SEXO,FCH_INGRESO.x,FCH_ALTA.x,FCH_NACIMIENTO,FCH_DEFUNCION)
codigos_ictus_TODOS<-unique(codigos_ictus_TODOS)
codigos_ictus_TODOS$FCH_NACIMIENTO<-as.Date(codigos_ictus_TODOS$FCH_NACIMIENTO)
codigos_ictus_TODOS<-codigos_ictus_TODOS %>%
  mutate(EDAD=round((FCH_INGRESO.x-FCH_NACIMIENTO)/365,0))
codigos_ictus_TODOS$EDAD<-as.numeric(codigos_ictus_TODOS$EDAD)
codigos_ictus_TODOS<-rename(codigos_ictus_TODOS,DIAGNOSTICO=DIAGNOSTICO.x,FCH_INGRESO=FCH_INGRESO.x,FCH_ALTA=FCH_ALTA.x)
#veamos el n?mero de pacientes diferentes en 26263 registros


pacientes_distintos<-codigos_ictus_TODOS %>% 
  group_by(Id) %>% count() #19431 pacientes diferentes #Hay un m?ximo de 15 ictus diferentes en un paciente

###### Objtevos: crear un dataframe con las variables: 
# 1. ICTUS = SI/NO
# 2. HIPERTENSION = SI/NO
# 3. DIABETES = SI/NO
# 4. HIPERLIPIDEMIA = SI/NO
# 5. FUMADOR = SI/NO
# 6. FA = SI/NO
# 7. REPETICION_INGRESO =SI/NO
# Esto por ahora.

ICTUS_FINAL<-codigos_ictus_TODOS

ICTUS_FINAL<-ICTUS_FINAL %>% 
  mutate(ICTUS = "1",
         HIPERTENSION=ifelse(grepl("401.9",DIAGNOSTICO)==T,"1","0"),
         DIABETES=ifelse(grepl("250.00",DIAGNOSTICO)==T,"1","0"),
         FA=ifelse(grepl("427.31",DIAGNOSTICO)==T,"1","0"),
         HIPERLIPIDEMIA=ifelse(grepl("272.4",DIAGNOSTICO)==T,"1","0"),
         TABAQUISMO=ifelse(grepl("305.1",DIAGNOSTICO)==T |grepl("v15.82",DIAGNOSTICO)==T,"1","0"))

ICTUS_FINAL$HIPERTENSION<-as.numeric(ICTUS_FINAL$HIPERTENSION)
ICTUS_FINAL$DIABETES<-as.numeric(ICTUS_FINAL$DIABETES)
ICTUS_FINAL$FA<-as.numeric(ICTUS_FINAL$FA)
ICTUS_FINAL$HIPERLIPIDEMIA<-as.numeric(ICTUS_FINAL$HIPERLIPIDEMIA)
ICTUS_FINAL$TABAQUISMO<-as.numeric(ICTUS_FINAL$TABAQUISMO)

#Yo a?adir?a un unique para evitar datos repetidos
ICTUS_FINAL<-unique(ICTUS_FINAL)

#######################################################################
#AQUI VEO UN POSIBLE ERROR EN EL FILTRADO Y CONTEO????????###REVISAR###
#######################################################################

#Adem?s lo cambiaria haciendo que si dos tienen el mismo id y en alguno de los ictus se le ha anotado un FR que lo tenga en todos lo ictus


ICTUS_FINAL<-ICTUS_FINAL %>% group_by(Id) %>% mutate(ictus_anteriores = row_number()-1)
ICTUS_FINAL$numFR<-NA
ICTUS_FINAL$numFR<-apply(ICTUS_FINAL[,c(35:39)],1,sum,na.rm=T)

ICTUS_FINAL$HIPERTENSION<-as.character(ICTUS_FINAL$HIPERTENSION)
ICTUS_FINAL$DIABETES<-as.character(ICTUS_FINAL$DIABETES)
ICTUS_FINAL$FA<-as.character(ICTUS_FINAL$FA)
ICTUS_FINAL$HIPERLIPIDEMIA<-as.character(ICTUS_FINAL$HIPERLIPIDEMIA)
ICTUS_FINAL$TABAQUISMO<-as.character(ICTUS_FINAL$TABAQUISMO)

#Dejamos solo los c?digos ICTUS, puesto que ya tenemos los distintos factores de riesgo


ICTUS_FINAL<-ICTUS_FINAL %>%
  mutate(cod_1=ifelse(cod_1 %in%codigos_ictus_juan,cod_1,NA),
         cod_2=ifelse(cod_2 %in%codigos_ictus_juan,cod_2,NA),
         cod_3=ifelse(cod_3 %in%codigos_ictus_juan,cod_3,NA),
         cod_4=ifelse(cod_4 %in%codigos_ictus_juan,cod_4,NA),
         cod_5=ifelse(cod_5 %in%codigos_ictus_juan,cod_5,NA),
         cod_6=ifelse(cod_6 %in%codigos_ictus_juan,cod_6,NA),
         cod_7=ifelse(cod_7 %in%codigos_ictus_juan,cod_7,NA),
         cod_8=ifelse(cod_8 %in%codigos_ictus_juan,cod_8,NA),
         cod_9=ifelse(cod_9 %in%codigos_ictus_juan,cod_9,NA),
         cod_10=ifelse(cod_10 %in%codigos_ictus_juan,cod_10,NA),
         cod_11=ifelse(cod_11 %in%codigos_ictus_juan,cod_11,NA),
         cod_12=ifelse(cod_12 %in%codigos_ictus_juan,cod_12,NA),
         cod_13=ifelse(cod_13 %in%codigos_ictus_juan,cod_13,NA),
         cod_14=ifelse(cod_14 %in%codigos_ictus_juan,cod_14,NA),
         cod_15=ifelse(cod_15 %in%codigos_ictus_juan,cod_15,NA),
         cod_16=ifelse(cod_16 %in%codigos_ictus_juan,cod_16,NA),
         cod_17=ifelse(cod_17 %in%codigos_ictus_juan,cod_17,NA),
         cod_18=ifelse(cod_18 %in%codigos_ictus_juan,cod_18,NA),
         cod_19=ifelse(cod_19 %in%codigos_ictus_juan,cod_19,NA),
         cod_20=ifelse(cod_20 %in%codigos_ictus_juan,cod_20,NA),
         cod_21=ifelse(cod_21 %in%codigos_ictus_juan,cod_21,NA),
         cod_22=ifelse(cod_22 %in%codigos_ictus_juan,cod_22,NA),
         cod_23=ifelse(cod_23 %in%codigos_ictus_juan,cod_23,NA),
         cod_24=ifelse(cod_24 %in%codigos_ictus_juan,cod_24,NA),
         cod_25=ifelse(cod_25 %in%codigos_ictus_juan,cod_25,NA))

#Yo lo corregir?a filtrando de modo que solo me quedo con los que tienen un ictus en el primer c?digo
#ICTUS_FINAL<-ICTUS_FINAL%>%
#  dplyr::filter(cod_1 %in% codigos_ictus_juan)

ICTUS_FINAL$numIctus_Ingreso<-NA
ICTUS_FINAL$numIctus_Ingreso<-apply(is.na(ICTUS_FINAL[,c(3:27)])==F,1,sum)

#PASAMOS A FORMATO LARGO 

library(tidyr)
ICTUS_FINAL_LARGO<-gather(ICTUS_FINAL,"orden","codigo",c(3:27))
ICTUS_FINAL_LARGO<-ICTUS_FINAL_LARGO %>% filter(is.na(codigo)==F)
ICTUS_FINAL_LARGO<-ICTUS_FINAL_LARGO %>% 
  select(Id,DIAGNOSTICO,codigo,HIPERTENSION,DIABETES,FA,HIPERLIPIDEMIA,TABAQUISMO,numFR,ictus_anteriores,numIctus_Ingreso,codigo,FCH_INGRESO,FCH_ALTA)

ICTUS_FINAL_LARGO<-ICTUS_FINAL_LARGO %>% mutate(tiempo_ingreso= FCH_ALTA-FCH_INGRESO) #30753 registros


#Vamos a atender un momento a aquellos pacientes, los cuales durante el ingreso tuvieron m?s de un c?digo ICTUS, 
#y centraremos la atenci?n a el tiempo de ingreso, y a s? ver si se puede establecer un criterio de los d?as en los q repite.

ICTUS_REP_INGRESO<-ICTUS_FINAL_LARGO %>% filter(numIctus_Ingreso>1)
#Aqu? est?n ducplicados, o triplicados, dependiendo de? n?mero de ICTUS en el ingreso, es decir, aparece una observaci?n 
#por ICTUS. Vamos a quitar la variable no com?n que es c?digo

ICTUS_REP_INGRESO<-ICTUS_REP_INGRESO %>% select(-codigo) %>% unique() #3831 registros en los que se repite 
#un ictus durante el ingreso, saquemos algunos estad?sticos de la media de d?as de ingreso 


#Incluimos la funci?n creada en Drimay de los estad?sticos.

my_summary<- function(x){
  # {
  #   x_num <- sapply(x, is.numeric) #cogemos de los datos aquellas que son num?ricas
  #   x<-x[,x_num] #nos quedamos s?lo con las que lo son
  min=apply(x,2,min,na.rm = TRUE)
  media = round(apply(x,2,mean,na.rm = TRUE),2)
  mediana = round(apply(x,2,median,na.rm = TRUE),2)
  max = apply(x,2,max,na.rm = TRUE)
  sd = round(apply(x,2,sd,na.rm = TRUE),2)
  
  tabla<-data.frame(min, max, media, mediana,sd)
  
  return (tabla)}
ICTUS_REP_INGRESO$tiempo_ingreso<-as.numeric(ICTUS_REP_INGRESO$tiempo_ingreso)
my_summary(ICTUS_REP_INGRESO[13]) #La media de d?as de ingreso en pacientes  a los que le repite el ictus es 15 d?as aprox
#Veamos el tiempo de ingreso en pacientes que no les repite 

ICTUS_NO_REP_INGRESO<-ICTUS_FINAL_LARGO %>% filter(numIctus_Ingreso==1)
ICTUS_NO_REP_INGRESO<-ICTUS_NO_REP_INGRESO %>% select(-codigo) %>% unique()
ICTUS_NO_REP_INGRESO$tiempo_ingreso<-as.numeric(ICTUS_NO_REP_INGRESO$tiempo_ingreso)
my_summary(ICTUS_NO_REP_INGRESO[13])

#ICTUS CON REPETICION
#                min max media mediana    sd
# tiempo_ingreso   0 401 22.25      12 33.55

#ICTUA SIN REPETICION
#               min max media mediana    sd
#tiempo_ingreso   0 714 13.59       8 21.98


#Puede apreciarse que los tiempos medios de ingreso no var?an, exceptuando el caso del m?ximo, 
#que es mayor en el caso de mo repetici?n de ictus, por lo que no parece que influya demasiado, 
#en el n?mero d d?as de ingreso, el n?mero de ICTUS que le ha dado, por lo tanto, se va a
#seguir bajo la suposic?n que en el caso de haberle repetido el ICTUS, este le ha dado durante las 
#primeras 24 horas. (1 d?a)

#PODRIAMOS APLICAR UN TEST DE INFERENCIA ESTADISTICA (COMO EL ANOVA TEST) PARA QUE LAS CONCLUSIONES
#QUE TOMEMOS SEAN MAS OBJETIVA

#Se va a estudiar las correlaciones existentes entre FR.

#Correlaci?n de distancias de Rizzo, Sz?kely. 
# Si se quiere obtener este coeficiente en R, se puede usar la funci?n dcov.test del paquete energy.
ICTUS_FINAL_LARGO$HIPERTENSION<-as.factor(ICTUS_FINAL_LARGO$HIPERTENSION)
ICTUS_FINAL_LARGO$DIABETES<-as.factor(ICTUS_FINAL_LARGO$DIABETES)
ICTUS_FINAL_LARGO$FA<-as.factor(ICTUS_FINAL_LARGO$FA)
ICTUS_FINAL_LARGO$HIPERLIPIDEMIA<-as.factor(ICTUS_FINAL_LARGO$HIPERLIPIDEMIA)
ICTUS_FINAL_LARGO$TABAQUISMO<-as.factor(ICTUS_FINAL_LARGO$TABAQUISMO)

# Las distancias de correlaciones de Rizzo y Skely no pueden obenerse para un conjunto de
#variables con datos categ?ricos.  Por lo tanto 
# library(energy)

#AQUI LO QUE SE HACE ES ESTUDIAR LA CORRELACION ENTRE FACTORES DE RIESGO
library(GoodmanKruskal)

seleccion<-data.frame(ICTUS_FINAL_LARGO[,c(4:8)])
GKmatrix <- GKtauDataframe(seleccion)
plot(GKmatrix)

#Se observa que no existen altas correlaciones entre las variables, interesar?a adem?s registros de estos
#pacientes sin ICTUS, para realmente comprobar si son fatores de riesgo.

# dcor.test(ICTUS_FINAL_LARGO[c(1:5000),4],ICTUS_FINAL_LARGO[c(1:5000),5],index=1.0)
# cor(ICTUS_FINAL_LARGO)
####################################################################################################
###################################################################################################3
#Volvemos a la base de datos de ICTUS 
#Para analiar los tiempos de repetici?n vamos a quedarnos con Identificadores de los pacientes que al menos
#hayan tenido una recurrencia de Ictus, es decir, dos Ictus al menos.

#colnames(ICTUS_FINAL_LARGO)

ICTUS_FINAL_LARGO<-ICTUS_FINAL_LARGO %>% arrange(Id,FCH_INGRESO,FCH_ALTA)

pacientes_rep_largo<-ICTUS_FINAL_LARGO %>% 
  group_by(Id) %>% 
  count() %>% 
  filter(n>1) %>% 
  arrange(desc(n)) 

#Para los tiempos de repetici?n de ICTUS, se va a continuar solo con los pacientes 
#que tienen registrados m?s de un c?digo ICTUS
#Son 3205 pacientes presentan una repetici?n de ICTUS 
nrow(pacientes_rep_largo)


##############################################
#Para este estudio, no se va a tener en cuenta el tipo de ICTUS que le ha dado, 
#solo el hecho de que le haya dado 
#Se va a crear un dataframe con las fechas de alta de los pacientes, 
#y se va a calcular el tiempo en d?as de la repetici?n, 
#primero agrupamos por id del paciente()
id_repeticiones<-pacientes_rep_largo$Id


###########################################################################
############         BUSQUEMOS LAS RECURRENCIAS          ##################
###########################################################################
id_repeticiones<-unique(id_repeticiones)
id_repeticiones<-as.character(id_repeticiones)
ICTUS_FINAL_FA<-ICTUS_FINAL%>%
  arrange(Id,FCH_INGRESO)
ICTUS_FINAL_FA<-ICTUS_FINAL %>%
  mutate(FCH_ING_ANT=lag(FCH_INGRESO,n=1, default=NA,order_by=Id))

ICTUS_FINAL_REP<-ICTUS_FINAL_FA %>%
  dplyr::filter(Id %in% id_repeticiones) %>%
  group_by(Id) %>%
  mutate(tiempoRepIngreso=ifelse(ictus_anteriores==0,NA,FCH_INGRESO-FCH_ING_ANT))%>%
  dplyr::filter(tiempoRepIngreso>0)

#mutate(tiempoIngreso_0=ifelse(ictus_anteriores>1,lag(tiempoRepIngreso,n=1, default=NA,order_by=Id)+tiempoRepIngreso,tiempoRepIngreso)) %>%
# dplyr::filter(is.na(tiempoIngreso_0)==F)

#El siguiente arreglo est? hecho para que tenga sentido los datos registrado bajo el punto de vista de los programadores
#No obstante los resultados tras realizar esta arreglo han empeorado el espacio muestral al que aplicamos el analisis de descriptivo

ICTUS_FINAL_REP<-ICTUS_FINAL_REP %>%
  arrange(FCH_INGRESO)

#ICTUS_FINAL_REP$HIPERTENSION<-ifelse(lag(ICTUS_FINAL_REP$HIPERTENSION,n=1,default=NA,order_by=ICTUS_FINAL_REP$Id)=="1"|ICTUS_FINAL_REP$HIPERTENSION=="1","1","0")
#ICTUS_FINAL_REP$DIABETES<-ifelse(lag(ICTUS_FINAL_REP$DIABETES,n=1,default=NA,order_by=ICTUS_FINAL_REP$Id)=="1"|ICTUS_FINAL_REP$DIABETES=="1","1","0")
#ICTUS_FINAL_REP$FA<-ifelse(lag(ICTUS_FINAL_REP$FA,n=1,default=NA,order_by=ICTUS_FINAL_REP$Id)=="1"|ICTUS_FINAL_REP$FA=="1","1","0")
#ICTUS_FINAL_REP$HIPERLIPIDEMIA<-ifelse(lag(ICTUS_FINAL_REP$HIPERLIPIDEMIA,n=1,default=NA,order_by=ICTUS_FINAL_REP$Id)=="1"|ICTUS_FINAL_REP$HIPERLIPIDEMIA=="1","1","0")
#ICTUS_FINAL_REP$TABAQUISMO<-ifelse(lag(ICTUS_FINAL_REP$TABAQUISMO,n=1,default=NA,order_by=ICTUS_FINAL_REP$Id)=="1"|ICTUS_FINAL_REP$TABAQUISMO=="1","1","0")

####???



TiempoIngresos<-ICTUS_FINAL_REP$tiempoRepIngreso
#7562 dias
library(tidyverse)
hist(TiempoIngresos)
#prob_acuecdf<-ecdf(TiempoIngresos)


########################################
#FUNCION DENSIDAD DE DISTRIBUCION

ICTUS_FINAL_REP %>%
  ggplot(aes(x = tiempoRepIngreso)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("orangered2")) +
  labs(title = "Tiempo de repeticion del Ictus en dias") +
  theme_bw() +
  theme(legend.position = "bottom")

#Asi se haria la funcion densidad filtrando factores de riesgo
#ICTUS_FINAL_REP %>%
#  filter(DIABETES=="1")%>%
#  ggplot(aes(x = tiempoRepIngreso)) +
#  geom_density(alpha = 0.5) +
#  scale_fill_manual(values = c("orangered2")) +
#  labs(title = "Tiempo de repeticion del Ictus en dias HIPERLIPIDEMIA") +
#  theme_bw() +
#  theme(legend.position = "bottom")

#########################################
#FUNCION DE DISTRIBUCION ACUMULADA

# Se ajustan las funciones ecdf con cada muestra. 
ecdf_TR<- ecdf(ICTUS_FINAL_REP %>%pull(tiempoRepIngreso))

# Se calcula la probabilidad acumulada de cada valor de salario observado con cada
# una de las funciones ecdf.
grid_TR <- unique(ICTUS_FINAL_REP %>% pull(tiempoRepIngreso))
prob_acumulada_ecdf_TR <- ecdf_TR(v = grid_TR)
# Se unen los valores calculados en un dataframe.
df_ecdf <- data.frame(
  tiempoRepIngreso = grid_TR,
  ecdf_TR= prob_acumulada_ecdf_TR
) %>%
  pivot_longer(
    cols = c(ecdf_TR),
    names_to = "TR",
    values_to = "ecdf"
  )

grafico_ecdf <- ggplot(data = df_ecdf,
                       aes(x = tiempoRepIngreso, y = ecdf)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("orangered1")) +
  labs(
    title = "Funci?n de distribuci?n acumulada emp?rica TRI"
  ) +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 12))

grafico_ecdf

lm(df_ecdf$tiempoRepIngreso~exp(df_ecdf$ecdf))
plot(df_ecdf$tiempoRepIngreso,df_ecdf$ecdf, main = "Curva de regresi?n")
curve(-1*exp(-0.02217*x^(0.57))+1,0,8000)
points(df_ecdf$tiempoRepIngreso,df_ecdf$ecdf,col='red')
##########################################
#COMPARATIVA FUNCION DE DISTRIBUCION ACUMULATIVA DE DIAS DE REPETICION DE INGRESO
#ENTRE PACIENTES SIN FACTORES DE RIESGOS Y PACIENTES CON HIPERTENSION



ecdf_NO <- ecdf(ICTUS_FINAL_REP %>% filter(HIPERTENSION=="0" & DIABETES=="0" & FA=="0"& HIPERLIPIDEMIA=="0" & TABAQUISMO=="0") %>% pull(tiempoRepIngreso))
ecdf_HIPER <- ecdf(ICTUS_FINAL_REP %>% filter(HIPERTENSION=="1") %>% pull(tiempoRepIngreso))

# Se calcula la probabilidad acumulada de cada valor de salario observado con cada
# una de las funciones ecdf.
prob_acumulada_ecdf_NO <- ecdf_NO(v = grid_TR)
prob_acumulada_ecdf_HIP <- ecdf_HIPER(v = grid_TR)

# Se unen los valores calculados en un dataframe.
df_ecdf_HIP <- data.frame(
  tiempoRepIngreso = grid_TR,
  ecdf_NO = prob_acumulada_ecdf_NO,
  ecdf_HIPER = prob_acumulada_ecdf_HIP
) %>%
  pivot_longer(
    cols = c(ecdf_NO, ecdf_HIPER),
    names_to = "HIPER_V",
    values_to = "ecdf"
  )

grafico_ecdf_HIP <- ggplot(data = df_ecdf_HIP,
                           aes(x = tiempoRepIngreso, y = ecdf, color = HIPER_V)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("gray60", "orangered1")) +
  labs(
    title = "Funci?n de distribuci?n acumulada comparaci?n HIPERTENSION",
    color = "HIPER_V",
    y = "Probabilidad acumulada"
  ) +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 12))

grafico_ecdf_HIP

#######DIABETES#######


ecdf_DIA <- ecdf(ICTUS_FINAL_REP %>% filter(DIABETES=="1") %>% pull(tiempoRepIngreso))

# Se calcula la probabilidad acumulada de cada valor de salario observado con cada
# una de las funciones ecdf.
prob_acumulada_ecdf_DIA <- ecdf_DIA(v = grid_TR)
# Se unen los valores calculados en un dataframe.
df_ecdf_DIA <- data.frame(
  tiempoRepIngreso = grid_TR,
  ecdf_NO = prob_acumulada_ecdf_NO,
  ecdf_DIA = prob_acumulada_ecdf_DIA
) %>%
  pivot_longer(
    cols = c(ecdf_NO, ecdf_DIA),
    names_to = "DIA_V",
    values_to = "ecdf"
  )

grafico_ecdf_DIA <- ggplot(data = df_ecdf_DIA,
                           aes(x = tiempoRepIngreso, y = ecdf, color = DIA_V)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("gray60", "orangered1")) +
  labs(
    title = "Funci?n de distribuci?n acumulada comparaci?n DIABETES",
    color = "DIA_V",
    y = "Probabilidad acumulada"
  ) +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 12))

grafico_ecdf_DIA

################FA


ecdf_FA <- ecdf(ICTUS_FINAL_REP %>% filter(FA=="1") %>% pull(tiempoRepIngreso))

# Se calcula la probabilidad acumulada de cada valor de salario observado con cada
# una de las funciones ecdf.
prob_acumulada_ecdf_FA <- ecdf_FA(v = grid_TR)
# Se unen los valores calculados en un dataframe.
df_ecdf_FA <- data.frame(
  tiempoRepIngreso = grid_TR,
  ecdf_NO = prob_acumulada_ecdf_NO,
  ecdf_FA = prob_acumulada_ecdf_FA
) %>%
  pivot_longer(
    cols = c(ecdf_NO, ecdf_FA),
    names_to = "FA_V",
    values_to = "ecdf"
  )

grafico_ecdf_FA <- ggplot(data = df_ecdf_FA,
                          aes(x = tiempoRepIngreso, y = ecdf, color = FA_V)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("gray60", "orangered1")) +
  labs(
    title = "Funci?n de distribuci?n acumulada comparaci?n FA",
    color = "FA_V",
    y = "Probabilidad acumulada"
  ) +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 12))

grafico_ecdf_FA

########################HIPERLIPIDEMIA


ecdf_HL <- ecdf(ICTUS_FINAL_REP %>% filter(HIPERLIPIDEMIA=="1") %>% pull(tiempoRepIngreso))

# Se calcula la probabilidad acumulada de cada valor de salario observado con cada
# una de las funciones ecdf.
prob_acumulada_ecdf_HL <- ecdf_HL(v = grid_TR)
# Se unen los valores calculados en un dataframe.
df_ecdf_HL <- data.frame(
  tiempoRepIngreso = grid_TR,
  ecdf_NO = prob_acumulada_ecdf_NO,
  ecdf_HL = prob_acumulada_ecdf_HL
) %>%
  pivot_longer(
    cols = c(ecdf_NO, ecdf_HL),
    names_to = "HL_V",
    values_to = "ecdf"
  )

grafico_ecdf_HL <- ggplot(data = df_ecdf_HL,
                          aes(x = tiempoRepIngreso, y = ecdf, color = HL_V)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("gray60", "orangered1")) +
  labs(
    title = "Funci?n de distribuci?n acumulada comparaci?n HIPERLIPIDEMIA",
    color = "HL_V",
    y = "Probabilidad acumulada"
  ) +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 12))

grafico_ecdf_HL

###############################TABAQUISMO

ecdf_TA <- ecdf(ICTUS_FINAL_REP %>% filter(TABAQUISMO=="1") %>% pull(tiempoRepIngreso))

# Se calcula la probabilidad acumulada de cada valor de salario observado con cada
# una de las funciones ecdf.
prob_acumulada_ecdf_TA <- ecdf_TA(v = grid_TR)
# Se unen los valores calculados en un dataframe.
df_ecdf_TA <- data.frame(
  tiempoRepIngreso = grid_TR,
  ecdf_NO = prob_acumulada_ecdf_NO,
  ecdf_TA = prob_acumulada_ecdf_TA
) %>%
  pivot_longer(
    cols = c(ecdf_NO, ecdf_TA),
    names_to = "TA_V",
    values_to = "ecdf"
  )

grafico_ecdf_TA <- ggplot(data = df_ecdf_TA,
                          aes(x = tiempoRepIngreso, y = ecdf, color = TA_V)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("orangered1","gray60")) +
  labs(
    title = "Funci?n de distribuci?n acumulada comparaci?n TABAQUISMO",
    color = "TA_V",
    y = "Probabilidad acumulada"
  ) +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 12))

grafico_ecdf_TA

########################################
#POR SEXO
ecdf_h<- ecdf(ICTUS_FINAL_REP %>% filter(SEXO=="H") %>% pull(tiempoRepIngreso))
ecdf_m<- ecdf(ICTUS_FINAL_REP %>% filter(SEXO=="M") %>% pull(tiempoRepIngreso))

# Se calcula la probabilidad acumulada de cada valor de salario observado con cada
# una de las funciones ecdf.
prob_acumulada_ecdf_H <- ecdf_h(v = grid_TR)
prob_acumulada_ecdf_M <- ecdf_m(v = grid_TR)
# Se unen los valores calculados en un dataframe.
df_ecdf_sex <- data.frame(
  tiempoRepIngreso = grid_TR,
  ecdf_h = prob_acumulada_ecdf_H,
  ecdf_m = prob_acumulada_ecdf_M
) %>%
  pivot_longer(
    cols = c(ecdf_h, ecdf_m),
    names_to = "SEXO_V",
    values_to = "ecdf"
  )

grafico_ecdf_sex <- ggplot(data = df_ecdf_sex,
                          aes(x = tiempoRepIngreso, y = ecdf, color = SEXO_V)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("orangered1","gray60")) +
  labs(
    title = "Funci?n de distribuci?n acumulada comparaci?n SEXO",
    color = "SEXO_V",
    y = "Probabilidad acumulada"
  ) +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 12))

grafico_ecdf_sex

#######################################
#SEXO EDAD
grid_E <- unique(ICTUS_FINAL_REP %>% pull(EDAD))
ecdf_he<- ecdf(ICTUS_FINAL_REP %>% filter(SEXO=="H") %>% pull(EDAD))
ecdf_me<- ecdf(ICTUS_FINAL_REP %>% filter(SEXO=="M") %>% pull(EDAD))

# Se calcula la probabilidad acumulada de cada valor de salario observado con cada
# una de las funciones ecdf.
prob_acumulada_ecdf_HE <- ecdf_he(v = grid_E)
prob_acumulada_ecdf_ME <- ecdf_me(v = grid_E)
# Se unen los valores calculados en un dataframe.
df_ecdf_sexE <- data.frame(
  EDAD = grid_E,
  ecdf_he = prob_acumulada_ecdf_HE,
  ecdf_me = prob_acumulada_ecdf_ME
) %>%
  pivot_longer(
    cols = c(ecdf_he, ecdf_me),
    names_to = "SEXOE_V",
    values_to = "ecdf"
  )

grafico_ecdf_sexE <- ggplot(data = df_ecdf_sexE,
                           aes(x = EDAD, y = ecdf, color = SEXOE_V)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("orangered1","gray60")) +
  labs(
    title = "Funci?n de distribuci?n acumulada comparaci?n SEXO/EDAD",
    color = "SEXOE_V",
    y = "Probabilidad acumulada"
  ) +
  theme_bw() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 12))

grafico_ecdf_sexE

#########################################
#OBTENCION DE MEDIANAS Y GRAFICADO DE FUNCION DE DENSIDAD
#Con la unica condicion es que tenga determinado FR
he<-ICTUS_FINAL%>%
  filter(SEXO=="H")
me<-ICTUS_FINAL%>%
  filter(SEXO=="M")

plot(density(me$EDAD), col = "purple")
lines(density(he$EDAD), col = "green")

mediana_todos<-median(ICTUS_FINAL_REP$tiempoRepIngreso)
mediana_NOFR<-ICTUS_FINAL_REP %>%
  filter(HIPERTENSION=="0"&DIABETES=="0"&FA=="0"&HIPERLIPIDEMIA=="0"&TABAQUISMO=="0")
mediana_HIPERT<-ICTUS_FINAL_REP %>%
  filter(HIPERTENSION=="1")
mediana_DIA<-ICTUS_FINAL_REP %>%
  filter(DIABETES=="1")
mediana_FA<-ICTUS_FINAL_REP %>%
  filter(FA=="1")
mediana_HIPERL<-ICTUS_FINAL_REP %>%
  filter(HIPERLIPIDEMIA=="1")
mediana_TA<-ICTUS_FINAL_REP %>%
  filter(TABAQUISMO=="1")
plot(density(mediana_TA$tiempoRepIngreso), col = "purple")
lines(density(ICTUS_FINAL_REP$tiempoRepIngreso), col = "green")
lines(density(mediana_HIPERT$tiempoRepIngreso), col = "red")
lines(density(mediana_DIA$tiempoRepIngreso), col = "black")
lines(density(mediana_FA$tiempoRepIngreso), col = "yellow")
lines(density(mediana_HIPERL$tiempoRepIngreso), col = "brown")
lines(density(mediana_NOFR$tiempoRepIngreso), col = "blue")
legend("topright", c("Tabaquismo","Sin FR","Todos","Hipertension", "Diabetes", "Fibrilacion Auricular", "Hiperlipidemia"),
       lty = 1, col = c("purple","blue", "green", "red","black","yellow","brown"))

mediana_NOFR<-median(mediana_NOFR$tiempoRepIngreso)
mediana_HIPERT<-median(mediana_HIPERT$tiempoRepIngreso)
mediana_DIA<-median(mediana_DIA$tiempoRepIngreso)
mediana_FA<-median(mediana_FA$tiempoRepIngreso)
mediana_HIPERL<-median(mediana_HIPERL$tiempoRepIngreso)
mediana_TA<-median(mediana_TA$tiempoRepIngreso)
namestablemedian<-c("todos","sin factores de riesgo","hipertension","diabetes","fibrilacion auricular","hiperlipidemia","tabaquismo")
valuestablemedian<-c(mediana_todos,mediana_NOFR,mediana_HIPERT,mediana_DIA,mediana_FA,mediana_HIPERL,mediana_TA)
tablemedian<-data.frame(namestablemedian,valuestablemedian)

#Con la condicion de que tenga determinado FR y ninguno mas

mediana_NOFR<-ICTUS_FINAL_REP %>%
  filter(HIPERTENSION=="0"&DIABETES=="0"&FA=="0"&HIPERLIPIDEMIA=="0"&TABAQUISMO=="0")
mediana_HIPERT_E<-ICTUS_FINAL_REP %>%
  filter(HIPERTENSION=="1"&DIABETES=="0"&FA=="0"&HIPERLIPIDEMIA=="0"&TABAQUISMO=="0")
mediana_DIA_E<-ICTUS_FINAL_REP %>%
  filter(HIPERTENSION=="0"&DIABETES=="1"&FA=="0"&HIPERLIPIDEMIA=="0"&TABAQUISMO=="0")
mediana_FA_E<-ICTUS_FINAL_REP %>%
  filter(FA=="1"&HIPERTENSION=="0"&DIABETES=="0"&HIPERLIPIDEMIA=="0"&TABAQUISMO=="0")
mediana_HIPERL_E<-ICTUS_FINAL_REP %>%
  filter(HIPERLIPIDEMIA=="1"&HIPERTENSION=="0"&DIABETES=="0"&FA=="0"&TABAQUISMO=="0")
mediana_TA_E<-ICTUS_FINAL_REP %>%
  filter(TABAQUISMO=="1"&HIPERTENSION=="0"&DIABETES=="0"&FA=="0"&HIPERLIPIDEMIA=="0")


summary(mediana_HIPERT_E$tiempoRepIngreso)
var(mediana_HIPERT_E$tiempoRepIngreso)
sqrt(var(mediana_NOFR$tiempoRepIngreso))
plot(density(mediana_TA_E$tiempoRepIngreso), col = "purple",main="Densidad segun el factor de riesgo")
lines(density(ICTUS_FINAL_REP$tiempoRepIngreso), col = "green")
lines(density(mediana_HIPERT_E$tiempoRepIngreso), col = "red")
lines(density(mediana_DIA_E$tiempoRepIngreso), col = "black")
lines(density(mediana_FA_E$tiempoRepIngreso), col = "yellow")
lines(density(mediana_HIPERL_E$tiempoRepIngreso), col = "brown")
lines(density(mediana_NOFR$tiempoRepIngreso), col = "blue")
legend("topright", c("Tabaquismo","Sin FR","Todos","Hipertension", "Diabetes", "Fibrilacion Auricular", "Hiperlipidemia"),
       lty = 1, col = c("purple","blue", "green", "red","black","yellow","brown"))

#density(ICTUS_FINAL_REP$tiempoRepIngreso)(800)
patologia<-c("Tabaquismo","Sin FR","Todos","Hipertension", "Diabetes", "Fibrilacion Auricular", "Hiperlipidemia")
frecabs<-c(NROW(mediana_TA_E),NROW(mediana_NOFR),NROW(ICTUS_FINAL_REP),NROW(mediana_HIPERT_E),NROW(mediana_DIA_E),NROW(mediana_FA_E),NROW(mediana_HIPERL_E))
tablafaFR<-data.frame(patologia,frecabs)

mediana_NOFR<-median(mediana_NOFR$tiempoRepIngreso)
mediana_HIPERT_E<-median(mediana_HIPERT_E$tiempoRepIngreso)
mediana_DIA_E<-median(mediana_DIA_E$tiempoRepIngreso)
mediana_FA_E<-median(mediana_FA_E$tiempoRepIngreso)
mediana_HIPERL_E<-median(mediana_HIPERL_E$tiempoRepIngreso)
mediana_TA_E<-median(mediana_TA_E$tiempoRepIngreso)
namestablemedian_E<-c("todos","sin factores de riesgo","hipertension","diabetes","fibrilacion auricular","hiperlipidemia","tabaquismo")
valuestablemedian<-c(mediana_todos,mediana_NOFR,mediana_HIPERT,mediana_DIA,mediana_FA,mediana_HIPERL,mediana_TA)
tablemedian<-data.frame(namestablemedian,valuestablemedian)

#########################################
#FILTRADO DE OUTLIERS

RG_IQ<-IQR(ICTUS_FINAL_REP$tiempoRepIngreso)
MAX_OUT<-as.numeric(quantile(ICTUS_FINAL_REP$tiempoRepIngreso,prob=0.75)+1.5*RG_IQ)
ICTUS_FINAL_REP_SINOUT<-ICTUS_FINAL_REP %>%
  dplyr::filter(tiempoRepIngreso<MAX_OUT)

