# ESTUDO DE CASO
# Como um compartilhamento de bicicletas possibilita o sucesso rápido?

#SOLICITANTE: Cyclist (empresa de compatilhamento de bicicletas em Chicago)
#PARTE INTERESSADA: Diretor de marketing

#-> TÍTULO:
#-> FOCO DO SETOR:
#-> DECLARAÇÃO DO PROBLEMA: 1)COMO OS MEMBROS ANUAIS E OS CICLISTAS CASUAIS USAM AS BICICLETAS DA CYCLISTIC DE FORMA DIFERENTE?
#						2) POR QUE OS PASSAGEIROS CASUAIS IRIAM QUERER ADQUIRIR PLANOS ANUAIS DA CYCLISTIC?
#						3) COMO A CYCLISTIC PODE USAR A MÍDIA DIGITAL PARA INFLUENCIAR OS PASSAGEIROS CASUAIS A SE TORNAREM MEMBROS?

#-> CASO DE USO DE NEGÓCIOS (O QUE VOCÊ ESTÁ RESOLVENDO?):
#-> GOALS / METRICS:
#-> ENTREGAS:
#-> OS CONJUNTOS DE DADOS ESTÃO DISPONÍVEIS?
#-> LISTA DE CONJUNTOS DE DADOS:
#-> SITES PARA RASTREAR OS DADOS NECESSÁRIOS:




---------------
#PERGUNTAR:
	# 1)COMO OS MEMBROS ANUAIS E OS CICLISTAS CASUAIS USAM AS BICICLETAS DA CYCLISTIC DE FORMA DIFERENTE?
 
 
 #Configurando biblioteca
	install.packages ( "tidyverse" )
	
	library (tidyverse)
	#ainda não usei 
	library ( ggplot2 )
	library ( lubridate )
	library ( caretaker )
	

# importando os dados ( DATASETS DE ABRIL 2020 À DEZEMBRO 2021)
janeiro_2022 <- read_csv ("C:/Users/keith/Desktop/Cursos Analise de Dados/Certificado Google/Datasets-bikes/202201-divvy-tripdata.csv")
fevereiro_2022 <- read_csv ("C:/Users/keith/Desktop/Cursos Analise de Dados/Certificado Google/Datasets-bikes/202202-divvy-tripdata.csv")
março_2022 <- read_csv ("C:/Users/keith/Desktop/Cursos Analise de Dados/Certificado Google/Datasets-bikes/202203-divvy-tripdata.csv")
abril_2022 <- read_csv ("C:/Users/keith/Desktop/Cursos Analise de Dados/Certificado Google/Datasets-bikes/202204-divvy-tripdata.csv")
maio_2022 <- read_csv ("C:/Users/keith/Desktop/Cursos Analise de Dados/Certificado Google/Datasets-bikes/202205-divvy-tripdata.csv")
junho_2022 <- read_csv ("C:/Users/keith/Desktop/Cursos Analise de Dados/Certificado Google/Datasets-bikes/202206-divvy-tripdata.csv")
julho_2022 <- read_csv ("C:/Users/keith/Desktop/Cursos Analise de Dados/Certificado Google/Datasets-bikes/202207-divvy-tripdata.csv")
agosto_2022 <- read_csv ("C:/Users/keith/Desktop/Cursos Analise de Dados/Certificado Google/Datasets-bikes/202208-divvy-tripdata.csv")
setembro_2022 <- read_csv ("C:/Users/keith/Desktop/Cursos Analise de Dados/Certificado Google/Datasets-bikes/202209-divvy-publictripdata.csv")
outubro_2022 <- read_csv ("C:/Users/keith/Desktop/Cursos Analise de Dados/Certificado Google/Datasets-bikes/202210-divvy-tripdata.csv")
novembro_2022 <- read_csv ("C:/Users/keith/Desktop/Cursos Analise de Dados/Certificado Google/Datasets-bikes/202211-divvy-tripdata.csv")
dezembro_2022 <- read_csv ("C:/Users/keith/Desktop/Cursos Analise de Dados/Certificado Google/Datasets-bikes/202212-divvy-tripdata.csv")
janeiro_2023 <- read_csv ("C:/Users/keith/Desktop/Cursos Analise de Dados/Certificado Google/Datasets-bikes/202301-divvy-tripdata.csv")
	
# Unindo os data frame
cyclist_data <- bind_rows (janeiro_2022, fevereiro_2022, março_2022, abril_2022, maio_2022, junho_2022, julho_2022, agosto_2022, setembro_2022, outubro_2022, novembro_2022, dezembro_2022, janeiro_2023)

cyclist_data

glimpse(cyclist_data)

# Instalando os pacotes HERE, SKIMR, JANITOR
	install.packages ("here")
	library ("here")
	
	install.packages ("skimr")
	library ("skimr")
	
	install.packages ("janitor")
	library ("janitor")
	
	install.packages ("dplyr")
	library ("dplyr")
	
		
# LIMPANDO OS DADOS
##  Renomeando as colunas e limpando as linhas com dados faltantes

	bike_data <- cyclist_data %>% drop_na() %>%
			rename (id_corrida = ride_id,tipo_bike = rideable_type, estação_partida = start_station_name, hora_partida = started_at,hora_chegada = ended_at, estação_partida = start_station_name, id_estação_partida = start_station_id,
			estação_chegada = end_station_name, id_estação_chegada = end_station_id, lat_inicial = start_lat, long_inicial = start_lng, lat_final = end_lat, long_final = end_lng, membro_casual = member_casual)

	glimpse (bike_data)


## Criando coluna viagens
	bike_data  <- mutate( bike_data , id_estação_chegada  = as.character( id_estação_chegada ), id_estação_partida  = as.character( id_estação_partida ))

## Removendo lat_inicial, long_inicial, lat_final e long_final.
	bike_data <-  bike_data % > % select( - lat_inicial, - long_inicial, - lat_final , - long_final ))

	View (bike_data)
	
## Definindo duração do passeio
###Separando a data e o horário
	bike_data <- separate (bike_data, hora_partida, into=c('data_partida', 'hora_partida'), sep=' ')
	bike_data <- separate (bike_data, hora_partida, into=c('data_chegada', 'hora_partida'), sep=' ')
	
	head (bike_data)
	
	
	
	
	
	
	bike_data $ date  <- as.Date( bike_data $ hora_partida )
	bike_data $ mês  <- format(as.Date( bike_data $ data ), " %m " )
	bike_data $ dia  <- format(as.Date( bike_data $ data ), " %d " )
	bike_data $ ano  <- format(as.Date( bike_data $ data ), " %y " )
	bike_data $ dia_da_semana <- format(as.Date( bike_data $ date ), " %A " )

	bike_data $ duracao_passeio <- difftime( bike_data $ hora_chegada , bike_data $ hora_partida )

## Média, Mediana, Max, Mín

	média ( bike_data $ duracao_passeio )
	mediana( bike_data $ duracao_passeio )
	max( bike_data $ duracao_passeio )
	min( bike_data $ duracao_passeio )


# VISUALIZANDO OS DADOS (GGPLOT2)

	library ("ggplot2")
	
	ggplot(data=cyclist_data)+
		geom_bar (mapping=aes (x = membro_casual, y = )