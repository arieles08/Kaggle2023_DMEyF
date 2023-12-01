/* 1ro: Voy a Crear una base_adhoc -> le AGREGO EL MES DE BAJA o si CONTINUA */
CREATE TEMPORARY TABLE competencia_03_temp AS
	WITH max_mes AS (SELECT MAX(foto_mes) as u  FROM competencia_03_crudo), -- con WITH creamos una tabla TEMPORAL "auxL" que usaremos luego en la consulta principal */
		 auxL AS (SELECT numero_de_cliente, foto_mes, (foto_mes/100-2019)*12 +foto_mes%100 as add_mesn,
				  MAX (foto_mes) OVER (PARTITION BY numero_de_cliente) AS add_last_mes, -- Tomo el ULTIMO Mes que estuvo
				  MAX ((foto_mes/100-2019)*12 +foto_mes%100) OVER (PARTITION BY numero_de_cliente) AS add_lastn -- Tomo el ULTIMO Mes que estuvo
				  FROM competencia_03_crudo) 
	SELECT c2.numero_de_cliente, c2.foto_mes, auxL.add_mesn, auxL.add_last_mes, auxL.add_lastn,
		CASE WHEN auxL.add_last_mes = (SELECT u FROM max_mes) THEN 0  --Si ultimo_mes_activo = ultima_foto (202107) -> O (Continua)
			 ELSE auxL.add_lastn - auxL.add_mesn +1      			  --sino BAJA+N? con N = numero_ultimo_mes - numero_mes_actual +1
		END AS clase_gral_n,
		CASE WHEN auxL.add_last_mes = (SELECT u FROM max_mes) THEN 'CONTINUA'
			 ELSE 'BAJA+' || (auxL.add_lastn - auxL.add_mesn + 1)       			      
		END AS clase_gral
	FROM competencia_03_crudo AS c2 
	JOIN auxL ON auxL.numero_de_cliente = c2.numero_de_cliente AND auxL.foto_mes = c2.foto_mes
	ORDER BY numero_de_cliente, foto_mes


/* 2do: Agregro CLASE_TERNARIA */
ALTER TABLE competencia_03_temp ADD clase_ternaria VARCHAR(50); --hago una ALTER TABLE para agregar la columna "clase_ternaria"
UPDATE competencia_03_temp SET clase_ternaria =
    CASE 
        WHEN clase_gral_n = 0 THEN 'CONTINUA'
        WHEN clase_gral_n = 1 OR clase_gral_n = 2 THEN clase_gral
        WHEN clase_gral_n > 2 THEN 'CONTINUA'
        ELSE 'REVISAR'
    END;


/* 3ro: Corrijo CLASE_TERNARIA -> cuando es 202109 y 202108 los CONTINUA en realidad son desconocidos*/
UPDATE competencia_03_temp
SET clase_ternaria = ''
WHERE foto_mes IN (202109, 202108) AND clase_ternaria = 'CONTINUA';



/* 4to: como Generar el mes_inverso, es decir empieza por el final con 1 (ultimo mes que estuvo) y de ahi va para atras */
SELECT *, ROW_NUMBER() OVER (ORDER BY add_mesn DESC) AS add_mesinv
FROM competencia_03_temp
		
		
		
/* 5to: JOIN a nueva Tabla_FINAL: competencia_03 */
CREATE TABLE competencia_03 AS
	SELECT c2.*, ct.add_last_mes, ct.add_mesn, ct.add_lastn, ct.clase_gral_n , ct.clase_gral , ct.clase_ternaria
	FROM competencia_03_crudo as c2
		JOIN competencia_03_temp as ct 
		ON ct.numero_de_cliente = c2.numero_de_cliente AND ct.foto_mes = c2.foto_mes;
		
		
/* 6to: EXPORTO a competencia_03.csv y la temporal*/
COPY competencia_03 TO 'D:\OneDrive\! DM en Econ y Fin 2023\datasets\competencia_03.csv' delimiters ',' WITH CSV HEADER;
COPY competencia_03_temp TO 'D:\OneDrive\! DM en Econ y Fin 2023\datasets\competencia_03_temp.csv' delimiters ',' WITH CSV HEADER;




/* Extra: VERIFICAMOS que quedó bien: Viendo algunos casos de Baja y continua, la cantidad de reg, group by clases_nuevas */
SELECT clase_ternaria, COUNT(numero_de_cliente)
FROM competencia_03
GROUP BY clase_ternaria
ORDER BY clase_ternaria

SELECT foto_mes, clase_ternaria, COUNT(numero_de_cliente)
FROM competencia_03
GROUP BY foto_mes, clase_ternaria
ORDER BY foto_mes, clase_ternaria


SELECT *
FROM competencia_03_temp
WHERE numero_de_cliente = 29213153 --(BAJA ?)
  OR numero_de_cliente = 29202717 --(CONTINUA)
  OR numero_de_cliente = 29202973 --(BAJA 202008)
  OR numero_de_cliente = 29201974 --(CONTINUA)
--ORDER BY numero_de_cliente, foto_mes	

SELECT clase_gral_n, clase_gral, COUNT(numero_de_cliente), 
FROM competencia_03_temp
GROUP BY clase_gral_n, clase_gral
ORDER BY clase_gral_n

SELECT add_lastn, COUNT(numero_de_cliente)
FROM competencia_03_temp
GROUP BY add_lastn
ORDER BY add_lastn

SELECT foto_mes, clase_ternaria, count(numero_de_cliente) AS Q
FROM competencia_03_temp
GROUP BY foto_mes, clase_ternaria
ORDER BY foto_mes

SELECT foto_mes, clase_gral, count(numero_de_cliente) AS Q
FROM competencia_03_temp
GROUP BY foto_mes, clase_ternaria
ORDER BY foto_mes

--Pruebas Varias:

SELECT MAX(foto_mes) as u  
FROM competencia_03_crudo
