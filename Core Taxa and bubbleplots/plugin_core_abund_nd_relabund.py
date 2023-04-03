import pandas as pd
# Definimos una función llamada `check_columns` que toma como parámetro una variable `file_path` que representa la ruta del archivo
def check_columns(file_path1, file_path2, columna, TAXA,HoB):
    df = pd.read_csv(file_path1)
    listado = pd.read_csv(file_path2)
    general = set(df["OTU_ID"])
    core = set(listado[columna])
    result = []
    for otu in general.union(core):
        general_only = otu in general and otu not in core
        core_only = otu in core and otu not in general
        if general_only:
            #result.append((otu, "ERROR_1"))
            continue
        elif core_only:
            #result.append((otu, "ERROR_2"))
            continue
        else:
# Solo almacenaremos los datos de nuestro grupo núcleo que estén en la base datos general, pero cuando se ejecuta por primera vez, descomentamos las líneas anteriores y usamos también esta columna como controles de que el proceso fue correcto. Cuando estamos seguros que la comparación es correcta, podemos volver a comentar los appends anteriores para tener archivos de resultados más limpios.
            result.append((otu, "BOTH"))         
# Crear un nuevo DataFrame con los resultados
    result_df = pd.DataFrame(result, columns=["OTU_ID", "RESULT"])
# Ordenamos la columna de resultados por orden alfabético
    result_df = result_df.sort_values("OTU_ID")
# Añade las otras columnas a los resultados haciendo Merge de entre el dataframe de frecuencias y el dataframe de resultados de la comparación previa
    df_merged = result_df.merge(df, on="OTU_ID", how="left")
# Define el nombre del archivo de salida para el dataframe con los valores de abundancia relativa. En este caso, se utiliza el mismo nombre que para el archivo original.
    nombre_archivo1 = f'C:\fichero_con_datos\{HoB}_{TAXA}_CORE_{columna}.tsv'
    df_merged.to_csv(nombre_archivo1, index=False)
# Define el nombre del archivo de salida para el dataframe con los valores de abundancia relativa. En este caso, se utiliza el mismo nombre que para el archivo original, añadiendo el sufijo "AR" al final.
    nombre_archivo2 = f'C:\fichero_con_datos\{HoB}_{TAXA}_CORE_{columna}_AR.tsv'
    df_merged.to_csv(nombre_archivo2, index=False)
# Corremos la función con todos sus parámetros
check_columns("C:\fichero_con_datos\HONGOS_FAM_FRECUENCIAS.csv","C:\fichero_con_datos\HONGOS_FAM_CORE_LIST.csv",'CON','FAM','HONGOS')
