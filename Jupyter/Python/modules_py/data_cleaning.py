import pandas as pd
import bib

def DataCleaning_MyData ():

    # Carregar os dados (função de carregamento está no bib.py)
    my_data = bib.load_data()

    # Renomeando o nome das caldeiras usando função do bib.py
    my_data['Produto.Nome'] = bib.rename_boilers(my_data['Produto.Nome'])

    # Tratamento de dados (seleciona as colunas numéricas, e o que não for, é categórico)
    numeric_columns = [
        'SomaDeHorasApontadas',
        'Poder Linear',
        'Poder Z',
        'Cap Normal Linear',
        'Cap Normal Z',
        'Evap Normal Linear',
        'Evap Normal Z',
        'Evaporacao(T/H)',
        'QuantidadePedida'
    ]

    categorical_columns = [col for col in my_data.columns if col not in numeric_columns]

    # Aplica tratamento aos dados categóricos e numéricos
    my_data = bib.process_data(my_data, categorical_columns, numeric_columns)

    # Criação de novas colunas e ajustes finais nos dados
    my_data['SomaDeHorasApontadasUnitario'] = bib.create_unit_hours(my_data)

    return my_data


def DataCleaning_GroupData():

    # Limpeza de dados com base no Poder.Linear e filtros de produto e departamento
    clean_data = bib.filter_clean_data(DataCleaning_MyData())

    # Agrupamento de dados
    clean_media = bib.group_data(clean_data)

    return clean_media


def DataCleaning_frequents():

    # Criando dummies para dados categóricos no df mais_frequentes

    # Aplicando a função de itens frequentes
    mais_frequentes = bib.get_frequent_items(DataCleaning_MyData())

    return mais_frequentes


def DataCleaning_dummies():
    # Criando as dummies
    dummies_frequentes = bib.create_dummies(DataCleaning_frequents())
    
    return dummies_frequentes



    # Visualizações (gráficos podem ser feitos no Jupyter, chamando a função apropriada)
    # Gera gráficos de boxplot, scatterplot ou outros
    # bib.generate_boxplot(my_data)
    # bib.generate_scatterplot(clean_data)
    # bib.generate_density_plot(my_data)


    # Exemplo de criação de um gráfico boxplot simplificado (se estiver no Jupyter)
    # BoxplotTodosItens = bib.create_boxplot(my_data)


    # Exemplo final de gráfico (se necessário no Jupyter)
    # bib.plot_frequent_items(mais_frequentes)
