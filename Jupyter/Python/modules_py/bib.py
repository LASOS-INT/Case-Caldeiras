# Importação de bibliotecas Python equivalentes aos do R

import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
import plotly.express as px
import plotly.graph_objects as go
# import fast_dummies
from sklearn.preprocessing import OneHotEncoder
from scipy import stats
from scipy.stats import triang
import os



# Funções para usar no data_cleaning


# Função para carregar o dataset

def load_data():
    data_path = "C:/Users/pedro/OneDrive/Documents/INT Projects/Git Clone/Case-Caldeiras/Jupyter/Data/AnaliseAtualizada-BDCaldeiras_13Jun22_RevAndrea.xlsx"
    my_data = pd.read_excel(data_path)
    return my_data

# Função para renomear os tipos de caldeiras (simplificando o nome)

def rename_boilers(produto_nome):
    replacements = {
        "CALDEIRAS NAO DEFINIDAS": "CND",
        "CALDEIRA DE RECUPERACAO QUIMICA": "CRQ",
        "CALDEIRA P/ QUEIMA DE COMBUSTIVEL SOLIDO": "CQCS",
        "CALDEIRA COMPACTA": "CC",
        "CALDEIRA DE RECUPERACAO DE CALOR, HRSG": "CRCHRSG",
        "CALDEIRA PARA QUEIMA DE OLEO E GAS": "CQOG",
        "OUTROS EQUIPAMENTOS DA AREA DE CALDEIRAS": "OUTROS",
        "OUTROS EQUIPAMENTOS DE PROCESSO": "OUTROS",
        "CILINDRO DE CLORO": "CILINDRO",
        "TANQUE, DUTO E CHAMINE": "TANQUE",
        "REATOR, VASO E TORRE": "REATOR",
        "TROCADOR DE CALOR": "TC",
        "TPRESTACAO DE SERVICOS EM GERAL": "OUTROS"
    }
    return produto_nome.replace(replacements)


# Separando dados categóricos e numéricos

def process_data(df, categorical_columns, numeric_columns):
    df['Andrea_DEPARTAMENTO.Descricao'] = df['Andrea_DEPARTAMENTO.Descricao'].str.upper()
    df[categorical_columns] = df[categorical_columns].apply(lambda x: x.astype('category'))
    df[numeric_columns] = df[numeric_columns].apply(pd.to_numeric, errors='coerce')
    df['Poder Z'] = df['Poder Z'].fillna(0)
    return df.dropna(subset=['SomaDeHorasApontadas'])



def create_unit_hours(df):
    return df['SomaDeHorasApontadas'] / df['QuantidadePedida']

def filter_clean_data(df):
    return df[
        (~df['Poder Linear'].isna()) & 
        (df['TipodeProduto'].isin(["Caldeira", "Trocador de Calor", "Vasos"])) & 
        (df['Andrea_DEPARTAMENTO.Descricao'].isin(["MONTAGEM", "SOLDA", "TRACAGEM"]))
    ].copy()

def group_data(df):
    return df.groupby(['TipodeProduto', 'Andrea_DEPARTAMENTO.Descricao', 'QuantidadePedida'], observed=True).agg(
        SomaDeHorasApontadasUnitario=('SomaDeHorasApontadasUnitario', 'mean'),
        Poder_Linear=('Poder Linear', 'mean')
    ).reset_index()

def get_frequent_items(my_data):
    # Filtrando para itens não nulos
    my_data = my_data[my_data['ITENS.Codigo'].notna()]
    
    # Agrupando pelos códigos e departamentos
    frequentes = (
        my_data.groupby(['ITENS.Codigo', 'Andrea_DEPARTAMENTO.Descricao'], observed=True)
        .size()
        .reset_index(name='count')
        .sort_values(by='count', ascending=False)
    )

    # Filtrando os itens com frequência >= 10
    frequentes = frequentes[frequentes['count'] >= 10]

    # Filtrando o DataFrame original baseado em itens frequentes e tipo de produto
    frequentes = my_data[
        (my_data['TipodeProduto'] == 'Caldeira') & 
        (my_data['ITENS.Codigo'].isin(frequentes['ITENS.Codigo']))
    ].copy()

    # Removendo colunas desnecessárias
    columns_to_drop = [
        'Fonte', 'OFFCod', 'OFFEscopo', 'SomaDeHorasApontadas', 'ITENS.Descricao',
        'Capacidade(TSS/D)', 'Cap Normal Linear', 'Cap Normal Z',
        'Evaporacao(T/H)', 'Evap Normal Linear', 'Evap Normal Z'
    ]
    frequentes = frequentes.drop(columns=[col for col in columns_to_drop if col in frequentes.columns])

    # Renomeando colunas
    rename_map = {
        "Andrea_DEPARTAMENTO.Descricao": "Depart",
        "ITENS.Codigo": "Item.Cod",
        "TipodeProduto": "Prod.Tipo",
        "Produto.Nome": "Prod.Nome",
        "QuantidadePedida": "Qtd",
        "Poder Linear": "Poder.Lin",
        "Poder Z": "Poder.Z",
        "SomaDeHorasApontadasUnitario": "HorasUnit"
    }
    frequentes = frequentes.rename(columns={k: v for k, v in rename_map.items() if k in frequentes.columns})

    # Selecionando apenas colunas necessárias
    frequentes = frequentes[
        ["HorasUnit", "Qtd", "Poder.Z", "Poder.Lin", "Depart", "Prod.Nome", "Item.Cod"]
    ]

    return frequentes



def create_dummies(df):
    required_columns = ["Depart", "Prod.Nome", "Item.Cod"]
    for col in required_columns:
        if col not in df.columns:
            raise KeyError(f"Coluna {col} está ausente do DataFrame!")
    
    # Criando as dummies
    dummies = pd.get_dummies(df, columns=required_columns, drop_first=True)

    return dummies


def create_boxplot(df):
    # Geração de gráfico usando plotly ou outra biblioteca gráfica
    pass

def plot_frequent_items(df):
    # Função para gerar gráficos de itens frequentes
    pass



