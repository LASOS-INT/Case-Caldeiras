import pandas as pd
import numpy as np
from sklearn.model_selection import KFold
from sklearn.preprocessing import OneHotEncoder, OrdinalEncoder, StandardScaler
from sklearn.compose import ColumnTransformer, make_column_transformer
from sklearn.pipeline import Pipeline
from sklearn.linear_model import LinearRegression, Ridge, Lasso, ElasticNet
from sklearn.ensemble import RandomForestRegressor, GradientBoostingRegressor
from sklearn.tree import DecisionTreeRegressor
from sklearn.svm import SVR
from xgboost import XGBRegressor
from lightgbm import LGBMRegressor
from catboost import CatBoostRegressor
from sklearn.metrics import mean_squared_error, r2_score, mean_absolute_error
from category_encoders import TargetEncoder
import re


def sanitizar_nomes_colunas(feature_names):
    """
    Sanitiza os nomes das colunas para remover caracteres especiais.
    :param feature_names: Lista de nomes das colunas.
    :return: Lista de nomes sanitizados.
    """
    return [re.sub(r'[\[\]<>,]', '_', col) for col in feature_names]


def aplicar_modelos_regressao(df, target_column, n_folds=4):
    """
    Aplica modelos de regressão com diferentes encoders (OneHot, Label, Target) usando validação cruzada com k-folds.
    :param df: DataFrame contendo os dados.
    :param target_column: Nome da coluna alvo.
    :param n_folds: Número de folds para validação cruzada.
    :return: DataFrame com os resultados.
    """
    # Identificando colunas categóricas e numéricas
    cat_cols = df.select_dtypes(include=['object', 'category']).columns
    num_cols = df.select_dtypes(include=['number']).columns.drop(target_column)

    # Convertendo valores categóricos para strings (garantindo compatibilidade com encoders)
    df[cat_cols] = df[cat_cols].astype(str)

    # Lista de encoders a serem testados
    encoders = {
        'onehot': OneHotEncoder(handle_unknown='ignore', sparse_output=False),
        'label': OrdinalEncoder(handle_unknown='use_encoded_value', unknown_value=-1),
        'target_enc': TargetEncoder()
    }

    # Modelos a serem testados
    modelos = {
        "Regressao Linear": LinearRegression(),
        "Ridge": Ridge(),
        "Lasso": Lasso(),
        "ElasticNet": ElasticNet(),
        "Arvore de Decisao": DecisionTreeRegressor(random_state=42),
        "Random Forest": RandomForestRegressor(random_state=42),
        "Gradient Boosting": GradientBoostingRegressor(),
        "XGBoost": XGBRegressor(objective='reg:squarederror', random_state=42),
        "LightGBM": LGBMRegressor(random_state=42),
        "CatBoost": CatBoostRegressor(random_state=42, verbose=0),
        "SVR (Linear Kernel)": SVR(kernel='linear', C=10, epsilon=0.2),
        "SVR (RBF Kernel)": SVR(kernel='rbf', C=10, epsilon=0.2),
        "SVR (Sigmoid Kernel)": SVR(kernel='sigmoid', C=10, epsilon=0.2)
    }

    resultados = []

    # K-Fold Cross Validation
    kf = KFold(n_splits=n_folds, shuffle=True, random_state=42)

    for encoder_name, encoder in encoders.items():
        print(f"\nProcessando com encoder: {encoder_name}")

        # Dicionário para armazenar métricas de cada modelo
        metricas_por_modelo = {nome_modelo: {"R2": [], "MAE": [], "RMSE": []} for nome_modelo in modelos.keys()}

        for fold, (train_index, test_index) in enumerate(kf.split(df)):
            X_train, X_test = df.iloc[train_index], df.iloc[test_index]
            y_train, y_test = df[target_column].iloc[train_index], df[target_column].iloc[test_index]

            # Aplicando o encoder
            if encoder_name == 'target_enc':
                # Target Encoding é aplicado apenas no treino
                X_train[cat_cols] = encoder.fit_transform(X_train[cat_cols], y_train)
                X_test[cat_cols] = encoder.transform(X_test[cat_cols])
                preprocessor = make_column_transformer(
                    (StandardScaler(), num_cols),
                    ('passthrough', cat_cols)
                )
            else:
                # OneHot ou Label Encoding aplicados no dataset completo
                preprocessor = make_column_transformer(
                    (StandardScaler(), num_cols),
                    (encoder, cat_cols)
                )

            # Pré-processar dados
            X_train_preprocessed = preprocessor.fit_transform(X_train)
            X_test_preprocessed = preprocessor.transform(X_test)

            # Converter para DataFrame para manter os nomes das colunas
            if hasattr(preprocessor, 'get_feature_names_out'):
                feature_names = preprocessor.get_feature_names_out()
            else:
                feature_names = num_cols.tolist() + cat_cols.tolist()

            # Sanitizar nomes das colunas
            feature_names = sanitizar_nomes_colunas(feature_names)

            X_train_preprocessed = pd.DataFrame(X_train_preprocessed, columns=feature_names)
            X_test_preprocessed = pd.DataFrame(X_test_preprocessed, columns=feature_names)

            for nome_modelo, modelo in modelos.items():
                pipeline = Pipeline(steps=[
                    ('regressor', modelo)
                ])

                # Treinar modelo
                pipeline.fit(X_train_preprocessed, y_train)

                # Previsões
                y_pred = pipeline.predict(X_test_preprocessed)

                # Métricas de avaliação
                rmse = np.sqrt(mean_squared_error(y_test, y_pred))
                r2 = r2_score(y_test, y_pred)
                mae = mean_absolute_error(y_test, y_pred)

                # Armazenar métricas no dicionário
                metricas_por_modelo[nome_modelo]["R2"].append(r2)
                metricas_por_modelo[nome_modelo]["MAE"].append(mae)
                metricas_por_modelo[nome_modelo]["RMSE"].append(rmse)

                # Armazenar resultados por fold
                resultados.append({
                    "Encoder": encoder_name,
                    "Fold": fold + 1,
                    "Modelo": nome_modelo,
                    "R2": r2,
                    "MAE": mae,
                    "RMSE": rmse
                })

        # Calcular médias das métricas por modelo e adicionar ao resultados
        for nome_modelo, metricas in metricas_por_modelo.items():
            media_r2 = np.mean(metricas["R2"])
            media_mae = np.mean(metricas["MAE"])
            media_rmse = np.mean(metricas["RMSE"])

            resultados.append({
                "Encoder": encoder_name,
                "Fold": "Média",
                "Modelo": nome_modelo,
                "R2": media_r2,
                "MAE": media_mae,
                "RMSE": media_rmse
            })

    return pd.DataFrame(resultados)


# Exemplo de uso
data = {
    'cat_feature_1': ['A', 'B', 'A', 'C', 'B', 'C', 'A', 'B'],
    'cat_feature_2': ['X', 'Y', 'X', 'Y', 'X', 'Y', 'X', 'Y'],
    'num_feature_1': [1, 2, 3, 4, 5, 6, 7, 8],
    'num_feature_2': [10, 20, 30, 40, 50, 60, 70, 80],
    'target': [100, 200, 150, 250, 300, 350, 400, 450]
}

df = pd.DataFrame(data)
resultados = aplicar_modelos_regressao(df, target_column='target', n_folds=4)
print(resultados)
