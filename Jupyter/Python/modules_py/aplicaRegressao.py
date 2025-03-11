import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split, cross_val_score, cross_validate
from sklearn.preprocessing import LabelEncoder, OrdinalEncoder, OneHotEncoder, StandardScaler
from sklearn.compose import ColumnTransformer
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



# Função para aplicar diferentes encoders
def aplicar_encoder(preprocessor, encoder, cat_cols, num_cols, y_train=None):
    """
    Aplica o encoder especificado nas colunas categóricas.
    :param preprocessor: ColumnTransformer para pré-processamento.
    :param encoder: Encoder a ser aplicado (ex: OneHotEncoder, LabelEncoder).
    :param cat_cols: Lista de colunas categóricas.
    :param num_cols: Lista de colunas numéricas.
    :param y_train: Target para o Target Encoding (obrigatório se encoder for 'target').
    :return: ColumnTransformer configurado.
    """
    if encoder == 'onehot':
        preprocessor.transformers = [
            ('num', StandardScaler(), num_cols),
            ('cat', OneHotEncoder(handle_unknown='ignore'), cat_cols)
        ]
    elif encoder == 'label':
        preprocessor.transformers = [
            ('num', StandardScaler(), num_cols),
            ('cat', OrdinalEncoder(handle_unknown='use_encoded_value', unknown_value=-1), cat_cols)
        ]
    elif encoder == 'target_enc':
        if y_train is None:
            raise ValueError("O target 'y_train' deve ser fornecido para o Target Encoding.")
        preprocessor.transformers = [
            ('num', StandardScaler(), num_cols),
            ('cat', TargetEncoder(), cat_cols)  # Usando o TargetEncoder
        ]
    else:
        raise ValueError(f"Encoder '{encoder}' não suportado.")
    return preprocessor




# Função para processar dados categóricos e aplicar regressão
def aplicar_modelos_regressao(dfs, target_column, encoder='onehot'):
    """
    Aplica modelos de regressão com diferentes encoders.
    :param dfs: Lista de DataFrames ou um único DataFrame.
    :param target_column: Nome da coluna alvo.
    :param encoder: Encoder a ser usado ('onehot', 'label', ou 'target_enc).
    :return: DataFrame com os resultados.
    """

    if isinstance(dfs, pd.DataFrame):
        dfs = [dfs]

    resultados = []

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

    for i, df in enumerate(dfs):
        print(f"Processando dataset {i + 1} com '{encoder}'")

        # Separando variáveis independentes (X) e dependentes (y)
        X = df.drop(columns=[target_column])
        y = df[target_column]

        # Dividindo em treino e teste
        X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3, random_state=42)


        # Identificaando colunas categóricas e numéricas
        cat_cols = X.select_dtypes(include=['object', 'category']).columns
        num_cols = X.select_dtypes(include=['number']).columns

        # Convertendo valores categóricos para strings em ambos os conjuntos de treino e teste (garantindo que o OneHotEncoder não dê erro)
        X_train[cat_cols] = X_train[cat_cols].astype(str)
        X_test[cat_cols] = X_test[cat_cols].astype(str)

        # Pipeline de pré-processamento
        preprocessor = ColumnTransformer(
            transformers=[
                ('num', StandardScaler(), num_cols),
                ('cat', OneHotEncoder(handle_unknown='ignore'), cat_cols) # Placeholder para ser substituído (dependendo do encoder)
            ]
        )

        # Aplicar o encoder especificado
        preprocessor = aplicar_encoder(preprocessor, encoder, cat_cols, num_cols, y_train)

        for nome_modelo, modelo in modelos.items():
            pipeline = Pipeline(steps=[
                ('preprocessor', preprocessor),
                ('regressor', modelo)
            ])

            # Validação cruzada para robustez (tentativa de detalhar por fold)
            scoring = {
                'rmse': 'neg_mean_squared_error',
                'r2': 'r2',
                'mae': 'neg_mean_absolute_error'
            }
            cv_results = cross_validate(pipeline, X_train, y_train, cv=5, scoring=scoring)

            # Calculando métricas médias e por fold
            mean_cv_rmse = np.sqrt(-cv_results['test_rmse'].mean())
            mean_cv_r2 = cv_results['test_r2'].mean()
            mean_cv_mae = -cv_results['test_mae'].mean()

            # Treinar modelo
            pipeline.fit(X_train, y_train)

            # Previsões
            y_pred = pipeline.predict(X_test)

            # Métricas de avaliação
            rmse = np.sqrt(mean_squared_error(y_test, y_pred))
            r2 = r2_score(y_test, y_pred)
            mae = mean_absolute_error(y_test, y_pred)

            # Armazenar resultados
            resultados.append({
                "Dataset": f"Dataset {i + 1}",
                "Encoder": encoder,
                "Modelo": nome_modelo,
                "R2": r2,
                "MAE": mae,
                "RMSE": rmse,
                "CV R2": mean_cv_r2,
                "CV RMSE": mean_cv_rmse,
                "CV MAE": mean_cv_mae,
                "CV RMSE por fold": cv_results['test_rmse'],
                "CV R2 por fold": cv_results['test_r2'],
                "CV MAE por fold": cv_results['test_mae']
            })

    return pd.DataFrame(resultados)
