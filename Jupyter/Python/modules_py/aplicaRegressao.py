import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split, cross_val_score
from sklearn.preprocessing import OneHotEncoder, StandardScaler
from sklearn.compose import ColumnTransformer
from sklearn.pipeline import Pipeline
from sklearn.linear_model import LinearRegression, Ridge, Lasso, ElasticNet
from sklearn.ensemble import RandomForestRegressor, GradientBoostingRegressor
from sklearn.tree import DecisionTreeRegressor
from sklearn.svm import SVR
from xgboost import XGBRegressor 
from sklearn.metrics import mean_squared_error, r2_score, mean_absolute_error

# Função para processar dados categóricos e aplicar regressão
def aplicar_modelos_regressao(dfs, target_column):

    if isinstance(dfs, pd.DataFrame):
        dfs = [dfs]

    resultados = []

    modelos = {
        "Regressao Linear": LinearRegression(),
        "Ridge": Ridge(),
        "Lasso": Lasso(),
        "ElasticNet": ElasticNet(),
        "Arvore de Decisao": DecisionTreeRegressor(),
        "Random Forest": RandomForestRegressor(),
        "Gradient Boosting": GradientBoostingRegressor(),
        "XGBoost": XGBRegressor(objective='reg:squarederror', random_state=42),
        "SVR (Linear Kernel)": SVR(kernel='rbf', C=10, epsilon=0.2),
        "SVR (RBF Kernel)": SVR(kernel='rbf', C=10, epsilon=0.2),
        "SVR (Sigmoid Kernel)": SVR(kernel='sigmoid', C=10, epsilon=0.2)
    }

    for i, df in enumerate(dfs):
        print(f"Processando dataset {i + 1}...")

        # Separar variáveis independentes (X) e dependente (y)
        X = df.drop(columns=[target_column])
        y = df[target_column]

        # Dividir em treino e teste
        X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3, random_state=42)


        # Identificar colunas categóricas e numéricas
        cat_cols = X.select_dtypes(include=['object', 'category']).columns
        num_cols = X.select_dtypes(include=['number']).columns

        # Converte valores categóricos para strings em ambos os conjuntos de treino e teste
        X_train[cat_cols] = X_train[cat_cols].astype(str)
        X_test[cat_cols] = X_test[cat_cols].astype(str)

        # Criar pipeline de pré-processamento
        preprocessor = ColumnTransformer(
            transformers=[
                ('num', StandardScaler(), num_cols),
                ('cat', OneHotEncoder(handle_unknown='ignore'), cat_cols)
            ]
        )

        for nome_modelo, modelo in modelos.items():
            pipeline = Pipeline(steps=[
                ('preprocessor', preprocessor),
                ('regressor', modelo)
            ])

            # Validação cruzada para robustez
            scores = cross_val_score(pipeline, X_train, y_train, cv=5, scoring='neg_mean_squared_error')
            mean_cv_rmse = np.sqrt(-scores.mean())

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
                "Modelo": nome_modelo,
                "R2": r2,
                "MAE": mae,
                "RMSE": rmse,
                "CV RMSE": mean_cv_rmse,
            })

    return pd.DataFrame(resultados)
