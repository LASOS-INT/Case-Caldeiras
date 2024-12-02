import pandas as pd
import numpy as np
from scipy.stats import kstest, gamma, norm, weibull_min, lognorm, expon
from matplotlib import pyplot as plt
from pathlib import Path
import warnings

def estima_distribuicao(datasets, filtros, coluna='SomaDeHorasApontadasUnitario', quant_amostragens=0, fracao=0.6):
    if filtros:
        quant_datasets = len(datasets)
    else:
        quant_datasets = 1

    for i in range(quant_datasets):
        for j in range(1, 3):  # Com ou sem outliers
            for k in range(quant_amostragens + 1):  # Amostragens
                print(f"i{i+1} j{j} k{k}")

                # Seleciona dataset
                filtrado = datasets[i] if filtros else datasets
                grupo_atividade = i + 1
                outlier_status = "Com" if j == 1 else "Sem"

                # Realiza amostragem se necessário
                if k > 0:
                    filtrado = filtrado.sample(frac=fracao, random_state=k)

                # Nome do arquivo
                nome_arquivo = f"DURACAO-{grupo_atividade}-{outlier_status}Outlier-{k}"

                # Ajusta a coluna de análise
                filtrado[coluna] = filtrado[coluna] / 1000
                filtrado2 = filtrado[coluna]

                # Detecção de outliers
                print("Detecção de outliers")
                q1 = np.percentile(filtrado2, 25)
                q3 = np.percentile(filtrado2, 75)
                iqr = q3 - q1
                lb, ub = q1 - 1.5 * iqr, q3 + 1.5 * iqr
                if j == 2:  # Remove outliers
                    filtrado2 = filtrado2[(filtrado2 >= lb) & (filtrado2 <= ub)]

                # Estima distribuições
                print("Estimando distribuições")
                x = filtrado2.to_numpy()

                def fit_distribution(data, dist, **params):
                    try:
                        with warnings.catch_warnings():
                            warnings.simplefilter("ignore")
                            result = dist.fit(data, **params)
                        return result
                    except Exception as e:
                        print(f"Falha ao ajustar {dist.name}: {e}")
                        return None

                distribs = {
                    "weibull": weibull_min,
                    "gamma": gamma,
                    "lognormal": lognorm,
                    "exponential": expon,
                    "normal": norm,
                }

                results = {name: fit_distribution(x, dist) for name, dist in distribs.items()}
                results = {k: v for k, v in results.items() if v is not None}

                # Salva gráficos
                # print("Gerando gráficos")
                # output_path = Path.cwd() / f"{nome_arquivo}.pdf"
                # with output_path.open('wb') as f:
                #     plt.figure(figsize=(10, 8))
                #     plt.suptitle(nome_arquivo)

                #     plt.subplot(2, 2, 1)
                #     plt.hist(x, bins=30, density=True, alpha=0.6, color='g')
                #     for name, dist in distribs.items():
                #         if name in results:
                #             pdf = dist.pdf(sorted(x), *results[name])
                #             plt.plot(sorted(x), pdf, label=name)
                #     plt.legend()

                #     plt.subplot(2, 2, 2)
                #     plt.boxplot(x)
                    
                #     plt.savefig(f)
                #     plt.close()

                # Testes de aderência
                print("Testes de aderência")
                ad_tests = {}
                for name, dist in distribs.items():
                    if name in results:
                        try:
                            test_stat, p_value = kstest(x, dist.cdf, args=results[name])
                            ad_tests[name] = {'statistic': test_stat, 'p_value': p_value}
                        except Exception as e:
                            print(f"Erro no teste de aderência para {name}: {e}")
                            ad_tests[name] = None

                print(ad_tests)

# Exemplo de uso
datasets = [pd.DataFrame({'SomaDeHorasApontadasUnitario': np.random.rand(100) * 1000})]
estima_distribuicao(datasets, filtros=True)

