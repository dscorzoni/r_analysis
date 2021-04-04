Boostrapping Básico
================
Danilo Scorzoni Ré

## Boostrap Básico

Bootstrapping é uma técnica de reamostragem para se realizar inferências
sobre acurácia de estimativas por amostras. Essa definição é um pouco
etérea então vamos entender mais através de simulação e exemplos no R.

## Nosso estudo de caso: Árvores

Imagine que você tem uma fazenda onde decidiu plantar árvores para
futura colheita dessa madeira para venda. Imagine também que você tem
recursos financeiros para realizar um inventário 100%, que significa
basicamente medir todas as árvores dessa floresta (censo) e registrar os
diâmetros dessas árvores. Essa métrics é importante pois com árvores de
maior diâmetro você poderá vender mais madeira (e por valor maior
também) e decidir se está na hora de cortar ou não.

Bom, para simular essa plantação completa, vamos simular os dados
populacionais. Imagine que sua floresta tenha 10.000 árvores, cuja média
de diâmetro seja 50 cm e com um desvio padrão de 10 cm.

``` r
# Definir um seed para as simulações para ter os mesmos resultados da análise:
set.seed(123)

# Carregar pacotes básicos para manipulação de dados e criação de gráficos:
library(tidyverse)
library(ggplot2)

# Simular os diâmetros da nossa população de árvores
pop = data.frame(
  diametros = rnorm(10000, mean = 50, sd = 10)
)

pop %>%
  ggplot(aes(x = diametros)) +
  geom_histogram(alpha = 0.3, fill = "blue") +
  labs(
    title = "Distribuição de Diâmetros da População",
    subtitle = paste0("Media: ", round(mean(pop$diametros),1), ". Desvio-padrão: ", 
                      round(sd(pop$diametros), 2))
  ) +
  theme_light()
```

![](00_intro_bootstrapping_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

Agora, imagine que você não tem tempo ou recursos financeiros para
realizar esse inventário 100%. Uma alternativa é realizar uma amostragem
aleatória de 100 árvores, medir os diâmetros e estimar a média e o
intervalo de confiância para a média para se ter uma ideia do quão
precisa é a sua estimativa para o diâmetro da população. Nesse caso,
usando o método padrão, você pode calcular o intervalo de confiância
para essa amostra:

``` r
amostra = pop %>%
  sample_n(100)

amostra  %>%
  ggplot(aes(x = diametros)) +
  geom_histogram(alpha = 0.3, fill = "red") +
  labs(
    title = "Distribuição de Diâmetros da Amostra",
    subtitle = paste0("Media: ", round(mean(amostra$diametros),1), ". Desvio-padrão: ", 
                      round(sd(amostra$diametros), 2))
  ) +
  theme_light()
```

![](00_intro_bootstrapping_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

Note que os estimadores são não-viesados e representam bem a nossa
amostra dado que esses dados são simulados em uma distribuição normal.
Para calcular o intervalo de confiância, podemos usar:

``` r
t.test(x = amostra$diametros)
```

    ## 
    ##  One Sample t-test
    ## 
    ## data:  amostra$diametros
    ## t = 46.51, df = 99, p-value < 2.2e-16
    ## alternative hypothesis: true mean is not equal to 0
    ## 95 percent confidence interval:
    ##  47.80772 52.06865
    ## sample estimates:
    ## mean of x 
    ##  49.93819

## Bootstrapping

Agora, vamos realizar a mesma análise da amostra, mas utilizando
bootstrapping. Pela teoria, é possível realizar múltiplas reamostragens
**com reposição** da nossa amostra para se criar uma amostra bootstrap
da nossa média e calcular o intervalo de confiância utilizando os
percentis 2.5% e 97.5% dessa distribuição criada por reamostragem. Vamos
aos códigos:

``` r
# Criar um vetor para armazenar o valor da reamostragem:
reamostras = rep(NA, 10000)

# Simular a reamostragem 10.000 vezes:
for (i in 1:10000) {
  reamostra = sample(amostra$diametros, replace = T) # Reamostragem de Diâmetros com reposição
  reamostras[i] = mean(reamostra) # Calcular a média de cada reamostra
}

# Transformar o vetor em formato tidy:
reamostras = data.frame(diametros = reamostras)

# Criar o gráfico de distribuição da média bootstrap:
reamostras  %>%
  ggplot(aes(x = diametros)) +
  geom_histogram(alpha = 0.3, fill = "red") +
  labs(
    title = "Distribuição de Diâmetros da Reamostragem",
    subtitle = paste0("Media: ", round(mean(amostra$diametros),1),".")
  ) + 
  theme_light()
```

![](00_intro_bootstrapping_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

E por fim, vamos calcular o intervalo de confiância através dos
percentis 2.5% e 97.5%:

``` r
quantile(reamostras$diametros, c(0.025, 0.975))
```

    ##     2.5%    97.5% 
    ## 47.81685 52.05008

Note como os intervalos de confiância utilizando o método padrão e o
método bootstrapping são semelhantes.

> **Pergunta relevante:** mas pra que vou utilizar o método bootstrap
> pra calcular o intervalo de confiância se eu posso usar o método
> clássico?

Esse exemplo acima foi apenas para ilustrar o funcionamento desta
metodologia. A grande riqueza em se utilizar o bootstrap é em situações
como:

-   Você não tem certeza ou uma premissa forte sobre a distribuição
    normal da população.
-   Você gostaria de estimar intervalos de confiância para métricas mais
    complexas que não possuem distribuição de probabilidade conhecidas
    ou facilmente definidas, como por exemplo, *R*<sup>2</sup> de uma
    regressão ou a mediana.

Desta forma, você pode ampliar muito as aplicações do bootstrap e ter
uma abordagem mais probabilística para problemas complexos.

## Bootstrap da Mediana usando o pacote *boot*

Vamos aproveitar a amostra anterior para calcular um intervalo de
confiência para a mediana utilizando o pacote **boot**. Este pacote
permite automatizar o processo de geração de estatísticas por
bootstrapping. Para isso você precisa informar:

-   Qual estatística alvo está sendo calculada, em formato de função.
    Neste caso, a mediana.
-   Qual o índice dos dados para a reamostragem ser realizada.

``` r
library(boot)

# Definindo o cálculo da estatística
mediana = function(data, indices) {
  return(median(data[indices]))
}

# Bootstrapping com 10.000 replicações
resultados = boot(data = amostra$diametros, statistic = mediana, R = 10000)

# Cálculo da mediana populacional:
median(pop$diametros)
```

    ## [1] 49.88911

``` r
# Cálculo da mediana da amostra:
median(amostra$diametros)
```

    ## [1] 49.91711

``` r
# Mediana por bootstrapping
resultados
```

    ## 
    ## ORDINARY NONPARAMETRIC BOOTSTRAP
    ## 
    ## 
    ## Call:
    ## boot(data = amostra$diametros, statistic = mediana, R = 10000)
    ## 
    ## 
    ## Bootstrap Statistics :
    ##     original    bias    std. error
    ## t1* 49.91711 0.1230301    0.938251

``` r
# Intervalo de confiância para a mediana por bootstrapping
boot.ci(resultados)
```

    ## BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
    ## Based on 10000 bootstrap replicates
    ## 
    ## CALL : 
    ## boot.ci(boot.out = resultados)
    ## 
    ## Intervals : 
    ## Level      Normal              Basic         
    ## 95%   (47.96, 51.63 )   (48.02, 51.77 )  
    ## 
    ## Level     Percentile            BCa          
    ## 95%   (48.06, 51.82 )   (47.88, 51.74 )  
    ## Calculations and Intervals on Original Scale

## Teste de Hipótese para 2 amostras

Supondo que você tenha duas plantações distintas em sua fazenda que você
gostaria de comparar a produtividade de sua plantação de árvores. Você
pode utilizar um teste-t para um teste de hipótese:

-   *H*<sub>0</sub>: as produtividades das duas áreas são iguais
-   *H*<sub>1</sub>: as produtividades das duas áreas são diferentes

Você então fez uma amostragem de 30 árvores em cada uma destas áreas e
seguiu para a comparação com o teste-t.

``` r
# Simulando as amostras
area_a = rnorm(30, mean = 50, sd = 5)
area_b = rnorm(30, mean = 40, sd = 5)

dados = data.frame(
  area = c(rep("A", 30), rep("B", 30)),
  diametros = c(area_a, area_b)
)

# Gráfico de boxplot para comparar as distribuições
dados %>%
  ggplot(aes(y = diametros, x = area, fill = area, col = area)) +
  geom_boxplot(alpha = 0.3, position = "identity") +
  labs(
    title = "Comparação de diâmetros entre áreas A e B",
    subtitle = paste0("Media area A: ", round(mean(area_a),1), 
                      ", média area B: ", round(mean(area_b),2))
  ) +
  theme_light() +
  theme(legend.position = "none")
```

![](00_intro_bootstrapping_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
# Teste-t para comparação
t.test(area_a, area_b)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  area_a and area_b
    ## t = 7.5295, df = 56.285, p-value = 4.482e-10
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##   7.373632 12.718661
    ## sample estimates:
    ## mean of x mean of y 
    ##  49.32616  39.28002

Os resultados mostram que existe diferença significativa entre as duas
áreas pois o intervalo de confiânça para a diferença de médias **não
contém o zero**.

Para realizar a versão bootstrap deste teste, você pode calcular a
diferença de médias como a sua estatística e, a partir de então, estimar
o intervalo de confiância para a diferênça de médias utilizando os
percentis **2.5%** e **97.5%**, como fizemos anteriormente.

``` r
# Função que retorna a estatística:
diferenca = function(data, indices) {
  dif = mean(data$area_a[indices]) - mean(data$area_b[indices]) # Calcula a diferença de médias
  return(dif)
}

# Bootstrapping com 10.000 replicações
teste_ab = boot(data = data.frame(area_a, area_b), statistic = diferenca, R = 10000)

teste_ab
```

    ## 
    ## ORDINARY NONPARAMETRIC BOOTSTRAP
    ## 
    ## 
    ## Call:
    ## boot(data = data.frame(area_a, area_b), statistic = diferenca, 
    ##     R = 10000)
    ## 
    ## 
    ## Bootstrap Statistics :
    ##     original     bias    std. error
    ## t1* 10.04615 0.02029602    1.260086

``` r
boot.ci(teste_ab)
```

    ## BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
    ## Based on 10000 bootstrap replicates
    ## 
    ## CALL : 
    ## boot.ci(boot.out = teste_ab)
    ## 
    ## Intervals : 
    ## Level      Normal              Basic         
    ## 95%   ( 7.56, 12.50 )   ( 7.59, 12.51 )  
    ## 
    ## Level     Percentile            BCa          
    ## 95%   ( 7.58, 12.50 )   ( 7.52, 12.45 )  
    ## Calculations and Intervals on Original Scale

Note que, novamente, é possível observar que as estimativas da diferença
e os intervalos de confiância são semelhantes ao teste t clássico.
