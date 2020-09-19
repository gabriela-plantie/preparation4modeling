preparation4modelling
================
gabriela plantie
2019-01

Overview
--------

This is a package that groups variables (categorical and numerical) according to their likelihood of producing the event analyzed. It can be used as a tool to explore information for building a model to predict a binary target.

Install
-------

``` r
library(devtools)
#devtools::install_github("gabriela-plantie/preparation4modeling", force=T, dependencies = F)
library(preparation4modeling)
```

Usage
-----

### Example table

``` r
set.seed(1)
x1 = rnorm(1000)
x2 = rnorm(1000)
x4='A'
x4=ifelse(x1>0.1,'B', x4)
x4=ifelse(x1>0.4,'C', x4 )
x4=ifelse(x1>0.6,'D', x4 )
x4=ifelse(x1>0.8,'E', x4 )
z = 1 + 3*x1
pr = 1/(1+exp(-z))
y = rbinom(1000,1,pr)
tbla = data.frame(y=y,x1=x1,x2=x2, x4=x4)
q_nas=100
x1[1:q_nas] = NA
x4[1:q_nas]=NA
 
```

### Analyzing variables against target

#### numerical variable with tree

``` r
agrupa_ctree (tbla, target_name='y', variable_name='x1',flag_numerica=1, max_q_groups=10, algoritmo='chaid' )
#>   variable_name nodo_pred     rangos_pred cant_nodo pos_nodo rt_nodo
#> 1            x1         1   (-Inf,-1.025]       164        2   0.012
#> 2            x1         2 (-1.016,-0.779]        59        9   0.153
#> 3            x1         2 (-0.772,-0.404]       119       50   0.420
#> 4            x1         3    (-0.4,0.333]       282      200   0.709
#> 5            x1         4   (0.341,0.616]       102       88   0.863
#> 6            x1         5      (0.62,Inf]       274      271   0.989
#>   participacion log_odds corte_inf corte_sup
#> 1         0.164   -4.411    -3.008    -1.025
#> 2         0.059   -1.711    -1.016    -0.779
#> 3         0.119   -0.323    -0.772    -0.404
#> 4         0.282    0.891    -0.400     0.333
#> 5         0.102    1.840     0.341     0.616
#> 6         0.274    4.499     0.620     3.810
```

#### categorical variable with tree

``` r
agrupa_ctree (tbla, target_name='y', variable_name='x4',flag_numerica=0, algoritmo='chaid' )
#>   variable_name   nodo_pred variable_valor cant_nodo pos_nodo rt_nodo
#> 1            x4           1              E       214      212   0.991
#> 2            x4           2              B       104       86   0.827
#> 3            x4           3              A       548      201   0.367
#> 5            x4 pocos_casos              D       134      121   0.903
#> 4            x4 pocos_casos              C       134      121   0.903
#>   participacion log_odds cant_var pos_var rt_var
#> 1         0.214    4.701      214     212  0.991
#> 2         0.104    1.565      104      86  0.827
#> 3         0.548   -0.545      548     201  0.367
#> 5         0.134    2.231       64      61  0.953
#> 4         0.134    2.231       70      60  0.857
```

#### categorical variable with hipergeometric test

``` r
agrupa_nominal_filtra_small(tbla, target_name='y', variable_name='x4',limite=0.05, symbol_to_split='%#%', limite_grupo=100)
#>    variable_name   nodo_pred variable_valor cant_nodo pos_nodo   rt_nodo
#> 3             x4           1              E       214      212 0.9910000
#> 2             x4           2              B       104       86 0.8270000
#> 1             x4           3              A       548      201 0.3670000
#> 11            x4 pocos_casos              C       134      121 0.9029851
#> 21            x4 pocos_casos              D       134      121 0.9029851
#>    participacion  log_odds cant_var pos_var    rt_var
#> 3          0.247  4.701000      214     212 0.9906542
#> 2          0.120  1.565000      104      86 0.8269231
#> 1          0.633 -0.545000      548     201 0.3667883
#> 11           Inf  2.230841       70      60 0.8571429
#> 21           Inf  2.230841       64      61 0.9531250
```

### example table 2

``` r
x1 = rnorm(1000)
x2 = rnorm(1000)
x3= ifelse(as.factor(x2>0.5)==T, 'A', 'B')
x4= ifelse(as.factor(x2>0.7)==T, 'C', 'D')
z = 1 + 2 * x1 + 3 * x2
pr = 1/(1+exp(-z))
y = rbinom(1000,1,pr)
tbla = data.frame(y=y,x1=x1,x2=x2, x3=x3, x4=x4)
```

### define level according to bad rate

``` r
tbla<-redefine_level_0( df_agrupada_y=tbla ,variables=c('x3',  'x4') ,nombre_target='y')
#> [1] "x3"
#> [1] "x4"
```

### generating model and scorecard table

``` r
filtros_train= (tbla$random=runif(nrow(tbla)))<0.5
f=formula(y~x3+x4)
lr <- glm(f, tbla[ filtros_train, ], family = 'binomial')
tabla_estimadores(lr)
#> $scorecard
#>     variable num_variable variable_nivel nivel  Estimate      P_value
#> 1 Intercepto            0                 <NA>  3.277144 1.217737e-10
#> 2         x3            1            x3A     A  0.000000           NA
#> 3         x3            1            x3B     B -1.808817 7.673659e-05
#> 4         x4            2            x4C     C  0.000000           NA
#> 5         x4            2            x4D     D -1.572396 1.990559e-02
#>   signif max_estim importancia ranking puntos
#> 1    ***        NA          NA       1      0
#> 2         1.808817       0.535       2      0
#> 3    ***  1.808817       0.535       1    535
#> 4         1.572396       0.465       2      0
#> 5    ***  1.572396       0.465       1    465
#> 
#> $multiplier
#> [1] -295.7518
#> 
#> $theorical_sum
#> [1] 1000
#> 
#> $real_sum
#> [1] 1000
```

### example table 3

``` r
x1 = rnorm(1000)
x2 = rnorm(1000)
z = 1 + 2 * x1 + 3 * x2
pr = 1/(1+exp(-z))
y = rbinom(1000,1,pr)
y1 = rbinom(1000,1,abs(pr-0.05))
tbla = data.frame(y=y,x1=x1,x2=x2, y1=y1)
f=formula(y~x1+x2)
lr <- glm(f, tbla, family = 'binomial')
tbla$prob<-predict(lr, tbla, type='response')
```

### generating performance table

``` r
ventiles(tbla, targets=c('y', 'y1'), score_name = 'prob')
#>             grupos  tot  br_y br_y1 ks_y ks_y1 min_prob max_prob
#> 20       [0,0.007]   50 0.000 0.060 0.12  0.10       NA   0.0070
#> 1   (0.007,0.0319]   50 0.020 0.000 0.25  0.21   0.0070   0.0319
#> 2  (0.0319,0.0792]   50 0.020 0.020 0.37  0.32   0.0319   0.0792
#> 3   (0.0792,0.145]   50 0.160 0.060 0.45  0.42   0.0792   0.1450
#> 4    (0.145,0.218]   50 0.140 0.120 0.55  0.51   0.1450   0.2180
#> 5    (0.218,0.304]   50 0.280 0.100 0.62  0.61   0.2180   0.3040
#> 6    (0.304,0.415]   50 0.500 0.280 0.64  0.66   0.3040   0.4150
#> 7    (0.415,0.533]   50 0.320 0.400 0.70  0.69   0.4150   0.5330
#> 8    (0.533,0.636]   50 0.620 0.540 0.70  0.70   0.5330   0.6360
#> 9    (0.636,0.728]   50 0.620 0.600 0.69  0.68   0.6360   0.7280
#> 10   (0.728,0.793]   50 0.800 0.680 0.65  0.66   0.7280   0.7930
#> 11   (0.793,0.852]   50 0.820 0.800 0.60  0.61   0.7930   0.8520
#> 12     (0.852,0.9]   50 0.900 0.840 0.54  0.56   0.8520   0.9000
#> 13     (0.9,0.932]   50 0.900 0.920 0.48  0.48   0.9000   0.9320
#> 14   (0.932,0.957]   50 0.960 0.920 0.40  0.41   0.9320   0.9570
#> 15   (0.957,0.975]   50 0.960 0.960 0.33  0.32   0.9570   0.9750
#> 16   (0.975,0.984]   50 1.000 0.940 0.25  0.25   0.9750   0.9840
#> 17   (0.984,0.993]   50 0.960 0.960 0.17  0.16   0.9840   0.9930
#> 18   (0.993,0.997]   50 1.000 0.940 0.08  0.09   0.9930   0.9970
#> 19     (0.997,1.1]   50 1.000 0.980 0.00  0.00   0.9970   1.1000
#> 21           todos 1000 0.599 0.556 0.70  0.70       NA       NA
```
