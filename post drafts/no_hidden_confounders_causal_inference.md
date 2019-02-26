No hidden confounders causal inference
================
Iyar Lin
17 January, 2019

-   [Intro](#intro)
-   [Relatively small effect size](#relatively-small-effect-size)

Intro
=====

In this post we'll restrict ourselves to the case of some binary treatment ![X](https://latex.codecogs.com/png.latex?X "X") with ![X = 1](https://latex.codecogs.com/png.latex?X%20%3D%201 "X = 1") indicating treatment assignment and ![X = 0](https://latex.codecogs.com/png.latex?X%20%3D%200 "X = 0") control assignment. We're interested with estimating the average treatment effect (ATE) of ![X](https://latex.codecogs.com/png.latex?X "X") on some continuous outcome variable ![Y](https://latex.codecogs.com/png.latex?Y "Y"). In mathmatical notation ![\\mathbb{E}(Y|X=1) - \\mathbb{E}(Y|X=0)](https://latex.codecogs.com/png.latex?%5Cmathbb%7BE%7D%28Y%7CX%3D1%29%20-%20%5Cmathbb%7BE%7D%28Y%7CX%3D0%29 "\mathbb{E}(Y|X=1) - \mathbb{E}(Y|X=0)").

We saw in a previous post ( ["Correlation is not causation". So what is?](https://github.com/IyarLin/causal-inference/blob/master/correlation_is_not_causation_so_what_is.md) ) that fitting a model ![Y = f(X) + \\epsilon](https://latex.codecogs.com/png.latex?Y%20%3D%20f%28X%29%20%2B%20%5Cepsilon "Y = f(X) + \epsilon") and computing ![\\hat{ATE} = \\hat{f}(1) - \\hat{f}(0)](https://latex.codecogs.com/png.latex?%5Chat%7BATE%7D%20%3D%20%5Chat%7Bf%7D%281%29%20-%20%5Chat%7Bf%7D%280%29 "\hat{ATE} = \hat{f}(1) - \hat{f}(0)") won't do the trick if there's some confounding variable ![Z](https://latex.codecogs.com/png.latex?Z "Z"). In general we saw that we need to fit the model ![Y = f(X,Z) + \\epsilon](https://latex.codecogs.com/png.latex?Y%20%3D%20f%28X%2CZ%29%20%2B%20%5Cepsilon "Y = f(X,Z) + \epsilon") where ![Z](https://latex.codecogs.com/png.latex?Z "Z") is a set of varibels called "adjutment set". We can than estimate the ATE by ![\\hat{ATE} = \\mathbb{E}\_Z(\\hat{f}(1, Z) - \\hat{f}(0, Z))](https://latex.codecogs.com/png.latex?%5Chat%7BATE%7D%20%3D%20%5Cmathbb%7BE%7D_Z%28%5Chat%7Bf%7D%281%2C%20Z%29%20-%20%5Chat%7Bf%7D%280%2C%20Z%29%29 "\hat{ATE} = \mathbb{E}_Z(\hat{f}(1, Z) - \hat{f}(0, Z))")

Even after we find the correct adjustment set ![Z](https://latex.codecogs.com/png.latex?Z "Z") we may still face some challenges which require the use of specially designed algorithms rather than the familiar run of the mill ML ones.

Relatively small effect size
============================

Classic ML algorithms are geared towards accurate prediction of ![f(X, Z)](https://latex.codecogs.com/png.latex?f%28X%2C%20Z%29 "f(X, Z)"), not ![f(1,Z) - f(0,Z)](https://latex.codecogs.com/png.latex?f%281%2CZ%29%20-%20f%280%2CZ%29 "f(1,Z) - f(0,Z)"). If the effect of changing ![X](https://latex.codecogs.com/png.latex?X "X") is small when compared with the effect of changes in some variables in ![Z](https://latex.codecogs.com/png.latex?Z "Z") than the difference ![f(1,Z) - f(0,Z)](https://latex.codecogs.com/png.latex?f%281%2CZ%29%20-%20f%280%2CZ%29 "f(1,Z) - f(0,Z)") might wash out.

Let's consider for example the following model (defined by a set of equations, also termed "structural equations"):

![Y = \\beta\_0 + \\beta\_1 X + \\beta\_2 Z + \\epsilon](https://latex.codecogs.com/png.latex?Y%20%3D%20%5Cbeta_0%20%2B%20%5Cbeta_1%20X%20%2B%20%5Cbeta_2%20Z%20%2B%20%5Cepsilon "Y = \beta_0 + \beta_1 X + \beta_2 Z + \epsilon")

![X = 1 \\, \\text{if} \\, Z + U\_x &gt; 0.4, \\, X = 0 \\, \\text{if} \\, Z + U\_x \\leq 0.4](https://latex.codecogs.com/png.latex?X%20%3D%201%20%5C%2C%20%5Ctext%7Bif%7D%20%5C%2C%20Z%20%2B%20U_x%20%3E%200.4%2C%20%5C%2C%20X%20%3D%200%20%5C%2C%20%5Ctext%7Bif%7D%20%5C%2C%20Z%20%2B%20U_x%20%5Cleq%200.4 "X = 1 \, \text{if} \, Z + U_x > 0.4, \, X = 0 \, \text{if} \, Z + U_x \leq 0.4")

and

![Z = U\_z](https://latex.codecogs.com/png.latex?Z%20%3D%20U_z "Z = U_z")

Where ![U\_x, \\, U\_z, \\, \\epsilon \\sim \\mathbb{N}(0,1)](https://latex.codecogs.com/png.latex?U_x%2C%20%5C%2C%20U_z%2C%20%5C%2C%20%5Cepsilon%20%5Csim%20%5Cmathbb%7BN%7D%280%2C1%29 "U_x, \, U_z, \, \epsilon \sim \mathbb{N}(0,1)") and ![\\{\\beta\_0, \\beta\_1, \\beta\_2\\} = \\{0.2, 0.1, -0.8\\}](https://latex.codecogs.com/png.latex?%5C%7B%5Cbeta_0%2C%20%5Cbeta_1%2C%20%5Cbeta_2%5C%7D%20%3D%20%5C%7B0.2%2C%200.1%2C%20-0.8%5C%7D "\{\beta_0, \beta_1, \beta_2\} = \{0.2, 0.1, -0.8\}")

So the treatment effect in this case is ![\\beta\_1 = 0.1](https://latex.codecogs.com/png.latex?%5Cbeta_1%20%3D%200.1 "\beta_1 = 0.1")

I've simulated a dataset from the above equations and fitted the model ![\\hat{f}(X,Z)](https://latex.codecogs.com/png.latex?%5Chat%7Bf%7D%28X%2CZ%29 "\hat{f}(X,Z)") using a decision tree. Below I plot the fitted tree:

``` r
N <- 1000
Z <- rnorm(N)
X <- Z + rnorm(N) > 0.4
Y <- 0.2 + X*(0.1) - 0.8 * Z + rnorm(N)
sim_data <- data.frame(X, Z, Y)

a <- rpart(Y ~ X + Z, data = sim_data)
rpart.plot(a)
```

![](no_hidden_confounders_causal_inference_files/figure-markdown_github/simulate%20dataset-1.png)

We can see that the tree completely ignores the ![X](https://latex.codecogs.com/png.latex?X "X") variable, giving the impression there's no treatment effect at all. I've simulated a very simple dataset and used a very simple model for illustration purposes. The problem I've demonstrated persists when using more sophisticated algortihms on high dimensional datasets with non-linear relationships.
