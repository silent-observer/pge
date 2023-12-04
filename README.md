# Prioritized Grammar Enumeration - Symbolic Regression solver

## Symbolic Regression
Statistics often gives us a problem of parametric regression - fitting a function with unknown parameters to a set of multidimensional data.
If the function is linear, the solution is extremely easy, to the point of being solved analytically - that's Linear Least Squares Regression.

When the function is not linear, there are still many known algorithms for it, even though iterative. An example of such an algorithm is the famous Levenberg–Marquardt algorithm.

What do you do, however, if the functional form is not known? You could train a neural network, but the result is completely not interpretable, and often it is important to have an explicit formula describing the data.
Finding such a formula is the problem of *Symbolic Regression*.

## Algorithm
The algorithm implemented here is heavily inspired by Prioritized Grammar Enumeration[1], while it does change a lot of implementation details.

The main idea of the algorithm is to enumerate all the possible formulas in order, and to fit each one of them using regular parametric regression.
The formulas enumeration order is such that the formulas that fit the data the best are used to generate "children" formulas, that are then also evaluated.
This allows the formula search to be directed and to not check every single formula.

## Optimizations
Still, since such an algorithm requires a lot of brute force, many optimizations have been performed.
For instance, it doesn't use pure Levenberg–Marquardt algorithm, instead it uses VarPro[2], which allows for higher precision and stability (and so fewer iterations) at the cost of some matrix computations.
The matrix computations are performed using *BLAS* and *LAPACK* libraries to maximize the performance.

Since every formula has to be evaluated many times (and also its partial derivatives, which are calculated using automatic differentiation), it is the main bottleneck of the algorithm.
As such, the formula evaluation is performed with *JIT compilation* - all the formulas are compiled into native machine code (only x86_64 supported for now). The compiled code also uses SIMD for maximum efficiency.

Also, to utilize multiple cores, the formula evaluation and data fitting is separated into a distinct executable, called an *evaluation server*.
A client can be connected to multiple evaluation servers running in parallel, and send them requests through a custom TCP protocol to evaluate the formulas.

There is also a simple web-server present to schedule the data to be evaluated.

## Sources
1. Petersen, B.K. et al. (2021) Deep symbolic regression: Recovering mathematical expressions from data via risk-seeking policy gradients, arXiv.org. Available at: https://arxiv.org/abs/1912.04871 (Accessed: 04 December 2023).
2. O’Leary, D.P. and Rust, B.W. (2012) ‘Variable projection for nonlinear least squares problems’, Computational Optimization and Applications, 54(3), pp. 579–593. doi:10.1007/s10589-012-9492-9. 
