# Experiments with the monad-bayes library

## Note: this sub-project may not be included in the book

## monad-bayes

[github repo for monad-bayes](https://github.com/adscib/monad-bayes)

Paper: [Practical Probabilistic Programming with Monads](http://mlg.eng.cam.ac.uk/pub/pdf/SciGhaGor15.pdf)




# Probabilistic Graph Models

We will be using the project [monad-bayes]() as described in the paper [Practical Probabilistic Programming with Monads](http://mlg.eng.cam.ac.uk/pub/pdf/SciGhaGor15.pdf) in this chapter.

## Building Blocks: Using the monad-bayes Package

Loading the project for this chapter ingo a GHCI repl loads many modules so the default prompt is uncomfortably long so we can use the **:set prompt** command:

*Main Control.Monad.Bayes Control.Monad.Bayes.Class Control.Monad.Bayes.Conditional Control.Monad.Bayes.Coprimitive Control.Monad.Bayes.Deterministic Control.Monad.Bayes.Dist Control.Monad.Bayes.Empirical Control.Monad.Bayes.Inference Control.Monad.Bayes.LogDomain Control.Monad.Bayes.Particle Control.Monad.Bayes.Primitive Control.Monad.Bayes.Prior Control.Monad.Bayes.Rejection Control.Monad.Bayes.Sampler Control.Monad.Bayes.Trace Control.Monad.Bayes.Weighted>> :set prompt >
>

The following show three types of probability distributions defined in **monad-bayes**:


> let u  = uniform 0 1
>:t u
u :: MonadDist m => m (CustomReal m)
>u
0.703581369867653
>u
0.8639193332801403
>u
0.1637863796943102
>let c = categorical [(0, 0.5), (1,0.5)]
>:t c
c :: (Num a, MonadDist m) => m a
>c
0
>c
0
>c
1
>let n = normal 0.0 10.0
>:t n
n :: MonadDist m => m (CustomReal m)
>n
-6.754402108032122
>n
9.091254983885165
>n
1.5738929570797735
>n
-4.231964722390378
>let uniform_dice = uniformD [1..6]
>uniform_dice 
3
>uniform_dice 
6
>uniform_dice 
5
>uniform_dice 
2
