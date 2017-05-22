## Notes

```stack build gtk --flag gtk:have-quartz-gtk```

"Repa does not support nested parallellism. This means that you cannot map a parallel worker function across an array and then call computeP to evaluate it, or pass a parallel worker to parallel reductions such as foldP. If you do then you will get a run-time warning and the code will run very slowly."
[Link](http://hackage.haskell.org/package/repa-3.4.1.2/docs/Data-Array-Repa.html)

## Links

[Repa Tutorial](https://wiki.haskell.org/Numeric_Haskell:_A_Repa_Tutorial)  
[SO – Stock Market Problem](http://stackoverflow.com/questions/7086464/maximum-single-sell-profitl)  
