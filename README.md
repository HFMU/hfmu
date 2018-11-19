# HFMU
https://ro-che.info/articles/2017-07-26-haskell-library-in-c-project
https://cabal.readthedocs.io/en/latest/developing-packages.html#foreign-libraries
https://github.com/ocharles/plhaskell/blob/master/smallmain.c


## Requirements 
- The user must implement the function **doStep :: X**.
- The user must implement the **native** function **InitDoStepFunctions :: IO ()**. TODO: Add complete signature
    The InitFMUFunctions must invoke **HFMU.StoreDoStepFunction** and pass the **doStep* function
    
## Development Information
### HFMIInterface 
The HFMIInterface represents different types needed to implement the C-functions of FMI.
Some of these types are generated using hsc2hs - see the hsc repository.


