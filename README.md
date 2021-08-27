# HFMU
https://ro-che.info/articles/2017-07-26-haskell-library-in-c-project
https://cabal.readthedocs.io/en/latest/developing-packages.html#foreign-libraries
https://github.com/ocharles/plhaskell/blob/master/smallmain.c


## Requirements 
- The user must implement the function **doStep :: X**.
- The user must implement the **native** function **InitDoStepFunctions :: IO ()**. TODO: Add complete signature
    The InitFMUFunctions must invoke **HFMU.StoreDoStepFunction** and pass the **doStep* function
    
## Development Information
This requires GHC 8.10.7 due to the following error:
```
Undefined symbols for architecture x86_64:
  "___darwin_check_fd_set_overflow", referenced from:
      _awaitEvent in libHSrts.a(Select.o)
ld: symbol(s) not found for architecture x86_64
clang: error: linker command failed with exit code 1 (use -v to see invocation)
`clang' failed in phase `Linker'. (Exit code: 1)
cabal: Failed to build exe:hsc2hs from hsc2hs-0.68.7 (which is required by
HaskellFMU-0.1.0.0). See the build log above for details.
```

This is considered in this issue: https://gitlab.haskell.org/ghc/ghc/-/issues/19950


### HFMIInterface 
The HFMIInterface represents different types needed to implement the C-functions of FMI.
Some of these types are generated using hsc2hs - see the hsc repository.

### Issue with foreign import declarations
Execution `cabal new-repl` throws several errors due to foreign import declarations

``` sh
src/HFMU.hs:39:1: error:
    • Illegal foreign declaration: requires unregisterised, llvm (-fllvm) or native code generation (-fasm)
    • When checking declaration:
        foreign export ccall "fmi2SetupExperiment" fmi2SetupExperiment
          :: FMISetupExperimentType
```

To solve this write: `cabal new-repl --repl-options -fobject-code**. 

**Alternative Approach:** the `.ghci` file has been fixed to account for this by
adding `:set -fobject-code`. Thereby it works with: `cabal new-repl`, `ghci` and
`ghcid -c "cabal new-repl"`.
Furthermore, it is possible to just execute `ghcid` as a `.ghcid` file has been
specified, which contains `-c "cabal new-repl"`


