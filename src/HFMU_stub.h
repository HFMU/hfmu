#include "HsFFI.h"
#ifdef __cplusplus
extern "C" {
#endif
extern HsStablePtr fmi2Instantiate(HsPtr a1, HsInt32 a2, HsPtr a3, HsPtr a4, HsPtr a5, HsWord8 a6, HsWord8 a7);
extern HsInt32 fmi2SetupExperiment(HsStablePtr a1, HsWord8 a2, HsDouble a3, HsDouble a4, HsWord8 a5, HsDouble a6);
extern HsInt32 fmi2EnterInitializationMode(HsStablePtr a1);
extern HsInt32 fmi2ExitInitializationMode(HsStablePtr a1);
extern HsInt32 fmi2SetInteger(HsStablePtr a1, HsPtr a2, HsWord64 a3, HsPtr a4);
extern HsInt32 fmi2SetReal(HsStablePtr a1, HsPtr a2, HsWord64 a3, HsPtr a4);
extern HsInt32 fmi2GetBoolean(HsStablePtr a1, HsPtr a2, HsWord64 a3, HsPtr a4);
extern HsInt32 fmi2DoStep(HsStablePtr a1, HsDouble a2, HsDouble a3, HsWord8 a4);
#ifdef __cplusplus
}
#endif

