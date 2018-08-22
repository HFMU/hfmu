#ifndef HFMU_HFMU_INCLUDED
#define HFMU_HFMU_INCLUDED

#include "HsFFI.h"
#include <stdio.h>

static void loaded(void) __attribute__((constructor));
static void loaded(void)
{
  static char *argv[] = { "libHFMU.so", 0 }, **argv_ = argv;
  static int argc = 1;
  hs_init(&argc, &argv_);
}

static void unloaded(void) __attribute__((destructor));
static void unloaded(void)
{
  hs_exit();
}

#endif
