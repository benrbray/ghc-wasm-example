// https://github.com/fourmolu/fourmolu/blob/8aa2200fb38345d624d9682a051682094017bf8e/web/fourmolu-wasm/cbits/init.c
#include <Rts.h>
#include "Main_stub.h"

__attribute__((export_name("wizer.initialize"))) void __wizer_initialize(void) {
  char *args[] = {
    "fourmolu-wasm.wasm",
    "+RTS",
    "--nonmoving-gc",
    "-H64m",
    "-RTS",
  };
  int argc = sizeof(args) / sizeof(args[0]);
  char **argv = args;
  hs_init_with_rtsopts(&argc, &argv);
  hs_perform_gc();
  hs_perform_gc();
  rts_clearMemory();
}