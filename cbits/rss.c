#ifndef _WIN32

#include <unistd.h>
#include <sys/resource.h>

#endif

/** Return the number of hard page faults that have occurred. */
size_t getHardPageFaults() {

#ifndef _WIN32

  struct rusage rusage;
  getrusage(RUSAGE_SELF, &rusage);
  return (size_t)rusage.ru_majflt;

#else
  /* Windows

     It's not clear how to obtain the number of hard page faults
     on Windows. Taking inspiraton from GHC's getPageFaults function
     (https://gitlab.haskell.org/ghc/ghc/blob/1285d6b95fbae7858abbc4722bc2301d7fe40425/rts/win32/GetTime.c#L148-154),
     we return a conservative lower bound. 
  */
  return 0;
#endif
}
