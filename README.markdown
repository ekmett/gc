gc
==

[![Build Status](https://secure.travis-ci.org/ekmett/gc.png?branch=master)](http://travis-ci.org/ekmett/gc)

Experimenting with building [Poor Richard's Memory Manager](http://www.cse.buffalo.edu/~mhertz/prmm-ismm-2011.pdf) in Haskell user space.


```haskell
import System.Mem.Manager

main = do
  _ <- selfishManager
  ...
```

Now an oracle would look for signs of memory pressure from the host operating system and attempt more aggressive garbage collection in response. The current implementation matches the 'GenMS+Selfish' scheme from the original paper due to lack of information from GHC about how many pages have been returned to the OS.

Contact Information
-------------------

Contributions and bug reports are welcome!

Please feel free to contact me through github or on the #haskell IRC channel on irc.freenode.net.

-Edward Kmett
