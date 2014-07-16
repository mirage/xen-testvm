Xen Test VM
===========

This kernel implements the interface defined in

  https://github.com/mirage/testvm-idl

and exports this interface over Vchan. A test harness can then
invoke operations via this interface and check the results.
