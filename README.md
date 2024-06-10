ctypesForFortran
================

This module facilitates the interfacing with python of FORTRAN code
compiled and assembled in a shared library.

The signature of FORTRAN routines is provided in the form of a list of python
objects; this signature is used by the module to call the FORTRAN routine and
return its result.

In addition to the full example available in the docstring of the
[main file](src/ctypesForFortran/__init__.py), here's a simple example.

The FORTRAN source code (compiled and linked to produce the foo.so shared lib):

    SUBROUTINE FOO(KIN, KOUT)
      INTEGER(KIND=4), INTENT(IN) :: KIN
      INTEGER(KIND=4), INTENT(OUT) :: KOUT
      KOUT = KIN + 1
      PRINT*, KIN, KOUT
    END SUBROUTINE FOO

The python code:

    import numpy
    import ctypesForFortran
  
    IN = ctypesForFortran.IN
    OUT = ctypesForFortran.OUT
    INOUT = ctypesForFortran.INOUT
  
    ctypesFF, handle = ctypesForFortran.ctypesForFortranFactory("./foo.so")

    @ctypesFF()
    def foo(KIN):
        return ([KIN],
                [(numpy.int32, None, IN), #KIN
                 (numpy.int32, None, OUT)], #KOUT
                None)

    assert foo(4) == 5
