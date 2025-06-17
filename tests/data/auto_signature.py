import numpy
import ctypesForFortran

IN = ctypesForFortran.IN
OUT = ctypesForFortran.OUT
INOUT = ctypesForFortran.INOUT

select = {('real', 2): numpy.float16,
          ('real', 4): numpy.float32,
          ('real', 8): numpy.float64,
          ('integer', 1): numpy.int8,
          ('integer', 2): numpy.int16,
          ('integer', 4): numpy.int32,
          ('integer', 8): numpy.int64,
          ('logical', 1): bool}

pre_suf = {}
pre_suf[''] = ('', '_')


ctypesFF, handle = ctypesForFortran.ctypesForFortranFactory('./foo.so')

@ctypesFF(*pre_suf[''], indexing='F')
def f_int(kin):
    return ([kin],
            [(select[('integer', 8)],None,IN)],
            (select[('integer', 8)],None))

@ctypesFF(*pre_suf[''], indexing='F')
def f_real(pin):
    return ([pin],
            [(select[('real', 8)],None,IN)],
            (select[('real', 8)],None))

@ctypesFF(*pre_suf[''], indexing='F')
def f_bool(lin):
    return ([lin],
            [(select[('logical', 1)],None,IN)],
            (select[('logical', 1)],None))

@ctypesFF(*pre_suf[''], indexing='F')
def foo(kin, kinout, kain, kainout, cdin, cdinout, cdain, cdainout, pin, pinout, pain, painout, lin, linout, lain, lainout, kain2):
    return ([kin, kinout, kain, kainout, cdin, cdinout, cdain, cdainout, pin, pinout, pain, painout, lin, linout, lain, lainout, kain2],
            [(select[('integer', 8)],None,IN),
             (select[('integer', 8)],None,OUT),
             (select[('integer', 8)],None,INOUT),
             (select[('integer', 8)],(kin, ),IN),
             (select[('integer', 8)],(kin, ),OUT),
             (select[('integer', 8)],(kin, ),INOUT),
             (str,(10, ),IN),
             (str,(20, ),OUT),
             (str,(20, ),INOUT),
             (str,(10, 2, 3, ),IN),
             (str,(10, 2, 3, ),OUT),
             (str,(10, 2, 3, ),INOUT),
             (select[('real', 8)],None,IN),
             (select[('real', 8)],None,OUT),
             (select[('real', 8)],None,INOUT),
             (select[('real', 8)],(kin, ),IN),
             (select[('real', 8)],(kin, ),OUT),
             (select[('real', 8)],(kin, ),INOUT),
             (select[('logical', 1)],None,IN),
             (select[('logical', 1)],None,OUT),
             (select[('logical', 1)],None,INOUT),
             (select[('logical', 1)],(40, ),IN),
             (select[('logical', 1)],(40, ),OUT),
             (select[('logical', 1)],(40, ),INOUT),
             (select[('integer', 8)],(4, 5, ),IN),
             (select[('integer', 8)],(4, 5, ),OUT)],
            None)

@ctypesFF(*pre_suf[''], indexing='F')
def convert(carrayin):
    return ([carrayin],
            [(str,(1, 10, ),IN),
             (str,(1, 12, ),OUT)],
            None)


