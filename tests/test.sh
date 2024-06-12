#!/bin/bash

cmd=cmp #for comparison to reference
#cmd=cp #for building new reference

#Temporary directory
TMP_LOC=$(mktemp -d)
trap "\rm -rf $TMP_LOC" EXIT

OLD_PWD=$PWD

###############################################################################################
### TEST 1: docstring example
###############################################################################################

#Extract FORTRAN example from docstring
sed -n '/<<BEGIN FORTRAN/, />>END FORTRAN/p' ../src/ctypesForFortran/__init__.py  | \
        head --lines=-1  | tail --lines=+2 > $TMP_LOC/foo.F90
python=$(sed -n '/<<BEGIN PYTHON/, />>END PYTHON/p' ../src/ctypesForFortran/__init__.py  | \
         head --lines=-1  | tail --lines=+2)

#Extract PYTHON example from docstring and remove indentation
first=$(echo "$python" | head -1)
pos=$(echo "$first" | grep -b -o $(echo $first | awk '{print $1}') | cut -d: -f1)
echo "$python" | cut -c $(($pos+1))- > $TMP_LOC/foo.py

#Compilation using gfortran
cd $TMP_LOC
gfortran -c -fPIC foo.F90 && gfortran -shared -g -o foo.so foo.o

#Execution
python3 foo.py

#Check
if [ $? -ne 0 ]; then
  echo "The first test has failed!"
fi

###############################################################################################
### TEST 2: signature building
###############################################################################################

#Automatic build of signature file
cd $OLD_PWD
python3 ../src/ctypesForFortran/__init__.py --solib=./foo.so --suffix="_" \
                                            --kind_real 8 --kind_integer 8 --kind_logical 1\
                                            $TMP_LOC/foo.F90 --Findexing > $TMP_LOC/auto_signature.py
$cmd $TMP_LOC/auto_signature.py data/auto_signature.py

#Check
if [ $? -ne 0 ]; then
  echo "The second test has failed!"
fi

###############################################################################################
### TEST 3: indexing
###############################################################################################

#FORTRAN part
cat - <<EOF > $TMP_LOC/indexing.F90
SUBROUTINE FOO(K)
  INTEGER(KIND=4), DIMENSION(4, 5), INTENT(INOUT) :: K
  K(1, :) = K(1, :) + 1
  K(2, :) = 0
  K(3, :) = K(4, :)
  K(4, :) = [1, 2, 3, 4, 5]
  IF(K(1, 1)==999) PRINT*, SIZE(K, 1), SIZE(K, 2) !to force linking with a compiler specific lib
END SUBROUTINE FOO
EOF

#Compilation using gfortran
cd $TMP_LOC
gfortran -c -fPIC indexing.F90 && gfortran -shared -g -o indexing.so indexing.o

#Build signature files
cd $OLD_PWD
python3 ../src/ctypesForFortran/__init__.py --solib=./indexing.so --suffix="_" \
                                            --kind_integer 4 \
                                            $TMP_LOC/indexing.F90 > $TMP_LOC/indexingC.py
python3 ../src/ctypesForFortran/__init__.py --solib=./indexing.so --suffix="_" \
                                            --kind_integer 4 \
                                            $TMP_LOC/indexing.F90 --Findexing > $TMP_LOC/indexingF.py

#Python part
pythonF="
kin = numpy.ndarray((4, 5), dtype=numpy.int32)
kin[0, :] = [10, 11, 12, 13, 14]
kin[1, :] = 1
kin[2, :] = [9, 9, 9, 9, 9]
kin[3, :] = [20, 21, 22, 23, 24]
kout = numpy.ndarray((4, 5), dtype=numpy.int32)
kout[0, :] = [11, 12, 13, 14, 15]
kout[1, :] = 0
kout[2, :] = [20, 21, 22, 23, 24]
kout[3, :] = [1, 2, 3, 4, 5]
"
pythonC="
kin = numpy.ndarray((5, 4), dtype=numpy.int32)
kin[:, 0] = [10, 11, 12, 13, 14]
kin[:, 1] = 1
kin[:, 2] = [9, 9, 9, 9, 9]
kin[:, 3] = [20, 21, 22, 23, 24]
kout = numpy.ndarray((5, 4), dtype=numpy.int32)
kout[:, 0] = [11, 12, 13, 14, 15]
kout[:, 1] = 0
kout[:, 2] = [20, 21, 22, 23, 24]
kout[:, 3] = [1, 2, 3, 4, 5]
"

cd $TMP_LOC

#Test 3.1 indexing='F' with signature file
sig="$(cat indexingF.py)"
python3 <<EOF
$sig
$pythonF
assert numpy.all(kout == foo(kin)), 'Test 3.1'
EOF
t31=$?

#Test 3.2 indexing='C' with signature file
sig="$(cat indexingC.py)"
python3 <<EOF
$sig
$pythonC
assert numpy.all(kout == foo(kin)), 'Test 3.2'
EOF
t32=$?

#Test 3.3 indexing='F' without signature file
python3 <<EOF
import numpy
import ctypesForFortran
INOUT = ctypesForFortran.INOUT
ctypesFF, handle = ctypesForFortran.ctypesForFortranFactory('./indexing.so')

@ctypesFF(indexing='F')
def foo(k):
    return ctypesForFortran.fortran2signature(filename='indexing.F90', as_string=False,
                                              prefix="", suffix="_", only='foo', indexing='F', K=k)

$pythonF
assert numpy.all(kout == foo(kin)), 'Test 3.3'
EOF
t33=$?

#Test 3.4 indexing='C' without signature file
python3 <<EOF
import numpy
import ctypesForFortran
INOUT = ctypesForFortran.INOUT
ctypesFF, handle = ctypesForFortran.ctypesForFortranFactory('./indexing.so')

@ctypesFF()
def foo(k):
    return ctypesForFortran.fortran2signature(filename='indexing.F90', as_string=False,
                                              prefix="", suffix="_", only='foo', K=k)

$pythonC
assert numpy.all(kout == foo(kin)), 'Test 3.4'
EOF
t34=$?

if [ $(($t31+$t32+$t33+$t34)) -ne 0 ]; then
  echo "The third test has failed!"
fi
