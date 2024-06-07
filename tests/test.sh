#!/bin/bash

cmd=cmp #for comparison to reference
#cmd=cp #for building new reference

#Temporary directory
TMP_LOC=$(mktemp -d)
trap "\rm -rf $TMP_LOC" EXIT

OLD_PWD=$PWD

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

#Automatic build of signature file
cd $OLD_PWD
python3 ../src/ctypesForFortran/__init__.py --solib=./foo.so --suffix="_" \
                                            --kind_real 8 --kind_integer 8 --kind_logical 1\
                                            $TMP_LOC/foo.F90 > $TMP_LOC/auto_signature.py
$cmd $TMP_LOC/auto_signature.py data/auto_signature.py

#Check
if [ $? -ne 0 ]; then
  echo "The second test has failed!"
fi
