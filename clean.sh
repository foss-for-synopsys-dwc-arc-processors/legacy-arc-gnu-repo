# This script cleans up ELF32/UCLIBC toolchain builds


rm -rf ./binutils/build
rm -rf ./gcc/build
mkdir binutils/build
mkdir gcc/build

cd uClibc-0.9.29
make clean
cd ..




 
