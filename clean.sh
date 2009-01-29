# This script cleans up ELF32/UCLIBC toolchain builds


rm -rf ./binutils/build
rm -rf ./gcc/build
rm -rf ./insight/build

mkdir binutils/build
mkdir gcc/build
mkdir insight/build

cd uClibc-0.9.29
make clean > /dev/zero 2> /dev/zero
cd ..

cd uClibc-0.9.30
make clean > /dev/zero 2> /dev/zero
cd ..




 
