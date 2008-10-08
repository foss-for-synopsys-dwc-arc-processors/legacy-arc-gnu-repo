#!/bin/sh

if [ $# -lt 1 ]
then
    echo "Usage: $0 SOURCE_DIR"
    exit 1
fi

SOURCE_DIR=$1

export CFLAGS="-mA7"
export LDFLAGS="-mA7"
export CC=arc-linux-uclibc-gcc

$1/gdb/gdbserver/configure --host=i386-redhat-linux-gnu --target=arc-linux-uclibc

make
