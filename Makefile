# Master makefile to build ARC Linux / uClibc / GNU toolchain using LSF

all: stamp-elf32-tools-install stamp-elf32-gcc stamp-elf32-insight stamp-uclibc-tools-install stamp-uclibc-gcc stamp-uclibc-libc stamp-uclibc-libstdcxx stamp-uclibc-insight stamp-uclibc-gdbserver

stamp-elf32-tools-install: stamp-elf32-binutils stamp-elf32-gas stamp-elf32-ld
	bsub -n 1,8 -R "span[hosts=1]" -K -q normal -o install-elf32-binutils.log "cd ${TRUNKDIR}/binutils/build32 && make -j -l 8 install-binutils install-gas install-ld" && echo success && touch $@

stamp-uclibc-tools-install: stamp-uclibc-binutils stamp-uclibc-gas stamp-uclibc-ld stamp-uclibc-gprof
	bsub -n 1,8 -R "span[hosts=1]" -K -q normal -o install-uclibc-binutils.log "cd ${TRUNKDIR}/binutils/build && make -j -l 8 install-binutils install-gas install-ld install-gprof" && echo success && touch $@

#configure elf32 toolchain and build host-libiberty, bfd and opcodes.
stamp-elf32-opcodes:
	bsub -J 'build arc-elf32 opcodes' -n 1,8 -R "span[hosts=1]" -K -q normal -o build-elf32-opcodes.log "cd ${TRUNKDIR}/binutils && rm -rf build32 && mkdir build32 && cd build32 && ../src/configure --prefix=${INSTALLDIR}/elf32 --target=arc-elf32 --disable-werror && make -j -l 8 all-libiberty all-bfd all-opcodes" && echo success && touch $@

stamp-elf32-binutils: stamp-elf32-opcodes
	bsub -J 'build arc-elf32 binutils' -n 1,8 -R "span[hosts=1]" -K -q normal -o build-elf32-binutils.log "cd ${TRUNKDIR}/binutils/build32 && make -j -l 8 all-binutils" && echo success && touch $@

stamp-elf32-gas: stamp-elf32-opcodes
	bsub -J 'build arc-elf32-gas' -n 1,8 -R "span[hosts=1]" -K -q normal -o build-elf32-gas.log "cd ${TRUNKDIR}/binutils/build32 && make -j -l 8 all-gas" && echo success && touch $@

stamp-elf32-ld: stamp-elf32-opcodes
	bsub -J 'build arc-elf32-ld' -n 1,8 -R "span[hosts=1]" -K -q normal -o build-elf32-ld.log "cd ${TRUNKDIR}/binutils/build32 && make -j -l 8 all-ld" && echo success && touch $@

stamp-elf32-gcc: stamp-elf32-tools-install
	bsub -J 'build arc-elf32-gcc' -n 1,8 -R "span[hosts=1]" -K -q normal -o build-elf32-gcc.log "eval ${ELF32_TOOLNAMES}; cd ${TRUNKDIR}/gcc && rm -rf build32 && mkdir build32 && cd build32 && ../src/configure --target=arc-elf32 --prefix=${INSTALLDIR}/elf32 --with-headers --enable-multilib --with-newlib --enable-languages=c,c++ && make -j && make -j -l 8 install" && echo success && touch $@

# ??? There seems to be a make file deficiency that foils parallel make.
stamp-elf32-insight: stamp-elf32-opcodes
	bsub -J 'build arc-elf32-insight' -n 1,1 -R "span[hosts=1]" -K -q normal -o build-elf32-insight.log "unset MAKE;cd ${TRUNKDIR}/insight && rm -rf build32 && mkdir build32 && cd build32 && ../src/configure --target=arc-elf32 --prefix=${INSTALLDIR}/elf32 --disable-werror && make && make install" && echo success && touch $@

#configure uclibc toolchain and build host-libiberty, bfd and opcodes.
stamp-uclibc-opcodes:
	bsub -J 'build arc-linux-uclibc opcodes' -n 1,8 -R "span[hosts=1]" -K -q normal -o build-uclibc-opcodes.log "cd ${TRUNKDIR}/binutils && rm -rf build && mkdir build && cd build && ../src/configure --prefix=${INSTALLDIR}/uclibc --target=arc-linux-uclibc --disable-werror && make -j -l 8 all-libiberty all-bfd all-opcodes" && echo success && touch $@

stamp-uclibc-binutils: stamp-uclibc-opcodes
	bsub -J 'build arc-linux-uclibc binutils' -n 1,8 -R "span[hosts=1]" -K -q normal -o build-uclibc-binutils.log "cd ${TRUNKDIR}/binutils/build && make -j -l 8 all-binutils" && echo success && touch $@

stamp-uclibc-gas: stamp-uclibc-opcodes
	bsub -J 'build arc-linux-uclibc-gas' -n 1,8 -R "span[hosts=1]" -K -q normal -o build-uclibc-gas.log "cd ${TRUNKDIR}/binutils/build && make -j -l 8 all-gas" && echo success && touch $@

stamp-uclibc-ld: stamp-uclibc-opcodes
	bsub -J 'build arc-linux-uclibc-ld' -n 1,8 -R "span[hosts=1]" -K -q normal -o build-uclibc-ld.log "cd ${TRUNKDIR}/binutils/build && make -j -l 8 all-ld" && echo success && touch $@

stamp-uclibc-gprof: stamp-uclibc-opcodes
	bsub -J 'build arc-linux-uclibc-gprof' -n 1,8 -R "span[hosts=1]" -K -q normal -o build-uclibc-ld.log "cd ${TRUNKDIR}/binutils/build && make -j -l 8 all-gprof" && echo success && touch $@

# copy bootstrap include files, copy required Linux headers,
# then build & install gcc.
stamp-uclibc-gcc: stamp-uclibc-tools-install
	bsub -J 'build arc-linux-uclibc-gcc' -n 1,8 -R "span[hosts=1]" -K -q normal -o build-uclibc-gcc.log "eval ${UCLIBC_TOOLNAMES}; \
	  cp -rf bootstrap ${INSTALLDIR}/uclibc/arc-linux-uclibc/include && \
	  cp -r ${LINUXDIR}/include/linux ${INSTALLDIR}/uclibc/arc-linux-uclibc/include && \
	  cp -r ${LINUXDIR}/include/asm-arc ${INSTALLDIR}/uclibc/arc-linux-uclibc/include/asm && \
	  cd ${TRUNKDIR}/gcc && rm -rf build && mkdir build && cd build && \
	  ../src/configure --target=arc-linux-uclibc --prefix=${INSTALLDIR}/uclibc --with-headers=${INSTALLDIR}/uclibc/arc-linux-uclibc/include --enable-shared --disable-multilib --without-newlib --enable-languages=c,c++ --with-cpu=arc700 --enable-c99 && \
	  make -j -l 8 all-gcc && echo built successfully && \
	  make -j -l 8 install-gcc" && echo success && touch $@

# build uclibc, copy dynamic linker, and make necessary soft links.
stamp-uclibc-libc: stamp-uclibc-gcc
	bsub -J 'build arc-linux-uclibc libc' -n 1,8 -R "span[hosts=1]" -K -q normal -o build-uclibc-libc.log \
	 "eval ${UCLIBC_TOOLNAMES}; \
	  cd ${TRUNKDIR}/uClibc-0.9.29 && \
	  sed -e "s#%LINUX%#${LINUXDIR}#" -e "s#%INSTALL%#${INSTALLDIR}/uclibc#"< arc_config > .config && \
	  make -j -l 8 CROSS=arc-linux-uclibc- && \
	  make -j -l 8 install && \
	  cp lib/ld-uClibc.so* ${INSTALLDIR}/uclibc/arc-linux-uclibc/lib && \
	  cd ${INSTALLDIR}/uclibc/arc-linux-uclibc/lib && \
	  ln -fs libm.so libm.so.0 && \
	  ln -fs libdl.so libdl.so.0 && \
	  ln -fs libgcc_s.so libgcc_s.so.0" && echo success && touch $@

# build libstdc++-v3 and copy it to its proper arc-linux installation place
stamp-uclibc-libstdcxx: stamp-uclibc-libc
	bsub -J 'build arc-linux-uclibc-libstdc++-v3' -n 1,8 -R "span[hosts=1]" -K -q normal -o build-uclibc-libstdc++-v3.log \
	 "eval ${UCLIBC_TOOLNAMES} ; \
	  cd ${TRUNKDIR}/gcc/build && \
	  make -j -l 8 all-target-libstdc++-v3 && \
	  make -j -l 8 install-target-libstdc++-v3 && \
	  cp ${INSTALLDIR}/uclibc/lib/libstdc++.so* ${INSTALLDIR}/uclibc/arc-linux-uclibc/lib && \
	  cp ${INSTALLDIR}/uclibc/lib/libstdc++.a ${INSTALLDIR}/uclibc/arc-linux-uclibc/lib && \
	  cp -r ${INSTALLDIR}/uclibc/include/c++ ${INSTALLDIR}/uclibc/arc-linux-uclibc/include" && echo success && touch $@

# ??? There seems to be a make file deficiency that foils parallel make.
stamp-uclibc-insight: stamp-uclibc-opcodes
	bsub -J 'build arc-linux-uclibc-insight/gdb' -n 1,1 -R "span[hosts=1]" -K -q normal -o build-uclibc-insight.log "unset MAKE; cd ${TRUNKDIR}/insight && rm -rf build && mkdir build && cd build && ../src/configure --target=arc-linux-uclibc --prefix=${INSTALLDIR}/uclibc --disable-werror && make && make install" && echo success && touch $@

stamp-uclibc-gdbserver: stamp-uclibc-insight stamp-uclibc-libc
	bsub -J 'build arc-linux-uclibc gdbserver' -n 1,8 -R "span[hosts=1]" -K -q normal -o build-uclibc-gdbserver.log "cd ${TRUNKDIR}/insight/src/gdb/gdbserver/build && ./build_gdbserver.sh && cp gdbserver ${INSTALLDIR}/uclibc/bin" && echo success && touch $@
