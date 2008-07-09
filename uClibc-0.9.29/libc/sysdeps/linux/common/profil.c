#include "syscalls.h"
#include <unistd.h>
_syscall4 (int, profil, u_short *,buf, size_t, bufsiz, size_t, offset, u_int, scale);
