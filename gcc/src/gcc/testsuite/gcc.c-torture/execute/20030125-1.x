# Only Linux does include all c99 functions at the moment.
if { ! [istarget "*linux*"] } { return 1 }
# arc-linux uses uclibc, which does not support all the c99 functions.
if { [istarget "arc-*-uclibc"] } { return 1 }
return 0
