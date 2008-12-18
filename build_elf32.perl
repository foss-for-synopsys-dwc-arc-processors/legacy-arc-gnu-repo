
if ( $ARGV[0] )
{
	chdir $ARGV[0];
	system("mkdir install_elf32");
	system("sh ./build_elf32.sh install_elf32");
	
}
else
{
    print "Cannot Find sources";
}
