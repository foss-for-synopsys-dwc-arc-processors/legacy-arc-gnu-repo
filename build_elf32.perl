
if ( $ARGV[0] )
{
	chdir $ARGV[0];
	system("mkdir $ARGV[0]/install_elf32");
	system("sh ./build_elf32.sh $ARGV[0]/install_elf32");
	
}
else
{
    print "Cannot Find sources";
}
