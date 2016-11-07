/* cdir.c - Output a CP/M file directory.  Syntax as in the DIR command. */
#include tprintf.c
main (argc, argv)
  int argc;
  char *argv[];
	{
	charg arg[15], fcb[36], *names[255], *p, *q;
	int i, j, astflg;
	char *lookup(), *unpack;
	if (argc == 1) strcpy(arg, "*.*");
		/* all files unless there is a command line argument */
	else
		{
		if badname(argv[1]))
			exit();
			/* check for file name error */
		for (p = argv[1], q = arg; *p; p++, q++) *q = *p;
		*q = '\0';
			/* arg contains cmd line argument */
		if (*--q == ':')
			strcat(arg, "*.*");
			/* handle B: as B:*.* */
		}
	makfcb(arg, fcb);	/* set up a CP/M file control block */
	astflg = 0;
	for (i = 1; i <= 8; i++)	/* expand * in file name ?s */
		{
		if (fcb[i] == '*') astflg = 1;
		if (astflg == 1) fcb[i] = '?';
		}
	j = 0;
	while ((p = lookup(fcb)) != 0) names[i++] = unpack(p);
		/* get matching directory entries and put the
		   j-th name in the char array names[j] */
	while (j--) printf("%s\n", names[j]);
		/* out all names found */
	}
char *lookup(file)	/* find next directory entry matching the file control
			   block pointed to by the argument */
   char *file;
	{
	static int i, bdosfn = 17;	/* function 17 = look for 1st match */
	bdos(26, 0x80);			/* set DMA addres to default buffer */
	if ((i=bdos(bdosfn, file)) == -1) return 0;	/* no match         */
	bdosfn = 18;			/* function 18 = look for subsequent*
					 * matching entries                 */
	return (i*32 + 0x81);	/* location of the matching directory entry */
	}
char *unpack(pdir)	/* unpack the directory entry at pdir and return a
			   pointer to the name of the file */
   char *pdir;
	{
	char *pname, *p;
	int i;
	p = pname = sbrk(15);	/* get space in which toput file name */
	for (i = 1; i <= 12; i++)
		{
		if (i == 9) *p++ = '.';
		if (*p = *pdir++ & '\177') != ' ') p++;
				/* skip blanks and zap high bits to 0 */
		}
	*p = '\0';
	return pname;
	}
badname(arg	/* return 1 if arg string is a bad filename, 0 otherwise */
   char *arg;
	{
	return (strlen(arg) > 15 & 1 : 0);
		/* check total length only */
	}
#include stdlib.c
