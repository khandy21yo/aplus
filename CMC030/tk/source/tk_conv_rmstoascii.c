/*
 *	%TITLE "tk_conv_rmstoascii - Convert RMS files to fixes ASCII files"
 */
#pragma module tk_conv_rmstoascii "V3.6 Calico"

/*
 * COPYRIGHT (C) 1992 BY
 * Computer Management Center
 *
 * Idaho Falls, Idaho.
 *
 * This software is furnished under a license and may be used and
 * copied only in accordance with terms of such license and with
 * the inclusion of the above copyright notice.  This software or
 * any other copies therof may not be provided or otherwise made
 * available to any other person.  No title to and ownership of
 * the software is hereby transferred.
 *
 * The information in this software is subject to change without
 * notice and should not be construed as a committment by
 * Computer management Center.
 *
 * CMC assumes no responsibility for the use or reliability of
 * its software on equipment which is not supported by CMC.
 *
 *++
 *
 * ABSTRACT:
 *
 *	This program converts RMS data files to ASCII format.
 *
 * Index:
 *
 * COMPILE:
 *
 *	$ CC TK_SOURCE:TK_CONV_RMSTOASCII/G_FLOAT
 *	$ LINK/EXE=TK_EXE: TK_CONV_RMSTOASCII, FUNC_LIB:CMCLINK/OPT
 *	$ DELETE TK_CONV_RMSTOASCII.OBJ;*
 *
 * AUTHOR:
 *
 *	07/16/92 - Kevin Handy
 *
 * MODIFICATION HISTORY:
 *
 *	04/17/95 - Kevin Handy
 *		(V3.6)
 *		Update to V3.6 Calico coding standards.
 *
 *	06/01/99 - Kevin Handy
 *		Modify to compile with DEC-C
 *		(module, stdlib)
 *
 *	06/09/99 - Kevin Handy
 *		Include string.h, sys$routines.h
 *
 *	06/11/99 - Kevin Handy
 *		Lose sys$routines (not available in DEC-C),
 *		but added a bunch of prototypes and STR$ROUTINES.H
 *
 *	12/27/99 - Kevin Handy
 *		Fix a lot of warning errors.
 *--
 */

/*
 * Include "C" header files
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <descrip.h>
#include <rms.h>
#include <str$routines.h>
#include <starlet.h>

/*
 * Create RMS structures
*/
struct FAB f_fab;		/* FAB */
struct XABSUM xabsum;		/* XAB */
struct RAB f_rab;		/* RAB */

union xabxxx
{
	struct XABKEY xabkey;
	struct XABSUM xabsum;
};

/*
 * Common variables
 */
char filename[64];
char outputname[64];
FILE *outputch;
char formatname[64];
FILE *formatch;

/*
 * Prototypes
 */
int getxstr(FILE *file, char *string);
int getxnum(FILE *file, int *number);
int getxret(FILE *file);
#ifdef COMP
int match_wild(char *pattern, char *candidate, int len);
#endif

/*
 * Field definition structure
 */
struct fieldnamestruct
{
	char name[32];
	char type[32];
	int start;
	int length;
	char fmt[32];
	char comp[64];
} fieldname[128];
int fieldcount = 0;


/*
 * External VMS functions used (not defined in headers)
 */
extern long STR$_MATCH;

/*
 * Prototypes of internal functions
 */
void dump_fab(struct FAB fab);
void dump_xab(union xabxxx xab);
struct XABKEY *allockey(int keynum);
void zprint(char *buffer);
void z1print(char *buffer, char type, char *fmt);

/*******************************************************************
 * Main program starts here
 *******************************************************************/

main()
{
	int rms_status;
	int keycount;
	int loop;
	int base;
	int flen;
	char *c1,*c2;
	struct XABKEY *xabkey;
	char buffer[2048];

	FILE *deffile;

	/*******************************************************************
	 * Get format file
	 *******************************************************************/
	printf("Filename: ");
	scanf("%s", filename);

	if ((deffile = fopen(filename, "r")) == 0)
	{
		printf("Unable to open def file\n");
		exit(1);
	}

	/*
	 * Load in data file name
	 */
	fscanf(deffile, "%s\n", filename);

	/*
	 * Load in field definitions
	 */
	do
	{
		rms_status = getxstr(deffile,
			fieldname[fieldcount].name);
		rms_status = getxstr(deffile,
			fieldname[fieldcount].type);
		rms_status = getxnum(deffile,
			&(fieldname[fieldcount].start));
		rms_status = getxnum(deffile,
			&(fieldname[fieldcount].length));
		rms_status = getxstr(deffile,
			fieldname[fieldcount].fmt);
		rms_status = getxstr(deffile,
			fieldname[fieldcount].comp);
		getxret(deffile);

		/*
		 * Strip out tabs, nulls.
		 */
		c1 = c2 = fieldname[fieldcount].comp;
		while (*c1)
		{
			if ((*c1 != '\t') && (*c1 != '\n'))
			{
				*c2++ = *c1;
			}
			c1++;
		}
		*c2++ = '\0';

#if 0
		if (rms_status != EOF)
		{
			printf("%s,%s,%d,%d,%s,%s\n",
				fieldname[fieldcount].name,
				fieldname[fieldcount].type,
				fieldname[fieldcount].start,
				fieldname[fieldcount].length,
				fieldname[fieldcount].fmt,
				fieldname[fieldcount].comp);
		}
#endif
		if (rms_status != EOF)
		{
			fieldcount++;
		}
	} while (rms_status != EOF);

	/*******************************************************************
	 * Open up data file
	 *******************************************************************/

	f_fab = cc$rms_fab;
	f_fab.fab$b_fac = FAB$M_GET;
	f_fab.fab$b_org = FAB$C_IDX;
	f_fab.fab$b_shr = FAB$M_SHRUPD;
	f_fab.fab$l_fna = filename;
	f_fab.fab$b_fns = strlen(filename);
	f_fab.fab$l_xab = &xabsum;

	xabsum = cc$rms_xabsum;

	rms_status = sys$open(&f_fab);
	if (rms_status != RMS$_NORMAL)
	{
		printf("RMS Open failure %d\n", rms_status);
		exit(rms_status);
	}

	keycount = xabsum.xab$b_nok;

	rms_status = sys$close(&f_fab);
	if (rms_status != RMS$_NORMAL)
	{
		printf("RMS Close failure %d\n", rms_status);
		exit(rms_status);
	}

	xabkey = xabsum.xab$l_nxt = allockey(0);

	for (loop = 1; loop < keycount; loop++)
	{
		xabkey->xab$l_nxt = allockey(loop);
		xabkey = xabkey->xab$l_nxt;
	}

	rms_status = sys$open(&f_fab);
	if (rms_status != RMS$_NORMAL)
	{
		printf("RMS Open failure (2) %d\n", rms_status);
		exit(rms_status);
	}

	/*
	 * Set up RAB
	 */
	f_rab = cc$rms_rab;
	f_rab.rab$l_fab = &f_fab;

	/*
	 * Try to connect the file
	 */
	rms_status = sys$connect(&f_rab);

	if (rms_status != RMS$_NORMAL)
	{
		printf("RMS connect error %ld %lx\n", rms_status, rms_status);
		exit(1);
	}

	/*******************************************************************
	 * Layout file
	 *******************************************************************/
#ifdef LAYOUT
	printf("Output Format Filename: ");
	scanf("%s", formatname);

	if ((formatch = fopen(formatname, "w")) == 0)
	{
		printf("Unable to open def file\n");
		exit(1);
	}

	base = 1;
	fprintf(formatch,"%12s %6s %-6s %-6s %-6s %-32s\n",
		"Name", "Type", "Start", "length", "Format", "Comp");
	fprintf(formatch,"%12s %6s %-6s %-6s %-6s %-32s\n",
		"-----------","------","------","------", "------",
		"--------------------------------");

	for (loop=0; loop<fieldcount; loop++)
	{
		flen = atoi(fieldname[loop].fmt);

		fprintf(formatch,"%12s %6s %6d %6d %-6s %s\n",
			fieldname[loop].name,
			fieldname[loop].type,
			base,
			flen,
			fieldname[loop].fmt,
			fieldname[loop].comp);

		base += flen;
	}
	fclose(formatch);
#endif

	/*******************************************************************
	 * Output file name
	 *******************************************************************/
	printf("Output Data Filename: ");
	scanf("%s", outputname);

	if ((outputch = fopen(outputname, "w")) == 0)
	{
		printf("Unable to open def file\n");
		exit(1);
	}

	/*******************************************************************
	 * Search for first record
	 *******************************************************************/
	f_rab.rab$b_krf = 0;		/* Key # to go by */

	rms_status = sys$rewind(&f_rab);

	if (rms_status != RMS$_NORMAL)
	{
		printf("RMS rewind error %ld %lx\n", rms_status, rms_status);
		exit(1);
	}

	/*
	 * Examine all records in file
	 */
	f_rab.rab$b_rac = RAB$C_SEQ;	/* Go sequentially */
	f_rab.rab$l_ubf = buffer;	/* Where to store data */
	f_rab.rab$w_usz = sizeof(buffer);
						/* Size of record */

	rms_status = RMS$_NORMAL;

	while (rms_status == RMS$_NORMAL)
	{
		rms_status = sys$get(&f_rab);

		if (rms_status == RMS$_NORMAL)
		{
			/*
			 * Write out record
			 */
			zprint(buffer);
		}
	}

	printf("Final loop status = %ld %lx\n", rms_status, rms_status);

	/*
	 * Try to connect the file
	 */
	fclose(outputch);
	rms_status = sys$close(&f_fab);

	if (rms_status != RMS$_NORMAL)
	{
		printf("RMS close error %ld %lx\n", rms_status, rms_status);
		exit(1);
	}

}


/*******************************************************************
 * allockey(keynum) - Allocate XAB for a key, and start filling
 * in XAB so open can complete fillibng it in
 *******************************************************************/
struct XABKEY *allockey(int keynum)
{
	struct XABKEY *tempxab;

	tempxab = malloc(sizeof *tempxab);

	*tempxab = cc$rms_xabkey;
	tempxab->xab$b_ref = keynum;

	return tempxab;
}

/*******************************************************************
 * zprint(buffer) - Print out one complete record to text file.
 *******************************************************************/

void zprint(char *buffer)
{
	int i;

	/*
	 * Scan wildcards
	 */
	for (i=0; i<fieldcount; i++)
	{
#ifdef COMP
		if (fieldname[i].comp[0] != '\0')
		{
			if (match_wild(fieldname[i].comp,
				buffer+fieldname[i].start,
				fieldname[i].length) == 0)
			{
				return;
			}
		}
#endif
	}
 
	/*
	 * Dump all fields
	 */
	for (i=0; i<fieldcount; i++)
	{
		if (i != 0)
		{
			fprintf(outputch, "\t");
		}

		z1print(buffer+fieldname[i].start, *fieldname[i].type,
			fieldname[i].fmt);
	}
	fprintf(outputch, "\n");
}

/*******************************************************************
 * z1print(buffer, type, fmt) - Print out one field to text file
 *******************************************************************/

void z1print(char *buffer, char type, char *fmt)
{
	/*
	 * For creating format strings
	 */
	size_t tlen;
	double *fptr;
	short int *wptr;
	char xfmt[128];
	int i;

	switch(type)
	{
	/*
	 * G_FLOAT data type
	 */
	case 'G':
		sprintf(xfmt, "%%0%sf", fmt);
		fptr = (double *)buffer;
		fprintf(outputch, xfmt, *fptr);
		break;

	/*
	 * Word data type
	 */
	case 'W':
		sprintf(xfmt, "%%0%sd", fmt);
		wptr = (short int *)buffer;
		fprintf(outputch, xfmt, *wptr);
		break;

	/*
	 * Assume text
	 */
	default:
		tlen = atoi(fmt);
		for (i = 0; i<tlen; i++)
		{
			if (buffer[i] == '\0')
				buffer[i] = ' ';
		}
		strncpy(xfmt, buffer,tlen);
		xfmt[tlen] = '\0';
		fprintf(outputch, "%s",xfmt);
		break;
	}
}

#ifdef COMP
/*******************************************************************
 * match_wild(pattern, candidate, length) - Do a wild card compare
 * between the candidate and a wild-card pattern.
 *******************************************************************/

int match_wild(char *pattern, char *candidate, int len)
{
	struct dsc$descriptor_s dcan;
	struct dsc$descriptor_s dpat;
	long test;

	/*
	 * Convert candidate to a descriptor
	 */
	dcan.dsc$a_pointer = candidate;
	dcan.dsc$w_length = len;
	dcan.dsc$b_class = DSC$K_CLASS_S;
	dcan.dsc$b_dtype = DSC$K_DTYPE_T;

	/*
	 * Convert pattern to a descriptor
	 */
	dpat.dsc$a_pointer = pattern;
	dpat.dsc$w_length = strlen(dpat.dsc$a_pointer);
	dpat.dsc$b_class = DSC$K_CLASS_S;
	dpat.dsc$b_dtype = DSC$K_DTYPE_T;

	/*
	 * Do comparison
	 */
	test = (str$match_wild(&dcan, &dpat) == 2393113);
	return(test);
}
#endif

int getxstr(FILE *file, char *string)
{
	int inchar;

	while ((inchar = getc(file)) != EOF)
	{
		switch(inchar)
		{
		case ' ':
		case '\t':
			*string = '\0';
			return(0);

		case '\n':
			ungetc(inchar, file);
			*string = '\0';
			return(0);

		default:
			*string = inchar;
			string++;
			break;
		}
	}

	return(EOF);
}

int getxret(FILE *file)
{
	int inchar = 0;

	while ((inchar = getc(file)) != EOF)
	{
		if (inchar == '\n')
		{
			return(0);
		}
	}

	return(EOF);
}

int getxnum(FILE *file, int *number)
{
	char string[32];
	int result;

	result = getxstr(file, string);
	*number = atoi(string);

	return(result);
}
