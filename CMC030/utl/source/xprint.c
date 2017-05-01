/*++
 * xprint.c
 *
 * Description
 *
 *	This program will print a text file to the printer
 *	port of a VT100 compatible terminal, with the ability
 *	to append an escape sequence to set an HP printer into
 *	compressed printing mode.
 *
 *
 * Notes
 *
 *	You must have already done a 'SET TERM/NOBROADCAST/NOWRAP/FORM/TAB'
 *	before using this program, otherwise it may not print correctly.
 *
 *	The compressed printing code assume that there is an HP PCL compatible
 *	printer attached to the terminal.
 *
 *	This program is designed to work with TEXT files only. Don't expect
 *	binary files to do the right thing.
 *
 * History
 *
 *	08/09/2001 - Kevin Handy
 *		Based on a DCL command procedure, and modified
 *		so that it did not attach a new line after the
 *		escape sequence used to switch the printer into
 *		compressed print mode.
 *--
 */

/*
 * Standard include files
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
 * Static functions
 */
static void print_file(char* filename);

/*
 * Static constants
 */
static char* printer_on = "\033[5i";	/* Turn on the printer port */
static char* printer_off = "\033[4i";	/* Turn off the printer port */
static char* compress =			/* Set printer to compressed print */
	"\033CB\033(8U\033(s0p16.66h10v0s0b0T\033&l6D\033&l0O";


/*
 * Static variables
 */
static int add_compress = 0;	/* Do we insert code to compress the printing */

/*
 * Here we go
 */
int main(int argc, char* argv[])
{
	int ap;

	/*
	 * Handle command line arguements
	 */
	for (ap = 1; ap < argc; ap++)
	{
fprintf(stderr, "Option: %s\n", argv[ap]);
		/*
		 * Turn on compress header?
		 */
		if (strcmp(argv[ap], "-c") == 0 || strcmp(argv[ap], "-C") == 0)
		{
			add_compress = -1;
		}
		/*
		 * Must be a file name
		 */
		else
		{
			print_file(argv[ap]);
		}
	}

	return EXIT_SUCCESS;
}

static void print_file(char* filename)
{
	FILE *fp;		/* Source file open channel */
	char buffer[512];

	fprintf(stderr, "Printing: %s\n", filename);

	fp = fopen(filename, "r");
	if (fp == 0)
	{
		fprintf(stderr, "Unable to open file %s.\n", filename);
		return;
	}

	/*
	 * Turn on printer port
	 */
	fputs(printer_on, stdout);

	/*
	 * Set up compressed printing is requested
	 */
	if (add_compress)
	{
		fputs(compress, stdout);
	}

	/*
	 * Copy the [text] file to the standard output
	 */
	while(fgets(buffer, sizeof(buffer), fp))
	{
		fputs(buffer, stdout);
	}

	/*
	 * Turn off printer port
	 */
	fputs(printer_off, stdout);

	/*
	 * Clean up used resources
	 */
	fclose(fp);
}

