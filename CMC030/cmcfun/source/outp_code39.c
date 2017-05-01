/*
 *	%TITLE "Generate escape sequence to print out a barcode"
 */
#pragma module outp_code39 "V3.6 Calico"

/*
 *
 *	COPYRIGHT (C) 1997 BY
 *	Software Solutions, Inc.
 *	Idaho Falls, Idaho.
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
 * Abstract:HELP
 *	.p
 *	This function is used to generate bar codes using the
 *	code39 symbology.
 *
 * Parameters:
 *
 *	MESSAGE$
 *		The data to be bar-coded
 *
 *	Type
 *		The type of printer to generate for
 *
 *	Return Value
 *		Escape sequence for the barcode, and stay on same line.
 *
 * Compile
 *
 *	$ cc FUNC_SOURCE:outp_code39/G_FLOAT
 *	$ LIB FUNC_LIB:CMCFUN/REP outp_code39
 *	$ DELETE outp_code39.OBJ;*
 *
 * Author:
 *
 *	09/24/97 - Kevin Handy
 *
 * Modification history:
 *
 *	10/13/1997 - Kevin Handy
 *		Define code tables using '01' instead of 'bBwW'.
 *		This will make it easier to handle other codes.
 *
 *	10/13/1997 - Kevin Handy
 *		Several changes (internal variable/function names)
 *		in order to make it easier to handle more printer
 *		types.
 *
 *	10/13/1997  Kevin Handy
 *		Rewrote fnencode39 function to make use of strchr
 *		function instead of an internal loop.
 *
 *	11/11/1997 - Kevin Handy
 *		Include 'function.h'
 *		Use definitions instead of hard coded numbers for
 *		comparison to PRINTTO.
 *		Lose commented out code.
 *
 *	05/27/99 - Kevin Handy
 *		Modify so will compile in DEC-C without errors
 *		(module, str$routines.h)
 *--
 */

/*
 * Standard include files
 */
#include <stdlib.h>
#include <string.h>
#include <descrip.h>
#include <str$routines.h>

/*
 * Local include files
 */
#include "func_include:function.h"

/*
 * Local Function Prototypes
 */
static char* fnencode39(const char* message, char* Result);
static void fnenCode39Bar_epson(const char* message, char* Buffer, long* Length);
static void fnenCode39Bar_hp(const char* message, char* Buffer, long* Length);

/*
 * Code39 tables
 */
static const char Code39Char[] =
	"*0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ-. $/+%";

static const char* Code39Bar[] =
{
	"1211212111","1112212111","2112111121","1122111121",	/* *012 */
	"2122111111","1112211121","2112211111","1122211111",	/* 3456 */
	"1112112121","2112112111","1122112111","2111121121",	/* 789A */
	"1121121121","2121121111","1111221121","2111221111",	/* BCDE */
	"1121221111","1111122121","2111122111","1121122111",	/* FGHI */
	"1111222111","2111111221","1121111221","2121111211",	/* JKLM */
	"1111211221","2111211211","1121211211","1111112221",	/* NOPQ */
	"2111112211","1121112211","1111212211","2211111121",	/* RSTU */
	"1221111121","2221111111","1211211121","2211211111",	/* VWXY */
	"1221211111","1211112121","2211112111","1221112111",	/* Z-.  */
	"1212121111","1212111211","1211121211","1112121211"	/* $/+% */
};

/*
 * Bars used for epson printers
 */
static const char b1line[] = {0xff,0xff};
static const char b2line[] = {0xff,0xff,0xff,0xff,0xff};
static const char b3line[] = {0xff,0xff,0xff,0xff,0xff,0xff,0xff};
static const char b4line[] = {0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff,0xff};
static const char w1line[] = {0x00, 0x00};
static const char w2line[] = {0x00,0x00,0x00,0x00,0x00};
static const char w3line[] = {0x00,0x00,0x00,0x00,0x00,0x00,0x00};
static const char w4line[] = {0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00};

/*
 * fnencode39
 *
 *	This function takes a text string and converts it into a
 *	string coded as '0110' for wide/narrow bars using the
 *	code39 symbology.
 *
 *	Any undefined characters will be ignored
 */
static char* fnencode39(const char* message, char* Result)
{
	char* ch;
	char* ChCode;
	int i;

	Result[0] = '\0';
	for (i = 0; i < strlen(message); i++)
	{
		ChCode = strchr(Code39Char, message[i]);
		if (ChCode != NULL)
		{
			strcat(Result, Code39Bar[ChCode - Code39Char]);
		}
	}
	return Result;
}

/*
 * fnenCode39Bar_epson
 *
 *	This function takes the barcodes coded as a '0110' string,
 *	and generates the necessary escape sequence to output a bar code
 *	to an epson style 9-pin dot matrix printer.
 */
static void fnenCode39Bar_epson(const char* message, char* Buffer, long* Length)
{
	int i;
	*Length = 4;

	Buffer[0] = 27;
	Buffer[1] = 'L';

	for (i = 0; i < strlen(message); i+=2)
	{
		/*
		 * Black Bar
		 */
		switch (message[i])
		{
		case '1':

			memcpy(Buffer + *Length, b1line, sizeof(b1line));
			*Length += sizeof(b1line);
			break;

		case '2':

			memcpy(Buffer + *Length, b2line, sizeof(b2line));
			*Length += sizeof(b2line);
			break;

		case '3':

			memcpy(Buffer + *Length, b3line, sizeof(b3line));
			*Length += sizeof(b3line);
			break;

		case '4':

			memcpy(Buffer + *Length, b4line, sizeof(b4line));
			*Length += sizeof(b4line);
			break;
		}

		switch (message[i + 1])
		{
		case '1':

			memcpy(Buffer + *Length, w1line, sizeof(w1line));
			*Length += sizeof(w1line);
			break;

		case '2':

			memcpy(Buffer + *Length, w2line, sizeof(w2line));
			*Length += sizeof(w2line);
			break;

		case '3':

			memcpy(Buffer + *Length, w3line, sizeof(w3line));
			*Length += sizeof(w3line);
			break;

		case '4':

			memcpy(Buffer + *Length, w4line, sizeof(w4line));
			*Length += sizeof(w4line);
			break;

		}
	}
	Buffer[2] = (*Length - 4) & 255;
	Buffer[3] = ((*Length - 4) >> 8) & 255;
}

/*
 * fnenCode39Bar_hp
 *
 *	This function takes the barcodes coded as a '0110' string,
 *	and generates the necessary escape sequence to output a bar code
 *	to a Hewlett Packard Laserjet II compatible printer.
 */
static void fnenCode39Bar_hp(const char* message, char* Buffer, long* Length)
{
	int i;			/* Loop variable */
	int BarWidth;		/* Width of a bar */

	/*
	 * Hack up character spacing (Assumes were using the
	 * standard character set)
	 */
	strcpy(Buffer, "\033&k1H");
	*Length = strlen(Buffer);

	/*
	 * Plop down bars and spaces
	 */
	for (i = 0; i < strlen(message); i += 2)
	{
		/*
		 * Black Bar
		 */
		BarWidth = (message[i] - '0') * 2;
		while(BarWidth--)
		{
			Buffer[*Length] = '|';
			(*Length)++;
		}

		/*
		 * White Bar
		 */
		BarWidth = (message[i + 1] - '0') * 2;
		while(BarWidth--)
		{
			Buffer[*Length] = ' ';
			(*Length)++;
		}
	}

	/*
	 * reset the spacing (Assumes were at 10 CPI)
	 */
	strcpy(Buffer + *Length, "\033&k10H");
	*Length = strlen(Buffer);
}

/*
 * outp_code39
 *
 *	Here's the main function. It translates from a descriptor into
 *	a generic C string, then processes the barcode using C based
 *	functions, then translates the funal barcode back to a
 *	descriptor.
 *
 *	The type field passed in the destination, so that we don't
 *	try to send bar codes out to the display, or to a printer
 *	port because neither case works.
 *
 */
void outp_code39(struct dsc$descriptor_s *returnstr,
	struct dsc$descriptor_s *source, long* Type, long* PrintTo)
{
	char message[64];		/* Original message */
	char Work1[512];		/* bwBW encoding */
	char Buffer[1024];		/* Bar code escape sequence */
	long Length = 0;		/* Length of resulting string */
	char* Tp1;			/* Test Point 1 */

	switch(*PrintTo)
	{
	case 2:
		/*
		 * HP Laserjet II Type Printer
		 */
		switch(*Type)
		{
		case OUTP_TODISPLAY:
			/*
			 * We can't send this barcode to the display
			 */
			strcpy(Buffer, "*BARCODE*");
			Length = strlen(Buffer);
			break;

		default:
			message[0] = '*';
			strncpy(message + 1, source->dsc$a_pointer,
				source->dsc$w_length);
			message[source->dsc$w_length + 1] = '*';
			message[source->dsc$w_length + 2] = '\0';
			Tp1 = fnencode39(message, Work1);
			fnenCode39Bar_hp(Tp1, Buffer, &Length);
			break;
		}
		break;

	default:
		/*
		 * Epson type printer
		 */
		switch(*Type)
		{
		case OUTP_TODISPLAY:
		case OUTP_TOLOCAL:
			/*
			 * We can't send this barcode through a printer
			 * Port or to the display
			 */
			strcpy(Buffer, "*BARCODE*");
			Length = strlen(Buffer);
			break;

		default:
			message[0] = '*';
			strncpy(message + 1, source->dsc$a_pointer,
				source->dsc$w_length);
			message[source->dsc$w_length + 1] = '*';
			message[source->dsc$w_length + 2] = '\0';
			Tp1 = fnencode39(message, Work1);
			fnenCode39Bar_epson(Tp1, Buffer, &Length);
			break;
		}
	}

	str$copy_r(returnstr, &Length, &Buffer);
}

