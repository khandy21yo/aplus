/*	%TITLE "Function to Enter an Option"
 */
#pragma module entr_3option "V3.6 Calico"

/*
 *
 * COPYRIGHT (C) 1986 BY
 * Computer Management Center, Inc.
 * Idaho Falls, Idaho.
 *
 * This software is furnished under a license and may be used and
 * copied only in accordance with terms of such license and with
 * the inclusion of the above copyright notice.  This software or
 * any other copies thereof may not be provided or otherwise made
 * available to any other person.  No title to and ownership of
 * the software is hereby transferred.
 *
 * The information in this software is subject to change without
 * notice and should not be construed as a commitment by
 * Computer Management Center, Inc.
 *
 * CMC assumes no responsibility for the use or reliability of
 * its software on equipment which is not supported by CMC.
 *
 *++
 *
 * Abstract:HELP
 *
 *	OP_FLAGW flags
 *	.b
 *	.list 0,"*"
 *	.le
 *		1 - Wide menu (132 columns) (IGNORED - NOT NECESSARY)
 *	.le
 *		2 - Leave menu on (IGNORED - NOT NECESSARY)
 *	.le
 *		4 - Flag stating that the menu is already displayed
 *		    (IGNORED - NOT NECESSARY)
 *	.le
 *		8 - Append current menu option to end of PRG_ITEM
 *		instead of replacing it.
 *	.le
 *		16 - Do not change PRG_ITEM in selection.
 *	.end list
 *
 * Parameters:
 *
 *	OP_GROUP
 *		The passed string that tells what type of option it is.
 *
 *	OP_POSSIBLE
 *		The passed choice of options the user gets.
 *
 *	OP_CUROP
 *		The returned integer that holds the number of options and
 *		which option is currently being looked at.
 *
 *
 *	Returned value
 *		This function is used by the maintainence programs
 *		to enter an option.  It returns the option to
 *		the user through a variable.
 *
 * Example:
 *
 *	OPT$ = ENTR_3OPTION("COMMAND", "Blank Store eXit", 1%, 0%)
 *
 * Index:
 *
 *	.x Enter>Option
 *	.x Option>Enter
 *
 * AUTHOR:
 *
 *	09/01/85 - Kevin Handy
 *
 * Compile:
 *
 *	$ CC/G_FLOAT FUNC_SOURCE:ENTR_3OPTION
 *	$ LIB FUNC_LIB:CMC_3VECTOR/REP ENTR_3OPTION
 *	$ DELETE ENTR_3OPTION.OBJ;*
 *
 * MODIFICATION HISTORY:
 *
 *	12/30/85 - Cal Rasmussen
 *		Allow exit withouth hitting return.
 *
 *	02/03/87 - Kevin Handy
 *		Modified for SMG.
 *
 *	12/07/89 - Kevin Handy
 *		Made sharable version.
 *
 *	02/18/90 - Kevin Handy
 *		Modified to highlight the character used to select
 *		the options.
 *
 *	04/27/90 - Kevin Handy
 *		Fixed bug that caused it to lose spaces at end
 *		of an option if the last letter was capitolized.
 *		("" <> " " was false, replaced with not("" == " ") ).
 *
 *	04/28/90 - Frank F. Starman
 *		Replace ENTR_3MESSAGE function with HELP_34MESSAGE.
 *
 *	08/28/90 - Frank F. Starman
 *		Add flag 16.
 *
 *	03/14/92 - Kevin Handy
 *		Cleaned up vars (checkvar)
 *
 *	03/25/93 - Kevin Handy
 *		Clean up (Check)
 *
 *	03/30/93 - Frank F. Starman
 *		Allow to display more options on the first line.
 *
 *	04/05/93 - Kevin Handy
 *		Clean up (Check)
 *
 *	07/21/93 - Kevin Handy
 *		Converted to C.
 *
 *	11/02/93 - Kevin Handy
 *		Modified to match internals more to old basic version so that
 *		odd access from MAIN_WINDOW will work better.
 *
 *	04/17/95 - Kevin Handy
 *		(V3.6)
 *		Update to V3.6 Calico coding standards.
 *
 *	06/06/95 - Kevin Handy
 *		Increased size of menu options (MAX_NAME) from 16 to 20.
 *
 *	07/26/95 - Kevin Handy
 *		Modified displaying of individual items to reduce total
 *		code necessary. Was printing the left in normal, the
 *		command char in bold, then the right in normal. Now it
 *		prints entire command in normal, then the command
 *		character in bold.
 *
 *	07/26/95 - Kevin Handy
 *		Put in some more begin/end display update calls to
 *		inprove display slightly.
 *
 *	07/26/95 - Kevin Handy
 *		Modifications to lose several warning errors
 *
 *	05/27/99 - Kevin Handy
 *		Modify so will compile in DEC-C without errors
 *		(module, smg$routines.h, option_list_struct)
 *--
 */

/*
 * Include files
 */
#include <descrip.h>
#include <string.h>
#include <ctype.h>
#include <smg$routines.h>
#include <smgdef.h>
#include <str$routines.h>
#include "func_include:cmcfun.h"

/*
 * Local functions
 */
static void drawoption(struct scope_struct *scope,
	struct option_list_struct *option_list,
	int flag, int flag_byte);

/*
 * Declarations
 */
#define MAX_OPTIONS 64
#define MAX_NAME 20

/*
 * #defined functions
 */
#define min(x,y) ((x)<(y)?(x):(y))

/*
 * Local structures
 */
#if 0
struct option_list_struct
{
	unsigned char option;	/* Character to type to select */
	int optionptr;		/* Pointer to option character in name */
	char name[MAX_NAME];	/* Entire word for option */
	int curoppos;		/* Position in original string */
	int line;		/* Line option will display on */
	int column;		/* Column will display on */
};
#endif

/*
 * Local variables
 */
static $DESCRIPTOR(p1,"Invalid option here");
static $DESCRIPTOR(p2,"W");
static $DESCRIPTOR(p3,"ENTR_3OPTION");
static $DESCRIPTOR(p4,"");
static $DESCRIPTOR(p5,"INVOPT");
static $DESCRIPTOR(colon,":");

/*
 * Main function
 */
void entr_3option(struct dsc$descriptor *returnstr,
	struct scope_struct *scope,
	struct dsc$descriptor_s *op_group,
	struct dsc$descriptor_s *op_possible,
	long *op_curop,
	long *op_flagw)
{
	/*
	 * Local variables
	 */
	struct option_list_struct option_list[MAX_OPTIONS];
	int option_count;
	int nameptr;
	int chptr;
	long ch;
	long c_width;
	int usedline;
	int usedwidth;
	long length;
	int curop = 0;
	int prevop;
	int flag_byte;

	/*
	 * Decide from OP_FLAGW how to handle help messages
	 */
	if ((*op_flagw & 16) != 0)
	{
		/*
		 * Don't change PRG_ITEM ever.
		 */
		flag_byte = -1;
	}
	else
	{
		if ((*op_flagw & 8) != 0)
		{
			/*
			 * Append to current PRG_ITEM.
			 */
			flag_byte = sizeof(scope->prg_item);
			while ((flag_byte > 0) &&
				(scope->prg_item[flag_byte] == ' '))
			{
				/*
				 * Replace PRG_ITEM
				 */
				flag_byte--;
			}
		}
		else
		{
			flag_byte = 0;
		}
	}

	/*
	 * Split the list of options
	 */
	option_count = 0;
	nameptr = 0;
	option_list[0].option = '\0';
	option_list[0].name[0] = '\0';
	option_list[0].curoppos = 1;

	for (chptr = 0; chptr < op_possible->dsc$w_length; chptr++)
	{
		ch = *((char*)op_possible->dsc$a_pointer + chptr);

		/*
		 * Handle seperator characters
		 */
		if (ch == ' ')
		{
			if (nameptr != 0)
			{
				option_count++;
				nameptr = 0;
				option_list[option_count].option = '\0';
				option_list[option_count].name[0] = '\0';
				option_list[option_count].curoppos = chptr + 2;
			}
			continue;
		}

		/*
		 * Append to name
		 */
		option_list[option_count].name[nameptr++] = ch;
		option_list[option_count].name[nameptr] = '\0';

		/*
		 * If first upper case letter, make command char
		 */
		if (isupper(ch) && (option_list[option_count].option == '\0'))
		{
			option_list[option_count].option = ch;
			option_list[option_count].optionptr = nameptr;
		}

	}

	/*
	 * Finish up list
	 */
	if (nameptr != 0)
	{
		option_count++;
	}

	/*
	 * Calculate screen width
	 */
	smg$change_pbd_characteristics(&(scope->smg_pbid), 0l, &c_width);

	/*
	 * Allocate positions on screen for each option, and determine
	 * initial option.
	 */
	usedline = 1;
	usedwidth = op_group->dsc$w_length + 3;

	for (chptr = 0; chptr < option_count; chptr++)
	{
		/*
		 * Make current option?
		 */
		if (*op_curop >= option_list[chptr].curoppos)
		{
			curop = chptr;
		}

		/*
		 * Switch to next line?
		 */
		length = strlen(option_list[chptr].name);
		if (usedwidth + length > c_width)
		{
			usedline++;
			usedwidth = op_group->dsc$w_length + 3;
		}

		/*
		 * Allocate position
		 */
		option_list[chptr].line = usedline;
		option_list[chptr].column = usedwidth;

		/*
		 * Increment for next option
		 */
		usedwidth += length + 1;
	}


	/*
	 * Display initial list of options
	 */
	smg$begin_display_update(&scope->smg_option);

	/*
	 * Start with a blank display
	 */
	smg$erase_display(&scope->smg_option);

	/*
	 * Title for options
	 */
	smg$put_chars(
		&scope->smg_option,		/* Window */
		op_group,			/* Text */
		&1l,				/* Line */
		&1l,				/* Column */
		&0l,				/* Erase line */
		&0l,				/* Attributes */
		0l
	);

	length = op_group->dsc$w_length + 1;
	smg$put_chars(&scope->smg_option, &colon,
		&1l, &length, &0l, &0l, 0l);

	/*
	 * Initially Display Options
	 */
	for (chptr = 0; chptr < option_count; chptr++)
	{
		drawoption(scope, &option_list[chptr], curop == chptr, flag_byte);
	}

	smg$end_display_update(&scope->smg_option);

	scope->scope_exit = 0;

	/*
	 * Process keyboard input
	 */
	while (1)
	{
		/*
		 * Enter one character
		 */
		ch = entr_4entry(scope, scope->smg_option, 0);
		ch = entr_4specialkeys(scope, scope->smg_option, 0, ch);
		if ((ch < 127) && islower(ch))
		{
			ch = toupper(ch);
		}

		/*
		 * Handle as many cases here as possible
		 */
		switch(ch)
		{
		/*
		 * Null input
		 */
		case 0:
			continue;

		/*
		 * Right Arrow
		 */
		case SMG$K_TRM_RIGHT:
			smg$begin_display_update(&scope->smg_option);
			drawoption(scope, &option_list[curop], 0, flag_byte);
			curop++;
			if (curop >= option_count)
			{
				curop = 0;
			}
			drawoption(scope, &option_list[curop], 1, flag_byte);
			smg$end_display_update(&scope->smg_option);
			continue;

		/*
		 * Left arrow
		 */
		case SMG$K_TRM_LEFT:
			smg$begin_display_update(&scope->smg_option);
			drawoption(scope, &option_list[curop], 0, flag_byte);
			curop--;
			if (curop < 0)
			{
				curop = option_count - 1;
			}
			drawoption(scope, &option_list[curop], 1, flag_byte);
			smg$end_display_update(&scope->smg_option);
			continue;


		/*
		 * Space
		 */
		case ' ':
			continue;

		/*
		 * Other key can't handle with switch
		 */
		default:
			/*
			 * Function key
			 */
			if ((ch < 32) ||
				((ch >= 127) && (ch <= 159)) ||
				(ch > 254))
			{
				scope->scope_exit = ch;
				goto ExitHere;
			}

			/*
			 * Search for the character typed in list of
			 * valid options
			 */
			prevop = curop;
			curop = -1;
			for (chptr = 0; chptr < option_count; chptr++)
			{
				if (ch == option_list[chptr].option)
				{
					curop = chptr;
					break;
				}
			}

			/*
			 * If we have a valid option, switch out
			 */
			if (curop != -1)
			{
				if (curop != prevop)
				{
					smg$begin_display_update(&scope->smg_option);
					drawoption(scope, &option_list[prevop], 0, flag_byte);
					drawoption(scope, &option_list[curop], 1, flag_byte);
					smg$end_display_update(&scope->smg_option);
				}
				scope->scope_exit = 13;
				goto ExitHere;
			}

			/*
			 * Otherwise, bitch and moan
			 */
			curop = prevop;
			help_34message(scope, &p1, &p2, &p3, &p4, &p5);
	/*++
	 * Warning:INVOPT
	 *	^*Invalid Option\*
	 *	.b
	 *	.lm +5
	 *	^*Explanation:\*
	 *	.b
	 *	An invalid option has been selected.
	 *	.b
	 *	^*User Action\*
	 *	.b
	 *	Check typed option and re-enter.
	 *	.b
	 *	^*Note:\* Only highlighted capital letters can be selected as a option.
	 *	.lm -5
	 *
	 * Index:
	 *	.x Invalid>Option
	 *
	 *--
	 */
		}
	}

 ExitHere:
	/*
	 * Copy over all return values
	 */
	*op_curop = option_list[curop].curoppos;
	length = 1;
	str$copy_r(returnstr, &length, &option_list[curop].option);

	/*
	 * Exit from the function
	 */
	smg$erase_display(&scope->smg_message);

}


/*
 * Display current option however necessary
 */
static void drawoption(struct scope_struct *scope,
	struct option_list_struct *option_list,
	int flag, int flag_byte)
{
	/*
	 * Local variables
	 */
	struct dsc$descriptor segment;
	long column;
	int loop;

	/*
	 * Determine how to display this option
	 */
	if (flag != 0)
	{
		/*
		 * Display entirely in bold
		 */
		segment.dsc$a_pointer = &option_list->name[0];
		segment.dsc$w_length = strlen(option_list->name);
		segment.dsc$b_class = DSC$K_CLASS_S;
		segment.dsc$b_dtype = DSC$K_DTYPE_T;

		smg$put_chars(
			&scope->smg_option,		/* Window */
			&segment,			/* Text */
			&option_list->line,		/* Line */
			&option_list->column,		/* Column */
			&0l,				/* Erase line */
			&SMG$M_BOLD,			/* Attributes */
			0l
		);

		if (flag_byte != -1)
		{
			/*
			 * Throw option name in SCOPE->PRG_ITEM (padded with spaces)
			 */
			column = min(sizeof(scope->prg_item) - flag_byte,
				strlen(option_list->name));
			strncpy(&scope->prg_item[flag_byte],
				option_list->name, column);
			for (loop = column + flag_byte;
				loop < sizeof(scope->prg_item);
				loop++)
			{
				scope->prg_item[loop] = ' ';
			}
		}
	}
	else
	{
		/*
		 * Display left side normal
		 */
#if 0
		if (option_list->optionptr > 1)
#endif
		{
			segment.dsc$a_pointer = &option_list->name[0];
#if 0
			segment.dsc$w_length = option_list->optionptr - 1;
#else
			segment.dsc$w_length = strlen(option_list->name);
#endif
			segment.dsc$b_class = DSC$K_CLASS_S;
			segment.dsc$b_dtype = DSC$K_DTYPE_T;

			smg$put_chars(
				&scope->smg_option,		/* Window */
				&segment,			/* Text */
				&option_list->line,		/* Line */
				&option_list->column,		/* Column */
				&0l,				/* Erase line */
				&0l,				/* Attributes */
				0l
			);
		}

		/*
		 * Display character bold
		 */
		if (option_list->optionptr > 0)
		{
			segment.dsc$a_pointer =
				&option_list->name[0] + option_list->optionptr - 1;
			segment.dsc$w_length = 1;
			segment.dsc$b_class = DSC$K_CLASS_S;
			segment.dsc$b_dtype = DSC$K_DTYPE_T;

			column = option_list->column + option_list->optionptr - 1;

			smg$put_chars(
				&scope->smg_option,		/* Window */
				&segment,			/* Text */
				&option_list->line,		/* Line */
				&column,			/* Column */
				&0l,				/* Erase line */
				&SMG$M_BOLD,			/* Attributes */
				0l
			);
		}

#if 0
		/*
		 * Display right side normal
		 */
		if (option_list->optionptr < strlen(option_list->name))
		{
			segment.dsc$a_pointer =
				&option_list->name[0] + option_list->optionptr;
			segment.dsc$w_length = strlen(option_list->name) - option_list->optionptr;
			segment.dsc$b_class = DSC$K_CLASS_S;
			segment.dsc$b_dtype = DSC$K_DTYPE_T;

			column = option_list->column + option_list->optionptr;

			smg$put_chars(
				&scope->smg_option,		/* Window */
				&segment,			/* Text */
				&option_list->line,		/* Line */
				&column,			/* Column */
				&0l,				/* Erase line */
				&0l,				/* Attributes */
				0l
			);
		}
#endif
	}
}
