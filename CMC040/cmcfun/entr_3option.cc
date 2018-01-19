//! \file
//! \brief Function to Enter an Option
//!
#pragma module entr_3option "V3.6 Calico"

/*
 * Include files
 */
#include <string>
#include <cstring>
#include <ctype.h>
#include "preferences.h"
#include "cmcfun.h"
#include "smg/smg.h"
#include "scopedef.h"

/*
 * Local functions
 */
static void drawoption(scope_struct &scope,
	struct option_list_struct &option_list,
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
 * Local variables
 */
static std::string p1 = "Invalid option here";
static std::string p2 = "W";
static std::string p3 = "ENTR_3OPTION";
static std::string p4 = "";
static std::string p5 = "INVOPT";
static std::string colon = ":";

//! \brief Function to Enter an Option
//!
//!
//! \returns This function is used by the maintainence programs
//!	to enter an option.  It returns the option to
//!	the user through a variable.
//!
//! Example:
//!
//!	OPT$ = ENTR_3OPTION("COMMAND", "Blank Store eXit", 1%, 0%)
//!
//! \author 09/01/85 - Kevin Handy
//!
//!
std::string entr_3option(
	scope_struct &scope,
		//!< Scope structure
	const std::string &op_group,
		//!< The passed string that tells what type of option it is.
	std::string &op_possible,
		//!< The passed choice of options the user gets.
	long &op_curop,
		//!< The returned integer that holds the number of options and
		//! which option is currently being looked at.
	long op_flagw)
		//!< 1 - Wide menu (132 columns) (IGNORED - NOT NECESSARY)
		//! 2 - Leave menu on (IGNORED - NOT NECESSARY)
		//! 4 - Flag stating that the menu is already displayed
		//!     (IGNORED - NOT NECESSARY)
		//! 8 - Append current menu option to end of PRG_ITEM
		//! instead of replacing it.
		//! 16 - Do not change PRG_ITEM in selection.
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
	if ((op_flagw & 16) != 0)
	{
		/*
		 * Don't change PRG_ITEM ever.
		 */
		flag_byte = -1;
	}
	else
	{
		if ((op_flagw & 8) != 0)
		{
			/*
			 * Append to current PRG_ITEM.
			 */
			flag_byte = sizeof(scope.prg_item);
			while ((flag_byte > 0) &&
				(scope.prg_item[flag_byte] == ' '))
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
	option_list[0].name = "";
	option_list[0].curoppos = 1;

	for (chptr = 0; chptr < op_possible.size(); chptr++)
	{
		ch = op_possible[chptr];

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
				option_list[option_count].name = "";
				option_list[option_count].curoppos = chptr + 2;
			}
			continue;
		}

		/*
		 * Append to name
		 */
		option_list[option_count].name += ch;
		nameptr++;

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
	smg$change_pbd_characteristics(scope.smg_pbid, 0, &c_width);

	/*
	 * Allocate positions on screen for each option, and determine
	 * initial option.
	 */
	usedline = 1;
	usedwidth = op_group.size() + 3;

	for (chptr = 0; chptr < option_count; chptr++)
	{
		/*
		 * Make current option?
		 */
		if (op_curop >= option_list[chptr].curoppos)
		{
			curop = chptr;
		}

		/*
		 * Switch to next line?
		 */
		length = option_list[chptr].name.size();
		if (usedwidth + length > c_width)
		{
			usedline++;
			usedwidth = op_group.size() + 3;
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
	smg$begin_display_update(scope.smg_option);

	/*
	 * Start with a blank display
	 */
	smg$erase_display(scope.smg_option);

	/*
	 * Title for options
	 */
	smg$put_chars(
		scope.smg_option,		/* Window */
		op_group,			/* Text */
		1,				/* Line */
		1,				/* Column */
		0,				/* Erase line */
		0,				/* Attributes */
		0
	);

	length = op_group.size() + 1;
	smg$put_chars(scope.smg_option, colon,
		1, length, 0, 0, 0);

	/*
	 * Initially Display Options
	 */
	for (chptr = 0; chptr < option_count; chptr++)
	{
		drawoption(scope, option_list[chptr], curop == chptr, flag_byte);
	}

	smg$end_display_update(scope.smg_option);

	scope.scope_exit = 0;

	/*
	 * Process keyboard input
	 */
	while (1)
	{
		/*
		 * Enter one character
		 */
		ch = entr_4entry(scope, scope.smg_option, 0);
		ch = entr_4specialkeys(scope, scope.smg_option, 0, ch);
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
			smg$begin_display_update(scope.smg_option);
			drawoption(scope, option_list[curop], 0, flag_byte);
			curop++;
			if (curop >= option_count)
			{
				curop = 0;
			}
			drawoption(scope, option_list[curop], 1, flag_byte);
			smg$end_display_update(scope.smg_option);
			continue;

		/*
		 * Left arrow
		 */
		case SMG$K_TRM_LEFT:
			smg$begin_display_update(scope.smg_option);
			drawoption(scope, option_list[curop], 0, flag_byte);
			curop--;
			if (curop < 0)
			{
				curop = option_count - 1;
			}
			drawoption(scope, option_list[curop], 1, flag_byte);
			smg$end_display_update(scope.smg_option);
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
				scope.scope_exit = ch;
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
					smg$begin_display_update(scope.smg_option);
					drawoption(scope, option_list[prevop], 0, flag_byte);
					drawoption(scope, option_list[curop], 1, flag_byte);
					smg$end_display_update(scope.smg_option);
				}
				scope.scope_exit = 13;
				goto ExitHere;
			}

			/*
			 * Otherwise, bitch and moan
			 */
			curop = prevop;
			help_34message(scope, p1, p2, p3, p4, p5);
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
	op_curop = option_list[curop].curoppos;

	/*
	 * Exit from the function
	 */
	smg$erase_display(scope.smg_message);

	return std::string(1, option_list[curop].option);
}


/*
 * Display current option however necessary
 */
static void drawoption(scope_struct &scope,
	struct option_list_struct &option_list,
	int flag, int flag_byte)
{
	/*
	 * Local variables
	 */
	std::string segment;
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
		smg$put_chars(
			scope.smg_option,		/* Window */
			option_list.name,		/* Text */
			option_list.line,		/* Line */
			option_list.column,		/* Column */
			0,				/* Erase line */
			SMG$M_BOLD,			/* Attributes */
			0
		);

		if (flag_byte != -1)
		{
			/*
			 * Throw option name in SCOPE->PRG_ITEM (padded with spaces)
			 */
			column = min(sizeof(scope.prg_item) - flag_byte,
				option_list.name.size());
			scope.prg_item= option_list.name[flag_byte];
			for (loop = column + flag_byte;
				loop < sizeof(scope.prg_item);
				loop++)
			{
				scope.prg_item[loop] = ' ';
			}
		}
	}
	else
	{
		/*
		 * Display left side normal
		 */
		smg$put_chars(
			scope.smg_option,		/* Window */
			option_list.name,		/* Text */
			option_list.line,		/* Line */
			option_list.column,		/* Column */
			0,				/* Erase line */
			0,				/* Attributes */
			0
		);

		/*
		 * Display character bold
		 */
		if (option_list.optionptr > 0)
		{
			segment =
				option_list.name[option_list.optionptr - 1];

			column = option_list.column + option_list.optionptr - 1;

			smg$put_chars(
				scope.smg_option,		/* Window */
				segment,			/* Text */
				option_list.line,		/* Line */
				column,				/* Column */
				0,				/* Erase line */
				SMG$M_BOLD,			/* Attributes */
				0
			);
		}

	}
}
