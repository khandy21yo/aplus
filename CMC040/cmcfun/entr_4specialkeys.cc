//! \file
//! \brief Subroutine to Enter Data From Scope/KB
//!
#pragma module entr_4specialkeys "V3.6 Calico"

/*
 * Include files
 */
#include <stdio.h>
#include <string.h>
#include <smg/smg.h>

#include "preferences.h"
#include "cmcfun.h"
#include "scopedef.h"

//!! \brief Subroutine to Enter Data From Scope/KB
//!
//!	This function handles special keys that the user may
//!	type.
//!
//! \returns 0 if Character was handled, else returns back the
//!	character.
//!
//! \author 09/24/89 - Kevin Handy
//!

long entr_4specialkeys(
	scope_struct &scope,
	smg_display_id &smg_option,
	long xflag,
	long retchar)
{
	long value = 0;
	long smg_status;
	long col_row, col_col;

	std::string  dsc_iden, dsc_prog, dsc_item, dsc_emtstr;
	
	/*
	 * Handle as many cases here as possible
	 */
	switch (retchar)
	{
		/*
		 * Repaint screen
		 */
		case SMG$K_TRM_F11:
		case SMG$K_TRM_CTRLW:

			smg_status = smg$repaint_screen(scope.smg_pbid);
			break;

		/*
		 * Help
		 */
		case SMG$K_TRM_HELP:

			if ((xflag & 1024) == 0)
			{
				/*
				 * Create descriptors for all names
				 */
				dsc_iden = scope.prg_ident;
				dsc_prog = scope.prg_program;
				dsc_item = scope.prg_item;
				dsc_emtstr = "";

				if (dsc_prog == "HELP_34MESSAGE")
				{
					value = retchar;
				}
				else
				{
					smg_status = smg$set_cursor_mode(scope.smg_pbid, 1);
					help_34message(scope, dsc_emtstr, dsc_iden,
						dsc_prog, dsc_emtstr ,dsc_item);
				}

			}
			break;

		/*
		 * Interupt
		 */
		case SMG$K_TRM_F6:
		case SMG$K_TRM_F20:

			if ((xflag & 1024) == 0)
			{
				/*
				 * Keep Interupt Menu from going into infinite levels.
				 */
				if (scope.imenu_levels > 1)
				{
					break;
				}
				else
				{
					scope.imenu_levels++;
				}

				smg_status = smg$set_cursor_mode(scope.smg_pbid, 1);

				col_col = smg$cursor_column(smg_option);
				col_row = smg$cursor_row(smg_option);

				menu_3interrupt(scope);

				scope.imenu_levels--;

				smg_status = smg$set_cursor_abs(smg_option,
					col_row, col_col);

			}
			break;

		/*
		 * Enter Macro
		 */
		case SMG$K_TRM_F9:

			if (scope.macroflag > 1)
			{
				value = retchar;
			}
			else
			{
				value = entr_macro(scope); 
			}
			break;

		/*
		 * Anything else, give them back their character
		 */
		default:

			if (scope.macroflag > 2)
			{
 				value = SMG$K_TRM_F10;
			}
			else
			{
				value = retchar;
			}
			goto ExitFunction;
			break;
	}

	if (xflag & 256)
	{
		smg_status = smg$set_cursor_mode(scope.smg_pbid, 0);
	}
	else
	{
		smg_status = smg$set_cursor_mode(scope.smg_pbid, 1);
	}

 ExitFunction:

	return(value);
}
