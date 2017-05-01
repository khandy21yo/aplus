/*	%TITLE "Subroutine to Enter Data From Scope/KB"
 */
#pragma module entr_4specialkeys "V3.6 Calico"

/*
 *
 * COPYRIGHT (C) 1986 BY
 * Computer Management Center
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
 * Abstract:
 *
 *	This function handles special keys that the user may
 *	type.
 *
 * Parameters:
 *
 *	Returned:
 *		0 if Character was handled, else returns back the
 *		character.
 *
 * Compile:
 *
 *	$ CC FUNC_SOURCE:entr_4specialkeys/G_FLOAT
 *	$ LIB FUNC_LIB:CMC_3VECTOR/REP entr_4specialkeys
 *	$ DELETE entr_4specialkeys.OBJ;*
 *
 * Author:
 *
 *	09/24/89 - Kevin Handy
 *
 * Modification history:
 *
 *	04/06/90 - Frank F. Starman
 *		Add function ENTR_MACRO, which allow to jump directly
 *		from one program to another (passing menu).
 *
 *	06/21/90 - Frank F. Starman
 *		Check for help key if has been pressed from help_34message
 *		function.
 *
 *	08/03/90 - Frank F. Starman
 *		Ignore macro if F9 has been pressed from MCL.
 *
 *	08/21/90 - Frank F. Starman
 *		Always enable the cursor if flaf & 256.
 *
 *	04/17/95 - Kevin Handy
 *		(V3.6)
 *		Update to V3.6 Calico coding standards.
 *
 *	04/19/95 - Kevin Handy
 *		Format modification history comments nicely.
 *
 *	05/27/99 - Kevin Handy
 *		Modify so will compile in DEC-C without errors
 *		(module, descriptors, string.h)
 *--
 */

/*
 * Include files
 */
#include <stdio.h>
#include <string.h>
#include <smgdef.h>
#include <descrip.h>
#include <smg$routines.h>

#include "func_include:cmcfun.h"

long entr_4specialkeys(struct scope_struct *scope, long smg_option,
  long xflag, long retchar)
{
	long value = 0;
	long smg_status;
	long col_row, col_col;

	struct dsc$descriptor_s  dsc_iden, dsc_prog, dsc_item, dsc_emtstr;
	
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

			smg_status = smg$repaint_screen(&(scope->smg_pbid));
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
				dsc_iden.dsc$w_length = sizeof(scope->prg_ident);
				dsc_iden.dsc$a_pointer = scope->prg_ident;
				dsc_iden.dsc$b_class = DSC$K_CLASS_S;
				dsc_iden.dsc$b_dtype = DSC$K_DTYPE_T;

				dsc_prog.dsc$w_length = sizeof(scope->prg_program);
				dsc_prog.dsc$a_pointer = scope->prg_program;
				dsc_prog.dsc$b_class = DSC$K_CLASS_S;
				dsc_prog.dsc$b_dtype = DSC$K_DTYPE_T;

				dsc_item.dsc$w_length = sizeof(scope->prg_item);
				dsc_item.dsc$a_pointer = scope->prg_item;
				dsc_item.dsc$b_class = DSC$K_CLASS_S;
				dsc_item.dsc$b_dtype = DSC$K_DTYPE_T;

				dsc_emtstr.dsc$w_length = 0;
				dsc_emtstr.dsc$a_pointer = "";
				dsc_emtstr.dsc$b_class = DSC$K_CLASS_S;
				dsc_emtstr.dsc$b_dtype = DSC$K_DTYPE_T;

				if (strncmp("HELP_34MESSAGE",dsc_prog.dsc$a_pointer,14) == 0)
				{
					value = retchar;
				}
				else
				{
					smg_status = smg$set_cursor_mode(&(scope->smg_pbid), &1L);
					help_34message(scope, &dsc_emtstr, &dsc_iden,
						&dsc_prog, &dsc_emtstr ,&dsc_item);
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
				if (scope->imenu_levels > 1)
				{
					break;
				}
				else
				{
					scope->imenu_levels++;
				}

				smg_status = smg$set_cursor_mode(&(scope->smg_pbid), &1L);

				col_col = smg$cursor_column(&smg_option);
				col_row = smg$cursor_row(&smg_option);

				menu_3interrupt(scope);

				scope->imenu_levels--;

				smg_status = smg$set_cursor_abs(&smg_option,
					&col_row, &col_col);

			}
			break;

		/*
		 * Enter Macro
		 */
		case SMG$K_TRM_F9:

			if (scope->macroflag > 1)
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

			if (scope->macroflag > 2)
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
		smg_status = smg$set_cursor_mode(&(scope->smg_pbid), &0L);
	}
	else
	{
		smg_status = smg$set_cursor_mode(&(scope->smg_pbid), &1L);
	}

 ExitFunction:

	return(value);
}
