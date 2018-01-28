//! \file
//! \brief Select a Choice from a List
// %SBTTL "ENTR_3CHOICE"
// %IDENT "V3.6a Calico"
//
// Source: ../../CMC030/cmcfun/source/entr_3choice.bas
// Translated from Basic to C++ using btran
// on Monday, January 15, 2018 at 15:12:56
//

#include <cstdlib>
#include <cstring>
#include <unistd.h>
#include "basicfun.h"

#include "preferences.h"
#include "cmcfun.h"
#include "smg/smg.h"
#include "smg/smg.h"
#include "scopedef.h"
#include "cmcfun/scroll.h"


//!
//! Abstract:HELP
//!	.B
//!	Point the arrow at the item that you wish to select, and press the
//!	Select key.
//!	...LM +5
//!	..This function is used to display a selection list
//!	..in it's own window, and allow the user to select an
//!	..item from the selection list. The function returns
//!	..a -1% if no line was selected otherwise it returns
//!	..a number greater than -1% equal to the array element
//!	..number.
//!	...literal
//!	..PCOL
//!	..|
//!	..v
//!	..PROW --------->+--------BORDER TITLE-----------+
//!	..###############|#######INTERNAL TITLE##########| -----
//!	..###############+-------------------------------+ ^
//!	..###############|###############################| |
//!	..###############|###############################| WROWS
//!	..###############|###############################| |
//!	..###############|###############################| v
//!	..###############|###############################| -----
//!	..###############+-------------------------------+
//!	..###############|<------------WCOLS------------>|
//!	...end literal
//!	...B
//!	..The values of FLAG are as follows:
//!	...TABLE 3,25
//!	...TE
//!	..0 Normal.
//!	...Te
//!	..1 Encode the lines.
//!	...Te
//!	..2 Draw lines.
//!	...Te
//!	..8 No begin/end updates on scrolling.
//!	...Te
//!	..16 Border Title.
//!	...Te
//!	..32 Returns only on EXIT with all selected
//!	...Te
//!	..values in the parameter SELECTED as
//!	...Te
//!	.."1,2,3,5,8,4,".
//!!	...Te
//!	..64 Reverse window	no border.
//!	...Te
//!	..128 Reverse window	border.
//!	...Te
//!	..256 Dont repaint
//!	...end table
//!	...lm -5
//!
//! Parameters:
//!
//!	WPOSITION
//!		The passed position at the begining.
//!
//!	WSIZE
//!		The passed size of the display section.
//!
//!	TEXT()
//!		Used to check against tables.
//!
//!	FLAG
//!
//!		.table
//!		0 Normal.
//!
//!		1 Encode the lines.
//!
//!		2 Draw lines.
//!
//!		8 No begin/end updates on scrolling.
//!
//!		16 Border Title.
//!
//!		32 Returns only on EXIT with all selected
//!		values in the parameter SELECTED as
//!		"1,2,3,5,8,4,".
//!
//!		64 Reverse window - no border.
//!
//!		128 Reverse window - border.
//!
//!		256 Dont repaint
//!
//!		512 Display only
//!		.end table
//!
//!	TITLE
//!		The passed title for the choice menu.
//!
//!	DRAW_COLS
//!		Used to draw the columns of the menu.
//!
//!	BORDER_POS
//!		Draws the border position.
//!
//!	SELECTED
//!		Is the choice the user selects.
//!
//!	Returned values
//!	A number greater that -1% equal to the array element.
//!
//! Example:
//!
//!	X% = ENTR_3CHOICE('', '', VTEXT$(), GETS$, 138%,
//!		"Example", DRAW_C$, '0'L)
//!
//! Author:
//!
//!	02/05/86 - Kevin Handy and B. Craig Larsen
//!
long entr_3choice(
	scope_struct &scope,
	const std::string &wposition,
	const std::string &wsize,
	std::vector<std::string> &text,
	std::string &selected,
	long flag,
	const std::string &title,
	const std::string &draw_cols,
	long border_pos)
{
	long Result;
	long al;
	std::string ec_select;
	long f;
	std::string finp;
	long i;
	std::string old_ident;
	std::string old_item;
	std::string old_program;
	long smg_status;
	long v;

	smg_scroll_cdd smg_scroll;
	smg_display_id smg_choice;
	smg_display_id old_option;
	smg_display_id old_message;
	long beg_elem;
	long end_elem;
	long prow;
	long pcol;
	long wrows;
	long wcols;
	long cols;
	long elen;
	long width;
	long height;
	long Temp;
	long Temp2;

	//
	// --
	// Save the OPTION  virtual display
	old_option = scope.smg_option;
	// Save the MESSAGE virtual display
	old_message = scope.smg_message;
	if ((flag & 512) == 0)
	{
		smg_status = smg$create_virtual_display(2, 132, scope.smg_option);
		smg_status = smg$paste_virtual_display(scope.smg_option, scope.smg_pbid, 21, 1);
		smg_status = smg$create_virtual_display(2, 132, scope.smg_message);
		smg_status = smg$paste_virtual_display(scope.smg_message, scope.smg_pbid, 23, 1);
	}
	Result = -1;
	// Save the COMMON Ident
	old_ident = scope.prg_ident;
	// Save the COMMON Program
	old_program = scope.prg_program;
	// Save the COMMON item
	old_item = scope.prg_item;
	scope.prg_ident = "H";
	scope.prg_program = "ENTR_3CHOICE";
	scope.prg_item = "HELP";
	dspl_splitcursor(wsize, wrows, wcols);
	dspl_splitcursor(wposition, prow, pcol);
	beg_elem = 1;
	if (text[0] == "")
	{
		i = 1;
		while (basic::edit(text[i], 4 + 128) != "")
		{
			i = i + 1;
		}
	}
	else
	{
		try
		{
			i = std::stol(text[0]) + 1;
		}
		catch(basic::BasicError &Be)
		{
			goto L_20;
		}
	}
L_20:;
	end_elem = i - 1;
	if (end_elem == 0)
	{
		goto exitfunction;
	}
	//
	// Determine the window size
	//
	for (i = 1; i <= end_elem; i++)
	{
		if (basic::edit(text[i], 4 + 128).size() + 2 > al)
		{
			al = basic::edit(text[i], 4 + 128).size() + 2;
		}
	}
#if 0
	//! \fixme ???What is this really trying to do
	if (boost::replace_regex_all_copy(title, "[\n\r\f\0x1a\0xff]", "").size() > al)
	{
		al = boost::replace_regex_all_copy(title, "[\n\r\f\0x1a\0xff]", "").size();
	}
#endif
	if ((al < wcols || wcols < 1) && al > 0)
	{
		wcols = al;
	}
	if (wcols > 78 || wcols < 1)
	{
		wcols = 78;
	}
	if (wrows < 1)
	{
		wrows = end_elem - beg_elem + 1;
	}
	if ((flag & 16) == 0)
	{
		wrows = wrows + 2;
	}
	if (wrows > 14 || wrows < 1)
	{
		wrows = 14;
	}
	//
	// Determine the window position
	//
	smg_status = smg$change_pbd_characteristics(scope.smg_pbid, 0, &width, 0, &height);
	if ((prow < 1 || prow > height) && height > 0)
	{
		prow = (height / 2) - (wrows / 2) + 2;
	}
	if (wrows + prow > 19)
	{
		prow = 4;
	}
	if ((pcol < 1 || pcol > width) && width > 0)
	{
		pcol = (width / 2) - (wcols / 2) + 1;
	}
	//
	// Create the window
	//
	if (flag & 64)
	{
		smg_status = smg$create_virtual_display(wrows, wcols,
			smg_choice, 0, SMG$M_REVERSE);
	}
	else
	{
		if (flag & 128)
		{
			smg_status = smg$create_virtual_display(wrows, wcols,
				smg_choice, SMG$M_BORDER, SMG$M_REVERSE);
		}
		else
		{
			smg_status = smg$create_virtual_display(wrows, wcols,
				smg_choice, SMG$M_BORDER);
		}
	}
	//
	// Output the title and title line
	//
	if (flag & 16)
	{
		smg_status = smg$label_border(smg_choice, title, border_pos);
	}
	else
	{
		smg_status = smg$put_chars(smg_choice, title, 1, 1);
		smg_status = smg$draw_line(smg_choice, 2, 1, 2, wcols);
	}
	smg_scroll.window = &smg_choice;
	smg_scroll.scroll_top = 3;
	if (flag & 16)
	{
		smg_scroll.scroll_top = 1;
	}
	// SMG_SCROLL::SCROLL_BOT	= SMG_SCROLL::SCROLL_TOP + WROWS
	smg_scroll.scroll_bot = wrows;
	smg_scroll.top_array = beg_elem;
	smg_scroll.bot_array = end_elem;
	smg_scroll.top_line = 1;
	smg_scroll.beg_element = beg_elem;
	smg_scroll.end_element = end_elem;
	ec_select = basic::edit(selected, 4 + 8 + 32 + 128 + 256);
	if (flag & 32)
	{
		selected = "";
	}
	if (draw_cols == "")
	{
		for (i = 0; i <= end_elem; i++)
		{
			if (ec_select == basic::edit(text[i], 4 + 8 + 32 + 128 + 256))
			{
				smg_scroll.cur_line = smg_scroll.find_line = i;
			}
		}
	}
	else
	{
		for (i = 0; i <= end_elem; i++)
		{
			if (ec_select == basic::edit(text[i].substr(0, std::stol(draw_cols.substr(0, 3)) - 3), 4 + 8 + 32 + 128 + 256))
			{
				smg_scroll.cur_line = smg_scroll.find_line = i;
			}
		}
	}
	if (smg_scroll.cur_line < 1)
	{
		smg_scroll.cur_line = smg_scroll.find_line = smg_scroll.top_line;
	}
	smg_scroll.cur_w_col = 1;
	smg_scroll.cur_w_row = 1;
	smg_scroll.prompt = "->";
	if (flag & 512)
	{
		smg_scroll.prompt = "  ";
	}
	smg_scroll.draw_cols = draw_cols;
	smg_scroll.smg_flag = 0;
	if (flag & 1)
	{
		smg_scroll.smg_flag = smg_scroll.smg_flag | 1;
	}
	if (flag & 2)
	{
		smg_scroll.smg_flag = smg_scroll.smg_flag | 2;
	}
	if (flag & 8)
	{
		smg_scroll.smg_flag = smg_scroll.smg_flag | 8;
	}
	Temp = 0;
	v = dspl_scroll(smg_scroll, text, Temp, "PAINT");
	smg_status = smg$paste_virtual_display(smg_choice, scope.smg_pbid, prow, pcol);
menu:;
	if (flag & 512)
	{
		goto exitfunction;
	}
	if (flag & 32)
	{
		entr_3message(scope, "<FIND>,<SELECT - * indicates selected>,<Action Keys> or", 12);
	}
	else
	{
		entr_3message(scope, "<FIND>,<SELECT>,<Action Keys> or", 12);
	}
checkkey:;
	// ** Converted from a select statement **
	// Uparrow
	// Downarrow
	// Left arrow
	// Right arrow
	// Previous screen
	// Next screen
	// Top
	// Bottom
	if ((scope.scope_exit == SMG$K_TRM_UP) ||
		(scope.scope_exit == SMG$K_TRM_DOWN) ||
		(scope.scope_exit == SMG$K_TRM_LEFT) ||
		(scope.scope_exit == SMG$K_TRM_RIGHT) ||
		(scope.scope_exit == SMG$K_TRM_PREV_SCREEN) ||
		(scope.scope_exit == SMG$K_TRM_NEXT_SCREEN) ||
		(scope.scope_exit == SMG$K_TRM_F18) ||
		(scope.scope_exit == SMG$K_TRM_F19))
	{
		v = dspl_scroll(smg_scroll, text, scope.scope_exit, "");
		//
		// Normal Exit
		//
	}
	else if ((scope.scope_exit == 0) ||
		(scope.scope_exit == 10) ||
		(scope.scope_exit == 12) ||
		(scope.scope_exit == 13) ||
		(scope.scope_exit == SMG$K_TRM_DO) ||
		(scope.scope_exit == SMG$K_TRM_F7))
	{
		scope.scope_exit = SMG$K_TRM_DO;
		goto exitfunction;
		//
		// Selected Exit
		//
	}
	else if (scope.scope_exit == SMG$K_TRM_SELECT)
	{
		Result = smg_scroll.cur_line;
		if (flag & 32)
		{
			selected = selected + std::to_string(smg_scroll.cur_line) + ",";
			text[smg_scroll.cur_line] = std::string("* ") + basic::right(text[smg_scroll.cur_line], 3);
			Temp = 0;
			v = dspl_scroll(smg_scroll, text, Temp, "PAINT");
			goto menu;
		}
		scope.scope_exit = SMG$K_TRM_DO;
		goto exitfunction;
		//
		// Exit Key
		//
	}
	else if ((scope.scope_exit == SMG$K_TRM_F10) ||
		(scope.scope_exit == SMG$K_TRM_CTRLZ))
	{
		goto exitfunction;
		//
		// Magic Key, allow to do something magical
		//
	}
	else if (scope.scope_exit == SMG$K_TRM_F17)
	{
		goto exitfunction;
		//
		// Find Key
		//
	}
	else if (scope.scope_exit == SMG$K_TRM_FIND)
	{
		//
find1:;
		// Search for certain keyword
		//
		smg_status = smg$erase_display(scope.smg_option);
		smg_status = smg$erase_display(scope.smg_message);
		smg_status = smg$put_chars(scope.smg_option, "Search for: ", 1, 1, 1);
		finp = std::string(67, ' ');
		// ** Converted from a select statement **
		// ^C, ^Z, Exit
		Temp2 = -1;
		Temp = entr_3enter(scope, scope.smg_option, 1, 13, finp, Temp2, 16 + 4096);
		if ((Temp == SMG$K_TRM_CTRLC) ||
			(Temp == SMG$K_TRM_F8) ||
			(Temp == SMG$K_TRM_F10) ||
			(Temp  == SMG$K_TRM_CTRLZ) ||
			(Temp  == SMG$K_TIMEOUT))
		{
			goto findend;
			// Good keys
		}
		else if ((Temp == 0) ||
			(Temp == 10) ||
			(Temp  == 12) ||
			(Temp == 13) ||
			(Temp == SMG$K_TRM_DO) ||
			(Temp == SMG$K_TRM_F7))
		{
		}
		else
		{
			entr_3badkey(scope, scope.scope_exit);
			goto find1;
		}
		finp = basic::edit(finp, 4 + 8 + 128 + 256);
		if (finp == "")
		{
			goto menu;
		}
		cols = 0;
		for (f = smg_scroll.beg_element; f <= smg_scroll.end_element; f++)
		{
find2:;
			cols = (boost::to_upper_copy(text[f]).find(finp, cols + 1 - 1) + 1);
			if (cols)
			{
				smg_scroll.find_line = f;
				Temp = SMG$K_TRM_FIND;
				elen = dspl_scroll(smg_scroll, text, Temp, "FIND");
				smg_status = smg$put_chars(smg_choice, basic::mid(text[f], cols, finp.size()), smg_scroll.cur_w_row, cols + 2, 0, SMG$M_BLINK);
find3:;
				help_34message(scope, "string match", "S", "ENTR_3CHOICE", "", "STRMATCH");
				// ++
				// Success:STRMATCH
				//	^*String Match\*
				//	.b
				//	.lm +5
				//	Matching string has been found.
				//	.table 3,25
				//	.te
				//	<Exit> key terminates searching
				//	.te
				//	<FIND> key prompts for a new searching string
				//	.te
				//	<RESUME> key will continue for the next match.
				//	.end table
				//	.lm -5
				//
				// Index:
				//
				// --
				// ** Converted from a select statement **
				// ^C, ^Z, Exit
				if ((scope.scope_exit == SMG$K_TRM_CTRLC) ||
					(scope.scope_exit == SMG$K_TRM_F8) ||
					(scope.scope_exit == SMG$K_TRM_F10) ||
					(scope.scope_exit == SMG$K_TRM_CTRLZ))
				{
					Temp = 0;
					elen = dspl_scroll(smg_scroll, text, Temp, "PAINT");
					goto findend;
					// Good keys
				}
				else if ((scope.scope_exit == 0) ||
					(scope.scope_exit == 10) ||
					(scope.scope_exit == 12) ||
					(scope.scope_exit == 13) ||
					(scope.scope_exit == SMG$K_TRM_DO) ||
					(scope.scope_exit == SMG$K_TRM_F7))
				{
					//
					// Find Key
					//
				}
				else if (scope.scope_exit == SMG$K_TRM_FIND)
				{
					Temp = 0;
					elen = dspl_scroll(smg_scroll, text, Temp, "PAINT");
					goto checkkey;
				}
				else
				{
					entr_3badkey(scope, scope.scope_exit);
					goto find3;
				}
				Temp = 0;
				elen = dspl_scroll(smg_scroll, text, Temp, "PAINT");
				goto find2;
			}
		}
		help_34message(scope, "no string match", "I", "ENTR_3CHOICE", "", "NOSTRMATCH");
		// ++
		// Information:NOSTRMATCH
		//	^*No String Match\*
		//	.b
		//	.lm +5
		//	No matching string has been found.
		//	.b
		//	Press <RESUME> to continue.
		//	.lm -5
		//
		// Index:
		//
		// --
findend:;
		smg_status = smg$erase_display(scope.smg_option);
		//
		// Otherwise, it is a bad key
		//
	}
	else
	{
		entr_3badkey(scope, scope.scope_exit);
	}
	goto menu;
exitfunction:;
	//
	// Delete all displays
	//
	if (!(flag & 512))
	{
		smg_status = smg$pop_virtual_display(scope.smg_option, scope.smg_pbid);
	}
	// Restore OPTION  virtual display
	scope.smg_option = old_option;
	// Restore MESSAGE virtual display
	scope.smg_message = old_message;
	// Restore the COMMON Ident
	scope.prg_ident = old_ident;
	// Restore the COMMON Program
	scope.prg_program = old_program;
	// Restore the COMMON item
	scope.prg_item = old_item;
	return Result;
}
