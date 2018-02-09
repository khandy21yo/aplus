//! \file
//! \brief Digital Standard Runoff for Text Array
// %SBTTL "LIBR_DIGSR"
// %IDENT "V3.6a Calico"

//
// Source: /home/kevin/aplus/CMC030/cmcfun/source/libr_digsr.bas
// Translated from Basic to C++ using btran
// on Wednesday, December 20, 2017 at 10:56:17
//

#include <cstdlib>
#include <string>
#include <unistd.h>

#include "basicfun.h"
#include "cmcfun.h"
#include "smg/smg.h"
#include "smg/lbr.h"

//!
//! \brief Build a 5 byte structure for attributes.
//!
//! This was handledusing a union in VAX Basic, but this is a better
//! way to handle it in C++ as it generates a std::string that is fixed
//! at 5 bytes intead of a char array that rerminates with a '\0'.
//!
static inline  std::string fd_build(int a, int b, char c)
{
	std::string Result;
	Result.resize(5);
	Result[0] = char(a);
	Result[1] = char(a >> 8);
	Result[2] = char(b);
	Result[3] = char(b >> 8);
	Result[4] = char(c);
	return Result;
}

//!
//! \brief Build a 2 byte structure for attributes.
//!
//! This was handledusing a union in VAX Basic, but this is a better
//! way to handle it in C++ as it generates a std::string that is fixed
//! at 5 bytes intead of a char array that rerminates with a '\0'.
//!
static inline  std::string fdl_build(int a)
{
	std::string Result;
	Result.resize(2);
	Result[0] = char(a);
	Result[1] = char(a >> 8);
	return Result;
}

//!
//!	This subroutine takes the filename and given array and
//!	opens the file and reads the DSR commands embedded in
//!	the text array and interprets them setting them up in
//!	the array for use by SMG routines(PUT_VIRTUAL_DISPLAY_ENCODED).
//!
//! Parameters:
//!
//!	LIB_NAME$
//!		The passed name of the library to pull the text from.
//!
//!	KEY_NAME$
//!		The passed name of the key to use to select the
//!		right text.
//!
//!	CODE$(0%)
//!		Contains the number of lines already in
//!		use in the CODE$() array.
//!
//!	CODE$()
//!		The text returned back in PRNT_ENCODED format.
//!		CODE$(0%) is modified to point to the last line.
//!
//!	Returns a status code.
//!
//! Example:
//!
//!	TEXT$(0%) = "0"
//!	ST% = LIBR_DIGSR("HELP_GL", "ADDRESS", TEXT$())
//!
//! Author:
//!
//!	02/20/87 - B. Craig Larsen
//!
long libr_digsr(
	const std::string &lib_name,
	const std::string &key_name,
	std::vector<std::string> &code)
{
	long Result;
	long ary_attr = 0;
	long blankl;
	long curr_line;
	long cur_attr;
	std::string dsr[11];
	std::string dsrcom;
	std::string dsrcom1;
	long example_table;
	long fill_flag;
	long i;
	long indent;
	std::string inp;
	long iz;
	long iz1;
	long j;
	long keep_attr;
	long left_mar;
	long list_list;
	long literal;
	lbr_index_cdd lr_index;
	long note_footnote;
	long note_note;
	long note_quote;
	long once_attr;
	long option_table;
	std::string part_text;
	long right_mar;
	long smg_status;
	long st;
	long table_table;
	std::string temp;
	long temp_V4;
	long temp1;
	std::string temp_attr;
	long temp_beg;
	long temp_end;
	long temp_i;
	long temp_length;
	long temp_used;
	long temp_xskip;
	std::string text;
	std::string this_text;
	std::string total_text;
	const long max_list = 20;

	BStack(20);
	OnErrorStack;
	long sub_match;
	long position;
	long txrfa;
	std::string list_char[max_list + 1];
	long list_num[max_list + 1];
	long list_skip[max_list + 1];
	long ary_attr_V1[201];
	long ary_attp[201];
	long ts[33];
	long oldts[33];

	//*******************************************************************
	// Blank line
	//
	// Insert a blank line into the output
	//*******************************************************************
	auto blankline = [&](void)
	{
		//
		// Handle text
		//
		curr_line = curr_line + 1;
		code[curr_line] = fdl_build(2);
		return;
	};
	//*******************************************************************
	// Break text - write out all of the line into the output
	// area.
	//
	// If input is blank, there is no output.
	//*******************************************************************
	auto breaktext = [&](void)
	{
		//
		// Init necessary information
		//
		total_text = boost::trim_right_copy(total_text);
		temp_used = 1;
		//
		// Force ending to attribute list
		//
		ary_attr = ary_attr + 1;
		ary_attr_V1[ary_attr] = 0;
		ary_attp[ary_attr] = total_text.size() + 1;
		while (total_text != "")
		{
			//
			// Maximum width
			//
			temp_length = right_mar - left_mar - indent;
			if (temp_length < 5)
			{
				temp_length = 40;
			}
			//
			// Search for previous space (if possible)
			//
			if (total_text.size() < temp_length)
			{
				part_text = total_text;
				total_text = "";
				temp_xskip = total_text.size();
			}
			else
			{
				temp_i = temp_length + 1;
				while (!((temp_i == 0) || (basic::mid(total_text, temp_i, 1) == " ")))
				{
					temp_i = temp_i - 1;
				}
				if (temp_i == 0)
				{
					part_text = total_text.substr(0, temp_length);
					total_text = basic::right(total_text, temp_length + 1);
					temp_xskip = temp_length;
				}
				else
				{
					part_text = total_text.substr(0, temp_i - 1);
					total_text = basic::right(total_text, temp_i + 1);
					temp_xskip = temp_i;
				}
			}
			//
			// Handle text.  First create the attributes for this line
			//
			temp_attr = "";
			for (temp_i = 1; temp_i <= ary_attr; temp_i++)
			{
				//
				// If attribute is in range, add it to the list
				//
				temp_beg = ary_attp[temp_i - 1] - temp_used + 1;
				if (temp_beg < 1)
				{
					temp_beg = 1;
				}
				temp_end = ary_attp[temp_i] - temp_used;
				if (temp_end > part_text.size())
				{
					temp_end = part_text.size();
				}
				if (temp_end >= temp_beg)
				{
					temp_attr = temp_attr + fd_build(
						temp_beg + left_mar + indent,
						temp_end - temp_beg + 1,
						ary_attr_V1[temp_i - 1]);
				}
			}
			curr_line = curr_line + 1;
			code[curr_line] = std::string(left_mar + indent, ' ') + part_text + temp_attr + fdl_build(2 + temp_attr.size());
			indent = 0;
			temp_used = temp_used + temp_xskip;
		}
		ary_attr = 0;
		return;
	};

	//*******************************************************************
	// Add text to current build buffer
	//
	// Adds THIS_TEXT$ to the output text, while keeping the attributes
	// set at this point.
	//*******************************************************************
	auto buildtext = [&](void)
	{
		//
		// Skip blank text
		//
		if (this_text == "")
		{
			goto L_9090;
		}
		//
		// Handle one time only attributes
		//
		if (once_attr != 0)
		{
			if ((once_attr | cur_attr) != ary_attr_V1[ary_attr])
			{
				if ((ary_attp[ary_attr] != total_text.size() + 1) || (ary_attr == 0))
				{
					ary_attr = ary_attr + 1;
				}
				ary_attr_V1[ary_attr] = cur_attr | once_attr;
				ary_attp[ary_attr] = total_text.size() + 1;
			}
			total_text = total_text + this_text.substr(0, 1);
			this_text = basic::right(this_text, 2);
			once_attr = 0;
		}
		//
		// Handle Any new attributes
		//
		if ((cur_attr != ary_attr_V1[ary_attr]) || (ary_attr == 0))
		{
			if ((ary_attp[ary_attr] != total_text.size() + 1) || (ary_attr == 0))
			{
				ary_attr = ary_attr + 1;
			}
			ary_attr_V1[ary_attr] = cur_attr;
			ary_attp[ary_attr] = total_text.size() + 1;
		}
		total_text = total_text + this_text;
L_9090:;
		return;
	};

	//*******************************************************************
	// Handle literal text
	//*******************************************************************
	auto literalline = [&](void)
	{
		temp = basic::edit(inp, -1);
		//
		// Handle end of literal text
		//
		if ((temp == ".EL") || (temp == ".ENDLITERAL"))
		{
			literal = 0;
			goto L_8490;
		}
		//
		// Insert literal text (we go directly into the output array
		// because it is the easiest way to go)
		//
		curr_line = curr_line + 1;
		code[curr_line] = std::string(left_mar, ' ') + inp + fdl_build(2);
L_8490:;
		return;
	};

	auto beginlist = [&](void)
	{
		list_list = list_list + 1;
		list_char[list_list] = "";
		list_num[list_list] = 1;
		list_skip[list_list] = 1;
		left_mar = left_mar + 4;
		dsr[1] = boost::trim_copy(dsr[1]);
		dsr[2] = boost::trim_copy(dsr[2]);
		if ((dsr[1].substr(0, 1) != "\"") && (dsr[1] != ""))
		{
			temp_V4 = 2;
			try
			{
				list_skip[list_list] = std::stol(dsr[1]);
			}
			catch(basic::BasicError &Be)
			{
				if (Be.err == 52)
				{
					goto L_8090;
				}
				throw Be;
			}
		}
		else
		{
			temp_V4 = 1;
		}
		if (dsr[temp_V4].substr(0, 1) == "\"")
		{
			temp1 = (dsr[temp_V4].find("\"", 1) + 1);
			list_char[list_list] = basic::Qseg(dsr[temp_V4], 2, temp1 - 1);
		}
L_8090:;
		return;
	};

	//**********************************************************************
	// Identify the Command line and execute the line
	//**********************************************************************
	auto commandline = [&](void)
	{
		dsrcom = basic::edit(basic::right(inp, 2), 4 + 8 + 16 + 32 + 128 + 256);
		for (i = 0; i <= 10; i++)
		{
			dsr[i] = "";
		}
		i = 0;
		temp_V4 = 1;
		dsrcom1 = "";
getcommand:;
		iz = (dsrcom.find(" ", 0) + 1);
		iz1 = (dsrcom.find(",", 0) + 1);
		if ((iz == 0) || ((iz1 > 0) && (iz1 < iz)))
		{
			iz = iz1;
		}
		if (iz)
		{
			dsr[i] = dsrcom.substr(0, iz - 1);
			dsrcom = basic::right(dsrcom, iz + 1);
			if (i == 0)
			{
				dsrcom1 = dsrcom;
			}
			i = i + 1;
			if (i < 10)
			{
				goto getcommand;
			}
		}
		dsr[i] = dsrcom;
		//
		// .end
		//
		if (dsr[0] == "END")
		{
			//
			// .end list
			//
			if (dsr[1] == "LIST")
			{
				breaktext();
				left_mar = left_mar - 4;
				blankline();
				list_list = list_list - 1;
				//
				// .end note
				//
			}
			else if (dsr[1] == "NOTE")
			{
				breaktext();
				blankline();
				note_note = note_note - 1;
				//
				// .end quote
				//
			}
			else if (dsr[1] == "QUOTE")
			{
				breaktext();
				left_mar = left_mar - 8;
				right_mar = right_mar + 8;
				blankline();
				note_quote = note_quote - 1;
				//
				// .end footnote
				//
			}
			else if (dsr[1] == "FOOTNOTE")
			{
				breaktext();
				left_mar = left_mar - 8;
				right_mar = right_mar + 8;
				blankline();
				note_footnote = note_footnote - 1;
			}
			else if (dsr[1] == "TABLE")
			{
				breaktext();
				blankline();
				table_table = table_table - 1;
				for (j = 0; j <= 32; j++)
				{
					ts[i] = oldts[j];
				}
				left_mar = left_mar - 5;
				right_mar = right_mar + 5;
			}
			else if (dsr[1] == "OPTION")
			{
				breaktext();
				blankline();
				option_table = option_table - 1;
				left_mar = left_mar - 5;
				for (i = 0; i <= 32; i++)
				{
					ts[i] = oldts[i];
				}
			}
			else if (dsr[1] == "EXAMPLE")
			{
				breaktext();
				blankline();
				example_table = example_table - 1;
				left_mar = left_mar - 5;
				for (i = 0; i <= 32; i++)
				{
					ts[i] = oldts[i];
				}
			}
			//
			// .b
			// .blank
			//
		}
		else if ((dsr[0] == "B") || (dsr[0] == "BLANK"))
		{
			breaktext();
			try
			{
				blankl = std::stol(dsr[1]);
			}
			catch(basic::BasicError &Be)
			{
				// Illegal number
				if (Be.err == 52)
				{
					blankl = 1;
					goto L_6010;
				}
				throw Be;
			}
			if (blankl < 1)
			{
				blankl = 1;
			}
L_6010:;
			for (i = 1; i <= blankl; i++)
			{
				blankline();
			}
			//
			// .br
			// .break
			//
		}
		else if ((dsr[0] == "BR") || (dsr[0] == "BREAK"))
		{
			breaktext();
			//
			// .i
			// .indent
			//
		}
		else if ((dsr[0] == "I") || (dsr[0] == "INDENT"))
		{
			breaktext();
			try
			{
				indent = std::stol(dsr[1]);
			}
			catch(basic::BasicError &Be)
			{
				// Illegal number
				if (Be.err == 52)
				{
					indent = 0;
					goto commandloop;
				}
				throw Be;
			}
			//
			// .lm
			//
		}
		else if (dsr[0] == "LM")
		{
			breaktext();
			// ** Converted from a select statement **
			if ((dsr[1].substr(0, 1) == "+") || (dsr[1].substr(0, 1) == "-"))
			{
				left_mar = left_mar + std::stol(dsr[1]);
			}
			else
			{
				left_mar = std::stol(dsr[1]);
			}
			//
			// .left
			//
		}
		else if (dsr[0] == "LEFT")
		{
			// ** Converted from a select statement **
			//
			// .left margin
			//
			if (dsr[1] == "MARGIN")
			{
				breaktext();
				// ** Converted from a select statement **
				if ((dsr[2].substr(0, 1) == "+") || (dsr[2].substr(0, 1) == "-"))
				{
					left_mar = left_mar + std::stol(dsr[2]);
				}
				else
				{
					left_mar = std::stol(dsr[2]);
				}
			}
			//
			// .lt
			// .literal
			//
		}
		else if ((dsr[0] == "LT") || (dsr[0] == "LITERAL"))
		{
			breaktext();
			literal = -1;
			//
			// .list
			//
		}
		else if (dsr[0] == "LIST")
		{
			//
			// .list element
			//
			if (dsr[1] == "ELEMENT")
			{
				breaktext();
				for (temp_V4 = 1; temp_V4 <= list_skip[list_list]; temp_V4++)
				{
					blankline();
				}
				//
				// Place a star (or number) on the line
				//
				keep_attr = cur_attr;
				cur_attr = SMG$M_BOLD;
				if (list_char[list_list] == "")
				{
					this_text = std::to_string(list_num[list_list]);
					list_num[list_list] = list_num[list_list] + 1;
				}
				else
				{
					this_text = list_char[list_list];
				}
				indent = -(this_text.size() + 1);
				buildtext();
				cur_attr = keep_attr;
				//
				// .list *
				//
			}
			else
			{
				breaktext();
				beginlist();
			}
			//
			// .els
			//
		}
		else if (dsr[0] == "ELS")
		{
			breaktext();
			left_mar = left_mar - 4;
			blankline();
			list_list = list_list - 1;
			//
			// .ls
			//
		}
		else if (dsr[0] == "LS")
		{
			breaktext();
			beginlist();
			//
			// .le
			//
		}
		else if (dsr[0] == "LE")
		{
			breaktext();
			for (temp_V4 = 1; temp_V4 <= list_skip[list_list]; temp_V4++)
			{
				blankline();
			}
			//
			// Place a star (or number) on the line
			//
			keep_attr = cur_attr;
			cur_attr = SMG$M_BOLD;
			if (list_char[list_list] == "")
			{
				this_text = std::to_string(list_num[list_list]);
				list_num[list_list] = list_num[list_list] + 1;
			}
			else
			{
				this_text = list_char[list_list];
			}
			indent = -(this_text.size() + 1);
			buildtext();
			cur_attr = keep_attr;
			//
			// .nt
			// .note
			//
		}
		else if ((dsr[0] == "NOTE") || (dsr[0] == "NT"))
		{
			breaktext();
			blankline();
			if (dsrcom1 == "")
			{
				dsrcom1 = "Note";
			}
			this_text = dsrcom1 + ": ";
			buildtext();
			note_note = note_note + 1;
			//
			// .qt
			// .quote
			//
		}
		else if ((dsr[0] == "QUOTE") || (dsr[0] == "QT"))
		{
			breaktext();
			left_mar = left_mar + 8;
			right_mar = right_mar - 8;
			total_text = "";
			blankline();
			note_quote = note_quote + 1;
			//
			// .en
			//
		}
		else if (dsr[0] == "EN")
		{
			breaktext();
			blankline();
			note_note = note_note - 1;
			//
			// .eqt
			//
		}
		else if (dsr[0] == "EQT")
		{
			breaktext();
			left_mar = left_mar - 8;
			right_mar = right_mar - 8;
			blankline();
			note_quote = note_quote - 1;
			//
			// .fn
			// .footnote
			//
		}
		else if ((dsr[0] == "FOOTNOTE") || (dsr[0] == "FN"))
		{
			breaktext();
			left_mar = left_mar + 8;
			right_mar = right_mar - 8;
			blankline();
			if (dsrcom1 == "")
			{
				dsrcom1 = "FOOTNOTE";
			}
			total_text = std::string((right_mar - left_mar) / 2 - dsrcom1.size() / 2, ' ') + dsrcom1;
			breaktext();
			blankline();
			note_footnote = note_footnote + 1;
			//
			// .efn
			//
		}
		else if (dsr[0] == "EFN")
		{
			breaktext();
			left_mar = left_mar - 8;
			right_mar = right_mar - 8;
			blankline();
			note_footnote = note_footnote - 1;
			//
			// .p
			// .paragraph
			//
		}
		else if ((dsr[0] == "P") || (dsr[0] == "PARAGRAPH"))
		{
			breaktext();
			blankline();
			if (dsr[1] != "")
			{
				indent = std::stol(dsr[1]);
			}
			else
			{
				indent = 5;
			}
			//
			// .right
			//
		}
		else if (dsr[0] == "RIGHT")
		{
			// ** Converted from a select statement **
			//
			// .right margin
			//
			if (dsr[1] == "MARGIN")
			{
				breaktext();
				// ** Converted from a select statement **
				if ((dsr[2].substr(0, 1) == "+") || (dsr[2].substr(0, 1) == "-"))
				{
					right_mar = right_mar + std::stol(dsr[2]);
				}
				else
				{
					right_mar = std::stol(dsr[2]);
				}
			}
			//
			// .rm
			//
		}
		else if (dsr[0] == "RM")
		{
			breaktext();
			// ** Converted from a select statement **
			if ((dsr[1].substr(0, 1) == "+") || (dsr[1].substr(0, 1) == "-"))
			{
				right_mar = right_mar + std::stol(dsr[1]);
			}
			else
			{
				right_mar = std::stol(dsr[1]);
			}
			//
			// .fill, .justify (assumed to be the same thing)
			//
		}
		else if ((dsr[0] == "FILL") || ((dsr[0] == "F") || ((dsr[0] == "JUSTIFY") || (dsr[0] == "J"))))
		{
			breaktext();
			fill_flag = -1;
			//
			// No fill, no justify
			//
		}
		else if ((dsr[0] == "NF") || (dsr[0] == "NJ"))
		{
			breaktext();
			fill_flag = 0;
			//
			// Tab Stops
			//
		}
		else if ((dsr[0] == "TS") || (dsr[0] == "TABSTOP"))
		{
			for (temp_V4 = 1; temp_V4 <= i; temp_V4++)
			{
				ts[temp_V4] = std::stol(dsr[temp_V4]);
			}
			ts[i + 1] = 32767;
			//
			// Tables
			//
		}
		else if (dsr[0] == "TABLE")
		{
			breaktext();
			blankline();
			table_table = table_table + 1;
			for (j = 0; j <= 32; j++)
			{
				oldts[j] = ts[j];
			}
			left_mar = left_mar + 5;
			right_mar = right_mar - 5;
			for (temp_V4 = 1; temp_V4 <= i; temp_V4++)
			{
				ts[temp_V4] = std::stol(dsr[temp_V4]) + left_mar;
			}
			ts[i + 1] = 32767;
		}
		else if (dsr[0] == "ENDTABLE")
		{
			breaktext();
			blankline();
			table_table = table_table - 1;
			for (j = 0; j <= 32; j++)
			{
				ts[j] = oldts[j];
			}
			left_mar = left_mar - 5;
			right_mar = right_mar + 5;
		}
		else if ((dsr[0] == "TE") || (dsr[0] == "TT"))
		{
			breaktext();
			//
			// Options
			//
		}
		else if (dsr[0] == "OPTION")
		{
			breaktext();
			blankline();
			total_text = "Option:";
			breaktext();
			option_table = option_table + 1;
			left_mar = left_mar + 5;
			for (i = 0; i <= 32; i++)
			{
				oldts[i] = ts[i];
			}
			for (i = 1; i <= 31; i++)
			{
				ts[i] = 8 * i;
			}
			ts[32] = 32767;
		}
		else if (dsr[0] == "ENDOPTION")
		{
			breaktext();
			blankline();
			option_table = option_table - 1;
			left_mar = left_mar - 5;
			for (i = 0; i <= 32; i++)
			{
				ts[i] = oldts[i];
			}
		}
		else if (dsr[0] == "OE")
		{
			breaktext();
			//
			// Examples
			//
		}
		else if (dsr[0] == "EXAMPLE")
		{
			breaktext();
			blankline();
			total_text = "Example:";
			breaktext();
			example_table = example_table + 1;
			left_mar = left_mar + 5;
			for (i = 0; i <= 32; i++)
			{
				oldts[i] = ts[i];
			}
			for (i = 1; i <= 31; i++)
			{
				ts[i] = 8 * i;
			}
			ts[32] = 32767;
		}
		else if (dsr[0] == "ENDEXAMPLE")
		{
			breaktext();
			blankline();
			option_table = option_table - 1;
			left_mar = left_mar - 5;
			for (i = 0; i <= 32; i++)
			{
				ts[i] = oldts[i];
			}
		}
		else if (dsr[0] == "EE")
		{
			breaktext();
		}
		else if (dsr[0] == "FIELD")
		{
			left_mar = left_mar + 5;
			ts[1] = 55;
			ts[2] = 32767;
			indent = -5;
		}
commandloop:;
		return;
	};

	//**********************************************************************
	// Check a text line for any current DSR flags/commands
	//**********************************************************************
	auto textline = [&](void)
	{
		//
		// Check for Margins and indent
		//
		position = 0;
		//
		// Append a space to front of line if appending to existing text
		//
		if (total_text != "")
		{
			this_text = " ";
			buildtext();
		}
		//	INP$ = EDIT$(INP$, 8% + 16% + 128%)
		inp = boost::trim_right_copy(inp);
text1:;
		//
		// Search for junk in input string
		//
		if (!(literal))
		{
#if 0
			smg_status = str$find_first_substring(inp, position, sub_match, "*", "&", "^*", "\\*", "^&", "\\&", "_", "#", "^~", "\\~", "\t");
#else
			//
			// Tru to do same thing as STR$ routine
			//
			static const std::string sp[] =
				{"^*", "\\*", "^&", "\\&", "^~", "\\~"};
			position = -1;
			for (int loop = 0; loop < inp.size(); loop++)
			{
				for (int loop2 = 0; loop2<= inp.size(); loop2++)
				{
					if (inp.compare(loop, sp[loop2].size(),
						sp[loop2]) == 0)
					{
						position = loop;
						sub_match = loop2 + 1;
						goto xtestx;
					}
				}
			}
xtestx:
			position--;
#endif
		}
		//
		// If none found, handle properly
		//
		if (position == 0)
		{
			this_text = inp;
			buildtext();
			goto endtext;
		}
		//
		// Handle starting part
		//
		this_text = inp.substr(0, position - 1);
		buildtext();
		inp = basic::right(inp, position + 1);
		switch (sub_match)
		{
		//
		// Bold char (*)
		//
		case 1:

			once_attr = once_attr | SMG$M_BOLD;
			//
			// Underline char (&)
			//
			break;

		case 2:

			once_attr = once_attr | SMG$M_UNDERLINE;
			//
			// Start bold (^*)
			//
			break;

		case 3:

			cur_attr = cur_attr | SMG$M_BOLD;
			inp = basic::right(inp, 2);
			//
			// Stop  Bold (\*)
			//
			break;

		case 4:

			cur_attr = cur_attr & (~SMG$M_BOLD);
			inp = basic::right(inp, 2);
			//
			// Start Underline (^&)
			// Start italicized (^~)
			//
			break;

		case 5:
		case 9:

			cur_attr = cur_attr | SMG$M_UNDERLINE;
			inp = basic::right(inp, 2);
			//
			// Stop  Underline (\&)
			// End italicized (\~)
			//
			break;

		case 6:
		case 10:

			cur_attr = cur_attr & (~SMG$M_UNDERLINE);
			inp = basic::right(inp, 2);
			//
			// Accept char (_)
			//
			break;

		case 7:

			this_text = inp.substr(0, 1);
			buildtext();
			inp = basic::right(inp, 2);
			//
			// Space char (#)
			//
			break;

		case 8:

			this_text = " ";
			buildtext();
			//
			// Tab Character
			//
			break;

		case 11:

			temp_V4 = total_text.size() + indent + left_mar;
			temp1 = 0;
			for (i = 32; i >= 1; i -= 1)
			{
				if (ts[i] > temp_V4)
				{
					temp1 = ts[i];
				}
			}
			if (temp1 > 80)
			{
				temp1 = 0;
			}
			this_text = std::string(temp1 - temp_V4, ' ');
			buildtext();
			break;

		}
		goto text1;
endtext:;
		return;
	};


	//
	// Assume success
	//
	Result = 1;
	// Left margin
	left_mar = 5;
	// Right Margin
	right_mar = 73;
	// Indent
	indent = 0;
	// In a list?
	list_list = 0;
	// In a table?
	table_table = 0;
	// In a note?
	note_note = 0;
	// In a note?
	note_quote = 0;
	// In a note?
	note_footnote = 0;
	// Fill/justify flag
	fill_flag = -1;
	// Current attributes
	cur_attr = 0;
	// Lowest attribute if none
	ary_attr_V1[0] = 0;
	// And starts at the first character
	ary_attp[0] = 0;
	//
	// Initilize tab stops
	//
	for (i = 1; i <= 31; i++)
	{
		ts[i] = 8 * i;
	}
	ts[32] = 32767;
	//
	// Initilization
	//
	curr_line = std::stol(code[0]);
	if (curr_line < 1)
	{
		curr_line = 0;
	}
	//
	// Set up the control structure if necessary
	//
	st = lbr$ini_control(lr_index, LBR$C_READ);
	if ((st & 1) == 0)
	{
		Result = st;
		return Result;
	}
	//
	// Open APP_TEXT library file
	//
	st = lbr$open(lr_index, lib_name, 0, ".TLB");
	if ((st & 1) == 0)
	{
		Result = st;
		goto exitprogram;
	}
	st = lbr$lookup_key(lr_index, key_name, txrfa);
	if ((st & 1) == 0)
	{
		Result = st;
		goto exitprogram;
	}
	//*******************************************************************
L_100:;
	// Read text in from input file
	//*******************************************************************
	text = std::string(150, ' ');
	st = lbr$get_record(lr_index, text);
	if ((st & 1) == 0)
	{
		breaktext();
		goto exitprogram;
	}
	inp = boost::trim_right_copy(text);
	//
	// Handle this line in whatever mode is necessary
	//
	if (literal)
	{
		literalline();
	}
	else
	{
		if (inp.substr(0, 1) == ".")
		{
			commandline();
		}
		else
		{
			textline();
			if (fill_flag == 0)
			{
				breaktext();
			}
		}
	}
	goto L_100;
	//*******************************************************************
	// Exit the program
	//*******************************************************************
exitprogram:;
	if (list_list != 0)
	{
		breaktext();
		this_text = "** Missing .END LIST";
		buildtext();
		breaktext();
	}
	if (table_table != 0)
	{
		breaktext();
		this_text = "** Missing .END TABLE";
		buildtext();
		breaktext();
	}
	if (note_note != 0)
	{
		breaktext();
		this_text = "** Missing .END NOTE";
		buildtext();
		breaktext();
	}
	if (note_quote != 0)
	{
		breaktext();
		this_text = "** Missing .END QUOTE";
		buildtext();
		breaktext();
	}
	if (note_footnote != 0)
	{
		breaktext();
		this_text = "** Missing .END FOOTNOTE";
		buildtext();
		breaktext();
	}
	code[0] = std::to_string(curr_line);
	code[curr_line + 1] = "";
	//
	// TEXT file(close)
	//
	st = lbr$close(lr_index);
	return Result;
}
