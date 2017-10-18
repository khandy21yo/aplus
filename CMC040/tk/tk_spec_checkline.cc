//
// Source: ../CMC030/tk/source/tk_spec_checkline.bas
// Translated from Basic to C++ using btran
// on Monday, October 16, 2017 at 16:12:56
//

#include <iostream>
#include <string>
#include <cstdlib>
#include <cstring>
#include <unistd.h>
#include "basicfun.h"
#include <fstream>



//
// Function Prototypes
//


int main(int argc, char **argv)
{
	long andsign;
	long asciiquote;
	long badcmc;
	long badcomit;
	long blankline;
	long casetab;
	long commentout;
	long context;
	long countline;
	long elsexxx;
	long equaltab;
	std::string errorlist;
	std::string extreme;
	std::string file_name;
	std::ifstream finam_ch;
	long foundselect;
	long i;
	long i1;
	long i2;
	long iftab;
	long incomment;
	long index;
	long intheform;
	long inthereport;
	std::string junk;
	long k;
	long killflag;
	long l;
	long l1;
	long last_x;
	long last_x1;
	long lookforselect;
	long lookforslect;
	long name_flag;
	long num;
	std::string numx;
	long num_found;
	long num_line;
	long onerrorgoto;
	long ordisplay;
	long pageline;
	long providesthe;
	long ptr;
	long scopestruct;
	long search_num;
	long sel_err;
	long settinginthe;
	long spacecomma;
	long spacecomment;
	long spaceend;
	long spacefront;
	long spacetab;
	long tabcomma;
	long tabend;
	long tabequal;
	long tabspace;
	long tailelse;
	long tailendif;
	long tailthen;
	std::string temp;
	std::string text;
	std::string text1;
	long thecontents;
	long thenxxx;
	long v;
	long valueentered;
	long whenerror;
	std::string x;
	long x_V1;
	std::string xfilename;

	BStack(20);
	long sys_status;
	long line_num[1001];
	std::string name_func[201];
// #pragma psect static_rw z,gbl,ovr
	std::string name_buffer;
// #pragma psect end z
	// %TITLE "Check Line Numbers"
	// %SBTTL "TK_SPEC_CHECKLINE"
	// %IDENT "V3.6a Calico"
	//
	// ++
	// Abstract:HELP
	//	.p
	//	This program is used to clean out garbage in
	//	other source codes, and to look for problems
	//	that may not be readily apparent.
	//	It is an attempt to produce better code by losing
	//	unnecessary stuff from source files.
	//	.p
	//	It checks other programs to
	//	make sure all of it's line numbers are
	//	in order, that error trapping is for
	//	existing lines, and
	//	all of it's declared functions
	//	are being used.
	//	.note
	//	This is not a very intelligent program.  It looks
	//	at things in ^~very\~ simple terms.  It is only an indication
	//	of possible problems, and is not a final authority.
	//	Please do not trust what this program tells you implictly.
	//	.end note
	//
	// Index:
	//
	// Option:
	//
	// Author:
	//
	//	09/29/88 - J. Shad Rydalch
	//
	//	07/11/91 - Jeff C. Beard
	//		Code for unused functions.
	//
	// --
#include "lib$routines.h"

L_100:;
	std::cout << std::endl;
	//
	// Get wildcard directory
	//
	try
	{
		std::cout << "Wildcard name to check (return to exit) <.BAS>";
		getline(std::cin, file_name);
	}
	catch(basic::BasicError &Be)
	{
		if (Be.err == 11)
		{
			goto L_5000;
		}
		throw Be;
	}
	if (basic::edit(file_name, -1) == "")
	{
		goto exitprogram;
	}
	context = 0;
	junk = "*.BAS";
	try
	{
		std::cout << "Extreme Tests <N>";
		getline(std::cin, extreme);
	}
	catch(basic::BasicError &Be)
	{
		if (Be.err == 11)
		{
			goto L_5000;
		}
		throw Be;
	}
	extreme = basic::edit(extreme, -1).substr(0, 1);
	//
L_200:;
	// Look up one file
	//
	sys_status = lib$find_file(file_name, name_buffer, context, junk);
	junk = "";
	if ((sys_status & 1) == 0)
	{
		goto L_100;
	}
	xfilename = boost::trim_right_copy(name_buffer);
	BGosub(L_4000);
	//
	// Set up for next file
	//
	goto L_200;
	//*******************************************************************
L_4000:;
	// Scan one file
	//*******************************************************************
	last_x = 0;
	last_x1 = 0;
	x_V1 = 0;
	name_flag = -1;
	num_line = 0;
	sel_err = 0;
	lookforselect = 0;
	foundselect = 0;
	spacefront = 0;
	blankline = 0;
	spaceend = 0;
	tabend = 0;
	spacecomment = 0;
	commentout = 0;
	spacetab = 0;
	spacecomma = 0;
	tabcomma = 0;
	tabspace = 0;
	thenxxx = 0;
	elsexxx = 0;
	iftab = 0;
	errorlist = "";
	andsign = 0;
	badcmc = 0;
	badcomit = 0;
	tailthen = 0;
	tailelse = 0;
	tailendif = 0;
	casetab = 0;
	equaltab = 0;
	tabequal = 0;
	pageline = 0;
	onerrorgoto = 0;
	whenerror = 0;
	scopestruct = 0;
	countline = 0;
	killflag = 0;
	asciiquote = 0;
	providesthe = 0;
	ordisplay = 0;
	inthereport = 0;
	thecontents = 0;
	intheform = 0;
	valueentered = 0;
	settinginthe = 0;
	incomment = 0;
	finam_ch.open(xfilename.c_str());
	if (finam_ch.bad())
	{
		std::cout << xfilename + "  " + "Error opening file" << std::endl;
		goto L_4900;
	}
L_4100:;
	try
	{
		if (finam_ch.eof()) { throw basic::BasicError(11); }	// End of file on device
		if (finam_ch.bad()) { throw basic::BasicError(12); }	// Fatal system I/O failure
		getline(finam_ch, text);
	}
	catch(basic::BasicError &Be)
	{
		if (Be.err != 11)
		{
			if (name_flag)
			{
				std::cout << std::endl;
				std::cout << xfilename << std::endl;
				name_flag = 0;
			}
			std::cout << "Error reading line" <<
				" for line " + basic::Qnum(num) << std::endl;
		}
		goto L_4900;
	}
	countline = countline + 1;
	//
	// Watch for %PAGE calls
	//
	if (basic::edit(text, -1) == "%PAGE")
	{
		//
		// Use an arbitrary limit for lines between %PAGE's
		//
		if (pageline < 8)
		{
			BGosub(printname);
			std::cout << "  %PAGE's very close together" << std::endl;
		}
		pageline = 0;
	}
	else
	{
		pageline = pageline + 1;
	}
	if ((std::string(" 0123456789").find(text.substr(0, 1), 0) + 1) >= 2)
	{
		//
		//	FIND LAST DIGIT
		//
		i = 1;
		while ((std::string(" 0123456789").find(basic::mid(text, i + 1, 1), 0) + 1) > 1)
		{
			i = i + 1;
		}
		x_V1 = std::stol(text.substr(0, i));
		text = basic::right(text, i + 1);
		if (x_V1 < last_x)
		{
			BGosub(printname);
			std::cout << "  Line out of sequence " << x_V1 << std::endl;
		}
		if (x_V1 == last_x)
		{
			BGosub(printname);
			std::cout << "  Line number duplicated " << x_V1 << std::endl;
		}
		//
		// Load all line numbers into array
		//
		num_line = num_line + 1;
		line_num[num_line] = x_V1;
	}
	if (x_V1 == 0)
	{
		BGosub(printname);
		std::cout << "  Missing first line number" << std::endl;
		x_V1 = -1;
	}
	if ((text.find("!""++", 0) + 1))
	{
		incomment = 1;
	}
	if ((text.find("!""--", 0) + 1))
	{
		incomment = 0;
	}
	if (incomment != 0)
	{
		if ((text.find("provides the ""means", 0) + 1) | (text.find("provides a ""means", 0) + 1) | (text.find("provides for ""entry", 0) + 1))
		{
			providesthe = 1;
		}
		if ((text.find("or ""display", 0) + 1) | (text.find("or be ""display", 0) + 1))
		{
			ordisplay = 1;
		}
		if ((text.find("in the ""report", 0) + 1) | (text.find("of the ""report", 0) + 1))
		{
			inthereport = 1;
		}
		if ((text.find("he contents ""of", 0) + 1))
		{
			thecontents = 1;
		}
		if ((text.find("in the ""form", 0) + 1) | (text.find("of the ""form", 0) + 1))
		{
			intheform = 1;
		}
		if ((text.find("value entered ""in", 0) + 1))
		{
			valueentered = 1;
		}
		if ((text.find("he setting ""in the", 0) + 1))
		{
			settinginthe = 1;
		}
	}
	if ((text.find("  ""- Kevin", 0) + 1) & extreme == "Y")
	{
		BGosub(printname);
		std::cout << "  Bad Modification Comment" << std::endl;
	}
	if ((text.find("\tIF\t", 0) + 1))
	{
		iftab = -1;
	}
	//
	// Check for goofy if-then-else-endif sequences.
	//
	text1 = basic::edit(text, 8 + 16 + 32 + 128);
	if (text1.substr(0, 5) == "KILL ")
	{
		killflag = -1;
	}
	if (text1.substr(0, 5) == "THEN")
	{
		if (basic::right(text1, 6) != "")
		{
			thenxxx = -1;
		}
	}
	if (text1.substr(0, 5) == "ELSE")
	{
		if (basic::right(text1, 6) != "")
		{
			elsexxx = -1;
		}
	}
	//
	// Strip off quoted strings
	//
	l = text.find_first_of("'""\"") + 1;
	while (l)
	{
		x = basic::mid(text, l, 1);
		l1 = (text.find(x, l + 1 - 1) + 1);
		if (l1 == 0)
		{
			l1 = text.size();
		}
		text = text.substr(0, l - 1) + "X" + basic::right(text, l1 + 1);
		l = text.find_first_of("'""\"") + 1;
	}
	//
	// Check for <Tab><,>
	//
	l = (text.find("\t"",", 0) + 1);
	if (l != 0)
	{
		if (boost::trim_right_copy(text.substr(0, l - 1)) != "")
		{
			tabcomma = -1;
		}
	}
	//
	// Check for CMC spelled wrong
	//
	if ((text.find("Computer management Center", 0) + 1))
	{
		badcmc = -1;
	}
	//
	// Check for commitment spelled wrong
	//
	if ((text.find("committment", 0) + 1))
	{
		badcomit = -1;
	}
	i = (text.find("=""\t", 0) + 1);
	if (i)
	{
		equaltab = -1;
	}
	i = ((text.find("\t""=", 0) + 1) & ((text.find("::", 0) + 1) == 0));
	if (i)
	{
		tabequal = -1;
	}
	//
	// Pull in more if there is a continuation
	//
	while (basic::right(text, text.size()) == "&")
	{
		if ((std::string(" \t\\").find(basic::mid(text, text.size() - 1, 1), 0) + 1) == 0)
		{
			andsign = -1;
		}
		if (finam_ch.eof()) { throw basic::BasicError(11); }	// End of file on device
		if (finam_ch.bad()) { throw basic::BasicError(12); }	// Fatal system I/O failure
		getline(finam_ch, text1);
		//
		// Strip off quoted strings
		//
		l = text1.find_first_of("'""\"") + 1;
		while (l)
		{
			x = basic::mid(text1, l, 1);
			l1 = (text1.find(x, l + 1 - 1) + 1);
			if (l1 == 0)
			{
				l1 = text1.size();
			}
			text1 = text1.substr(0, l - 1) + "X" + basic::right(text1, l1 + 1);
			l = text1.find_first_of("'""\"") + 1;
		}
		//
		// Check for <Tab><,>
		//
		l = (text1.find("\t"",", 0) + 1);
		if (l != 0)
		{
			if (boost::trim_right_copy(text1.substr(0, l - 1)) != "")
			{
				tabcomma = -1;
			}
		}
		i = (text1.find("=""\t", 0) + 1);
		if (i)
		{
			equaltab = -1;
		}
		i = ((text.find("\t""=", 0) + 1) & ((text.find("::", 0) + 1) == 0));
		if (i)
		{
			tabequal = -1;
		}
		text = boost::trim_right_copy(text.substr(0, text.size() - 1)) + text1;
		//		TEXT$ = TEXT$ + TEXT1$
	}
	//
	// Check for various useless whitespace
	//
	if ((text != "") && (basic::edit(text, -1) == ""))
	{
		blankline = -1;
	}
	else
	{
		if ((text.find(" ""\t", 0) + 1))
		{
			spacetab = -1;
		}
		if (((text.find(" ,", 0) + 1) != 0) && ((text.find(", ,", 0) + 1) == 0))
		{
			spacecomma = -1;
		}
		i = (text.find("\t"" ", 0) + 1);
		if (i)
		{
			i1 = (text.find("!", 0) + 1);
			if ((i1 == 0) || (i1 > i))
			{
				tabspace = -1;
			}
		}
		i = (text.find("CASE""\t", 0) + 1);
		if (i)
		{
			casetab = -1;
		}
		if (text[0] == 32)
		{
			if (text.substr(0, 2) == " !")
			{
				commentout = -1;
			}
			else
			{
				//
				// Don't display error if it is a label
				//
				if ((text.find(":", 0) + 1) == 0)
				{
					spacefront = -1;
				}
			}
		}
		i = basic::right(text, text.size())[0];
		if (i == 32)
		{
			spaceend = -1;
		}
		if (i == 9)
		{
			tabend = -1;
		}
	}
	//
	// Past line 19999, stop looking for error trapping
	//
	if (text == "\tEND SELECT")
	{
		lookforslect = 0;
		foundselect = 0;
	}
	//
	// Lose any junk spaces
	//
	text1 = basic::edit(text, -1);
	if ((text.find("!""  ", 0) + 1) | (text.find("! ""\t", 0) + 1))
	{
		spacecomment = -1;
	}
	//
	// Lose comments
	//
	i = (text1.find("!", 0) + 1);
	if (i)
	{
		if ((text1.find(".TABLE 3.25", i - 1) + 1))
		{
			BGosub(printname);
			std::cout << "  Table 3.25" << std::endl;
		}
		text1 = text1.substr(0, i - 1);
	}
	//
	// Check for ASCII("x")
	//
	if ((text1.find("ASCII(\"", 0) + 1))
	{
		asciiquote = -1;
	}
	if ((text1.find("ASCII('", 0) + 1))
	{
		asciiquote = -1;
	}
	//
	// Lose any text strings
	//
	i = (text1.find("\"", 0) + 1);
	i1 = (text1.find("\"", i + 1 - 1) + 1);
	while (i)
	{
		if (i1)
		{
			text1 = text1.substr(0, i - 1) + basic::right(text1, i1 + 1);
			i = (text1.find("\"", i + 1 - 1) + 1);
			i1 = (text1.find("\"", i + 1 - 1) + 1);
		}
		else
		{
			text1 = text1.substr(0, i - 1);
			i = 0;
		}
	}
	//
	// Check for "xxx THEN xxx"
	//
	if ((text1 != "THEN") && (((std::string(" ") + text1 + " ").find(" THEN ", 0) + 1) != 0) && (thenxxx == 0))
	{
		tailthen = -1;
	}
	//
	// Check for "xxx ELSE xxx"
	//
	if (((text1 != "ELSE") && (text1 != "CASE ELSE")) && (((std::string(" ") + text1 + " ").find(" ELSE ", 0) + 1) != 0) && (elsexxx == 0))
	{
		tailelse = -1;
	}
	//
	// Check for "xxx END IF xxx"
	//
	if ((text1 != "END IF") && (((std::string(" ") + text1 + " ").find(" END IF ", 0) + 1) != 0))
	{
		tailendif = -1;
	}
	//
	// Search for external definitions
	//
	if ((text1.find("EXTERNAL", 0) + 1))
	{
		v = (text1.find("FUNCTION", 0) + 1);
		if (v)
		{
			temp = basic::right(text1, v + 8);
			BGosub(functionnames);
			goto L_4100;
		}
	}
	//
	// Check for old "ON ERROR GOTO" error trapping
	//
	if ((text1.find("ONERRORGOTO", 0) + 1))
	{
		if ((text1.find("ONERRORGOTO%", 0) + 1) == 0)
		{
			onerrorgoto = onerrorgoto + 1;
		}
	}
	if ((text1.find("WHENERRORIN", 0) + 1))
	{
		whenerror = whenerror + 1;
	}
	//
	// Check for SCOPE and SCOPE_STRUCT
	//
	//	Leave these two items on the same line so that it
	//	doesn't show up as an error when they are seen.
	//
	if ((text1.find("SCOPE", 0) + 1) & ((text1.find("SCOPE_STRUCT", 0) + 1) == 0))
	{
		scopestruct = scopestruct | 1;
	}
	if ((text1.find("SCOPE_STRUCT", 0) + 1))
	{
		scopestruct = scopestruct | 2;
	}
	//
	// See if external functions are used here
	//
	for (search_num = 0; search_num <= last_x1 - 1; search_num++)
	{
		if (name_func[search_num] != "")
		{
			//
			// MAKES IT BLANK IF IT'S NOT THE DECLARATION STATEMENT
			//
			if ((text1.find(name_func[search_num], 0) + 1))
			{
				name_func[search_num] = "";
			}
		}
	}
	if (lookforselect)
	{
		goto looktrap;
	}
	//
	// At line 19000, so start looking for error trapping
	//
	// (X% = 19000%) OR
	if ((text1.find("SELECTERL", 0) + 1) != 0)
	{
		lookforselect = -1;
		foundselect = -1;
	}
	last_x = x_V1;
	goto L_4100;
looktrap:;
	if (lookforselect)
	{
		if ((text1.find("SELECTERL", 0) + 1))
		{
			foundselect = -1;
			goto L_4100;
		}
		if ((text1.find("SELECT", 0) + 1))
		{
			foundselect = ~foundselect;
		}
	}
	if (foundselect)
	{
		ptr = (text1.find("CASE", 0) + 1);
		if (ptr)
		{
			ptr = ptr + 4;
			BGosub(pullline);
			while (num)
			{
				BGosub(extratrap);
				if (basic::mid(text1, ptr, 1) == ",")
				{
					ptr = ptr + 1;
					BGosub(pullline);
				}
				else
				{
					num = 0;
				}
			}
			goto L_4100;
		}
		ptr = (text1.find("ERL=", 0) + 1);
		if (ptr)
		{
			ptr = ptr + 4;
			BGosub(pullline);
			BGosub(extratrap);
			goto L_4100;
		}
	}
	goto L_4100;
	//*******************************************************************
L_4900:;
	// Finish up
	//*******************************************************************
	finam_ch.close();
	//
	// Program too short
	//
	if (countline < 5)
	{
		BGosub(printname);
		std::cout << "  Source code seems to be too short" << std::endl;
	}
	//
	// Print out unused functions
	//
	for (i = 0; i <= last_x1 - 1; i++)
	{
		if (!(name_func[i] == ""))
		{
			BGosub(printname);
			std::cout << "  Function declared but not used: " << name_func[i] << std::endl;
		}
	}
	if (spacefront)
	{
		BGosub(printname);
		std::cout << "  Spaces at front of lines instead of tabs" << std::endl;
	}
	if (blankline)
	{
		BGosub(printname);
		std::cout << "  Lines consisting of whitespace" << std::endl;
	}
	if (spaceend)
	{
		BGosub(printname);
		std::cout << "  Lines ending with spaces" << std::endl;
	}
	if (tabend)
	{
		BGosub(printname);
		std::cout << "  Lines ending with tabs" << std::endl;
	}
	if (spacecomment)
	{
		BGosub(printname);
		std::cout << "  Spaces instead of tabs used to format comments" << std::endl;
	}
	//	IF COMMENTOUT%
	//	THEN
	//		GOSUB PrintName
	//		PRINT "  Lines commented out"
	//	END IF
	if (spacetab)
	{
		BGosub(printname);
		std::cout << "  <Space><tab> sequence seen" << std::endl;
	}
	if (spacecomma)
	{
		BGosub(printname);
		std::cout << "  <Space><,> sequence seen" << std::endl;
	}
	if (tabcomma)
	{
		BGosub(printname);
		std::cout << "  <Tab><,> sequence seen" << std::endl;
	}
	if (casetab)
	{
		BGosub(printname);
		std::cout << "  CASE<tab> sequence seen" << std::endl;
	}
	if (equaltab)
	{
		BGosub(printname);
		std::cout << "  =<tab> sequence seen" << std::endl;
	}
	if (tabequal)
	{
		BGosub(printname);
		std::cout << "  <tab>= sequence seen" << std::endl;
	}
	if (andsign)
	{
		BGosub(printname);
		std::cout << "  <&> not preceded by whitespace" << std::endl;
	}
	if (tabspace)
	{
		BGosub(printname);
		std::cout << "  <tab><Space> sequence seen" << std::endl;
	}
	if (thenxxx)
	{
		BGosub(printname);
		std::cout << "  'THEN' not alone on a line" << std::endl;
	}
	if (elsexxx)
	{
		BGosub(printname);
		std::cout << "  'ELSE' not alone on a line" << std::endl;
	}
	if (asciiquote)
	{
		BGosub(printname);
		std::cout << "  ASCII('X') should be A'X'B" << std::endl;
	}
	if (iftab)
	{
		BGosub(printname);
		std::cout << "  IF<tab> Seen" << std::endl;
	}
	if (badcmc)
	{
		BGosub(printname);
		std::cout << "  Computer >m<anagement Center" << std::endl;
	}
	if (badcomit)
	{
		BGosub(printname);
		std::cout << "  Commitment spelled Committment" << std::endl;
	}
	if (tailthen)
	{
		BGosub(printname);
		std::cout << "  'THEN' not on it's own line" << std::endl;
	}
	if (tailelse)
	{
		BGosub(printname);
		std::cout << "  'ELSE' not on it's own line" << std::endl;
	}
	if (tailendif)
	{
		BGosub(printname);
		std::cout << "  'ENDIF' not on it's own line" << std::endl;
	}
	if ((onerrorgoto != 0) && (extreme == "Y") && (whenerror == 0))
	{
		BGosub(printname);
		std::cout << "  Uses old 'ON ERROR"" GOTO'" << std::endl;
	}
	if (scopestruct == 1)
	{
		BGosub(printname);
		std::cout << "  Uses SCOPE but never references SCOPE_STRUCT" << std::endl;
	}
	if (killflag == 1)
	{
		BGosub(printname);
		std::cout << "  Uses KILL instead of LIB$DELETE_FILE" << std::endl;
	}
	if (providesthe != 0)
	{
		BGosub(printname);
		std::cout << "  Has 'provides"" the means to' in documentation" << std::endl;
	}
	if (ordisplay != 0)
	{
		BGosub(printname);
		std::cout << "  Has 'or ""display' in documentation" << std::endl;
	}
	if (inthereport != 0)
	{
		BGosub(printname);
		std::cout << "  Has 'in the ""report' in documentation" << std::endl;
	}
	if (thecontents != 0)
	{
		BGosub(printname);
		std::cout << "  Has 'the contents ""of' in documentation" << std::endl;
	}
	if (intheform != 0)
	{
		BGosub(printname);
		std::cout << "  Has 'in the ""form' in documentation" << std::endl;
	}
	if (valueentered != 0)
	{
		BGosub(printname);
		std::cout << "  Has 'value entered ""in' in documentation" << std::endl;
	}
	if (settinginthe != 0)
	{
		BGosub(printname);
		std::cout << "  Has 'The setting ""in the' in documentation" << std::endl;
	}
	BReturn;

	//*******************************************************************
L_5000:;
	// Exit Program
	//*******************************************************************
exitprogram:;
	goto L_32767;
	//*******************************************************************
	// Check error trap lines
	//*******************************************************************
extratrap:;
	//
	// If we aren't trapping errors, this doesn't mean anything
	//
	if (onerrorgoto == 0)
	{
		BReturn;

	}
	num_found = 0;
	for (index = 1; index <= num_line; index++)
	{
		if (num == line_num[index])
		{
			num_found = 1;
		}
	}
	if (num_found == 0)
	{
		BGosub(printname);
		std::cout << std::string("  Error trapping for line ") + basic::Qnum(num) + ", which does not exist" << std::endl;
	}
	numx = std::string("!") + std::to_string(num) + "!";
	if ((errorlist.find(numx, 0) + 1))
	{
		BGosub(printname);
		std::cout << std::string("  Duplicate Error Trap for line ") + basic::Qnum(num) << std::endl;
	}
	errorlist = errorlist + numx;
	BReturn;

	//*******************************************************************
	// Print out program name, unless it's already been printed.
	//*******************************************************************
printname:;
	if (name_flag)
	{
		std::cout << std::endl;
		std::cout << xfilename << std::endl;
		name_flag = 0;
	}
	BReturn;

	//*******************************************************************
	// Pull line number off of program
	//*******************************************************************
pullline:;
	k = ptr;
	while (!((std::string(" 0123456789").find(basic::mid(text1, ptr + 1, 1), 0) + 1) <= 1 || ptr >= text1.size()))
	{
		ptr = ptr + 1;
	}
	try
	{
		num = std::stoi(basic::Qseg(text1, k, ptr));
	}
	catch(basic::BasicError &Be)
	{
		num = 0;
	}
	ptr = ptr + 1;
	while (!((std::string(" %").find(basic::mid(text1, ptr, 1), 0) + 1) <= 1 || ptr >= text1.size()))
	{
		ptr = ptr + 1;
	}
	BReturn;

	//*******************************************************************
	// Rip function names off of source lines
	//*******************************************************************
functionnames:;
	//
L_17210:;
	// Strip off parameter declarations
	//
	i1 = (temp.find("()", 0) + 1);
	if (i1 != 0)
	{
		temp = temp.substr(0, i1 - 1) + basic::right(temp, i1 + 2);
		goto L_17210;
	}
	i1 = (temp.find("(", 0) + 1);
	i2 = (temp.find(")", i1 - 1) + 1);
	if ((i1 != 0) && (i2 != 0))
	{
		temp = temp.substr(0, i1 - 1) + basic::right(temp, i2 + 1);
		goto L_17210;
	}
	//
	// Pull off all function names, even if seperated by commas.
	//
	temp = temp + ",";
	while ((temp.find(",", 0) + 1))
	{
		i1 = (temp.find(",", 0) + 1);
		name_func[last_x1] = basic::edit(temp.substr(0, i1 - 1), -1);
		last_x1 = last_x1 + 1;
		temp = basic::right(temp, i1 + 1);
	}
	//
	// We is done.
	//
	BReturn;

L_32767:;

	return EXIT_SUCCESS;
}
