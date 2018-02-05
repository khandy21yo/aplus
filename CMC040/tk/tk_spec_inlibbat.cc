// %TITLE "Read Help Messages from the Source Code"
// %SBTTL "TK_SPEC_INSRTLIB"
// %IDENT "V3.6a Calico"
//
// ++
// Abstract:HELP
//	.p
//	Create documentation from source code.
//	.p
//	To do all, enter '*', 'P', '*'
//
// --
//
//
// Source: tk_spec_insrtlib.bas
// Translated from Basic to C++ using btran
// on Wednesday, October 25, 2017 at 17:57:51
//

#include <iostream>
#include <fstream>
#include <string>
#include <cstdlib>
#include <cstring>
#include <unistd.h>
#include "basicfun.h"

#include "preferences.h"
#include "cmcfun.h"


//
// Function Prototypes
//



int main(int argc, char **argv)
{
	long colon;
	std::string desc;
	long dexcl;
	long dir_loop;
	std::string direct;
	std::string filename;
	std::string file_name;
	long gls;
	long gts;
	long hlp;
	long i;
	long i_loop;
	long idx;
	long ii;
	long iii;
	long j;
	std::string k_ext;
	std::string key_name;
	std::string lib_name;
	long lin;
	std::string line;
	long loop;
	std::string module;
	std::string name;
	long pos1;
	long pos2;
	std::string prog_type;
	std::ifstream read_file_ch;
	std::string severity;
	long smg_status;
	long spc;
	long st;
	long stat;
	std::string text;
	std::string text_line;
	std::string title;
	long tpos1;
	long tpos2;
	long ttl;
	std::string type_s;
	std::ofstream writ_file_ch;
	std::string w_file_name;
	std::string ztext;

	std::vector<std::string> file_name_V1;
	std::vector<std::string> dir_name;
	std::string prefix[4];
	std::string task[4];
	std::string ext[4];
	std::string text_lines[501];
	std::string index_term[51];
	std::string index_lines[51];
	std::string gloss_lines[51];

	// Handle input/output file
	//
	w_file_name = "/tmp/" + read_sysjob() + ".TMP";

	//*******************************************************************
	// Get info from the user
	//*******************************************************************
	std::cout << "\tThis program will take the help messages from the source code" << std::endl;
	std::cout << "\tand place it in the help library on the REF:HELP_<Directory>.TLB." << std::endl;
	std::cout << "Directory:   ";
	std::cin >> direct;
	std::cout << "Program, File, or Menu: ";
	std::cin >> name;
	std::cout << "Module Name: ";
	std::cin >> module;
#if 0
	// Old VMS value
	find_file("SOURCE:[000000]*.DIR", dir_name, 16, "", "");
#else
	find_file(aplushome + "/aplus/CMC030/*", dir_name, 16, "", "");
#endif
	dir_loop = std::stol(dir_name[0]);
	task[1] = "P";
	task[2] = "F";
	task[3] = "M";
#if 0
	ext[1] = ".BAS";
	ext[2] = ".HLP";
	ext[3] = ".HLP";
#else
	ext[1] = ".bas";
	ext[2] = ".hlp";
	ext[3] = ".hlp";
#endif
	//*******************************************************************
	// Loop through all directories specified
	//*******************************************************************
	for (j = 1; j <= dir_loop; j++)
	{
		if (comp_string(dir_name[j], direct) == 0)
		{
			goto nextj;
		}
#if 0
		prefix[1] = std::string("SOURCE:[") + dir_name[j] + ".SOURCE]";
		prefix[2] = std::string("SOURCE:[") + dir_name[j] + ".OPEN]";
		prefix[3] = std::string("SOURCE:[") + dir_name[j] + "]";
#else
		prefix[1] = aplushome + "/aplus/CMC030/" +
			dir_name[j] + "/source/";
		prefix[2] = aplushome + "/aplus/CMC030/" +
			dir_name[j] + "/open/";
		prefix[3] = aplushome + "/aplus/CMC030/" +
			dir_name[j] + "/";
#endif
		if (dir_name[j].size() > 2)
		{
			lib_name = "help_default";
		}
		else
		{
			lib_name = std::string("help_") +
				dir_name[j];
		}
		//*******************************************************************
		// Loop through three possible extensions
		//*******************************************************************
		for (loop = 1; loop <= 3; loop++)
		{
			if (comp_string(task[loop], name) == 0)
			{
				goto nextloop;
			}
			find_file(prefix[loop] + module + ext[loop], 
				file_name_V1, 16, "", ext[loop]);
			i_loop = std::stol(file_name_V1[0]);
			//************************************************************
			// Loop through all files found
			//************************************************************
			for (i = 1; i <= i_loop; i++)
			{
				//
				// Program type
				//
				pos1 = file_name_V1[i].find("_", 0) + 2;
				pos2 = file_name_V1[i].find("_", pos1 - 1);
				prog_type = basic::Qseg(file_name_V1[i], pos1, pos2);
				//
				// Open file to read from
				//
				read_file_ch.open((prefix[loop] + 
					file_name_V1[i] + ext[loop]).c_str());
				if (!read_file_ch.is_open())
				{
					std::cout << "Unable to open " << file_name_V1[i] << std::endl;
					goto L_540;
				}
				//
				// Open the new file we are going to create
				//
				writ_file_ch.open(w_file_name.c_str());
				std::cout << std::string("Extacting from: ") + prefix[loop] + file_name_V1[i] << std::endl;
				desc = "";
				idx = lin = gls = 0;
				//**************************************************
				// Get the file Title in the first line
				//**************************************************
				try
				{
					std::getline(read_file_ch, text);
					if (read_file_ch.eof()) { throw basic::BasicError(11); }	// End of file on device
					if (read_file_ch.fail()) { throw basic::BasicError(11); }	// Fatal system I/O failure
				}
				catch(basic::BasicError &Be)
				{
					if (Be.err == 11)
					{
						goto L_540;
					}
					filename = file_name;
					goto helperror;
				}
				if ((text.find("%TITLE", 0) + 1))
				{
					tpos1 = text.find("\"", 0) + 2;
					tpos2 = text.find("\"", tpos1 - 1);
					title = basic::Qseg(text, tpos1, tpos2);
					lin = lin + 1;
					text_lines[lin] = std::string("^*") + title + "\\*";
				}
				severity = "";
				//**************************************************
				// Handle rest of file
				//**************************************************
				while (1)
				{
					try
					{
						std::getline(read_file_ch, text);
						if (read_file_ch.eof()) { throw basic::BasicError(11); }	// End of file on device
						if (read_file_ch.fail()) { throw basic::BasicError(11); }	// Fatal system I/O failure
					}
					catch(basic::BasicError &Be)
					{
						if (Be.err == 11)
						{
							goto L_540;
						}
						filename = file_name;
						goto helperror;
					}
					ztext = basic::edit(text,-1);
					//
					// Check for message key
					//
					if (ztext.find("!ABSTRACT:", 0) + 1)
					{
						severity = "H";
					}
					if (ztext.find("!WARNING:", 0) + 1)
					{
						severity = "W";
					}
					if (ztext.find("!INFORMATION:", 0) + 1)
					{
						severity = "I";
					}
					if (ztext.find("!SUCCESS:", 0) + 1)
					{
						severity = "S";
					}
					if (ztext.find("!ERROR:", 0) + 1)
					{
						severity = "E";
					}
					if (ztext.find("!FATALERROR:", 0) + 1)
					{
						severity = "F";
					}
					if (severity == "" || basic::right(text, text.find("!", 0) + 2)[0] != 32)
					{
						goto L_530;
					}
					colon = (ztext.find(":", 0) + 1);
					type_s = basic::right(ztext, colon + 1);
					key_name = severity + "$" + file_name_V1[i] + "$" + type_s;
					ttl = 0;
L_525:;
					try
					{
						std::getline(read_file_ch, text);
						if (read_file_ch.eof()) { throw basic::BasicError(11); }	// End of file on device
						if (read_file_ch.fail()) { throw basic::BasicError(11); }	// Fatal system I/O failure
					}
					catch(basic::BasicError &Be)
					{
						if (Be.err == 11)
						{
							goto L_540;
						}
						filename = file_name;
						goto helperror;
					}
					ztext = basic::edit(text,-1);
					if (ztext == "!")
					{
						goto L_525;
					}
					if (ztext == "!INDEX:")
					{
						idx = 1;
						goto L_525;
					}
					if (ztext == "!--" ||
						((text.find(":", 0) + 1) != 0 && 
						 basic::right(text, (text.find("!", 0) + 1) + 1)[0] == 32))
					{
						goto subcode;
					}
					text_line = basic::right(boost::trim_right_copy(text), 4);
					dexcl = 0;
checkexcl:;
					dexcl = (text_line.find("!!", dexcl + 1 - 1) + 1);
					if (dexcl)
					{
						text_line = text_line.substr(0, dexcl) + basic::right(text_line, dexcl + 2);
						goto checkexcl;
					}
					if (idx == 0)
					{
						lin = lin + 1;
						text_lines[lin] = text_line;
						if (loop == 3 && ttl == 0)
						{
							tpos1 = (text_line.find("^*", 0) + 1) + 2;
							tpos2 = (text_line.find("\\*", tpos1 - 1) + 1) - 1;
							title = basic::Qseg(text_line, tpos1, tpos2);
							ttl = -1;
						}
					}
					else
					{
						if (basic::edit(text_line, -1).substr(0, 2) == ".Y")
						{
							gls = gls + 1;
							gloss_lines[gls] = text_line;
						}
						else
						{
							spc = (text_line.find(" ", 0) + 1);
							gts = ((text_line + ">").find(">", 0) + 1);
							index_term[idx] = basic::edit(basic::Qseg(text_line, spc + 1, gts - 1), -1);
							index_lines[idx] = text_line;
							idx = idx + 1;
						}
					}
					goto L_525;
L_530:;
				}
L_540:;
				writ_file_ch.close();
				read_file_ch.close();
			}
nextloop:;
			writ_file_ch.close();
			read_file_ch.close();
		}
nextj:;
	}
exitprogram:;
	//*******************************************************************
	// Exit program
	//*******************************************************************
	smg_status = unlink(w_file_name.c_str());
	goto endprogram;
subcode:;
	//*******************************************************************
	// ??? Who knows ???
	//*******************************************************************
	for (ii = 1; ii <= lin; ii++)
	{
		writ_file_ch << text_lines[ii] << std::endl;
		if (ii == 1)
		{
			for (iii = 1; iii <= idx - 1; iii++)
			{
				writ_file_ch << index_lines[iii] << std::endl;
			}
		}
	}
	for (ii = 1; ii <= gls; ii++)
	{
		writ_file_ch << gloss_lines[ii] << std::endl;
	}
	idx = lin = gls = 0;
	//
	// If help message for menu screen
	//
	if (loop == 3)
	{
		writ_file_ch << ".!!" << std::endl;
		writ_file_ch << ".!.SCREEN " << type_s <<
			"$SCREEN" << "\\caption{" << title << 
			" Menu Screen}" << 
			std::endl;
		goto opt;
	}
	hlp = (type_s.find("HELP", 0) + 1);
	if (hlp == 0 && type_s != "COMMAND")
	{
		goto opt;
	}
	if (hlp > 1)
	{
		k_ext = std::string("$") + type_s.substr(0, hlp - 2);
	}
	else
	{
		k_ext = "";
	}
	// ** Converted from a select statement **
	if ((prog_type == "RPRT") ||
		((prog_type == "POST") ||
		(prog_type == "FORM")))
	{
		writ_file_ch << ".!!" << std::endl;
		writ_file_ch << ".!.SCREEN " << 
			file_name_V1[i] <<
			k_ext << "$SCREEN" << 
			"\\caption{" << title << " Screen}" << std::endl;
		writ_file_ch << ".!.FIELD H$" << 
			file_name_V1[i] << k_ext << "$FLD*" << std::endl;
		writ_file_ch << ".!!" << std::endl;
		line = std::string(".!.SCREEN ") + file_name_V1[i] + k_ext +
			"$REPORT" + "\\caption{" + title;
		// Add Report to title if not last word in title
		if (0 == (title.find(" Report", title.size() - 11) + 1))
		{
			line = line + " Report";
		}
		writ_file_ch << line + "}" << std::endl;
	}
	else
	{
		if (prog_type.size() != 4)
		{
			goto opt;
		}
		writ_file_ch << ".!!" << std::endl;
		writ_file_ch << std::string(".!.SCREEN ") + file_name_V1[i] + 
			k_ext + "$SCREEN" + "\\caption{" + 
			title + " Screen}" << std::endl;
		writ_file_ch << std::string(".!.FIELD H$") + file_name_V1[i] + 
			k_ext + "$FLD*" << std::endl;
	}
	//*******************************************************************
	// ??? Who knows ???
	//*******************************************************************
opt:;
	if (basic::edit(text, -1) != "!OPTION:")
	{
		goto putlib;
	}
L_18510:;
	std::getline(read_file_ch, text);
	if (read_file_ch.eof()) { throw basic::BasicError(11); }	// End of file on device
	if (read_file_ch.fail()) { throw basic::BasicError(12); }	// Fatal system I/O failure
	if (basic::edit(text, -1) == "!")
	{
		goto L_18510;
	}
	if (basic::edit(text, -1) == "!--" || 
		((text.find(":", 0) + 1) != 0 && 
		basic::right(text, (text.find("!", 0) + 1) + 1)[0] == 32))
	{
		goto putlib;
	}
	writ_file_ch << ".!!" << std::endl;
	writ_file_ch << std::string(".!.OPTION H$") + 
		basic::right(boost::trim_left_copy(text), 3) << std::endl;
	goto L_18510;
putlib:;
	//*******************************************************************
	// Place in library
	//*******************************************************************
	writ_file_ch.close();
	st = libr_3insert(lib_name, w_file_name, key_name);
	std::cout << std::string(12, ' ') + 
		(key_name + basic::Qstring(40, '.')).substr(0, 40) + 
		lib_name << std::endl;

	smg_status = unlink(w_file_name.c_str());

	//*******************************************************************
	// Reopen the new file we are going to create
	//*******************************************************************
	writ_file_ch.open(w_file_name.c_str());
	severity = "";
	goto L_530;

helperror:;
	//*******************************************************************
	// Help Message for an Error
	//*******************************************************************
	std::cout << "ERR" << '\t' << filename << '\t' << 
		std::string("ERROR") << std::endl;
	goto exitprogram;

endprogram:;

	return EXIT_SUCCESS;
}
