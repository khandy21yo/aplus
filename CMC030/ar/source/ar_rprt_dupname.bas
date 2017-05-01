1	%TITLE "Duplicate Name Search Report"
	%SBTTL "AR_RPRT_DUPNAME"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1993 BY
	!
	! Computer Management Center, Inc.
	! Idaho Falls, Idaho.
	!
	! This software is furnished under a license and may be used and
	! copied only in accordance with terms of such license and with
	! the inclusion of the above copyright notice.  This software or
	! any other copies thereof may not be provided or otherwise made
	! available to any other person.  No title to and ownership of
	! the software is hereby transferred.
	!
	! The information in this software is subject to change without
	! notice and should not be construed as a commitment by
	! Computer Management Center, Inc.
	!
	! CMC assumes no responsibility for the use or reliability of
	! its software on equipment which is not supported by CMC.
	!
	!++
	! ID:AR051
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This report searches for duplicate names in a master
	!	file.
	!	This program searches for duplicates based on the
	!	customer name, city, state, and zip.
	!	It searches the entire master file, there is no option
	!	to limit its search area.
	!	.lm -5
	!
	! Index:
	!	.x Labels>Customer
	!	.x Customer>Labels
	!	.x Print>Customer Labels
	!
	! Option:
	!
	! Author:
	!
	!	08/13/93 - Kevin Handy
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_RPRT_DUPNAME/LINE
	!	$ LINK/EXECUTABLE=AR_EXE:*.EXE AR_RPRT_DUPNAME, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AR_RPRT_DUPNAME.OBJ;*
	!
	! Modification history:
	!
	!	08/17/93 - Kevin Handy
	!		Improved translation tables.
	!
	!	10/28/93 - Kevin Handy
	!		Modified table to increase matches.
	!
	!	01/10/94 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/25/94 - Kevin Handy
	!		Modified for more matches
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/29/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	05/05/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	10/30/2000 - Kevin Handy
	!		Use A"x"B
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP (AR_35CUSTOM)	AR_35CUSTOM_CDD		AR_35CUSTOM

	RECORD SORTFILE_STRUCT
		STRING	CUSNUM = 10
		STRING	CUSNAM = 50
		STRING	ADD1 = 25
		STRING	CITY = 15
		STRING	STATE = 2
		STRING	ZIP = 5
	END RECORD

	MAP (AR_SORTFILE)	SORTFILE_STRUCT		AR_SORTFILE
	MAP (AR_SORTFILE_LAST)	SORTFILE_STRUCT		AR_SORTFILE_LAST

	!
	! Dimensions
	!
	DIM TRANSOURCE$(150%), TRANDEST$(150%)

	%PAGE

	!
	! Translation data. Try to convert to a shorter version, maybe
	! more addresses will match then.
	!
	! Some funny spellings in this list are caused by translations
	! occuring before them.
	!
	DATA	".",		" ", &
		"-",		" ", &
		" A ",		" ", &
		" B ",		" ", &
		" C ",		" ", &
		" D ",		" ", &
		" E ",		" ", &
		" F ",		" ", &
		" G ",		" ", &
		" H ",		" ", &
		" I ",		" ", &
		" J ",		" ", &
		" K ",		" ", &
		" L ",		" ", &
		" M ",		" ", &
		" N ",		" ", &
		" O ",		" ", &
		" P ",		" ", &
		" Q ",		" ", &
		" R ",		" ", &
		" S ",		" ", &
		" T ",		" ", &
		" U ",		" ", &
		" V ",		" ", &
		" W ",		" ", &
		" X ",		" ", &
		" Y ",		" ", &
		" Z ",		" ", &
		"BB",		"B", &
		"EE",		"E", &
		"EI",		"I", &
		"FF",		"F", &
		"IA",		"I", &
		"LL",		"L", &
		"NN",		"N", &
		"OO",		"O", &
		"SCH",		"SH", &
		"SS",		"S", &
		"TT",		"T", &
		"Y",		"I", &
		" ALEN ",	" ALAN ", &
		" AND ",	" & ", &
		" ARTHUR ",	" ART ", &
		" BILI ",	" BIL ", &
		" BOBI ",	" BOB ", &
		" CHARLES ",	" CHUCK ", &
		" CLIFORD ",	" CLIF ", &
		" CLIFTON ",	" CLIF ", &
		" CO ",		" ", &
		" CORP ",	" ", &
		" DANIL ",	" DAN ", &
		" DANI ",	" DAN ", &
		" DAVID ",	" DAVE ", &
		" DDS",		" ", &
		" DEBI ",	" DEBRA ", &
		" DEBORAH ",	" DEBRA ", &
		" DIST ",	" ", &
		" DISTRICT",	" ", &
		" DONALD ",	" DON ", &
		" DOUGLAS ",	" DOUG ", &
		" DR ",		" ", &
		" EDDI ",	" ED ", &
		" EDWARD ",	" ED ", &
		" FOR ",	" ", &
		" GAILE ",	" GAIL ", &
		" GERALD ",	" JERI ", &
		" II ",		" ", &
		" III ",	" ", &
		" IN ",		" ", &
		" INC ",	" ", &
		" IV ",		" ", &
		" JEFREI ",	" JEF ", &
		" JENIFER ",	" JENI ", &
		" JENNI ",	" JENI ", &
		" JR ",		" ", &
		" KENETH ",	" KEN ", &
		" LTD ",	" ", &
		" MAC",		" MC", &
		" MFG ",	" ", &
		" MICHAEL ",	" MIKE ", &
		" MIS ",	" ", &
		" MR ",		" ", &
		" MRS ",	" ", &
		" NON ",	" ", &
		" OF ",		" ", &
		" ON ",		" ", &
		" PETER ",	" PETE ", &
		" PHILIP ",	" PHIL ", &
		" RICHARD ",	" RICK ", &
		" ROBERT ",	" BOB ", &
		" SH ",		" SD ", &
		" SHOL ",	" SD ", &
		" SR ",		" ", &
		" STEVEN ",	" STEVE ", &
		" SUSAN ",	" SUE ", &
		" SUZI ",	" SUE ", &
		" THE ",	" ", &
		" THOMAS ",	" TOM ", &
		" TO ",		" ", &
		" VERNON ",	" VERN ", &
		" WILIM ",	" BIL ", &
		" WILBER ",	" BIL ", &
		" WM ",		" BIL ", &
		"",		""

	!
	! Set up translation tables
	!
	TRANSOURCE% = 0%
	READ A$, B$
	WHILE A$ <> ""
		TRANSOURCE% = TRANSOURCE% + 1%
		TRANSOURCE$(TRANSOURCE%) = A$
		TRANDEST$(TRANSOURCE%) = B$
		READ A$, B$
	NEXT

	%PAGE

	ON ERROR GOTO 19000

	!
	! Define translation function
	!
	DEF FNTRANSLATE$(IN$)

		IN_WORK$ = " " + EDIT$(IN$, 1% + 4% + 8% + 16% + 32% + 128%)

		!
		! Translate anything possible
		!
		FOR IN_WORK% = 1% TO TRANSOURCE%
 ReLoop:
			IN_FIND% = INSTR(1%, IN_WORK$, TRANSOURCE$(IN_WORK%))
			IF IN_FIND%
			THEN
				IN_WORK$ = LEFT(IN_WORK$, IN_FIND% - 1%) + &
					TRANDEST$(IN_WORK%) + &
					RIGHT(IN_WORK$, IN_FIND% + &
					LEN(TRANSOURCE$(IN_WORK%)))
				GOTO ReLoop
			END IF

		NEXT IN_WORK%

		!
		! Xlate anything left over
		!
		FNTRANSLATE$ = XLATE(IN_WORK$, XLSTR$)

	FNEND

 Init:	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Build up an XLATE string for cleaning up names & addresses
	!
	XLSTR$ = STRING$(256%, 0%)

	X$ = "0123456789"
	X% = A"0"B
	XLSTR$ = LEFT(XLSTR$, X%) + X$ + RIGHT(XLSTR$, X% + LEN(X$) + 1%)

	X$ = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
	X% = A"A"B
	XLSTR$ = LEFT(XLSTR$, X%) + X$ + RIGHT(XLSTR$, X% + LEN(X$) + 1%)
	X% = A"a"B
	XLSTR$ = LEFT(XLSTR$, X%) + X$ + RIGHT(XLSTR$, X% + LEN(X$) + 1%)

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.OPN"
	USE
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

320	!
	! Generate work file
	!
	CALL ENTR_3MESSAGE(SCOPE, "Creating work file . . .", 1% + 16%)

	CALL ASSG_CHANNEL(AR_TEMP.CH%, STAT%)
	CALL READ_DEVICE("UTL_WORK", UTL_WORK.DEV$, STAT%)

	OPEN UTL_WORK.DEV$ + "AR_TEMP.TMP" FOR OUTPUT AS FILE AR_TEMP.CH%, &
		ORGANIZATION INDEXED FIXED, &
		TEMPORARY, &
		BUFFER 32%, &
		MAP AR_SORTFILE, &
		PRIMARY KEY &
		( &
			AR_SORTFILE::ZIP, &
			AR_SORTFILE::STATE, &
			AR_SORTFILE::CITY, &
			AR_SORTFILE::CUSNAM &
		) DUPLICATES, &
		ALLOW NONE, &
		ACCESS MODIFY

330	!
	! Build sort file
	!
	WHEN ERROR IN
		RESET #AR_35CUSTOM.CH%
	USE
		CONTINUE ReportTitle
	END WHEN

 GetCustomRec:
	WHEN ERROR IN
		GET #AR_35CUSTOM.CH%, REGARDLESS
	USE
		CONTINUE ReportTitle
	END WHEN

	!
	! Mangle customer information to make matches more likely
	!
	AR_SORTFILE::CUSNUM = AR_35CUSTOM::CUSNUM
	AR_SORTFILE::CUSNAM = FNTRANSLATE$(AR_35CUSTOM::CUSNAM)
	IF TRM$(AR_35CUSTOM::ADD1) <> ""
	THEN
		AR_SORTFILE::ADD1 = XLATE(AR_35CUSTOM::ADD1, XLSTR$)
	ELSE
		IF TRM$(AR_35CUSTOM::ADD2) <> ""
		THEN
			AR_SORTFILE::ADD1 = XLATE(AR_35CUSTOM::ADD2, XLSTR$)
		ELSE
			AR_SORTFILE::ADD1 = XLATE(AR_35CUSTOM::ADD3, XLSTR$)
		END IF
	END IF
	AR_SORTFILE::CITY = XLATE(AR_35CUSTOM::CITY, XLSTR$)
	AR_SORTFILE::STATE = EDIT$(AR_35CUSTOM::STATE, 4% + 8% + 16% + 32% + 64%)
	AR_SORTFILE::ZIP = LEFT(AR_35CUSTOM::ZIP, 5%)

	!
	! Dump out record
	!
	PUT #AR_TEMP.CH%

	GOTO GetCustomRec

 ReportTitle:
	!
	! Set up titles
	!
	TITLE$(1%) = "Duplicate Customer Name Report"
	TITLE$(2%) = ""

	TITLE$(3%) = "Cust #    Name"
	TITLE$(4%) = ""

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		RESET #AR_TEMP.CH%

		!
		! Prime the pump with the first customer in the file
		!
		GET #AR_TEMP.CH%, REGARDLESS
	USE
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

	AR_SORTFILE_LAST = AR_SORTFILE
	PRINTED_ONE% = 0%

 GetNextRec:
17020	!
	! Main loop
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #AR_TEMP.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

	!
	! Compare this record to the previous record
	!
	IF (AR_SORTFILE::ZIP = AR_SORTFILE_LAST::ZIP) AND &
		(AR_SORTFILE::CITY = AR_SORTFILE_LAST::CITY) AND &
		(AR_SORTFILE::STATE = AR_SORTFILE_LAST::STATE) AND &
		(AR_SORTFILE::CUSNAM = AR_SORTFILE_LAST::CUSNAM)
	THEN
		!
		! Output this record since we have a match
		!
		IF (PRINTED_ONE% = 0%)
		THEN
			!
			! Print the first record so they can be compared
			!
			TEXT$ = STRING$(78%, 45%)
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -1%)

			GOSUB PrintLast
			PRINTED_ONE% = -1%
		END IF

		!
		! Print the current record
		!
		AR_SORTFILE_LAST = AR_SORTFILE
		GOSUB PrintLast
	ELSE
		AR_SORTFILE_LAST = AR_SORTFILE
		PRINTED_ONE% = 0%
	END IF


17340	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Try for next record
	!
	GOTO GetNextRec

 ExitTotal:
	!
	! Handle end of report
	!
	TEXT$ = STRING$(78%, 45%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

 ExitProgram:
	CALL OUTP_FINISHNOTITLE(UTL_REPORTX)

	!
	! Exit to next program or menu
	!
	IF TRM$(UTL_REPORTX::NEXTRUN) = ""
	THEN
		CALL SUBR_3EXITPROGRAM(SCOPE, "", "")
	ELSE
		CALL SUBR_3EXITPROGRAM(SCOPE, "RUN " + UTL_REPORTX::NEXTRUN, "")
	END IF

	%PAGE

	!
	! Print whatever name is in AR_SORTFILE_LAST
	!
 PrintLast:
	GET #AR_35CUSTOM.CH%, &
		KEY #0% EQ AR_SORTFILE_LAST::CUSNUM, &
		REGARDLESS

	IF TRM$(AR_35CUSTOM::ADD1) <> ""
	THEN
		ADD1$ = AR_35CUSTOM::ADD1
	ELSE
		IF TRM$(AR_35CUSTOM::ADD2) <> ""
		THEN
			ADD1$ = AR_35CUSTOM::ADD2
		ELSE
			ADD1$ = AR_35CUSTOM::ADD3
		END IF
	END IF

	TEXT$ = AR_35CUSTOM::CUSNUM + " " + &
		LEFT(AR_35CUSTOM::CUSNAM, 25%) + " " + &
		ADD1$ + " " + &
		AR_35CUSTOM::CITY + " " + &
		AR_35CUSTOM::STATE + " " + &
		AR_35CUSTOM::ZIP

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	RETURN

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	UTL_REPORTX::STAT = -1%
	GOTO ExitProgram

19000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
