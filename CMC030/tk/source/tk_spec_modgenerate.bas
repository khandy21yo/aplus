1	%TITLE "Summarize Author/Modification Data from Source Codes"
	%SBTTL "TK_SPEC_MODGENERATE"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1990 BY
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
	! Abstract:HELP
	!	.p
	!	This report builds a work file consisting of
	!	Author/Modification information for one or more program
	!	source codes.  Further, this report will then summarize
	!	the file and print out a summary (ONLY the summary).
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS TK_SOURCE:TK_SPEC_MODGENERATE/LINE
	!	$ LINK/EXE=TK_EXE: TK_SPEC_MODGENERATE, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE TK_SPEC_MODGENERATE.OBJ;*
	!
	! Author:
	!
	!	06/01/90 - Aaron Redd
	!
	! Modification history:
	!
	!	06/05/90 - Kevin Handy
	!		Modified to keep case of modification history/
	!		Programmer name, and to lose initial exclimation
	!		point on text.
	!
	!	06/05/90 - Kevin Handy
	!		Fixed compile statement to use right program name.
	!
	!	05/17/91 - Kevin Handy
	!		Modifications to handle Author information just
	!		like Modification information, and speed things up
	!		a little.
	!
	!	05/24/91 - Kevin Handy
	!		Modified to use TK_ALIAS file to get a good
	!		programmer name list.
	!
	!	05/29/91 - Kevin Handy
	!		Modified to process "*.C" files also.  Made
	!		subroutines out of spagetti jumps.
	!
	!	09/04/91 - Kevin Handy
	!		Purged out spaces on front of names as they are
	!		read in.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/16/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/28/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	09/19/2000 - Kevin Handy
	!		Use LIB$DELETE_FILE instead of kill
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD	UTL_REPORTX

	%INCLUDE "SOURCE:[TK.OPEN]TK_MODINFO.HB"
	MAP	(TK_MODINFO)	TK_MODINFO_CDD	TK_MODINFO

	%INCLUDE "SOURCE:[TK.OPEN]TK_ALIAS.HB"
	MAP	(TK_ALIAS)	TK_ALIAS_CDD	TK_ALIAS

	!
	! Dimension variables
	!
	DIM FILE_LIST$(1000%), DIR_LIST$(100%)

	!
	! Set some variables to initial values
	!
	A_TOTAL%, M_TOTAL%, F_TOTAL% = 0%
	DIR.INDEX% = 1%

	CALL ASSG_CHANNEL(SOURCE_CODE.CH%, STATUS%)
	CALL ASSG_CHANNEL(STORE_DATE.CH%, STATUS%)

	FILENAM$ = "TK_SOURCE:TK_OUTP_DATES.LOG"

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

	!
	! Initilize report function of the program
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 80%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Set up user input
	!
	TARGET_DIR$ = EDIT$(UTL_REPORTX::OPTDEF(0%), -1%)
	TARGET_FILE$ = EDIT$(UTL_REPORTX::OPTDEF(1%), -1%)

	!
	! Kill off any and all previous work files
	!
300	WHEN ERROR IN
		CALL READ_DEVICE("TK_MODINFO", TK_MODINFO.DEV$, STAT%)
	USE
		CONTINUE 310
	END WHEN

 !	KILL TK_MODINFO.DEV$ + "TK_MODINFO.MAS" FOR I% = 1% TO 10%

	SMG_STATUS% = LIB$DELETE_FILE(TK_MODINFO.DEV$ + "TK_MODINFO.MAS;*")

	!
	! Create a new work file
	!
310	%INCLUDE "SOURCE:[TK.OPEN]TK_MODINFO.PST"

320	WHEN ERROR IN
		%INCLUDE "SOURCE:[TK.OPEN]TK_ALIAS.OPN"
	USE
		CONTINUE 330
	END WHEN

325	OPEN FILENAM$ FOR OUTPUT AS FILE #STORE_DATE.CH%

330	%PAGE

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "Summarize Author/Modification Information For:"
	TITLE$(2%) = "Directory:  " + TARGET_DIR$ + "  and Files:  " + TARGET_FILE$
	TITLE$(3%) = "TK System"
	TITLE$(4%) = ""

	!
	! Heading
	!
	TITLE$(5%) = "."

	%PAGE

	!***********************************************************************
	! Seek out the indicated files and pull out Modification information
	!***********************************************************************

	!
	! Find the indicated directory or directories
	!
1000	CALL FIND_FILE("SOURCE:[000000]*.DIR", DIR_LIST$(), 16%, "", "")
	LAST_DIR% = VAL%(DIR_LIST$(0%))

	GOSUB NextDir FOR DIR.INDEX% = 1% TO LAST_DIR%

	GOTO 17000

 NextDir:
	!
	! Check the current directory from the list vs. the target
	!
	IF (COMP_STRING(DIR_LIST$(DIR.INDEX%), TARGET_DIR$) = 0%)
	THEN
		RETURN
	END IF

	!
	! Find all the matching programs in this directory
	!
	EXTENSION$ = ".BAS"
	PREFIX$ = "SOURCE:[" + DIR_LIST$(DIR.INDEX%) + ".SOURCE]"
	CALL FIND_FILE(PREFIX$ + TARGET_FILE$ + EXTENSION$, &
		FILE_LIST$(), 16%, "", "")
	LAST_FILE% = VAL%(FILE_LIST$(0%))

	F_TOTAL% = F_TOTAL% + LAST_FILE%

	!
	! Set a few more initial values
	!
	GOSUB NextFile FOR FILE.INDEX% = 1% TO LAST_FILE%

	!
	! Find all the matching programs in this directory
	!
	EXTENSION$ = ".C"
	PREFIX$ = "SOURCE:[" + DIR_LIST$(DIR.INDEX%) + ".SOURCE]"
	CALL FIND_FILE(PREFIX$ + TARGET_FILE$ + EXTENSION$, &
		FILE_LIST$(), 16%, "", "")
	LAST_FILE% = VAL%(FILE_LIST$(0%))

	F_TOTAL% = F_TOTAL% + LAST_FILE%

	!
	! Set a few more initial values
	!
	GOSUB NextFile FOR FILE.INDEX% = 1% TO LAST_FILE%

	RETURN

 NextFile:
	!
	! Check the current file from the list vs. the target
	!
	IF (COMP_STRING(FILE_LIST$(FILE.INDEX%), TARGET_FILE$) = 0%)
	THEN
		GOTO 1350
	END IF

	!
	! Open the current file
	!
1100	WHEN ERROR IN
		OPEN PREFIX$ + FILE_LIST$(FILE.INDEX%) + EXTENSION$ FOR INPUT &
			AS FILE SOURCE_CODE.CH%, &
			ACCESS READ, ALLOW MODIFY
	USE
		CONTINUE 1400
	END WHEN

	!
	! Set some of the variables in the record
	!
	TK_MODINFO::PROGNAME = FILE_LIST$(FILE.INDEX%) + EXTENSION$
	TK_MODINFO::SYSTEM = DIR_LIST$(DIR.INDEX%)

	!
	! Tell the user what we're doing
	!
	CALL ENTR_3MESSAGE(SCOPE, &
		"Extracting from: " + TK_MODINFO::PROGNAME, 1%)

 GetNextLine:
	!
	! Get the (next) line from the source code
	!
1200	WHEN ERROR IN
		LINPUT #SOURCE_CODE.CH%, TEXT_LINE$
	USE
		CONTINUE 1350 IF ERR = 11%
		FILENAME$ = FILE_LIST$(FILE.INDEX%)
		CONTINUE HelpError
	END WHEN

	!
	! Fix up the text line, and see what to do
	!
1210	GOTO GetNextLine IF (EDIT$(TEXT_LINE$, 8% + 16% + 128%) = "!") OR &
		(EDIT$(TEXT_LINE$, 8% + 16% + 128%) = "*")

	SELECT EDIT$(TEXT_LINE$, 8% + 16% + 32% + 128%)

	!
	! Author information
	!
	CASE "! AUTHOR:", "* AUTHOR:"
		TK_MODINFO::MODFLAG = "A"
		GOSUB ModInfo
		GOTO 1210

	!
	! Modification information
	!
	CASE "! MODIFICATION HISTORY:", "* MODIFICATION HISTORY:"
		TK_MODINFO::MODFLAG = "M"
		GOSUB ModInfo
		GOTO 1210

	!
	! End of useful info
	!
	CASE "!--", "*--"
		GOTO 1350

	!
	! We don't even want to know
	!
	CASE ELSE
		GOTO GetNextLine

	END SELECT

	!
	! Close the file, and otherwise finish up
	!
1350	CLOSE #SOURCE_CODE.CH%
	LAST_DATE$ = ""

1400	RETURN

	%PAGE

17000	!***********************************************************************
	! OUTPUT REPORT:  Print summary of what has been done
	!***********************************************************************
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Print two blank lines
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	GOTO Exitprogram IF UTL_REPORTX::STAT

	!
	! Give the total number of files searched
	!
	TEXT$ = "     Total Number of Files Searched:            " + &
		FORMAT$(F_TOTAL%, "######")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO Exitprogram IF UTL_REPORTX::STAT

	!
	! Give the total number of Authors found
	!
	TEXT$ = "     Total Number of Author Records Created:    " + &
		FORMAT$(A_TOTAL%, "######")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO Exitprogram IF UTL_REPORTX::STAT

	!
	! Give the total number of Modifiers found
	!
	TEXT$ = "     Total Number of Modifier Records Created:  " + &
		FORMAT$(M_TOTAL%, "######")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

 ExitProgram:
18000	!*******************************************************************
	! Exit program
	!*******************************************************************

	!
	! Finish up here
	!
	CALL ASSG_FREECHANNEL(SOURCE_CODE.CH%)
	CALL OUTP_FINISH(UTL_REPORTX)

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

 ModInfo:
	!*******************************************************************
	! Find Modification History information, and build records
	!*******************************************************************

	!
	! Set some initial values
	!
18100	INDEX%, FLAG% = 0%

 NextModLine:
	WHEN ERROR IN
		LINPUT #SOURCE_CODE.CH%, TEXT_LINE$
	USE
		CONTINUE 18150
	END WHEN


	!
	! End of header?
	!
	IF (EDIT$(TEXT_LINE$, 8% + 16% + 32% + 128%) = "!--") OR &
		(MID(TEXT_LINE$, 2%, 2%) == "! ") OR &
		(EDIT$(TEXT_LINE$, 8% + 16% + 32% + 128%) = "*--") OR &
		(MID(TEXT_LINE$, 2%, 2%) == "* ")
	THEN
		GOTO 18150
	END IF

	!
	! Is it the modifier/date information?
	!
	SLASH1% = INSTR(1%, TEXT_LINE$, "/")
	SLASH2% = INSTR(SLASH1% + 1%, TEXT_LINE$, "/")
	HYPHEN% = INSTR(1%, TEXT_LINE$, "-")

	IF (SLASH2% > 0%) AND (HYPHEN% > 0%)
	THEN
		IF (FLAG% = -1%)
		THEN
			!
			! Put the previous record in the file
			!
			PUT #TK_MODINFO.CH%
			INDEX% = 0%
			IF TK_MODINFO::MODFLAG = "A"
			THEN
				A_TOTAL% = A_TOTAL% + 1%
			ELSE
				M_TOTAL% = M_TOTAL% + 1%
			END IF
		END IF

		TK_MODINFO::MODDATE = &
			DATE_STOREDATE(MID(TEXT_LINE$, SLASH1% - 2%, &
			HYPHEN% - SLASH1% + 2%))

		!
		! STORE INFO IF DATES ARE OUT OF ORDER
		!
		IF (LAST_DATE$ > TK_MODINFO::MODDATE) AND (LAST_DATE$<>"")
		THEN
			PRINT_STRING$ = TK_MODINFO::PROGNAME + &
				"  DATES ARE OUT OF ORDER" + LF + CR
			PRINT #STORE_DATE.CH%,  PRINT_STRING$
		ELSE
			LAST_DATE$ = TK_MODINFO::MODDATE
		END IF

		TK_MODINFO::PROGRAMMER = &
			EDIT$(RIGHT(TEXT_LINE$, HYPHEN% + 2%), 4% + 8% + 16%)
		TK_MODINFO::MODDESCR(I%) = SPACE$(80%) FOR I% = 0% to 20%
		FLAG% = -1%
		GOSUB DoAlias
	ELSE
		IF (FLAG% = -1%)
		THEN
			TK_MODINFO::MODDESCR(INDEX%) = RIGHT(TEXT_LINE$, 3%)
			INDEX% = INDEX% + 1%
		END IF

		IF (INDEX% = 20%)
		THEN
			!
			! The modification information is too long; put
			! the first 20 lines in the file, and skip the rest
			!
			PUT #TK_MODINFO.CH%
			IF TK_MODINFO::MODFLAG = "A"
			THEN
				A_TOTAL% = A_TOTAL% + 1%
			ELSE
				M_TOTAL% = M_TOTAL% + 1%
			END IF
			INDEX% = 1%
			FLAG% = 0%
		END IF
	END IF

	GOTO NextModLine

18150	IF (FLAG% = -1%)
	THEN
		PUT #TK_MODINFO.CH%
		IF TK_MODINFO::MODFLAG = "A"
		THEN
			A_TOTAL% = A_TOTAL% + 1%
		ELSE
			M_TOTAL% = M_TOTAL% + 1%
		END IF
	END IF

	RETURN

	%PAGE

	!*******************************************************************
	! Check for alias
	!*******************************************************************
 DoAlias:

18200	IF LEFT(TK_MODINFO::PROGRAMMER, 30%) <> TK_ALIAS::ALIAS
	THEN
		WHEN ERROR IN
			GET #TK_ALIAS.CH%, &
				KEY #0% EQ LEFT(TK_MODINFO::PROGRAMMER, 30%), &
				REGARDLESS
		USE
			CONTINUE 18290
		END WHEN
	END IF

	TK_MODINFO::PROGRAMMER = TK_ALIAS::PROGRAMMER

18290	RETURN

	%PAGE

 HelpError:
	!*******************************************************************
	! Help Message for an Error
	!*******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	UTL_REPORTX::STAT = -1%

	GOTO ExitProgram

19000	!*******************************************************************
	! Error trapping
	!*******************************************************************

	FILENAME$ = "TK_SPEC_MODGENERATE"
	RESUME HelpError

32767	END
