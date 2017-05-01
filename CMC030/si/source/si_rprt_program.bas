1	%TITLE "Print All Programs Used in a System"
	%SBTTL "SI_RPRT_PROGRAM"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1987 BY
	!
	! Computer Management Center
	! Idaho Falls, Idaho.
	!
	! This software is furnished under a license and may be used and
	! copied only in accordance with terms of such license and with
	! the inclusion of the above copyright notice.  This software or
	! any other copies therof may not be provided or otherwise made
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
	! ID:SIP001
	!
	! Abstract:HELP
	!	.p
	!	This program prints all programs that it can find out
	!	about that are used in a system.  It only looks for
	!	programs that menu and report call, so a program that
	!	chains to another will not show up here.
	!
	! Index:
	!
	! Option:
	!
	! Input:
	!
	!
	! Output:
	!
	!
	! Example:
	!
	! Author:
	!
	!	02/17/88 - Kevin Handy
	!
	! Compile:
	!
	!	$ BAS SI_SOURCE:SI_RPRT_PROGRAM/LINE
	!	$ LINK/EXECUTABLE=SI_EXE:*.EXE SI_RPRT_PROGRAM, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE SI_RPRT_PROGRAM.OBJ;*
	!
	! Modification history:
	!
	!	08/05/91 - Kevin Handy
	!		Added ACCESS READ to open statement.
	!
	!	04/15/92 - Kevin Handy
	!		Tag in code to update TK_RELATION file.
	!
	!	05/06/92 - Kevin Handy
	!		Modified to have wildcard menu names instead
	!		of needing to do one at a time.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	09/13/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/26/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/22/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[TK.OPEN]TK_RELATION.HB"
	MAP (TK_RELATION) TK_RELATION_CDD TK_RELATION

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE UTL_REPORTX_CDD UTL_REPORTX

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORT.HB"
	MAP	(UTL_REPORT)	UTL_REPORT_CDD	UTL_REPORT

	!
	! Array to hold device and program
	!
	RECORD PROG_RECORD
		STRING	PRODEV = 39
		STRING	PRONAM = 39
	END RECORD

	DECLARE INTEGER CONSTANT MAX_PROG = 1000%

	DIM PROG_RECORD PROG_LIST(MAX_PROG)
	DIM FILENAME$(100%)

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION FIND_FILEEXISTS

	%PAGE

	ON ERROR GOTO 19000

	CALL ASSG_CHANNEL(MENU.CH%, STAT%)

 Init:	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 80%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	WILDFILENAME$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

310	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_SYSREP.OPN"
	USE
		FILENAME$ = "UTL_REPORT"
		CONTINUE HelpError
	END WHEN

320	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[TK.OPEN]TK_RELATION.MOD"
	USE
		FILENAME$ = "TK_RELATION"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "Programs used in a system List"
	TITLE$(2%) = "Menu file: " + WILDFILENAME$
	TITLE$(3%) = "System Install"
	TITLE$(4%) = ""

	!
	! Heading
	!
	TITLE$(5%) = ""

	%PAGE

900	!*******************************************************************
	! Do a wild-card lookup for all menu files with this name
	!*******************************************************************

	CALL FIND_FILE("CMC:" + WILDFILENAME$ + ".MNU", FILENAME$(), 16%, &
		"", ".MNU")

	FOR FILENAME% = 1% TO VAL%(FILENAME$(0%))

		XFILENAME$ = FILENAME$(FILENAME%)

		GOSUB 1000

	NEXT FILENAME%

	GOTO ExitProgram

1000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

1005	WHEN ERROR IN
		OPEN XFILENAME$ FOR INPUT AS FILE MENU.CH%, &
			DEFAULTNAME "CMC:.MNU", &
			ACCESS READ, ALLOW MODIFY
	USE
		CONTINUE 17100
	END WHEN

	TOTAL_FILE% = 0%

16000	!
	! Remove any TK_RELATION references to this menu
	!
	WHEN ERROR IN
		GET #TK_RELATION.CH%, KEY #0% EQ XFILENAME$
	USE
		CONTINUE 16090
	END WHEN

16010	WHILE TK_RELATION::PARENT = XFILENAME$

		WHEN ERROR IN
			DELETE #TK_RELATION.CH%

			GET #TK_RELATION.CH%
		USE
			CONTINUE 16090
		END WHEN

	NEXT

16090	!
	! Constants when updateing TK_RELATION file
	!
	TK_RELATION::PARENT	= XFILENAME$
	TK_RELATION::QUANTITY	= 1%
	TK_RELATION::DEFREF	= "M"
	TK_RELATION::CDATE	= DATE_TODAY
	TK_RELATION::CTIME	= TIME_NOW

 GetNextRec:
17000	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next record
	!
	WHEN ERROR IN
		LINPUT #MENU.CH%, INLINE$
	USE
		CONTINUE 17100 IF ERR = 11%
		FILENAME$ = "MENU"
		CONTINUE HelpError
	END WHEN

	INLINE$ = EDIT$(INLINE$, 16% + 32% + 128%)

	!*******************************************************************
	! Parse one line
	!*******************************************************************

	!
	! Check for special file
	!
	IF LEFT(INLINE$, 6%) = "!.FILE"
	THEN
		INLINE$ = RIGHT(INLINE$, 8%)
		I% = INSTR(1%, INLINE$, ":")
		J% = INSTR(1%, INLINE$, "]")
		I% = J% IF I% < J%
		TLB_PROGRAM$ = RIGHT(INLINE$, I% + 1%)
		TLB_DEVICE$ = LEFT(INLINE$, I%)
		GOSUB AddProgram
		GOTO 17090
	END IF

	!
	! Lose Comments
	!
	IF LEFT(INLINE$, 1%) = "!"
	THEN
		GOTO 17090
	END IF

	!
	! Pull off dots
	!
	DOT_COUNTER% = 0%
	WHILE MID(INLINE$, DOT_COUNTER% + 1%, 1%) = "."
		DOT_COUNTER% = DOT_COUNTER% + 1%
	NEXT
	INLINE$ = RIGHT(INLINE$, DOT_COUNTER% + 1%)

	!
	! Pull apart line
	!
	FIRST_SPACE% = INSTR(1%, INLINE$, " ")

	FIRST_ARROW% = INSTR(FIRST_SPACE% + 1%, INLINE$, ">")
	FIRST_ARROW% = INSTR(FIRST_SPACE% + 1%, INLINE$, "<") &
		IF FIRST_ARROW% = 0%

	IF FIRST_ARROW% = 0%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "Unable to parse menu '" + &
			INLINE$ +  "'", 0%)
		GOTO 17090
	END IF

	SECOND_ARROW% = INSTR(FIRST_ARROW% + 1%, INLINE$, ">")
	SECOND_ARROW% = INSTR(FIRST_ARROW% + 1%, INLINE$, "<") &
		IF SECOND_ARROW% = 0%

	IF SECOND_ARROW% = 0%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"Unable to parse menu '" + INLINE$ + "'", 0%)
		GOTO 17090
	END IF

	!
	! Seperate parts
	!
	MENU_FILE$ = RIGHT(INLINE$, SECOND_ARROW% + 1%)
	IF MID(INLINE$, FIRST_ARROW%, 1%) = ">"
	THEN
		MENU_TYPE$ = "P"
	ELSE
		!
		! Skip help items since they are not programs
		!
		GOTO 17090
	END IF

	CALL HELP_MENUHELPKEY(MENU_FILE$, &
		MENU_TYPE$, &
		MENU_SYSTEM$, &
		TLB_IDENT$, &
		TLB_PROGRAM$, &
		TLB_ITEM$, &
		TLB_DEVICE$, &
		1%)

	GOSUB AddProgram

17090	GOTO GetNextRec

	%PAGE

 AddProgram:
	IF INSTR(1%, TLB_PROGRAM$, ".") = 0%
	THEN
		TLB_PROGRAM$ = TLB_PROGRAM$ + ".EXE"
	END IF

	!
	! Search file list for currently existing file
	!
	RETURN IF (PROG_LIST(I%)::PRONAM = TLB_PROGRAM$) &
		FOR I% = 1% TO TOTAL_FILE%

	!
	! Item not found, create it
	!
	I%, TOTAL_FILE% = TOTAL_FILE% + 1%

	WHILE (I% > 1%) AND (PROG_LIST(I% - 1%)::PRONAM > TLB_PROGRAM$)
		PROG_LIST(I%) = PROG_LIST(I% - 1%)
		I% = I% - 1%
	NEXT

	PROG_LIST(I%)::PRODEV = TLB_DEVICE$
	PROG_LIST(I%)::PRONAM = TLB_PROGRAM$

	RETURN

17100	!
	! Print out one line
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	TEXT$ = "Files used in " + XFILENAME$
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	FOR LOOP% = 1% TO TOTAL_FILE%

		TEXT$ = TRM$(PROG_LIST(LOOP%)::PRODEV) + &
			TRM$(PROG_LIST(LOOP%)::PRONAM)

		IF FIND_FILEEXISTS(TEXT$, 0%) = 0%
		THEN
			TEXT$ = LEFT(TEXT$ + SPACE$(50%), 50%) + &
				" Program is missing"
		END IF

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO ExitProgram IF UTL_REPORTX::STAT

17200		!
		! Add record to TK_RELATION file
		!
		I% = INSTR(1%, PROG_LIST(LOOP%)::PRONAM, ".EXE")
		I% = LEN(PROG_LIST(LOOP%)::PRONAM) + 1% IF I% = 0%
		TK_RELATION::CHILD = LEFT(PROG_LIST(LOOP%)::PRONAM, I% - 1%)

		WHEN ERROR IN
			PUT #TK_RELATION.CH%
		USE
			CONTINUE 17900
		END WHEN

17900	NEXT LOOP%

	RETURN

 ExitTotal:
	!
	! Handle end of report
	!

 ExitProgram:
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
