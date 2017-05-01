1	%TITLE "Compile a Menu Source File into an Indexed File"
	%SBTTL "UT_SUBR_MENULOAD"
	%IDENT "V3.6a Calico"

	FUNCTION LONG UT_SUBR_MENULOAD(SCOPE_STRUCT SCOPE, &
		LONG SOURCE.CH, &
		LONG POINTER, STRING CURLINE, LONG CURLEVEL, &
		MENU_CDD MENU())

	!
	! COPYRIGHT (C) 1990 BY
	! Computer Management Center, Inc.
	! Idaho Falls, Idaho  83401.
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
	!
	! ABSTRACT:HELP
	!	.p
	!	This function is used to compile the menu into a form
	!	usable by the menu program.  This function is recursive.
	!	.p
	!	Returns a pointer to the top item added.
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS UTL_SOURCE:UT_SUBR_MENULOAD/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP UT_SUBR_MENULOAD
	!	$ DELETE UT_SUBR_MENULOAD.OBJ;*
	!
	! AUTHOR:
	!
	!	09/28/90 - Kevin Handy
	!
	! MODIFICATION HISTORY:
	!
	!	08/01/91 - Kevin Handy
	!		Modifications to try to gain a little more
	!		speed out of the menu. (1% increase)
	!
	!	08/02/91 - Kevin Handy
	!		Rewrote FNLEVEL% to use STR$FIND... function
	!		to speed up this thing a little more.
	!		Also a few more changes for hopefully a little
	!		more speed. (5% increase)
	!
	!	08/02/91 - Kevin Handy
	!		Modified to remove FNLEVEL% function, and write
	!		the code inline.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/30/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/13/99 - Kevin Handy
	!		Use BASIC$STARLET for STR$
	!
	!	10/19/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"
	%INCLUDE "FUNC_INCLUDE:MENU.INC"

	%INCLUDE "STR$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

	DIM MENU_CDD TEMP_MENU(50)

	%PAGE

	TEMP_COUNT% = 0%

8000	!
	! Load in current line, unless we already have a line
	!
	IF CURLINE = ""
	THEN
		WHEN ERROR IN
			LINPUT #SOURCE.CH, CURLINE
		USE
			CONTINUE 8900 IF ERR = 11%
			EXIT HANDLER
		END WHEN

 !
 ! This line is just busy work, and shouldn't be needed
 !
 !		CURLINE = EDIT$(CURLINE, 8% + 128%)

		IF (CURLINE = "") OR (LEFT(CURLINE, 1%) = "!")
		THEN
			CURLINE = ""
			GOTO 8000
		END IF
	END IF

8100	!
	! Find out what level this line is at.
	!
	XLEVEL% = FNLEVEL%(CURLINE)
 !	XLEVEL% = str$find_first_not_in_set(x$, ".") - 1%

	!
	! The level dropped down.  Recurse out.
	!
	IF (XLEVEL% < CURLEVEL)
	THEN
		GOTO 8900
	END IF

	!
	! If it is too high, then there is an error.  (Increasing
	! levels not handled here)
	!
 !
 ! This is our problem, not the users, and this error
 ! would only serve to confuse and irritate them.
 !
 !	IF XLEVEL% > CURLEVEL
 !	THEN
 !		CALL ENTR_3MESSAGE(SCOPE, &
 !			"Error: Invalid indent level for line" + &
 !			NUM1$(CURLEVEL), 0%)
 !		GOTO 8800
 !	END IF

	!
	! Strip off leading dots
	!
	XLINE$ = EDIT$(RIGHT(CURLINE, CURLEVEL + 1%), 16%)

	!
	! It must be a option name then.  Search for the space delimiter.
	!
	I1% = INSTR(1%, XLINE$, " ")

	IF I1% <= 1%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "Error: No option delimiter found", 0%)
		GOTO 8800
	END IF

	!
	! Strip command into storage areas so they won't get distroyed.
	!
	XLINE1$ = RIGHT(XLINE$, I1% + 1%)

	!
	! Search for arrow, (Program to run)
	!
 !	I% = 0%

8150	!
	! From here it parses either a program or a level definition.
	! Either way, we need to read in another line sooner or later,
	! so we might as well do it now and get it over with.
	!
	I% = INSTR(1%, XLINE1$, "P>")
	IF I%
	THEN
 !
 ! This code has never been used by anybody, and anyone who tries
 ! to use it ought to be shot anyway.
 !
 !		IF MID(XLINE1$, I% - 1%, 1%) = "_"
 !		THEN
 !			XLINE1$ = LEFT(XLINE1$, I% - 2%) + RIGHT(XLINE1$, I%)
 !			GOTO 8150
 !		END IF

		A% = INSTR(I% + 2%, XLINE1$, ">")
		TASKTYPE$ = MID(XLINE1$, I% + 2%, A% - (I% + 2%))
		A% = I% + 1% IF A% = 0%

		TEMP_COUNT% = TEMP_COUNT% + 1%
		TEMP_MENU(TEMP_COUNT%)::OPT = LEFT(XLINE$, I1% - 1%)
		TEMP_MENU(TEMP_COUNT%)::OPTLEN = 0%
		TEMP_MENU(TEMP_COUNT%)::DESCR = LEFT(XLINE1$, I% - 1%)
		TEMP_MENU(TEMP_COUNT%)::SVALUE = RIGHT(XLINE1$, A% + 1%)
		TEMP_MENU(TEMP_COUNT%)::EFLAG = 0%

		IF (TASKTYPE$ = "RPRT")
		THEN
			TEMP_MENU(TEMP_COUNT%)::OPTV = OPTV_REPORT
		ELSE
			TEMP_MENU(TEMP_COUNT%)::OPTV = OPTV_RUN
		END IF

	ELSE
		CURLINE = ""
		UPLEVEL% = UT_SUBR_MENULOAD(SCOPE, SOURCE.CH, &
			POINTER, CURLINE, CURLEVEL + 1%, MENU())

		!
		! Handle whatever is returned
		!
		IF UPLEVEL% = 0%
		THEN
			CALL ENTR_3MESSAGE(SCOPE, "Error: Unable to parse further levels", 1%)
			GOTO 8900
		END IF

		!
		! Search for arrow, (Program to run)
		!
 !		I% = 0%

 L8160:		!
		! Look for an arrow specifying help files
		!
		I% = INSTR(1%, XLINE1$, "H<")
		IF I%
		THEN
 !
 ! This code has never been used by anybody, and anyone who tries
 ! to use it ought to be shot anyway.
 !
 !			IF MID(XLINE1$, I% - 1%, 1%) = "_"
 !			THEN
 !				XLINE1$ = LEFT(XLINE1$, I% - 2%) + RIGHT(XLINE1$, I%)
 !				GOTO L8160
 !			END IF

			A% = INSTR(I% + 2%, XLINE1$, "<")
			TASKTYPE$ = MID(XLINE1$, I% + 2%, A% - (I% + 2%))
			A% = I% + 1% IF A% = 0%

			TEMP_OPTV% = OPTV_MENU
			TEMP_SVALUE$ = RIGHT(XLINE1$, A%+1%)

			IF LEFT(TEMP_SVALUE$, 1%) = "*"
			THEN
				TEMP_SVALUE$ = RIGHT(TEMP_SVALUE$, 2%)
				TEMP_OPTV% = OPTV_INCLUDE
			END IF

			TEMP_COUNT% = TEMP_COUNT% + 1%
			TEMP_MENU(TEMP_COUNT%)::OPT = LEFT(XLINE$, I1% - 1%)
			TEMP_MENU(TEMP_COUNT%)::OPTLEN = 0%
			TEMP_MENU(TEMP_COUNT%)::DESCR = LEFT(XLINE1$, I% - 1%)
			TEMP_MENU(TEMP_COUNT%)::SVALUE = TEMP_SVALUE$
			TEMP_MENU(TEMP_COUNT%)::OVALUE = UPLEVEL%
			TEMP_MENU(TEMP_COUNT%)::EFLAG = 0%
			TEMP_MENU(TEMP_COUNT%)::OPTV = TEMP_OPTV%
		ELSE
			CALL ENTR_3MESSAGE(SCOPE, "Error: Odd menu format", 0%)
			GOTO 8800
		END IF

		!
		! Keep this CURLINE
		!
		GOTO 8000
	END IF

8800	!
	! Read in next line
	!
	CURLINE = ""
	GOTO 8000

8900	!*******************************************************************
	! Exit level.  Store everything we got into the
	! real array, and go back up to the previous level.
	!*******************************************************************

	!
	! Remember where start of menu level is
	!
	UT_SUBR_MENULOAD = POINTER + 1%

	!
	! Decide on unique characters
	!
	SCHAR$ = ""

8920	FOR LOOP% = 1% TO TEMP_COUNT%

		!
		! Figure out how many characters it takes to make command unique
		!
		TEMP% = 1%
		OPTION$ = TEMP_MENU(LOOP%)::OPT

8935		IF INSTR(1%, SCHAR$, " " + LEFT(OPTION$, TEMP%))
		THEN
			TEMP% = TEMP% + 1%
			GOTO 8935 IF TEMP% < LEN(OPTION$)
		END IF

		SCHAR$ = SCHAR$ + " " + TRM$(OPTION$)
		TEMP_MENU(LOOP%)::OPTLEN = TEMP%

	NEXT LOOP%

	!
	! Install into menu array
	!
	FOR LOOP% = 1% TO TEMP_COUNT%
		POINTER = POINTER + 1%
		MENU(POINTER) = TEMP_MENU(LOOP%)
	NEXT LOOP%

	MENU(POINTER)::EFLAG = -1%

	EXIT FUNCTION

	%PAGE

11000	!******************************************************************
	! FNLEVEL%() - Calculate the current level of the line.
	!
	! Works by counting leading spaces and tabs.
	!******************************************************************

	DEF FNLEVEL%(X$)

		FNLEVEL% = str$find_first_not_in_set(x$, ".") - 1%

	FNEND

32767	END FUNCTION
	!+-+-+
	!++
	! Abstract:HELP
	!	.p
	!	This function is used to compile the menu into a form
	!	usable by the menu program.  This function is recursive.
	!	.p
	!	Returns a pointer to the top item added.
	!
	! Index:
	!
	!--
