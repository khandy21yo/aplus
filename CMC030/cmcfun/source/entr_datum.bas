1	%TITLE "Function to Enter a Date with Full Year"
	%SBTTL "ENTR_DATUM"
	%IDENT "V3.6a Calico"

	FUNCTION STRING ENTR_DATUM(XX_VDID%, CPOS$, &
		PROMPT$, XDFLT$, FLAG%, &
		XFORMAT$, DEFLT$)

	!
	!	COPYRIGHT (C) 1986 BY
	!	Computer Management Center, Idaho Falls, Idaho.
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
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This function enters a date with full year
	!	CPOS$ '' - Do not display on top area
	!	FLAG%
	!	.table 3,25
	!	.te
	!	1	Don't enter data (display only?)
	!	.te
	!	4	Force keypunch input(no <CR> after input)
	!	.te
	!	8	Indicates a timeout on input will occur
	!	.te
	!	32	Use default value
	!	.te
	!	64	Don't display
	!	.te
	!	128	Return fincal value in default
	!	.end table
	!	.lm -5
	!
	! Index:
	!
	! Parameter:
	!
	!	XX_VDID%
	!		Passed variable that creates or deletes the window that holds the string.
	!
	!	PROMPT$
	!		The passed string used for the prompt and its initialization
	!
	!	XDFLT$
	!		One of the passed defaults for the date.
	!
	!	XFORMAT$
	!		The passed format for the date.
	!
	!	DEFLT$
	!		The default form the string size.
	!
	!
	!	This function enters a date on the screen with the full year.
	!
	! Example:
	!
	! Index:
	!
	!	.x Enter>Datum
	!	.x Datum>Enter
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:ENTR_DATUM
	!	$ LIB FUNC_LIB:CMCFUN/REP ENTR_DATUM
	!	$ DELETE ENTR_DATUM.OBJ;*
	!
	! AUTHOR:
	!
	!	09/04/88 - Frank Starman
	!
	! MODIFICATION HISTORY:
	!
	!	02/09/89 - Kevin Handy
	!		Modified for change in ENTR_ENTER.
	!
	!	07/06/93 - Kevin Handy
	!		Changed VAL to VAL% which is slightly faster.
	!
	!	07/16/93 - Kevin Handy
	!		Removed line "LSET GETS$ = GETS$"
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	04/20/95 - Kevin Handy
	!		Changed XX_VDID to XX_VDID%. This makes the
	!		function match how it has been called.
	!
	!	09/03/96 - Kevin Handy
	!		Reformat source code.
	!
	!	09/09/97 - Kevin Handy
	!		Lose unecessary external definitions
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/29/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

	DECLARE LONG XPOS, YPOS

	DIM MONTH$(12%)

	MONTH$(0%) = "   "
	MONTH$(1%) = "JAN"
	MONTH$(2%) = "FEB"
	MONTH$(3%) = "MAR"
	MONTH$(4%) = "APR"
	MONTH$(5%) = "MAY"
	MONTH$(6%) = "JUN"
	MONTH$(7%) = "JUL"
	MONTH$(8%) = "AUG"
	MONTH$(9%) = "SEP"
	MONTH$(10%) = "OCT"
	MONTH$(11%) = "NOV"
	MONTH$(12%) = "DEC"

	%PAGE

	!
	! Seperate cursor position
	!
	CALL DSPL_SPLITCURSOR(CPOS$, X%, Y%)
	XPOS = X%
	YPOS = Y%

	!
	! Get the current century and year
	!
	SMG_STATUS% = LIB$DATE_TIME(CENTIS$)
	YEARIS$ = MID(CENTIS$, 10%, 2%)
	CENTIS$ = MID(CENTIS$, 8%, 2%)

	!
	! Choose the default to use
	!
	IF (FLAG% AND 32%) = 0%
	THEN
		GETS$ = XDFLT$
	ELSE
		GETS$ = DEFLT$ + SPACE$(LEN(XDFLT$) - LEN(DEFLT$))
	END IF

	!
	! Check the size - make it 6 or 8
	!
	IF LEN(GETS$) <= 6%
	THEN
		GETS$ = LEFT(GETS$ + SPACE$(6%), 6%)
		DEF_LEN% = 6%
	ELSE
		GETS$ = LEFT(GETS$ + SPACE$(2%), 8%)
		DEF_LEN% = 8%
	END IF

	!
	! Calculate correct size for result
	!
	XFORMAT% = 2%
	XFORMAT% = 0% IF XFORMAT$ = "0"

	!
	! Display only?
	!
	GOTO L3000 IF (FLAG% AND 1%) <> 0%

	!
	! Display original
	!
	SMG_STATUS% = SMG$PUT_CHARS(XX_VDID%, PRNT_DATUM(GETS$, XFORMAT%), &
		XPOS, YPOS, 0%, SMG$M_REVERSE) &
			IF (CPOS$ <> "") AND ((FLAG% AND 64%) = 0%)

	SCOPE::SCOPE_EXIT = 0%

 L1000:	!
	! Initialization/prompt
	!
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, PROMPT$ + " <date>:", &
		1%, 1%, 1%, 0%)
	Y1POS% = LEN(PROMPT$) + 10%

 L1100:	!
	! Normal entry
	!
	DAY$ = ""
	T_GETS$ = GETS$
	GETS$ = RIGHT(GETS$, DEF_LEN% - 3%) + LEFT(GETS$, DEF_LEN% - 4%)

 L1110:	GETS$ = PRNT_DATUM(GETS$, XFORMAT% + 5%)

	SELECT ENTR_3ENTER(SCOPE, SCOPE::SMG_OPTION, 1%, Y1POS%, &
		GETS$, -1%, FLAG%)

	!
	! Pick out all of the exit keys
	!
	CASE SMG$K_TRM_CTRLC, SMG$K_TRM_F8, &
		SMG$K_TRM_F10, SMG$K_TRM_CTRLZ

		GETS$ = T_GETS$
		GOTO L3000
	END SELECT

	GETS$ = EDIT$(GETS$, 2%)

	GOTO L1200 IF GETS$ = XLATE(GETS$, STRING$(48%, 0%) + "0123456789") &
		AND LEN(GETS$) > 4%

	FL% = 0%
	FOR MON% = 1% TO 12%
		FL% = INSTR(1%, GETS$, MONTH$(MON%))
		GOTO ReadMonth IF FL%
	NEXT MON%

	MON% = 0%
	GOTO 3500 IF LEN(GETS$) > 4%

 ReadMonth:
	IF FL%
	THEN
		T.GETS$ = LEFT(GETS$, FL% - 1%) + RIGHT(GETS$, FL% + 3%)
	ELSE
		T.GETS$ = GETS$
	END IF
	GOTO 3500 IF T.GETS$ <> XLATE(T.GETS$, STRING$(48%, 0%) + "0123456789")
	YEAR$ = RIGHT(T.GETS$, LEN(T.GETS$) - 3%)
	YEAR$ = SPACE$(4%) IF YEAR$ = ""
	WHEN ERROR IN
		DAY$ = FORMAT$(VAL%(LEFT(T.GETS$, LEN(T.GETS$) - 4%)), "<0>#")
	USE
		DAY$ = "00"
	END WHEN
	GETS$ = FORMAT$(MON%, "<0>#") + DAY$ + YEAR$

 L1200:
	SELECT LEN(GETS$)

	CASE 0%		! Return blank string on C/R
			! Skip range check

		GETS$ = SPACE$(DEF_LEN%)
		GOTO L3000

	CASE 4%		! Assume no year

		GETS$ = GETS$ + YEARIS$
		GOTO L1200

	CASE 6%		! Date given in MMDDYY format

		TEMP$ = RIGHT(GETS$, 5%)

		IF EDIT$(TEMP$, 2%) = ""
		THEN
			GETS$ = LEFT(GETS$, 4%)
			GOTO L1200
		END IF

		GETS$ = LEFT(GETS$, 4%) + CENTIS$ + TEMP$

	CASE 8%		! Date given in MMDDYYYY format
			! (Don't need to do anything)

	CASE ELSE

		CALL ENTR_3MESSAGE(SCOPE, "Invalid date or format", 0%)
		GETS$ = GETS$ + SPACE$(DEF_LEN% - LEN(GETS$))
		GOTO L1110
	END SELECT

2000	!
	! Now put the year in front - Check out the date (Allow a blank date)
	!
	T_GETS$ = GETS$
	GETS$ = RIGHT(GETS$, 5%) + LEFT(GETS$, 4%)

	IF GETS$ <> DATE_INVDCODE(DATE_DAYCODE(GETS$)) AND DAY$ <> "00"
	THEN
		GETS$ = T_GETS$
		GOTO 3500
	END IF

	GETS$ = RIGHT(GETS$, 3%) IF DEF_LEN% = 6%

 L3000:	!
	! Display result
	!
	SMG_STATUS% = SMG$PUT_CHARS(XX_VDID%, PRNT_DATUM(GETS$, XFORMAT%), &
		XPOS, YPOS, 0%, SMG$M_BOLD) &
			IF (CPOS$ <> "") AND ((FLAG% AND 64%) = 0%)

	ENTR_DATUM = GETS$

	!
	! Return text in default if supposed to
	!
	DEFLT$ = GETS$ IF (FLAG% AND 128%)

	EXIT FUNCTION

3500	CALL ENTR_3MESSAGE(SCOPE, "Bad Date!", 0%)
	GETS$ = GETS$ + SPACE$(DEF_LEN% - LEN(GETS$))
	GOTO L1110

	END FUNCTION
