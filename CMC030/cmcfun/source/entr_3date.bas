1	%TITLE "Function to Enter a Date"
	%SBTTL "ENTR_3DATE"
	%IDENT "V3.6a Calico"

	FUNCTION STRING ENTR_3DATE(SCOPE_STRUCT SCOPE, &
		LONG XX_VDID, STRING OPT_CPOS, STRING OPT_PROMPT, &
		STRING OPT_XDFLT, LONG OPT_FLAG, STRING OPT_XFORMAT, &
		STRING OPT_DEFLT)

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
	!	This function enters a date.
	!	OPT_CPOS '' - Do not display on top area
	!	OPT_FLAG
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
	! Parameters:
	!
	!	SCOPE
	!		Structure created during window initilization.
	!
	!	XX_VDID
	!		 Passed variable that creates or deletes the window that holds the string.
	!
	!	OPT_CPOS
	!		Position to place on XX_VDID.
	!		Does not display on XX_VDID if blank.
	!
	!	OPT_PROMPT
	!		The passed string used for the prompt and its initialization
	!
	!	OPT_XDFLT
	!		One of the passed defaults for the date.
	!
	!	OPT_FLAG
	!		.TABLE
	!		  1 - Don't enter data (display only?)
	!
	!		  4 - Force keypunch input(no <CR> after input)
	!
	!		  8 - Indicates a timeout on input will occur
	!
	!		 32 - Use default value
	!
	!		 64 - Don't display
	!
	!		128 - Return final value in default
	!		.END TABLE
	!
	!	OPT_XFORMAT
	!		The passed format for the date.
	!
	!	OPT_DEFLT
	!		The passed default form the string size.
	!
	!
	!	This function enters a date on the screen.
	!
	! Example:
	!
	! Index:
	!
	!	.x Date>Enter
	!	.x Enter>Date
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:ENTR_3DATE
	!	$ LIB FUNC_LIB:CMC_3VECTOR/REP ENTR_3DATE
	!	$ DELETE ENTR_3DATE.OBJ;*
	!
	! AUTHOR:
	!
	!	07/02/85 - Kevin Handy
	!
	! MODIFICATION HISTORY:
	!
	!	01/06/86 - Cal Rasmussen
	!		Allow return of null string if C/R hit
	!
	!	01/07/86 - Cal Rasmussen
	!		Strips trailing spaces from default unless null or
	!		all spaces.  Map variables are accommodated also.
	!
	!	01/23/87 - Kevin Handy
	!	04/23/87 - Kevin Handy
	!		Modified to check the date range.  Must be numeric
	!		and each part must be in range.
	!
	!	05/24/88 - Kevin Handy
	!		Modified to use DATE_3SELECT function.
	!		Cleaned up some of the code used to force in
	!		more complete date information.
	!
	!	02/09/89 - Kevin Handy
	!		Modified for change in ENTR_ENTER
	!
	!	12/20/89 - Kevin Handy
	!		Modified for sharable library
	!
	!	05/01/90 - Frank F. Starman
	!		Replace ENTR_3MESSAGE with HELP_34MESSAGE function.
	!
	!	03/23/93 - Kevin Handy
	!		Clean up (CHeck)
	!
	!	07/16/93 - Kevin Handy
	!		Clean out lots of goofy code. Use DATE_STOREDATE
	!		to reformat it so don't double up the date
	!		formatting. Removed error trap for 2000 since
	!		no errors are possible. Removed line
	!		"LSET GETS$ = GETS$" which did nothing.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	07/11/96 - Kevin Handy
	!		Check for year 2000 problems. May be funny going
	!		over century (adding wrong century to a two digit
	!		entry, but user can type 4 digit year anyway)
	!		Reformat source code.
	!
	!	04/14/97 - Kevin Handy
	!		Lose code that calculated CENTIS$ and YEARIS$,
	!		which was not used anywhere.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	DECLARE LONG XPOS
	DECLARE LONG YPOS

	%PAGE

	!
	! Seperate cursor position
	!
	CALL DSPL_SPLITCURSOR(OPT_CPOS, X%, Y%)
	XPOS = X%
	YPOS = Y%

	!
	! Choose the default to use
	!
	IF (OPT_FLAG AND 32%) = 0%
	THEN
		GETS$ = OPT_XDFLT
	ELSE
		GETS$ = OPT_DEFLT + SPACE$(LEN(OPT_XDFLT) - LEN(OPT_DEFLT))
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
	XFORMAT% = LEN(GETS$)
	XFORMAT% = 6% IF OPT_XFORMAT = "6"
	XFORMAT% = 8% IF OPT_XFORMAT = "8"

	!
	! Display only?
	!
	GOTO L3000 IF (OPT_FLAG AND 1%) <> 0%

	!
	! Display original
	!
	SMG_STATUS% = SMG$PUT_CHARS(XX_VDID, PRNT_DATE(GETS$, XFORMAT%), &
		XPOS, YPOS, 0%, SMG$M_REVERSE) &
		IF (OPT_CPOS <> "") AND ((OPT_FLAG AND 64%) = 0%)

	SCOPE::SCOPE_EXIT = 0%

 L1000:	!
	! Initialization/prompt
	!
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, &
		OPT_PROMPT + " <date>:", 1%, 1%, 1%, 0%)
	Y1POS% = LEN(OPT_PROMPT) + 10%

 L1100:	!
	! Normal entry
	!
	T_GETS$ = GETS$
	GETS$ = RIGHT(GETS$, DEF_LEN% - 3%) + LEFT(GETS$, DEF_LEN% - 4%)

 L1110:	SELECT ENTR_3ENTER(SCOPE, SCOPE::SMG_OPTION, &
		1%, Y1POS%, GETS$, -1%, OPT_FLAG)

	!
	! Pick out all of the exit keys
	!
	CASE SMG$K_TRM_CTRLC, SMG$K_TRM_F8, &
		SMG$K_TRM_F10, SMG$K_TRM_CTRLZ

		GETS$ = T_GETS$
		GOTO L3000

	END SELECT

	T_GETS$ = GETS$ + ""
	GETS$ = EDIT$(GETS$, 2%)

	!
	! Check date input for valid format
	!
	IF TRM$(GETS$) <> ""
	THEN
		IF GETS$ <> XLATE(GETS$, STRING$(48%, 0%) + "0123456789")
		THEN
			GOTO 3500
		END IF
	END IF

	GETS$ = DATE_STOREDATE(GETS$)

	IF TRM$(GETS$) <> ""
	THEN
		IF LEN(TRM$(GETS$)) <> 8%
		THEN
			CALL HELP_34MESSAGE(SCOPE, "invalid date format", "W", &
				"ENTR_3DATE", "", "INVDATE")
			GETS$ = T_GETS$
			GOTO L1110
		END IF
	END IF

	!++
	! Warning:INVDATE
	!	^*Invalid Date\*
	!	.b
	!	.lm +5
	!	^*Explanation\*
	!	.b
	!	Invalid date or illegal date format.
	!	.b
	!	^*User Action\*
	!	.b
	!	The format for date entry is MMDDYY or MMDDYYYY, where
	!	MM is the month, DD is the day, YY is the year without entering the
	!	century (default is the current century), and YYYY is a full year.
	!	.b
	!	Check for the correct date or format and re-enter the input.
	!	.lm -5
	!
	! Index:
	!	.x date
	!
	!--


2000	!
	! Now Check out the date (Allow a blank date)
	!
	IF (TRM$(GETS$) <> "")
	THEN
		IF (GETS$ <> DATE_INVDCODE(DATE_DAYCODE(GETS$)))
		THEN
			GETS$ = T_GETS$
			GOTO 3500
		END IF
	END IF

	!
	! Handle user pressing list choices
	!
	IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14) AND ((OPT_FLAG AND 1024%) = 0%)
	THEN
		GETS$ = DATE_3SELECT(SCOPE, GETS$)
		GOTO L1100
	END IF

	GETS$ = RIGHT(GETS$, 3%) IF DEF_LEN% = 6%

 L3000:	!
	! Display result
	!
	SMG_STATUS% = SMG$PUT_CHARS(XX_VDID, PRNT_DATE(GETS$, XFORMAT%), &
		XPOS, YPOS, 0%, SMG$M_BOLD) &
		IF (OPT_CPOS <> "") AND ((OPT_FLAG AND 64%) = 0%)

	ENTR_3DATE = GETS$

	!
	! Return text in default if supposed to
	!
	OPT_DEFLT = GETS$ IF (OPT_FLAG AND 128%)

	EXIT FUNCTION

3500	CALL HELP_34MESSAGE(SCOPE, "invalid date", "W", &
		"ENTR_3DATE", "", "INVDATE")

	GETS$ = GETS$ + SPACE$(DEF_LEN% - LEN(GETS$))
	GOTO L1110

	END FUNCTION
