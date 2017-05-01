1	%TITLE "Function to Enter a Real Number"
	%SBTTL "ENTR_3NUMBER"
	%IDENT "V3.6a Calico"

	FUNCTION REAL ENTR_3NUMBER(SCOPE_STRUCT SCOPE, &
		LONG XX_VDID, STRING OP_CPOS, STRING OP_PROMPT, &
		REAL XDEFLT, LONG OP_FLAG, &
		STRING OP_XFORMAT, STRING OP_DEFLT)

	!
	! COPYRIGHT (C) 1987 BY
	! Computer Management Center
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
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This function will enter numbers.
	!	.lm -5
	!
	! Index:
	!
	! Parameters:
	!
	!	OP_CPOS
	!		'' - Do not display on top area
	!
	!	OP_FLAG
	!	.table
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
	!		128 - Return fincal value in default
	!	.endtable
	!
	!	XX_VDID
	!		Passed variable that creates or deletes the
	!		window that holds the string.
	!
	!	OP_PROMPT
	!		The passed string used for the prompt and its initialization
	!
	!	XDEFLT
	!		One of the passed defaults for the number.
	!
	!	OP_XFORMAT
	!		The passed format for the number.
	!
	!	OP_DEFLT
	!		The default form of the number size.
	!
	!	Returned value
	!		Enters a formatted number on the screen.
	!
	! Example:
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:ENTR_3NUMBER/LINE
	!	$ LIB FUNC_LIB:CMC_3VECTOR/REP ENTR_3NUMBER
	!	$ DELETE ENTR_3NUMBER.OBJ;*
	!
	! Author:
	!
	!	06/24/85 - Kevin Handy
	!
	! Modification history:
	!
	!	11/27/85 - Kevin Handy
	!		Modified to allow comma's, dollar signs, O for 0,
	!		l for 1.
	!
	!	01/26/87 - Kevin Handy
	!		Modified for default value.
	!
	!	07/14/87 - Kevin Handy
	!		Modified for return in OP_DEFLT ON 128.
	!
	!	11/10/88 - Frank F. Starman
	!		Added test for "Number out of Range" and delete message.
	!
	!	12/08/88 - Kevin Handy
	!		Moved test (re. 11/10/88) so that it will not occur
	!		if not entering (since there is nothing that can be
	!		done about it at that time anyway).
	!
	!	12/08/88 - Kevin Handy
	!		Modified check (see previous modification) so
	!		that it only checks for a leading % sign, because
	!		in payroll it is displaying a % in the number format.
	!
	!	02/09/89 - Kevin Handy
	!		Modified for change in ENTR_ENTER.
	!
	!	12/11/89 - Kevin Handy
	!		Modified to put into the sharable library.
	!
	!	05/02/90 - Frank F. Starman
	!		Replace ENTR_3MESSAGE with HELP_34MESSAGE function.
	!
	!	04/30/91 - Frank F. Starman
	!		Replace RESUME L3000 with GOTO L3000.
	!
	!	06/16/92 - Kevin Handy
	!		Clean up (check)
	!
	!	08/25/92 - Kevin Handy
	!		Fix buggered error trapping for 120.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/13/96 - Kevin Handy
	!		Reformat source code.
	!		Lose extra '&' before 'then'
	!
	!	08/15/96 - Kevin Handy
	!		Move 'on error goto' up to try to catch
	!		errors better.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/28/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	DECLARE LONG XPOS, YPOS

	!
	! Initilize for error trapping (illegal number)
	!
	ON ERROR GOTO 19000

	!
	! Split out cursor positioning function
	!
	CALL DSPL_SPLITCURSOR(OP_CPOS, X%, Y%)
	XPOS = X%
	YPOS = Y%

	%PAGE

	!
	! Function to replace letters with other letters
	!
	DEF FNREP$(ORGIN$, LOKFOR$, REPWIT$)
 L100:		I% = INSTR(1%, ORGIN$, LOKFOR$)
		IF I%
		THEN
			ORGIN$ = LEFT(ORGIN$, I% - 1%) + REPWIT$ + &
				RIGHT(ORGIN$, I% + 1%)
			GOTO L100
		END IF

		FNREP$ = ORGIN$
	FNEND

	!
	! Select value to use
	!
120	IF (OP_FLAG AND 32%)
	THEN
		WHEN ERROR IN
			GETS = VAL(OP_DEFLT)
		USE
			GETS = 0.0
			CONTINUE 130 IF (ERL = 120%)
			CONTINUE IllNum
		END WHEN
	ELSE
		GETS = XDEFLT
	END IF

	!
	! Handle if print only flag set
	!
130	GOTO L3000 IF (OP_FLAG AND 1%)

	!
	! Set up a format string to enter a number. Allow maximum
	! display to be 999999999.<xlen>
	!
	JUNK% = INSTR(1%, OP_XFORMAT, ".")
	XLEN2% = 0%
	XLEN2% = XLEN2% + 1% IF MID(OP_XFORMAT, I%, 1%) = "#" &
		FOR I% = JUNK% + 1% TO LEN(OP_XFORMAT) IF JUNK%

	!
	! Display original value
	!
	IF (OP_CPOS <> "") AND ((OP_FLAG AND 64%) = 0%)
	THEN
		WHEN ERROR IN
			SMG_STATUS% = SMG$PUT_CHARS( &
				XX_VDID, &
				FORMAT$(GETS, OP_XFORMAT), &
				XPOS, &
				YPOS, &
				0%, &
				SMG$M_REVERSE &
			)
		USE
			IF ERR = 116%
			THEN
				OP_XFORMAT = "<0>#####.##"
				RETRY
			END IF
			EXIT HANDLER
		END WHEN
	END IF

	!
	! Initilize default value
	!
	GETS1$ = NUM1$(FUNC_ROUND(GETS * 10.0 ^ XLEN2%, 0%))
	GETS1$ = GETS1$ + SPACE$(LEN(OP_XFORMAT) - LEN(GETS1$))

1000	!
	! Initilization/prompt
	!
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
	SMG_STATUS% = SMG$PUT_CHARS &
		(SCOPE::SMG_OPTION, OP_PROMPT + " <value>:", 1%, 1%, 1%)
	Y1POS% = LEN(OP_PROMPT) + 11%

	!
	! Normal entry
	!
	GETS$ = GETS1$ + ""
	TEMP% = ENTR_3ENTER(SCOPE, SCOPE::SMG_OPTION, 1%, Y1POS%, &
		GETS$, -1%, OP_FLAG)

	SELECT SCOPE::SCOPE_EXIT

	CASE 3%, 256%, 290%	! Cntrl C, PF1, Exit
		GETS = XDEFLT
		GOTO L3000
	END SELECT

	!
	! Clean up input string as necessary
	!
	GETS$ = EDIT$(GETS$, -1%)		! Remove garbage
	GETS$ = FNREP$(GETS$, ",", "")		! Strip comma's
	GETS$ = FNREP$(GETS$, "$", "")		! Strip dollars
	GETS$ = FNREP$(GETS$, "O", "0")		! Change O's to 0's
	GETS$ = FNREP$(GETS$, "L", "1")		! Change l's to 1's

	!
	! Take value entered
	!
	WHEN ERROR IN
		GETS = VAL(GETS$)
	USE
		CONTINUE IllNum
	END WHEN
	GETS = GETS / (10.0 ^ XLEN2%) UNLESS INSTR(1%, GETS$, ".")
	GETS = FUNC_ROUND(GETS, XLEN2%)

	!
	!Test for range
	!
	WHEN ERROR IN
		TEST.FORMAT$ = FORMAT$(GETS, OP_XFORMAT)
	USE
		IF ERR = 116%
		THEN
			OP_XFORMAT = "<0>#####.##"
			RETRY
		END IF
		EXIT HANDLER
	END WHEN
	IF LEFT(EDIT$(TEST.FORMAT$, -1%), 1%) = "%"
	THEN
		CALL HELP_34MESSAGE(SCOPE, "number out of range", &
			"W", "ENTR_3NUMBER", "", "ILLNUMBER")

		GETS1$ = GETS$ + SPACE$(LEN(OP_XFORMAT) - LEN(GETS$))
		GOTO 1000
	END IF

 L3000:	!
	! Exit function
	!
	IF (OP_CPOS <> "") AND ((OP_FLAG AND 64%) = 0%)
	THEN
		WHEN ERROR IN
			SMG_STATUS% = SMG$PUT_CHARS( &
				XX_VDID, &
				FORMAT$(GETS, OP_XFORMAT), &
				XPOS, &
				YPOS, &
				0%, &
				SMG$M_BOLD)
		USE
			IF ERR = 116%
			THEN
				OP_XFORMAT = "<0>#####.##"
				RETRY
			END IF
			EXIT HANDLER
		END WHEN
	END IF

	ENTR_3NUMBER = GETS

	!
	! Return value in OP_DEFLT if supposed to
	!
	IF (OP_FLAG AND 128%)
	THEN
		WHEN ERROR IN
			OP_DEFLT = FORMAT$(GETS, OP_XFORMAT)
		USE
			IF ERR = 116%
			THEN
				OP_XFORMAT = "<0>#####.##"
				RETRY
			END IF
			EXIT HANDLER
		END WHEN
	END IF

	!
	! Erase message if there are any
	!
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)

	EXIT FUNCTION

 IllNum:
	CALL HELP_34MESSAGE(SCOPE, "illegal number format", &
		"W", "ENTR_3NUMBER", "", "ILLNUMBER")
	!++
	! Warning:ILLNUMBER
	!	^*Illegal Number\*
	!	.b
	!	.lm +5
	!	^*Explanation\*
	!	.b
	!	The number has an invalid format or out of range value.
	!	.b
	!	^*User Action\*
	!	.b
	!	Check the format or help message for the range, and
	!	re-enter the value.
	!	.lm -5
	!
	! Index:
	!	.x Number
	!
	!--

	GETS1$ = GETS$ + SPACE$(LEN(OP_XFORMAT) - LEN(GETS$))
	GOTO 1000

18900	!*******************************************************************
	! Display error message
	!*******************************************************************

	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", "ENTR_3NUMBER", "", NUM1$(ERR))

	ENTR_3NUMBER = XDEFLT

	EXIT FUNCTION

19000	!*******************************************************************
	! Error trapping
	!*******************************************************************

	RESUME 18900

	END FUNCTION
