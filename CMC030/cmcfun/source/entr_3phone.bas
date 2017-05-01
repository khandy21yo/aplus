1	%TITLE "Function to Enter a Phone Number"
	%SBTTL "ENTR_3PHONE"
	%IDENT "V3.6a Calico"

	FUNCTION STRING ENTR_3PHONE(SCOPE_STRUCT SCOPE, &
		LONG XX_VDID, STRING OP_CPOS, &
		STRING OP_PROMPT, STRING OP_XDEFLT, LONG OP_FLAG, &
		LONG OP_XFORMAT, STRING OP_DEFLT)

	!
	! COPYRIGHT (C) 1987 BY
	!
	! Computer Management Center,
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
	!	This function enters a phone number.
	!	.lm -5
	!
	! Index:
	!
	! Parameters:
	!
	!	OP_CPOS
	!		Position to display phone number.
	!		"" - Do not display on top area
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
	!		128 - Return value in OP_DEFLT
	!	.endtable
	!
	!	XX_VDID
	!		The passed variable that creates or deletes the
	!		window that holds the string.
	!
	!	OP_PROMPT
	!		Passed string used for the prompt and its initialization
	!
	!	OP_XDEFLT
	!		One of the passed defaults for the phone.
	!
	!	XFORMAT$
	!		The passed format for the phone.
	!
	!	OP_DEFLT
	!		The default form of the phone size.
	!
	!
	!	This function enters a phone number on the screen.
	!
	! Example:
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:ENTR_3PHONE/NOLINE
	!	$ LIB FUNC_LIB:CMC_3VECTOR/REP ENTR_3PHONE
	!	$ DELETE ENTR_3PHONE.OBJ;*
	!
	! Author:
	!
	!	05/15/87	B. Craig Larsen
	!
	! Modification history:
	!
	!	07/14/87	Kevin Handy
	!		Modified to return value in OP_DEFLT
	!
	!	02/09/89	Kevin Handy
	!		Modified for change in ENTR_ENTER.
	!
	!	12/11/89	Kevin Handy
	!		Modified to put in sharable library.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/03/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include's
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	DECLARE LONG XPOS, YPOS

	%PAGE

	!
	! Seperate cursor position
	!
	CALL DSPL_SPLITCURSOR(OP_CPOS, XPOS, YPOS)

	!
	! Choose the default to use
	!
	IF (OP_FLAG AND 32%) = 0%
	THEN
		GETS$ = OP_XDEFLT
	ELSE
		GETS$ = OP_DEFLT + SPACE$(LEN(OP_XDEFLT) - LEN(OP_DEFLT))
	END IF

	!
	! Display only?
	!
	GOTO L3000 IF (OP_FLAG AND 1%)

	!
	! Display original
	!
	SMG_STATUS% = SMG$PUT_CHARS(XX_VDID, PRNT_PHONE(GETS$, OP_XFORMAT), &
		XPOS, YPOS, 0%, SMG$M_REVERSE) &
		IF (OP_CPOS <> "") AND ((OP_FLAG AND 64%) = 0%)

	SCOPE::SCOPE_EXIT = 0%

 L1000:	!
	! Initialization/prompt
	!
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, OP_PROMPT + " <phone>:", &
		1%, 1%, 1%, 0%)
	Y1POS% = LEN(OP_PROMPT) + 11%


	!
	! Normal entry
	!
 L1110:	SELECT ENTR_3ENTER(SCOPE, SCOPE::SMG_OPTION, 1%, Y1POS%, GETS$, -1%, OP_FLAG)

	!
	! Pick out all of the exit keys
	!
	CASE SMG$K_TRM_CTRLC, SMG$K_TRM_F8, SMG$K_TRM_F10, &
		SMG$K_TRM_CTRLZ

		GETS$ = OP_XDEFLT
	END SELECT

	GETS$ = EDIT$(GETS$, 2%)

 L3000:	!
	! Display result
	!
	SMG_STATUS% = SMG$PUT_CHARS(XX_VDID, PRNT_PHONE(GETS$, OP_XFORMAT), &
		XPOS, YPOS, 0%, SMG$M_BOLD) &
		IF (OP_CPOS <> "") AND ((OP_FLAG AND 64%) = 0%)

	!
	! Return value
	!
	ENTR_3PHONE = GETS$

	!
	! Return value in OP_DEFLT if told to
	!
	OP_DEFLT = GETS$ IF (OP_FLAG AND 128%)

	END FUNCTION
