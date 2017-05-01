1	%TITLE "Initilize to Use a Form"
	%SBTTL "OUTP_36FORMINIT"
	%IDENT "V3.6a Calico"

	FUNCTION LONG OUTP_36FORMINIT(SCOPE_STRUCT SCOPE, &
		STRING FORM_LIB, &
		STRING FORM_NAME, STRING FORM_TEXT, &
		LONG FORM_GROUP, FORM_GROUP_CDD FORM_GROUP())
	!
	!	COPYRIGHT (C) 1987 BY
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
	!	.p
	!	This function is used to load in a form, and do
	!	preliminary processing on it.
	!
	! Parameters:
	!
	!	FORM_LIB
	!		The passed form library
	!
	!	FORM_NAME
	!		The name of the form to load in.
	!
	!	FORM_TEXT
	!		The passed text of the form.
	!
	!	FORM_GROUP
	!		Passed pointers to the start of each group.
	!
	!	FORM_GROUP
	!		Number of groups that make up this form.
	!
	! Example:
	!
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:OUTP_36FORMINIT
	!	$ LIB FUNC_LIB:CMC_3VECTOR/REP OUTP_36FORMINIT
	!	$ DELETE OUTP_36FORMINIT.OBJ;*
	!
	! Author:
	!
	!	10/06/87 - Kevin Handy
	!
	! Modification history:
	!
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/20/95 - Kevin Handy
	!		Taken from OUPT_FORMINIT, and modified to add
	!		SCOPE to parameter list so that the function
	!		could be made sharable.
	!
	!	04/02/96 - Kevin Handy
	!		Reformatted so source won't wrap on 80 columns.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/19/2000 - Kevin Handy
	!		Lose error trapping, which SHOULD be unnecessary.
	!		None of the code here should be able to cause an error.
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"
	%INCLUDE "FUNC_INCLUDE:LIBRARY.COM"

	!
	! Map stuff
	!
	%INCLUDE "SOURCE:[UTL.OPEN]FORM_GROUP.HB"

	!
	! Declare variables
	!
	DECLARE RFA TXRFA

	%PAGE

	!
	! Initilize information
	!
	OUTP_36FORMINIT = 0%
	FORM_TEXT = ""
	FORM_GROUP = 0%

	!
	! Set up the control structure if necessary
	!
	ST% = LBR$INI_CONTROL(LR.INDEX%, LBR$C_READ)

	IF (ST% AND 1%) = 0%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "Unable to initilize library", 0%)
		OUTP_36FORMINIT = ST%
		GOTO ExitProgram
	END IF

1000	!
	! Open work file
	!
	ST% = LBR$OPEN(LR.INDEX%, FORM_LIB, , ".TLB")

	IF (ST% AND 1%) = 0%
	THEN
		OUTP_36FORMINIT = ST%
		GOTO ExitProgram
	END IF

1300	!
	! Search for key in file
	!
	ST% = LBR$LOOKUP_KEY(LR.INDEX%, FORM_NAME, TXRFA)

	IF (ST% AND 1%) = 0%
	THEN
		OUTP_36FORMINIT = ST%
		GOTO ExitProgram
	END IF

1400	!
	! Search for a line
	!
	TEXT$ = SPACE$(132%)

	ST% = LBR$GET_RECORD(LR.INDEX%, TEXT$)

	IF (ST% AND 1%) = 0%
	THEN
		GOTO CloseLibrary
	END IF

	GOTO 1400 IF LEFT(TEXT$, 1%) = "!"

	INLINE$ = INLINE$ + TRM$(TEXT$)

	!
	! Keep reading if line has a continuation marker
	!
	IF RIGHT(INLINE$, LEN(INLINE$) - 1%) = "<>"
	THEN
		INLINE$ = LEFT(INLINE$, LEN(INLINE$) - 2%)
		GOTO 1400
	END IF

1500	!
	! Search for the start of any groups
	!
	BEGIN_GROUP% = INSTR(1%, INLINE$, "<<")

	IF BEGIN_GROUP%
	THEN
		!
		! Find the end of the group
		!
		END_GROUP% = INSTR(BEGIN_GROUP%, INLINE$, ">>")

		IF END_GROUP% = 0%
		THEN
			CALL ENTR_3MESSAGE(SCOPE, "Unterminated group '" + &
				RIGHT(INLINE$, BEGIN_GROUP%) + "'", 0%)
			END_GROUP% = LEN(INLINE$) + 2%
		END IF

		!
		! Strip off information from the group definition
		!
		GROUP$ = SEG$(INLINE$, BEGIN_GROUP% + 2%, END_GROUP% - 1%) + " "
		BEGIN_NUMBER% = INSTR(1%, GROUP$, " ")

		FORM_GROUP = FORM_GROUP + 1%
		FORM_GROUP(FORM_GROUP)::FGROUP = LEFT(GROUP$, BEGIN_NUMBER%)
		FORM_GROUP(FORM_GROUP)::POINTER = LEN(FORM_TEXT) + BEGIN_GROUP%
		FORM_GROUP(FORM_GROUP)::NUMBER = &
			VAL%(RIGHT(GROUP$, BEGIN_NUMBER%))

		!
		! Rip the group definition out of the text, because we don't
		! need to store it and it would require intrepretation to
		! be duplicated in the output_form printer.
		!
		INLINE$ = LEFT(INLINE$, BEGIN_GROUP% - 1%) + &
			RIGHT(INLINE$, END_GROUP% + 2%)

		!
		! Look for additional groups.
		!
		GOTO 1500
	END IF

1600	!
	! Connect this bit to any currently there.  The '10'C markes the
	! end of a line.
	!
	FORM_TEXT = FORM_TEXT + INLINE$ + '10'C

	INLINE$ = ""

	!
	! Get more lines
	!
	GOTO 1400

	%PAGE

2000	!*******************************************************************
	! Finish up
	!*******************************************************************
 CloseLibrary:
	!
	! Point to tail end
	!
	FORM_GROUP(FORM_GROUP + 1%)::POINTER = LEN(FORM_TEXT) + 1%

3000	!
	! Now we are done
	!
 ExitProgram:
	ST% = LBR$CLOSE(LR.INDEX%)

32767	END FUNCTION
