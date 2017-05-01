1	%TITLE "Insert a Help File for a Particular System"
	%SBTTL "TK_SPEC_JOINFORM"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 2002 BY
	!
	! Software Solutions, Inc.
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
	! Software Solutions, Inc.
	!
	! Software Solutions, Inc. assumes no responsibility for the use
	! or reliability of its software on equipment which is not
	! supported by Software Solutions, Inc.
	!
	!++
	! Abstract:HELP
	!	.p
	!	Merge two forms together to generate a double wide form.
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS TK_SOURCE:TK_SPEC_JOINFORM/LINE
	!	$ LINK/EXECUTABLE=TK_EXE: TK_SPEC_JOINFORM, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE TK_SPEC_JOINFORM.OBJ;*
	!
	! Author:
	!
	!	01/28/2002 - Kevin Handy
	!
	! Modification history:
	!
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]FORM_GROUP.HB"
	DIM FORM_GROUP_CDD FORM_GROUP1(10%)	! Max of 10 groups
	DIM FORM_GROUP_CDD FORM_GROUP2(10%)	! Max of 10 groups

	!
	! External functions
	!
	EXTERNAL LONG   FUNCTION OUTP_FORMINIT

	%PAGE

	!
	! Handle input/output file
	!
	CALL ASSG_CHANNEL(FINAL.CH%, STAT%)

	!
	! Get info from the user
	!
	LINPUT "Source 1:   "; FORM1$
	INPUT  "Left Size:  "; LEFT_SIZE%
	LINPUT "Source 2:   "; FORM2$
	LINPUT "Output:     "; FINAL$


	CALL READ_DEVICE("PR_FORM", PR_FORM.DEV$, STAT%)

	SMG_STATUS% = OUTP_FORMINIT(PR_FORM.DEV$ + "PR_FORM", FORM1$, &
		FORM_TEXT1$, FORM_GROUP1%, FORM_GROUP1())

	IF SMG_STATUS% <> 0%
	THEN
		PRINT "Can't read FORM1"
		goto ExitProgram
	END IF

	SMG_STATUS% = OUTP_FORMINIT(PR_FORM.DEV$ + "PR_FORM", FORM2$, &
		FORM_TEXT2$, FORM_GROUP2%, FORM_GROUP2())

	IF SMG_STATUS% <> 0%
	THEN
		PRINT "Can't read FORM1"
		goto ExitProgram
	END IF

	OPEN FINAL$ FOR OUTPUT AS FILE FINAL.CH%, &
		RECORDSIZE 255%

	!
	! Loop through all the groups in form1
	!
	FOR FORM_LOOP% = 1% TO FORM_GROUP1%

		!
		! Left side stuff
		!
		CODE1$ = SEG$(FORM_TEXT1$, &
			FORM_GROUP1(FORM_LOOP%)::POINTER, &
			FORM_GROUP1(FORM_LOOP% + 1%)::POINTER - 1%)

		!
		! Right side stuff
		!
		FORM_LOOP2% = 0%
		FORM_LOOP2% = LOOP% &
			IF FORM_GROUP2(LOOP%)::FGROUP = &
				FORM_GROUP1(FORM_LOOP%)::FGROUP &
			FOR LOOP% = 1% TO FORM_GROUP2%

		IF FORM_LOOP2% = 0%
		THEN
PRINT "NO MATCH"
			CODE2$ = ""
		ELSE
			CODE2$ = SEG$(FORM_TEXT2$, &
				FORM_GROUP2(FORM_LOOP2%)::POINTER, &
				FORM_GROUP2(FORM_LOOP2% + 1%)::POINTER - 1%)
		END IF

		PRINT #FINAL.CH%, &
			"<<" + TRM$(FORM_GROUP1(FORM_LOOP%)::FGROUP) + ">><>"

		GOSUB JoinGroup

	NEXT FORM_LOOP%

	GOTO ExitProgram

	%PAGE

	!*******************************************************************
	! Given two blocks of forms, break then up to join them line-by-line
	!*******************************************************************

 JoinGroup:
	IF CODE1$ <> ""
	THEN
		!
		! Left hand line
		!
		I% = INSTR(1%, CODE1$, '10'C)
		IF I% <> 0%
		THEN
			LINE1$ = LEFT(CODE1$, I% - 1%)
			I% = I% + 1% &
				IF MID(CODE1$, I% + 1%, 1%) == '13'C
			CODE1$ = RIGHT(CODE1$, I% + 1%)
		ELSE
			LINE1$ = CODE1$
			CODE1$ = ""
		END IF

		!
		! Right hand line
		!
		I% = INSTR(1%, CODE2$, '10'C)
		IF I% <> 0%
		THEN
			LINE2$ = LEFT(CODE2$, I% - 1%)
			I% = I% + 1% &
				IF MID(CODE2$, I% + 1%, 1%) == '13'C
			CODE2$ = RIGHT(CODE2$, I% + 1%)
		ELSE
			LINE2$ = CODE2$
			CODE2$ = ""
		END IF

		GOSUB JoinLines

		GOTO JoinGroup
	END IF

 JoinGroup2:
	IF CODE2$ <> ""
	THEN
		CODE1$ = ""

		!
		! Right hand line
		!
		I% = INSTR(1%, CODE2$, '10'C)
		IF I% <> 0%
		THEN
			LINE2$ = LEFT(CODE2$, I% - 1%)
			I% = I% + 1% &
				IF MID(CODE2$, I% + 1%, 1%) == '13'C
			CODE2$ = RIGHT(CODE2$, I% + 1%)
		ELSE
			LINE2$ = CODE2$
			CODE2$ = ""
		END IF

		GOSUB JoinLines

		GOTO JoinGroup2
	END IF

	RETURN

	%PAGE

	!*******************************************************************
	! Given two form lines, try to join them together
	!*******************************************************************

 JoinLines:

	!
	! We need to calculate the length of the left hand part, so that
	! it can be properly padded
	!
	XLENGTH% = 0%
	XPOS% = 1%

	WHILE XPOS% < LEN(LINE1$)

		IF MID(LINE1$, XPOS%, 1%) = "<"
		THEN
			J% = INSTR(XPOS%, LINE1$, ">")
			IF J%
			THEN
				IF MID(LINE1$, XPOS% + 1%, 1%) = '"'
				THEN
					J1% = INSTR(XPOS% + 2%, LINE1$, '"')
					XLENGTH% = XLENGTH% + J1% - XPOS% - 2%
					OUTTAKE$ = SEG$(LINE1$, XPOS% + 2%, &
						J1% - 1%)
					XLENGTH% = XLENGTH% - 2% &
						IF INSTR(1%, OUTTAKE$, "<")
					J% = INSTR(J1%, LINE1$, ">")
					J% = LEN(LINE1$) IF J% = 0%
				END IF
				XPOS% = J% + 1%
			ELSE
				PRINT "Missing '>' for '<'"
				XLENGTH% = XLENGTH% + 1%
				XPOS% = XPOS% + 1%
			END IF
		ELSE
			XLENGTH% = XLENGTH% + 1%
			XPOS% = XPOS% + 1%
		END IF
	NEXT

	!
	! Output the lines
	!
	IF LINE1$ = "" AND LINE2$ = ""
	THEN
		PRINT #FINAL.CH%
	ELSE
		IF LINE1$ <> "" AND LINE2$ = ""
		THEN
			PRINT #FINAL.CH%, &
				LINE1$

		ELSE
			IF LINE1$ <> ""
			THEN
				PRINT #FINAL.CH%, &
					LINE1$ + "<>"
			END IF
		END IF

		IF LINE2$ <> ""
		THEN
			PRINT #FINAL.CH%, &
				SPACE$(LEFT_SIZE% - XLENGTH%) + "<>"

			PRINT #FINAL.CH%, &
				TRM$(LINE2$)
		ELSE
			PRINT #FINAL.CH%
		END IF
	END IF

	RETURN

 ExitProgram:

	CLOSE FINAL.CH%

32767	END
