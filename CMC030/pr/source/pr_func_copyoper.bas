1	%TITLE "Copy from Operation to Operation"
	%SBTTL "PR_FUNC_COPYOPER"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PR_FUNC_COPYOPER(PR_OPER.CH%)

	!
	! COPYRIGHT (C) 1986, 1988 BY
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
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This program copies data from an Operation
	!	number, and copies it to a series of operation numbers
	!	specified by the user.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_FUNC_COPYOPER
	!	$ LIB FUNC_LIB:CMCFUN/REP PR_FUNC_COPYOPER
	!	$ DELETE PR_FUNC_COPYOPER.OBJ;*
	!
	! Author:
	!
	!	07/08/88 - Aaron Redd
	!
	!	07/08/88 - Lance Williams
	!
	! Modification history:
	!
	!	07/11/88 - Kevin Handy
	!		Modified to fix problem if original rate was zero,
	!		or new rate entered was zero.
	!
	!	06/19/90 - Frank F. Starman
	!		Add error trapping for line 1100.
	!
	!	06/03/91 - Kevin Handy
	!		Changed PRG_PROGRAM from "PR_FUNC_COPY" to
	!		"PR_FUNC_COPYOPER" so that help messages will
	!		work properly.
	!
	!	06/03/91 - Kevin Handy
	!		Added FORRATE business so that user can change
	!		only those that have an existing rate to a new one.
	!
	!	06/03/91 - Kevin Handy
	!		Modified to look only at most current opertion,
	!		and to ignore historical operations.
	!
	!	06/18/91 - Kevin Handy
	!		Modified to use FUNC_ROUND at various places in
	!		this function.
	!
	!	06/19/91 - Kevin Handy
	!		Modified to look at first three digits of rate only.
	!
	!	04/13/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/13/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards.
	!		Change SMG_COPY to SMG_COPY%
	!
	!	05/15/97 - Kevin Handy
	!		Reformat source code
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	09/13/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Map file
	!
	%INCLUDE "SOURCE:[PR.OPEN]PR_OPER.HB"
	MAP	(PR_OPER)	PR_OPER_CDD	PR_OPER

	!
	! Declare some variables
	!
	DECLARE LONG XLONG, YLONG
	DECLARE PR_OPER_CDD PR_OPER_LAST

	!
	! Declare constants
	!
	DECLARE INTEGER CONSTANT MAX_ITEM = 5%

	!
	! Dimension Array
	!
	DIM DATA_ARRAY$(5000%)
	DIM DATA_ARRAY(5000%)
	DIM DATA_HOURRATE(5000%)

	%PAGE

	ON ERROR GOTO 19000

	!
	! Set up for help
	!
	SCOPE::PRG_IDENT	= "H"
	SCOPE::PRG_PROGRAM	= "PR_FUNC_COPYOPER"
	SCOPE::PRG_ITEM	= "HELP"

	COPYFROM_START$ = "        "
	COPYFROM_END$ = "        "
	EFFECTDATE$ = "        "
	HOURRATE = 0.0

900	!
	! Create a display window
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY &
	( &
		18%, &
		78%, &
		SMG_COPY%, &
		SMG$M_BORDER &
	)

	!
	! Label the display
	!
	SMG_STATUS% = SMG$LABEL_BORDER(SMG_COPY%, &
		" Copy Operation to Operation ")

	GOSUB Repaint

	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY &
	( &
		SMG_COPY%, &
		SCOPE::SMG_PBID, &
		2%, &
		2% &
	)

	%PAGE

1000	!******************************************************************
	! Main option menu
	!******************************************************************

	GOSUB Repaint

	LOOP% = 0%
 Changer:
	LOOP% = LOOP% + 1%
	GOTO Changer IF LOOP% < 1%

	GOTO Copy  IF LOOP% > MAX_ITEM

	LOOP1% = LOOP%

 Changer1:
	FLAG% = 0%
	GOSUB DataEntry

	SELECT SCOPE::SCOPE_EXIT
	!
	! Control c
	!
	CASE 3%
		GOTO 1000

	!
	! Uparrow
	!
	CASE SMG$K_TRM_UP
		LOOP% = LOOP% - 1% IF LOOP% > 1%
		GOTO Changer1

	!
	! SMG$K_TRM_DOWN
	!
	CASE SMG$K_TRM_DOWN
		LOOP% = LOOP% + 1% IF LOOP% < MAX_ITEM
		GOTO Changer1

	!
	! Exit key
	!
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO ExitProgram

	END SELECT

	GOTO Changer

	%PAGE

 Copy:
	!*****************************************************
	! Copy
	!*****************************************************

	SCOPE::PRG_ITEM = "CONFIRM"
	INP$ = ENTR_3YESNO(SCOPE, SMG_COPY%, &
		"", "Confirm copy  - then press <Do> ", "N", 0%, "", "")

	SMG_STATUS% = SMG$PUT_CHARS(SCOPE::SMG_OPTION, SPACE$(80%), 1%, 1%)

	IF INP$ <> "Y"
	THEN
		GOTO ExitProgram
	END IF

	CALL ENTR_3MESSAGE(SCOPE, "", 1% + 16%)

	!
	! Find the first record
	!
1100	WHEN ERROR IN
		GET #PR_OPER.CH%, KEY #0% GE COPYFROM_START$, REGARDLESS
	USE
		CONTINUE NoOper IF ERR = 155%
		FILENAME$ = "PR_OPER"
		CONTINUE HelpError
	END WHEN
	PR_OPER_LAST = PR_OPER

	LOOP1% = 0%

2000	!
	! First loop starts here
	!
	! Get next record
	!
	WHEN ERROR IN
		GET #PR_OPER.CH%, REGARDLESS
	USE
		CONTINUE 2100 IF ERR=11%
		FILENAME$ = "PR_OPER"
		CONTINUE HelpError
	END WHEN

	IF (PR_OPER::OPER <> PR_OPER_LAST::OPER)
	THEN
		GOTO 2110 IF COPYFROM_END$ < PR_OPER_LAST::OPER
		GOSUB LookAtOper
	END IF

	PR_OPER_LAST = PR_OPER

	GOTO 2000

	%PAGE

	!*******************************************************************
	! Look at operation, and create array element if necessary
	!*******************************************************************

 LookAtOper:
	IF (FORRATE = 0.0) OR &
		(FUNC_ROUND(FORRATE - PR_OPER_LAST::HOUR_RATE, 3%) = 0.0)
	THEN
		LOOP1% = LOOP1% + 1%

		DATA_ARRAY$(LOOP1%) = PR_OPER_LAST::OPER

		IF (PR_OPER_LAST::HOUR_RATE = 0.0) OR (HOURRATE = 0.0)
		THEN
			DATA_ARRAY(LOOP1%) = PR_OPER_LAST::PIECE_RATE
			DATA_HOURRATE(LOOP1%) = PR_OPER_LAST::HOUR_RATE
		ELSE
			DATA_ARRAY(LOOP1%) = &
				FUNC_ROUND((PR_OPER_LAST::PIECE_RATE / &
				PR_OPER_LAST::HOUR_RATE) * HOURRATE, 6%)
			DATA_HOURRATE(LOOP1%) = HOURRATE
		END IF

		SMG_STATUS% = SMG$PUT_CHARS(SMG_COPY%, &
			"Copying " + PR_OPER_LAST::OPER + " to array", &
			17%, 22%,, SMG$M_REVERSE)
	END IF

	!
	! Try for next record
	!
	RETURN

	%PAGE

2100	!*******************************************************************
	! Second loop starts here
	!*******************************************************************

	GOSUB LookAtOper IF PR_OPER_LAST::OPER <= COPYFROM_END$

2110	GOTO 2200 IF LOOP1% = 0%

	SMG_STATUS% = SMG$PUT_CHARS(SMG_COPY%, SPACE$(30%), &
		17%, 22%,,)

	FOR LOOP2% = 1% to LOOP1%

		PR_OPER::OPER = DATA_ARRAY$(LOOP2%)
		PR_OPER::PIECE_RATE = DATA_ARRAY(LOOP2%)
		PR_OPER::EFFDATE = EFFECTDATE$
		PR_OPER::HOUR_RATE = DATA_HOURRATE(LOOP2%)

		SMG_STATUS% = SMG$PUT_CHARS(SMG_COPY%, &
			"Copying " + PR_OPER::OPER, &
			17%, 22%,, SMG$M_REVERSE)

		WHEN ERROR IN
			PUT #PR_OPER.CH%
		USE
			CONTINUE 2190 IF ERR = 134%
			FILENAME$ = "PR_OPER"
			CONTINUE HelpError
		END WHEN

2190	NEXT LOOP2%

2200	CALL ENTR_3MESSAGE(SCOPE, "", 1%)

	%PAGE

 ExitProgram:
	!******************************************************************
	! Exit the Function
	!******************************************************************

	SMG_STATUS% = SMG$UNPASTE_VIRTUAL_DISPLAY(SMG_COPY%, SCOPE::SMG_PBID)

	EXIT FUNCTION

 Repaint:
	!******************************************************************
	! Repaint the screen
	!******************************************************************

	DATA	4,20,	"Range to copy from:", &
		6,20,	"(01) From Operation#", &
		7,20,	"(02) To Operation#", &
		8,20,	"(03) For Rate", &
		10,20,	"Defualts Values:", &
		12,20,	"(04) Effective Date", &
		13,20,	"(05) Hour rate", &
		0, 0,	""

	RESTORE
	READ XLONG, YLONG, ATEXT$

	WHILE XLONG
		SMG_STATUS% = SMG$PUT_CHARS(SMG_COPY%, ATEXT$, XLONG, YLONG)
		READ XLONG, YLONG, ATEXT$
	NEXT

	FLAG% = 1%
	GOSUB DataEntry FOR LOOP% = 1% TO MAX_ITEM

	RETURN

	%PAGE

 DataEntry:
	!******************************************************************
	! Enter/Diaplay items
	!******************************************************************

	TEMP$ = TRM$(SCOPE::PRG_ITEM)

	SCOPE::PRG_ITEM = "FLD" + FORMAT$(LOOP%, "<0>##")

	SELECT LOOP%

	CASE 1%
	!++
	! Abstract:FLD001
	!	^*(01) From Operation Number\*
	!	.b
	!	.lm +5
	!	The ^*From Operation Number\* field enters the operation
	!	number which will be copied.
	!	.lm -5
	!
	! Index:
	!	.X Copy>Operation
	!	.x Operation>Copy
	!	.x From Operation Number
	!	.x Operation Number>From
	!
	!--
		COPYFROM_START$ = ENTR_3STRING(SCOPE, SMG_COPY%, "6;45", &
			"Operation #", &
			COPYFROM_START$, FLAG%, "'E", DEFLT$)

	CASE 2%
	!++
	! Abstract:FLD002
	!	^*(02) To Operation Number\*
	!	.b
	!	.lm +5
	!	The ^*To Operation Number\* enters the operation number
	!	to which the information will be copied.
	!	.lm -5
	!
	! Index:
	!	.x Operation Number>To
	!	.x To>Operation Number
	!
	!--
		COPYFROM_END$ = ENTR_3STRING(SCOPE, SMG_COPY%, "7;45", &
			"Operation #", &
			COPYFROM_END$, FLAG%, "'E", DEFLT$)

	CASE 3%
	!++
	! Abstract:FLD003
	!	^*(03) For Rate\*
	!	.b
	!	.lm +5
	!	The ^*For Rate\* field is used to specify which operations will
	!	receive the new rate specified in the following fields.
	!	It will create a new rate only if the current rate matches
	!	this rate.
	!	.b
	!	^*Note\*: This only looks at the first three decimal points of the
	!	original rate.
	!	.lm -5
	!
	! Index:
	!	.x For Rate
	!
	!--
		FORRATE = ENTR_3NUMBER(SCOPE, SMG_COPY%, "8;45", &
			"Hourly Rate", &
			FORRATE, FLAG%, "###.###", DEFLT$)

	CASE 4%

	!++
	! Abstract:FLD004
	!	^*(04) Effective Date\*
	!	.b
	!	.lm +5
	!	The ^*Effective Date\* field enters the date the copied
	!	operation number becomes effective.
	!	.lm -5
	!
	! Index:
	!	.x Effective Date
	!	.x Date>Effective
	!
	!--
		EFFECTDATE$ = ENTR_3DATE(SCOPE, SMG_COPY%, "12;45", &
			"Effective Date #", &
			EFFECTDATE$, FLAG%, "8", DEFLT$)

	CASE 5%
	!++
	! Abstract:FLD005
	!	^*(05) Hour Rate\*
	!	.b
	!	.lm +5
	!	The ^*Hour Rate\* field enters the hourly rate which
	!	will be used in the specified operation.
	!	.lm -5
	!
	! Index:
	!	.x Hour Rate
	!	.x Rate>Hour
	!
	!--
		HOURRATE = ENTR_3NUMBER(SCOPE, SMG_COPY%, "13;45", &
			"Hourly Rate", &
			HOURRATE, FLAG%, "###.######", DEFLT$)

	END SELECT

	SCOPE::PRG_ITEM = TEMP$

	RETURN

 NoOper:
	CALL HELP_34MESSAGE(SCOPE, "Can't Find an Operation", "E", &
		SCOPE::PRG_PROGRAM, "", "NOOPER")

	!++
	! Error:NOOPER
	!	^*Can't Find an Operation\*
	!	.p
	!	^*Explanation\*
	!	.p
	!	Can't find the starting operation for copy.
	!	.p
	!	^*User Action\*
	!	Check the starting operation or existance of oparation table
	!	and restart copy process again.
	!--

	GOTO ExitProgram

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	GOTO ExitProgram

19000	!******************************************************************
	! Error trapping
	!******************************************************************

	FILENAME$ = ""
	RESUME HelpError

32767	END FUNCTION
