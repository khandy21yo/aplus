1	%TITLE "Convert from RSTS/E to VMS"
	%SBTTL "WP_CONV_CONVERT"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1992 BY
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
	!	.b
	!	.lm +5
	!	This program is used in the conversion from RSTS/E
	!	to VMS.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS WP_SOURCE:WP_CONV_CONVERT/LINE
	!	$ LINK/EXECUTABLE=WP_EXE: WP_CONV_CONVERT, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE WP_CONV_CONVERT.OBJ;*
	!
	! Author:
	!
	!	08/20/92 - Dan Perkins
	!
	! Modification history:
	!
	!	09/22/92 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	05/19/97 - Kevin Handy
	!		Reformat source code.
	!
	!	08/28/97 - Kevin Handy
	!		Lose unecessary external definitions
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	09/19/2000 - Kevin Handy
	!		Use LIB$DELETE_FILE instead of KILL
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

10	!
	! Include files
	!
	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	MAP (SB_SUBACCOUNT)	SB_SUBACCOUNT_CDD	SB_SUBACCOUNT

	%INCLUDE "SOURCE:[JC.OPEN]JC_JOB.HB"
	MAP (SB_SUBACCOUNT)	JC_JOB_CDD		JC_JOB

	%INCLUDE "SOURCE:[WP.OPEN]WP_REGLINE.HB"
	MAP (WP_REGLINE)	WP_REGLINE_CDD		WP_REGLINE

	%INCLUDE "SOURCE:[WP.OPEN]WP_REQREGISTER.HB"
	MAP (WP_REQREGISTER)	WP_REQREGISTER_CDD	WP_REQREGISTER

	!*******************************************************************
	! Initilize File to Convert
	!*******************************************************************

	ON ERROR GOTO 19000

	!
	! Declare channels
	!
	CALL ASSG_CHANNEL(WPRMS.CH%, STAT%)

	CALL READ_INITIALIZE

	CALL READ_DEVICE("WPRMS_ASC", WPRMS_ASC.DEV$, STAT%)

	!
	! Open input file
	!
300	WHEN ERROR IN
		OPEN WPRMS_ASC.DEV$ + "WPRMS.ASC" FOR INPUT AS FILE WPRMS.CH%
	USE
		CALL ENTR_3MESSAGE(SCOPE, "File to convert is missing", 0%)
		CONTINUE ExitProgram
	END WHEN

	!
	! Process the input file
	!
	FILEFLAG% = 0%

1000	WHEN ERROR IN
		INPUT LINE #WPRMS.CH%, INP$
	USE
		CONTINUE ExitProgram
	END WHEN

	GOTO 1000 IF INP$ = ""

	INP$ = EDIT$(INP$, 4%)
	I2%  = INSTR(1%, INP$, ">")
	FLD$ = SEG$(INP$, 2%, I2% - 1%)
	DTA$ = RIGHT(INP$, I2% + 1%)

	SELECT FLD$

	CASE "STARTFILE SB_SUBACCOUNT"
		CONF$ = EDIT$(ENTR_3YESNO(SCOPE, DISPLAY_ID%, &
			"9;22", "Confirm converting SB_SUBACCOUNT file", &
			"N", 16%, "'", "N"), -1%)

		IF CONF$ = "Y"
		THEN
			GOSUB 3000
			GOSUB InitJCJobRec
			WORKFILE% = 1%
			FILEFLAG% = -1%
		END IF

	CASE "ENDFILE SB_SUBACCOUNT"
		CLOSE #SB_SUBACCOUNT.CH%
		CALL ASSG_FREECHANNEL(SB_SUBACCOUNT.CH%)
		FILEFLAG% = 0%

	CASE "STARTFILE WP_REGLINE"
		CONF$ = EDIT$(ENTR_3YESNO(SCOPE, DISPLAY_ID%, &
			"9;21", "Confirm converting WP_REGLINE file", &
			"N", 16%, "'", "N"), -1%)

		IF CONF$ = "Y"
		THEN
			GOSUB 3100
			GOSUB InitWPReglineRec
			WORKFILE% = 2%
			FILEFLAG% = -1%
		END IF

	CASE "ENDFILE WP_REGLINE"
		CLOSE #WP_REGLINE.CH%
		CALL ASSG_FREECHANNEL(WP_REGLINE.CH%)
		FILEFLAG% = 0%

	CASE "STARTFILE WP_REQREGISTER"
		CONF$ = EDIT$(ENTR_3YESNO(SCOPE, DISPLAY_ID%, &
			"9;23", "Confirm converting WP_REQREGISTER file", &
			"N", 16%, "'", "N"), -1%)

		IF CONF$ = "Y"
		THEN
			GOSUB 3200
			GOSUB InitWPReqregisterRec
			WORKFILE% = 3%
			FILEFLAG% = -1%
		END IF

	CASE "ENDFILE WP_REQREGISTER"
		CLOSE #WP_REQREGISTER.CH%
		CALL ASSG_FREECHANNEL(WP_REQREGISTER.CH%)
		FILEFLAG% = 0%

	END SELECT

	ON WORKFILE% GOSUB 10000, 11000, 12000 IF FILEFLAG%

	GOTO 1000

3000	!
	! Create JC_JOB file
	!
	CALL ENTR_3MESSAGE(SCOPE, "Creating new JC_JOB file", 1%)
 !	KILL SB_SUBACCOUNT.DEV$ + "SB_SUBACCOUNT.MAS"

	SMG_STATUS% = LIB$DELETE_FILE(SB_SUBACCOUNT.DEV$ &
		+ "SB_SUBACCOUNT.MAS;*")

3010	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.CRE"

	RETURN

3100	!
	! Create WP_REGLINE file
	!
	CALL ENTR_3MESSAGE(SCOPE, "Creating new WP_REGLINE file", 1%)
 !	KILL WP_REGLINE.DEV$ + "WP_REGLINE.HIS"

	SMG_STATUS% = LIB$DELETE_FILE(WP_REGLINE.DEV$ + "WP_REGLINE.HIS;*")

3110	%INCLUDE "SOURCE:[WP.OPEN]WP_REGLINE.CRE"

	RETURN

3200	!
	! Create WP_REQREGISTER file
	!
	CALL ENTR_3MESSAGE(SCOPE, "Creating new WP_REQREGISTER file", 1%)
 !	KILL WP_REQREGISTER.DEV$ + "WP_REQREGISTER.HIS"

	SMG_STATUS% = LIB$DELETE_FILE(WP_REQREGISTER.DEV$ + &
		"WP_REQREGISTER.HIS;*")

3210	%INCLUDE "SOURCE:[WP.OPEN]WP_REQREGISTER.CRE"

	RETURN

10000	SELECT FLD$

	CASE "ENDRECORD"
		WHEN ERROR IN
			PUT #SB_SUBACCOUNT.CH% IF ERRFLAG% = 0%
		USE
			FILENAME$ = "JC_JOB"
			CONTINUE HelpError
		END WHEN

		GOSUB InitJCJobRec

	CASE "SUBJECT"
		JC_JOB::SUBJECT		= DTA$

	CASE "JOB"
		JC_JOB::JOB		= DTA$

	CASE "DESCR"
		JC_JOB::DESCR		= DTA$

	CASE "TTYPE"
		JC_JOB::TTYPE		= DTA$

	CASE "CLASS"
		JC_JOB::CLASS		= DTA$

	CASE "BDATE"
		JC_JOB::BDATE		= DTA$

	CASE "SSTATUS"
		JC_JOB::SSTATUS		= DTA$

	CASE "EDATE"
		JC_JOB::EDATE		= DTA$

	CASE "LOCATION"
		JC_JOB::LOCATION	= DTA$

	CASE "OPERATOR"
		JC_JOB::OPERATOR	= DTA$

	CASE "REFNO"
		JC_JOB::REFNO		= DTA$

	CASE "BATCH"
		JC_JOB::BATCH		= DTA$

	CASE "POST_TIME"
		JC_JOB::POST_TIME	= DTA$

	CASE "POST_DATE"
		JC_JOB::POST_DATE	= DTA$

	END SELECT

	RETURN

11000	SELECT FLD$

	CASE "ENDRECORD"
		WHEN ERROR IN
			PUT #WP_REGLINE.CH% IF ERRFLAG% = 0%
		USE
			FILENAME$ = "WP_REGLINE"
			CONTINUE HelpError
		END WHEN

		GOSUB InitWPReglineRec

	CASE "JOB"
		WP_REGLINE::JOB		= DTA$

	CASE "ITEM"
		WP_REGLINE::LLINE	= DTA$

	CASE "TTYPE"
		WP_REGLINE::REC_TYPE	= DTA$
		WP_REGLINE::REC_TYPE	= "02" IF WP_REGLINE::REC_TYPE = "04"

	CASE "REC_TYPE"
		WP_REGLINE::TTYPE	= DTA$

	CASE "ITEMCODE"
		WP_REGLINE::ITEMCODE	= DTA$

	CASE "COST"
		WP_REGLINE::COST	= VAL(DTA$)

	CASE "DESCR"
		WP_REGLINE::DESCR	= DTA$

	CASE "QTY"
		WP_REGLINE::QTY		= VAL(DTA$)

	CASE "START_DATE"
		WP_REGLINE::START_DATE	= DTA$

	CASE "COMP_DATE"
		WP_REGLINE::COMP_DATE	= DTA$

	CASE "BATCH"
		WP_REGLINE::BATCH	= DTA$

	CASE "POST_TIME"
		WP_REGLINE::POST_TIME	= DTA$

	CASE "POST_DATE"
		WP_REGLINE::POST_DATE	= DTA$

	END SELECT

	RETURN

12000	SELECT FLD$

	CASE "ENDRECORD"
		WHEN ERROR IN
			PUT #WP_REQREGISTER.CH% IF ERRFLAG% = 0%
		USE
			FILENAME$ = "WP_REQREGISTER"
			CONTINUE HelpError
		END WHEN


		GOSUB InitWPReqregisterRec

	CASE "JOB"
		WP_REQREGISTER::JOB		= DTA$

	CASE "LLINE"
		WP_REQREGISTER::LLINE		= DTA$

	CASE "REQNUM"
		WP_REQREGISTER::REQNUM		= DTA$

	CASE "REQLINE"
		WP_REQREGISTER::REQLIN		= DTA$

	CASE "RECTYPE"
		WP_REQREGISTER::RECTYP		= DTA$

	CASE "PRODUCT"
		WP_REQREGISTER::PRODUCT		= DTA$

	CASE "LOCATION"
		WP_REQREGISTER::LOCATION	= DTA$

	CASE "QTY"
		WP_REQREGISTER::QTY		= VAL(DTA$)

	CASE "AMT"
		WP_REQREGISTER::AMT		= VAL(DTA$)

	CASE "TRANDATE"
		WP_REQREGISTER::TRANDATE	= DTA$

	CASE "OPERATOR"
		WP_REQREGISTER::OPERATOR	= DTA$

	CASE "PERIOD"
		WP_REQREGISTER::PERIOD		= DTA$

	CASE "POSTDATE"
		WP_REQREGISTER::POSTDATE	= DTA$

	CASE "POSTTIME"
		WP_REQREGISTER::POSTTIME	= DTA$

	CASE "BATCH"
		WP_REQREGISTER::BATCH		= DTA$

	END SELECT

	RETURN

 InitJCJobRec:
	JC_JOB::SUBJECT		= "J"
	JC_JOB::JOB		= ""
	JC_JOB::DESCR		= ""
	JC_JOB::TTYPE		= ""
	JC_JOB::CLASS		= ""
	JC_JOB::BDATE		= ""
	JC_JOB::SSTATUS		= "A"
	JC_JOB::EDATE		= ""
	JC_JOB::LOCATION	= ""
	JC_JOB::OPERATOR	= ""
	JC_JOB::REFNO		= ""
	JC_JOB::BATCH		= "CONV"
	JC_JOB::POST_TIME	= "000000"
	JC_JOB::POST_DATE	= "19920801"

	ERRFLAG% = 0%

	RETURN

 InitWPReglineRec:
	WP_REGLINE::JOB		= ""
	WP_REGLINE::LLINE	= "0000"
	WP_REGLINE::REC_TYPE	= ""
	WP_REGLINE::TTYPE	= "M"
	WP_REGLINE::ITEMCODE	= ""
	WP_REGLINE::COST	= 0.
	WP_REGLINE::DESCR	= ""
	WP_REGLINE::QTY		= 0.
	WP_REGLINE::START_DATE	= ""
	WP_REGLINE::COMP_DATE	= ""
	WP_REGLINE::BATCH	= "CONV"
	WP_REGLINE::POST_TIME	= "000000"
	WP_REGLINE::POST_DATE	= "19920801"

	ERRFLAG% = 0%

	RETURN

 InitWPReqregisterRec:
	WP_REQREGISTER::JOB		= ""
	WP_REQREGISTER::LLINE		= "0000"
	WP_REQREGISTER::REQNUM		= ""
	WP_REQREGISTER::REQLIN		= ""
	WP_REQREGISTER::RECTYP		= ""
	WP_REQREGISTER::PRODUCT		= ""
	WP_REQREGISTER::LOCATION	= ""
	WP_REQREGISTER::QTY		= 0.
	WP_REQREGISTER::AMT		= 0.
	WP_REQREGISTER::TRANDATE	= ""
	WP_REQREGISTER::OPERATOR	= ""
	WP_REQREGISTER::PERIOD		= ""
	WP_REQREGISTER::POSTDATE	= "19920801"
	WP_REQREGISTER::POSTTIME	= "000000"
	WP_REQREGISTER::BATCH		= "CONV"

	ERRFLAG% = 0%

	RETURN

 ExitProgram:
	!*******************************************************************
	! Exit program
	!*******************************************************************

	CLOSE #WPRMS.CH%
	CALL ASSG_FREECHANNEL(WPRMS.CH%)

	GOTO 32767

	%PAGE

 HelpError:
	!*******************************************************************
	! Help Message for an Error
	!*******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	GOTO ExitProgram

19000	!*******************************************************************
	! Error trapping
	!*******************************************************************

	FILENAME$ = ""
	RESUME HelpError

32767	END
