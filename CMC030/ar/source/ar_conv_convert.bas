1	%TITLE "Convert AR from RSTS/E"
	%SBTTL "AR_CONV_CONVERT"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1989 BY
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
	!	.B
	!	.LM +5
	!	.LM -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_CONV_CONVERT/LINE
	!	$ LINK/EXEC:AR_EXE AR_CONV_CONVERT,FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AR_CONV_CONVERT.OBJ;*
	!
	! Author:
	!
	!	05/22/89 - Kevin Handy
	!
	! Modification history:
	!
	!	05/17/89 - Kevin Handy
	!		Added recordsize option to open statement.
	!
	!	08/21/92 - Kevin Handy
	!		Added more fields that can be converted in.
	!
	!	01/06/93 - Kevin Handy
	!		Modified to trim leading spaces off invoice number.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/11/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	07/20/99 - Kevin Handy
	!		Clean up code to prepare for a new set of
	!		conversions. (Added a lot of the newer fields)
	!
	!	09/14/2000 - Kevin Handy
	!		Use LIB$DELETE_FILE instead of KILL
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

10	ON ERROR GOTO 19000

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[AR.OPEN]AR_CLOSED.HB"
	MAP (AR_CLOSED)		AR_CLOSED_CDD	AR_CLOSED

	%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.HB"
	MAP (AR_CONTROL)	AR_CONTROL_CDD	AR_CONTROL

	%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.HB"
	MAP (AR_OPEN)		AR_OPEN_CDD	AR_OPEN

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP (AR_35CUSTOM)		AR_35CUSTOM_CDD	AR_35CUSTOM

	%INCLUDE "SOURCE:[AR.OPEN]AR_CUSBAL.HB"
	MAP (AR_CUSBAL)		AR_CUSBAL_CDD	AR_CUSBAL

	EXTERNAL STRING  FUNCTION FIND_STRING
	EXTERNAL STRING  FUNCTION DATE_TODAY

	CALL ASSG_CHANNEL(AR_CHKFRM.CH%, STAT%)
	CALL ASSG_CHANNEL(AR_ATTFRM.CH%, STAT%)
	CALL ASSG_CHANNEL(ARSYS.CH%, STAT%)

	!
	! Open the keyboard
	!
	CALL READ_INITIALIZE

200	! RESUME LINE

250	WHEN ERROR IN
		OPEN ARSYS_ASC.DEV$ + "ARSYS.ASC" FOR INPUT AS FILE ARSYS.CH%, &
			RECORDSIZE 512%, &
			ACCESS READ
	USE
		CALL ENTR_3MESSAGE(SCOPE, "ARSYS.ASC file is missing", 0%)
		CONTINUE ExitProgram
	END WHEN

550	WHEN ERROR IN
		INPUT LINE #ARSYS.CH%, INP$
	USE
		CONTINUE 560 IF STRG$ <> ""
		CONTINUE 600
	END WHEN

	IF INSTR(1%, INP$, "<ENDFILE>") = 0% AND &
		INSTR(1%, INP$, "<STARTFILE>") = 0%
	THEN
		IF INSTR(1%, INP$, "<>") = 0%
		THEN
			STRG$ = STRG$ + "<" + EDIT$(INP$, 4%) + ">"
			GOTO 550
		ELSE
			STRG$ = STRG$ + INP$
		END IF
	END IF

	STRG$ = STRG$ + INP$

560	TEMP$ = "<STARTFILE>"

	GOTO 580 IF INSTR(1%, STRG$, TEMP$) = 0%

	ON_LOOP% = 0%
	ON_LOOP% =  1% IF INSTR(1% + LEN(TEMP$), STRG$, "CONTRL")
	ON_LOOP% =  2% IF INSTR(1% + LEN(TEMP$), STRG$, "CUSTOM")
	ON_LOOP% =  3% IF INSTR(1% + LEN(TEMP$), STRG$, "ARREG")
	ON_LOOP% =  4% IF INSTR(1% + LEN(TEMP$), STRG$, "ARCLOS")
	ON_LOOP% =  5% IF INSTR(1% + LEN(TEMP$), STRG$, "CUSBAL")

	IF ON_LOOP% = 0%
	THEN
		PRINT STRG$
		STOP
	END IF

	ON ON_LOOP% GOSUB &
		1000,	2000,	3000,	4000,	5000

	STRG$ = ""
	GOTO 550

580	TEMP$ = "<ENDFILE>"

	IF INSTR(1%, STRG$, TEMP$)
	THEN

		ON ON_LOOP% GOSUB &
			1200,	2200,	3200,	4200,	5200

		STRG$ = ""
		GOTO 550
	END IF

	ON ON_LOOP% GOSUB &
		1100,	2100,	3100,	4100,	5100

	STRG$ = ""

	GOTO 550

600	GOTO ExitProgram

1000	!
	! AR_CONTROL file does not exist, so create it
	!
	!======================================================================
	CALL ENTR_3MESSAGE(SCOPE, "Creating new AR_CONTROL file", 1%)

 !	KILL AR_CONTROL.DEV$ + "AR_CONTROL.CTR"

	SMG_STATUS% = LIB$DELETE_FILE(AR_CONTROL.DEV$ + "AR_CONTROL.CTR;*")

1050	%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.CRE"

	RETURN

1100	!
	! Create new control record
	!
	AR_CONTROL::AR_ACCT = EDIT$(FIND_STRING(STRG$, "AR_ACCT"), -1%)

	AR_CONTROL::CLOSEFLAG = "0"
	AR_CONTROL::LASTPERCLOSE = 0%
	AR_CONTROL::YEAR = DATE_TODAY
	AR_CONTROL::RETAIN = 20%
	AR_CONTROL::AGEPER(I%) = 30% FOR I% = 0% TO 4%
	AR_CONTROL::AGENAM(0%) = "Current"
	AR_CONTROL::AGENAM(1%) = "30 Days"
	AR_CONTROL::AGENAM(2%) = "60 Days"
	AR_CONTROL::AGENAM(3%) = "90 days"
	AR_CONTROL::AGENAM(4%) = "Past 120"
	AR_CONTROL::CTITLE = "Customer"
	AR_CONTROL::METHOD = "O"

	PUT #AR_CONTROL.CH%, RECORD 1%

1190	RETURN

1200	CLOSE #AR_CONTROL.CH%
	RETURN

2000	!
	! Vendor file does not exist, so create it
	!
	!======================================================================
	CALL ENTR_3MESSAGE(SCOPE, "Creating new  AR_35CUSTOM file", 1%)

 !	KILL AR_35CUSTOM.DEV$ + "AR_35CUSTOM.MAS"

	SMG_STATUS% = LIB$DELETE_FILE(AR_35CUSTOM.DEV$ + "AR_35CUSTOM.MAS;*")

2050	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.CRE"

2090	! Convert the Vendor
	COUNTER% = 0%
	RETURN

2100	!
	! Create new AR_35CUSTOM record
	!
	COUNTER% = COUNTER% + 1%
	AR_35CUSTOM::CUSNUM	= FIND_STRING(STRG$, "CUSNUM")
	AR_35CUSTOM::CUSNAM	= FIND_STRING(STRG$, "CUSNAM")
	AR_35CUSTOM::ADD1	= FIND_STRING(STRG$, "ADD1")
	AR_35CUSTOM::SSTATUS	= "A"
	AR_35CUSTOM::BDATE	= ""
	AR_35CUSTOM::EDATE	= ""
	AR_35CUSTOM::ADD2	= FIND_STRING(STRG$, "ADD2")
	AR_35CUSTOM::ADD3	= ""
	AR_35CUSTOM::CITY	= FIND_STRING(STRG$, "CITY")
	AR_35CUSTOM::STATE	= FIND_STRING(STRG$, "STATE")
	AR_35CUSTOM::ZIP	= FIND_STRING(STRG$, "ZIP")
	AR_35CUSTOM::COUNTRY	= FIND_STRING(STRG$, "COUNTRY")
	AR_35CUSTOM::COUNTY	= ""
	TEMP$ = FIND_STRING(STRG$, "PHONE")
	WHILE INSTR(1%, TEMP$, "-")
		I% = INSTR(1%, TEMP$, "-")
		TEMP$ = LEFT(TEMP$, I% - 1%) + RIGHT(TEMP$, I% + 1%)
	NEXT
	RSET AR_35CUSTOM::PHONE	= TEMP$
	AR_35CUSTOM::ALPSRT	= FIND_STRING(STRG$, "ALPSRT")
	AR_35CUSTOM::METHOD	= FIND_STRING(STRG$, "METHOD")
	AR_35CUSTOM::STMTFLG	= FIND_STRING(STRG$, "STMTFLG")
	AR_35CUSTOM::SERCHRG	= FIND_STRING(STRG$, "SERCHRG")
	AR_35CUSTOM::SALESMAN	= FIND_STRING(STRG$, "SLSMAN")
	AR_35CUSTOM::LOCATION	= "0" + FIND_STRING(STRG$, "LOC")
	AR_35CUSTOM::TTYPE	= FIND_STRING(STRG$, "TYPE")
	AR_35CUSTOM::CATEGORY	= FIND_STRING(STRG$, "CODE")
	AR_35CUSTOM::TAXCODE	= FIND_STRING(STRG$, "STATE")
	AR_35CUSTOM::TAXEXEMP	= FIND_STRING(STRG$, "STAXNUM")
	AR_35CUSTOM::TERMS	= ""
	AR_35CUSTOM::CARRIER	= ""
	AR_35CUSTOM::CREDITLIM	= 0.0
	AR_35CUSTOM::DISCOUNT	= 0.0
	IF (FIND_STRING(STRG$, "TAXFLG") = "N")
	THEN
		AR_35CUSTOM::TAXFLAG	= "4"
	ELSE
		AR_35CUSTOM::TAXFLAG	= "1"
	END IF
	AR_35CUSTOM::BACKORDER	= "Y"

	AR_35CUSTOM::COUNTY	= ""

	PUT	#AR_35CUSTOM.CH%

	RETURN

2200	CLOSE #AR_35CUSTOM.CH%
	RETURN

3000	!
	! AP open file does not exist, so create it
	!
	!======================================================================
	CALL ENTR_3MESSAGE(SCOPE, "Creating new  AR_OPEN file", 1%)

 !	KILL AR_OPEN.DEV$ + "AR_OPEN.LED"

	SMG_STATUS% = LIB$DELETE_FILE(AR_OPEN.DEV$ + "AR_OPEN.LED;*")

3050	%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.CRE"

3090	! Convert the AP OPEN
	COUNTER% = 0%
	RETURN

3100	!
	! Create new AR_OPEN record
	!
	COUNTER% = COUNTER% + 1%

	AR_OPEN::CUSNUM		= FIND_STRING(STRG$, "CUSNUM")
	AR_OPEN::INVNUM		= EDIT$(FIND_STRING(STRG$, "INVNUM"), 8%)
	AR_OPEN::TRATYP		= FIND_STRING(STRG$, "TRATYP")
	AR_OPEN::TRADAT		= FIND_STRING(STRG$, "TRADAT")
	AR_OPEN::SALAMT		= VAL(FIND_STRING(STRG$, "SALAMT")) / 100.0
	AR_OPEN::DISAMT		= VAL(FIND_STRING(STRG$, "DISAMT")) / 100.0
	AR_OPEN::OTHCHG		= VAL(FIND_STRING(STRG$, "OTHCHG")) / 100.0

	AR_OPEN::RECNUM		= FIND_STRING(STRG$, "RECNUM")
	AR_OPEN::CHKNUM		= FIND_STRING(STRG$, "CHKNUM")
	AR_OPEN::ARACCT		= EDIT$(FIND_STRING(STRG$, "ARACCT"), -1%)
	AR_OPEN::SUBACC		= FIND_STRING(STRG$, "SUBACC")
	AR_OPEN::DESCR		= FIND_STRING(STRG$, "DESCR")
	AR_OPEN::SALNUM		= FIND_STRING(STRG$, "SALNUM")
	AR_OPEN::BATCH		= FIND_STRING(STRG$, "BATCH")
	AR_OPEN::UPDATED	= FIND_STRING(STRG$, "UPDATED")
	AR_OPEN::CLOSEDATE	= FIND_STRING(STRG$, "CLOSEDATE")
	AR_OPEN::DISCOUNTDATE	= AR_OPEN::TRADAT
	AR_OPEN::DUEDATE	= AR_OPEN::TRADAT

	AR_OPEN::BATCH		= AR_OPEN::CUSNUM IF (AR_OPEN::BATCH = "")

	PUT #AR_OPEN.CH%

	RETURN

3200	CLOSE #AR_OPEN.CH%
	RETURN

4000	!
	! AP open file does not exist, so create it
	!
	!======================================================================
	CALL ENTR_3MESSAGE(SCOPE, "Creating new  AR_CLOSED file", 1%)

 !	KILL AR_CLOSED.DEV$ + "AR_CLOSED.LED"

	SMG_STATUS% = LIB$DELETE_FILE(AR_CLOSED.DEV$ + "AR_CLOSED.LED;*")

4050	%INCLUDE "SOURCE:[AR.OPEN]AR_CLOSED.CRE"

4090	! Convert the AP OPEN
	COUNTER% = 0%
	RETURN

4100	!
	! Create new AR_CLOSED record
	!
	COUNTER% = COUNTER% + 1%

	AR_CLOSED::CUSNUM	= FIND_STRING(STRG$, "CUSNUM")
	AR_CLOSED::INVNUM	= EDIT$(FIND_STRING(STRG$, "INVNUM"), 8%)
	AR_CLOSED::TRATYP	= FIND_STRING(STRG$, "TRATYP")
	AR_CLOSED::TRADAT	= FIND_STRING(STRG$, "TRADAT")
	AR_CLOSED::SALAMT	= VAL(FIND_STRING(STRG$, "SALAMT")) / 100.0
	AR_CLOSED::DISAMT	= VAL(FIND_STRING(STRG$, "DISAMT")) / 100.0
	AR_CLOSED::OTHCHG	= VAL(FIND_STRING(STRG$, "OTHCHG")) / 100.0

	AR_CLOSED::RECNUM	= FIND_STRING(STRG$, "RECNUM")
	AR_CLOSED::CHKNUM	= FIND_STRING(STRG$, "CHKNUM")
	AR_CLOSED::ARACCT	= EDIT$(FIND_STRING(STRG$, "ARACCT"), -1%)
	AR_CLOSED::SUBACC	= FIND_STRING(STRG$, "SUBACC")
	AR_CLOSED::DESCR	= FIND_STRING(STRG$, "DESCR")
	AR_CLOSED::SALNUM	= FIND_STRING(STRG$, "SALNUM")
	AR_CLOSED::BATCH	= FIND_STRING(STRG$, "BATCH")
	AR_CLOSED::UPDATED	= FIND_STRING(STRG$, "UPDATED")
	AR_CLOSED::CLOSEDATE	= FIND_STRING(STRG$, "CLOSEDATE")
	AR_CLOSED::DISCOUNTDATE	= AR_CLOSED::TRADAT
	AR_CLOSED::DUEDATE	= AR_CLOSED::TRADAT

	PUT	#AR_CLOSED.CH%

	RETURN

4200	CLOSE #AR_CLOSED.CH%
	RETURN

5000	!
	! AR_CUSBAL file does not exist, so create it
	!
	!======================================================================
	CALL ENTR_3MESSAGE(SCOPE, "Creating new AR_CUSBAL file", 1%)

 !	KILL AR_CUSBAL.DEV$ + "AR_CUSBAL.TBL"

	SMG_STATUS% = LIB$DELETE_FILE(AR_CUSBAL.DEV$ + "AR_CUSBAL.TBL;*")

5050	%INCLUDE "SOURCE:[AR.OPEN]AR_CUSBAL.CRE"

	RETURN

5100	!
	! Create new AR_CUSBAL record
	!
	AR_CUSBAL::CUSNUM	= FIND_STRING(STRG$, "CUSNUM")
	AR_CUSBAL::ACCT		= FIND_STRING(STRG$, "ACCT")
	AR_CUSBAL::CREDIT	= VAL(FIND_STRING(STRG$, "CREDIT")) / 100.0
	AR_CUSBAL::AGING(0%)	= VAL(FIND_STRING(STRG$, "AGE1")) / 100.0
	AR_CUSBAL::AGING(1%)	= VAL(FIND_STRING(STRG$, "AGE2")) / 100.0
	AR_CUSBAL::AGING(2%)	= VAL(FIND_STRING(STRG$, "AGE3")) / 100.0
	AR_CUSBAL::AGING(3%)	= VAL(FIND_STRING(STRG$, "AGE4")) / 100.0
	AR_CUSBAL::AGING(4%)	= VAL(FIND_STRING(STRG$, "AGE5")) / 100.0
	AR_CUSBAL::FUTURE	= VAL(FIND_STRING(STRG$, "FUTURE")) / 100.0
	AR_CUSBAL::YTDSERVICE	= VAL(FIND_STRING(STRG$, "YTDSERVICE")) / 100.0
	AR_CUSBAL::YTDSALES	= VAL(FIND_STRING(STRG$, "YTDSALES")) / 100.0
	AR_CUSBAL::CHARGE	= VAL(FIND_STRING(STRG$, "CHARGE")) / 100.0
	AR_CUSBAL::LAST_CHARGE	= FIND_STRING(STRG$, "LAST_CHARGE")
	AR_CUSBAL::LAST_PAYMENT	= FIND_STRING(STRG$, "LAST_PAYMENT")
	AR_CUSBAL::LAST_UPDATE	= FIND_STRING(STRG$, "LAST_UPDATE")

	PUT #AR_CUSBAL.CH%

5190	RETURN

5200	CLOSE #AR_CUSBAL.CH%
	RETURN

 ExitProgram:
15000	!*******************************************************************
	! Exit program
	!*******************************************************************

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	GOTO ExitProgram

19000	!*******************************************************************
	! Error trapping
	!*******************************************************************

	!===================================================================
	! Error NUMBER(ERR) cases
	!===================================================================
	SELECT ERR

	CASE 134%, 153%	! Dup key detected, record already exists
		IF ERL > 1000%
		THEN
			CALL ENTR_3MESSAGE(SCOPE, "DUPLICATE KEY/RECORD EXISTS ERL" + &
				NUM1$(ERL) + " ERR" + NUM1$(ERR), 1%)
			RESUME 1190
		END IF
	END SELECT

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
