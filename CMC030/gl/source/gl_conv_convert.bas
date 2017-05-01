1	%TITLE "Convert from RSTS/E to VMS"
	%SBTTL "GL_CONV_CONVERT"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1987, 1988 BY
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
	!	This program is used in the conversion from RSTS/E
	!	to VMS.
	!	.LM -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_CONV_CONVERT/LINE
	!	$ LINK/EXECUTABLE=GL_EXE: GL_CONV_CONVERT, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE GL_CONV_CONVERT.OBJ;*
	!
	! Author:
	!
	!	01/12/86 - Kevin Handy and B. Craig Larsen
	!
	! Modification history:
	!
	!	09/18/92 - Kevin Handy
	!		Fix bugs in conversion of GL_FINSTA.
	!
	!	03/26/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	12/15/95 - Kevin Handy
	!		Reformat source closer to 80 columns.
	!		Change RIGHT(NUM1$()) to FORMAT$().
	!
	!	05/14/97 - Kevin Handy
	!		Reformat source code
	!
	!	06/18/98 - Kevin Handy
	!		Changes to clean up some junk that RSTS
	!		seems to frequently put into fin codes.
	!
	!	08/13/98 - Kevin Handy
	!		Change '%INCLUDE %FROM %CDD' to '%INCLUDE'
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/23/99 - Kevin Handy
	!		Fix problem with the last period closed being
	!		set to 13 when it should be 1.
	!
	!	05/10/99 - Kevin Handy
	!		Ignore GLTOPR.DAS file
	!
	!	07/30/99 - Kevin Handy
	!		Use summary code "1" if it somes in as "0"
	!
	!	09/15/99 - Kevin Handy
	!		More problems with period numbers (assigned period
	!		16 when should have been 8). Subtracting a negitive
	!		number caused this.
	!
	!	09/15/2000 - Kevin Handy
	!		Use LIB$DELETE_FILE instead of KILL
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

10	ON ERROR GOTO 19000

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART)		GL_CHART_CDD	GL_CHART

	%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.HB"
	MAP (GL_PERIOD)		GL_PERIOD_CDD	GL_PERIOD

	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	MAP (GL_YYYY_PP)	GL_YYYY_PP_CDD	GL_YYYY_PP

	%INCLUDE "SOURCE:[GL.OPEN]GL_BUD_YYYY.HB"
	MAP (GL_BUD_YYYY)	GL_BUD_YYYY_CDD	GL_BUD_YYYY

	%INCLUDE "SOURCE:[GL.OPEN]GL_GJ_LINE.HB"
	MAP (GL_GJ_LINE)	GL_GJ_LINE_CDD	GL_GJ_LINE

	%INCLUDE "SOURCE:[GL.OPEN]GL_FINSTA.HB"
	MAP (GL_FINSTA)		GL_FINSTA_CDD	GL_FINSTA

	!
	! Declare channels
	!
	CALL ASSG_CHANNEL(GLSYS.CH%, STAT%)

	DIM PERIOD$(12%), MONTH$(12%)

	!*******************************************************************
	! Initilize Convert
	!*******************************************************************

	BATCH$ = "000000"
	LAST_BATCH$ = ""
	TOTAL = 0.0

	CALL READ_INITIALIZE

	CALL READ_DEVICE("GLSYS_ASC", GLSYS_ASC.DEV$, STAT%)

250	WHEN ERROR IN
		OPEN GLSYS_ASC.DEV$ + "GLSYS.ASC" FOR INPUT AS FILE GLSYS.CH%, &
			RECORDSIZE 512%, &
			ACCESS READ, &
			ALLOW READ
	USE
		CALL ENTR_3MESSAGE(SCOPE, "GLSYS.ASC file is missing", 0%)
		CONTINUE ExitProgram
	END WHEN

550	WHEN ERROR IN
		INPUT LINE #GLSYS.CH%, INP$
	USE
		CONTINUE 560 IF STRG$ <> ""
		CONTINUE 600
	END WHEN

	IF INSTR(1%, INP$, CHR$(13%)) = 0%
	THEN
		STRG$ = STRG$ + INP$
		GOTO 550
	END IF

	STRG$ = STRG$ + INP$

560	TEMP$ = "<STARTFILE>"
	IF INSTR(1%, STRG$, TEMP$)
	THEN
		ON_LOOP% = 5%
		ON_LOOP% = 1% IF INSTR(1% + LEN(TEMP$), STRG$, "CONTRL")
		ON_LOOP% = 2% IF INSTR(1% + LEN(TEMP$), STRG$, "CHART")
		ON_LOOP% = 3% IF INSTR(1% + LEN(TEMP$), STRG$, "FINSTA")
		ON_LOOP% = 4% IF INSTR(1% + LEN(TEMP$), STRG$, ".FS")
		ON_LOOP% = 6% IF INSTR(1% + LEN(TEMP$), STRG$, "BDGT")
		ON_LOOP% = 7% IF INSTR(1% + LEN(TEMP$), STRG$, "GJL")
		ON_LOOP% = 8% IF INSTR(1% + LEN(TEMP$), STRG$, "GLTOPDA")
		IF ON_LOOP% = 0%
		THEN
			PRINT STRG$
			STOP
		END IF

		ON ON_LOOP% GOSUB 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000%

		STRG$ = ""
		GOTO 550
	END IF

	TEMP$ = "<ENDFILE>"
	IF INSTR(1%, STRG$, TEMP$)
	THEN

		ON ON_LOOP% GOSUB 1200, 2200, 3200, 4200, 5200, 6200, 7200, 8200%

		STRG$ = ""
		GOTO 550
	END IF

	ON ON_LOOP% GOSUB 1100, 2100, 3100, 4100, 5100, 6100, 7100, 8100%

	STRG$ = ""

	GOTO 550

600	GOTO ExitProgram

1000	!
	! GL_PERIOD file does not exist, so create it
	!
	CALL ENTR_3MESSAGE(SCOPE, "Creating new GL_PERIOD file", 1%)
 !	KILL GL_PERIOD.DEV$ + "GL_PERIOD.CTR"
	SMG_STATUS% = LIB$DELETE_FILE(GL_PERIOD.DEV$ + "GL_PERIOD.CTR;*")

1050	%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.CRE"

	RETURN

1100	!
	! Create a period record
	!
	MONTH$(1%) = "JANUARY"
	MONTH$(2%) = "FEBURARY"
	MONTH$(3%) = "MARCH"
	MONTH$(4%) = "APRIL"
	MONTH$(5%) = "MAY"
	MONTH$(6%) = "JUNE"
	MONTH$(7%) = "JULY"
	MONTH$(8%) = "AUGUST"
	MONTH$(9%) = "SEPTEMBER"
	MONTH$(10%) = "OCTOBER"
	MONTH$(11%) = "NOVEMBER"
	MONTH$(12%) = "DECEMBER"

	WORK%, TEMP% = VAL(MID(STRG$, 20%, 2%))
	LASTPERCLO% = VAL(MID(STRG$, 12%, 2%))

	LASTPERCLO% = LASTPERCLO% - WORK% + 1%
	LASTPERCLO% = 12% + LASTPERCLO% IF LASTPERCLO% < 1%
	LASTPERCLO% = 1% IF LASTPERCLO% = 13%

	GL_PERIOD::NEWYEAR = WORK%

	GL_PERIOD::LASTPERCLO = LASTPERCLO%

	FOR LOOP% = 1% TO 12%
		PERIOD$(LOOP%) = MONTH$(TEMP%)
		TEMP% = TEMP% + 1%
		TEMP% = 1% IF TEMP% > 12%
	NEXT LOOP%

	GL_PERIOD::PERIOD(I%) = PERIOD$(I%) FOR I% = 1% TO 12%
	GL_PERIOD::PERIOD(13%) = "** NOT USED **"

	GL_PERIOD::FPFY = 12%
	YEAR% = VAL%(MID(STRG$, 16%, 2%))
	YEAR$ = MID(STRG$, 16%, 2%)
	IF (GL_PERIOD::NEWYEAR <> 1%) AND &
		(GL_PERIOD::LASTPERCLO >= GL_PERIOD::NEWYEAR)
	THEN
		!
		! Increment to next year because that is when the fiscal
		! period ends
		!
		GL_PERIOD::YEAR = FORMAT$(1901% + VAL%(MID(STRG$, 16%, 2%)), &
			"<0>###")
	ELSE
		GL_PERIOD::YEAR = FORMAT$(1900% + VAL%(MID(STRG$, 16%, 2%)), &
			"<0>###")
	END IF

	GL_PERIOD::BTHNUM = ""
	GL_PERIOD::CLOSEFLAG = "0"
	GL_PERIOD::SUMMARYTOTAL = 0.0
	GL_PERIOD::SUMMARYACCT = EDIT$(MID(STRG$, 2%, 8%), 8% + 128%)
	GL_PERIOD::ENDDATE(I%) = "" FOR I% = 1% TO 13%

	PUT #GL_PERIOD.CH%, RECORD 1%

1190	RETURN

1200	CLOSE #GL_PERIOD.CH%

	CALL ASSG_FREECHANNEL(GL_PERIOD.CH%)

	RETURN

2000	!
	! CHART file does not exist, so create it
	!
	CALL ENTR_3MESSAGE(SCOPE, "Creating new GL_CHART file", 1%)
 !	KILL GL_CHART.DEV$ + "GL_CHART.MAS"
	SMG_STATUS% = LIB$DELETE_FILE(GL_CHART.DEV$ + "GL_CHART.MAS;*")

2050	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.CRE"

2090	! Convert the chart of accounts
	RETURN

2100	GL_CHART::ACCT	= EDIT$(MID(STRG$, 2%, 8%), 4% + 8%)

	GOTO 2190 IF GL_CHART::ACCT = "ZZZZZ[1]"

	GL_CHART::DESCR	= EDIT$(MID(STRG$, 12%, 40%), 4%)
	GL_CHART::ACCTYPE = EDIT$(MID(STRG$, 54%, 1%), 4%)
	GL_CHART::ACCTYPE = "S" &
		IF MID(STRG$, 54%, 2%) = "OR"
	GL_CHART::SUMMARY = EDIT$(MID(STRG$, 58%, 1%), 4%)
	GL_CHART::SUMMARY = "1" &
		IF GL_CHART::SUMMARY = "0"

	GL_CHART::FLOW	= EDIT$(MID(STRG$, 61%, 4%), 4%)
	GL_CHART::FLOW	= "" &
		IF GL_CHART::FLOW  = "niqu"

	GL_CHART::WORK	= EDIT$(MID(STRG$, 67%, 4%), 4%)
	GL_CHART::WORK	= "" &
		IF GL_CHART::WORK = "e fi" &

	GL_CHART::FINTYPE = EDIT$(MID(STRG$, 73%, 4%) + &
		MID(STRG$, 79%, 2%) + &
		MID(STRG$, 83%, 2%) + &
		MID(STRG$, 87%, 2%), 4%)
	GL_CHART::FINTYPE = "" &
		IF GL_CHART::FINTYPE = "ame from u" &

	GL_CHART::CPERIOD = LASTPERCLO%

	GL_CHART::DOLLAR(I%) = 0.0 FOR I% = 0% TO 20%
	GL_CHART::UNIT(I%) = 0.0 FOR I% = 0% TO 20%
	GL_CHART::HOUR(I%) = 0.0 FOR I% = 0% TO 20%

	WHEN ERROR IN
		GL_CHART::DOLLAR(I% - 1%) = &
			VAL(MID(STRG$, 81% + I% * 14%, 12%)) / 100.0 &
			FOR I% = 1% TO 15%
	USE
		GL_CHART::DOLLAR(I% - 1%) = 0.0
	END WHEN

	GL_CHART::RUNDOL = 0.0
	GL_CHART::RUNUNIT = 0.0
	GL_CHART::RUNHOUR = 0.0
	GL_CHART::CURDOL = 0.0
	GL_CHART::CURUNIT = 0.0
	GL_CHART::CURHOUR = 0.0

	PUT #GL_CHART.CH%

2190	RETURN

2200	CLOSE #GL_CHART.CH%

	CALL ASSG_FREECHANNEL(GL_CHART.CH%)

	RETURN

3000	!
	! GL_FINSTA file does not exist, so create it
	!
	CALL ENTR_3MESSAGE(SCOPE, "Creating new GL_FINSTA.MAS file", 1%)

 !	KILL GL_FINSTA.DEV$ + "GL_FINSTA.MAS"
	SMG_STATUS% = LIB$DELETE_FILE(GL_FINSTA.DEV$ + "GL_FINSTA.MAS;*")

3050	%INCLUDE "SOURCE:[GL.OPEN]GL_FINSTA.CRE"

	RETURN

3100	GL_FINSTA::PROMPT	= MID(STRG$, 2%, 4%)

	GL_FINSTA::DESCR	= MID(STRG$, 8%, 30%)
	GL_FINSTA::REPTITLE	= MID(STRG$, 40%, 50%)
	GL_FINSTA::CMDFIL	= MID(STRG$, 92%, 20%)
	GL_FINSTA::FINTYPE	= MID(STRG$, 114%, 1%)
	GL_FINSTA::FINCMD(I%)	= MID(STRG$, 117% + I% * 18%, 16%) &
		FOR I% = 0% TO 8%

	PUT #GL_FINSTA.CH%

	RETURN

3200	CLOSE #GL_FINSTA.CH%

	CALL ASSG_FREECHANNEL(GL_FINSTA.CH%)

	RETURN

4000	!
	! GL_FINCMD file does not exist, so create it
	!
	TEMP% = INSTR(1%, STRG$, "=")
	STOP IF TEMP% = 0%
	FILE_NAME$ = EDIT$(RIGHT(STRG$,TEMP% + 1%), -1%)
	CALL ENTR_3MESSAGE(SCOPE, "Creating new " + FILE_NAME$ + " file", 1%)

 !	KILL FILE_NAME$
	SMG_STATUS% = LIB$DELETE_FILE(FILE_NAME$ + ";*")

4050	CALL ASSG_CHANNEL(GL_FINCMD.CH%, STAT%)

	OPEN FILE_NAME$ FOR OUTPUT AS FILE GL_FINCMD.CH%, &
		RECORDSIZE 132%

	RETURN

4100	PRINT #GL_FINCMD.CH%, STRG$

	RETURN

4200	CLOSE GL_FINCMD.CH%

	CALL ASSG_FREECHANNEL(GL_FINCMD.CH%)

	RETURN

5000	!
	! GL LEDGER file does not exist, so create it
	!
	LAST_BATCH$ = ""
	TOTAL = 0.0

	STRG$ = EDIT$(STRG$, -1%)
	TEMP% = INSTR(1%, STRG$, "=")
	TEMP$ = RIGHT(STRG$, TEMP% + 1%)
	STOP IF TEMP% = 0%
	PP$ = MID(TEMP$, 3%, 3%)
	YY$ = "19" + MID(TEMP$, 6%, 2%)
	YY$ = "19" + YEAR$ IF YY$ = "19DA"

	TEST% = 0%
	FOR LOOP% = 1% TO 12%
		TEST% = LOOP% IF LEFT(PERIOD$(LOOP%), 3%) = PP$
	NEXT LOOP%

	TEST1% = 0%
	FOR LOOP% = 1% TO 12%
		TEST1% = LOOP% IF LEFT(MONTH$(LOOP%), 3%) = PP$
	NEXT LOOP%

	IF TEST%
	THEN
		PP$ = FORMAT$(TEST%, "<0>#") IF TEST%
	END IF

	IF (GL_PERIOD::NEWYEAR <> 1%) AND &
		(TEST1% >= GL_PERIOD::NEWYEAR)
	THEN
		!
		! Increment to next year because that is when the fiscal
		! period ends
		!
		YY$ = FORMAT$(VAL%(YY$) + 1%, "<0>###")
	END IF

	YYYY_PP$ = YY$ + "_" + PP$

	CALL ENTR_3MESSAGE(SCOPE, "Creating new " + YYYY_PP$ + &
		" file from " + EDIT$(STRG$, -1%), 1%)

 !	KILL GL_YYYY_PP.DEV$ + "GL_" +  YYYY_PP$ + ".LED"
	SMG_STATUS% = LIB$DELETE_FILE(GL_YYYY_PP.DEV$ + "GL_" + &
		YYYY_PP$ + ".LED;*")

5050	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.CRE"

	RETURN

5100	GL_YYYY_PP::ACCT	= EDIT$(MID(STRG$, 2%, 8%), 8% + 128%)
	GL_YYYY_PP::SOURCE	= MID(STRG$, 12%, 2%)
	GL_YYYY_PP::REFNO	= EDIT$(MID(STRG$, 16%, 16%), 8% + 128%)
	GL_YYYY_PP::TRANDAT	= "19" + MID(STRG$, 34%, 6%)
	GL_YYYY_PP::DESCR	= MID(STRG$, 42%, 26%)
	GL_YYYY_PP::AMOUNT	= VAL(MID(STRG$, 70%, 12%)) / 100.0
	GL_YYYY_PP::XREFNO	= EDIT$(MID(STRG$, 84%, 6%), 8% + 128%)
	GL_YYYY_PP::POSTIM	= MID(STRG$, 92%, 6%)
	GL_YYYY_PP::POSDAT	= "19" + MID(STRG$, 100%, 6%)
	GL_YYYY_PP::CKNO	= EDIT$(MID(STRG$, 116%, 8%), 8% + 128%)
	GL_YYYY_PP::TRANKEY	= EDIT$(MID(STRG$, 126%, 6%), 8% + 128%)
	GL_YYYY_PP::SUBACC	= EDIT$(MID(STRG$, 134%, 6%), 8% + 128%)
	GL_YYYY_PP::OPERATION	= MID(STRG$, 142%, 6%)
	GL_YYYY_PP::UNITS	= 0.0

	WHEN ERROR IN
		GL_YYYY_PP::HOURS	= VAL(MID(STRG$, 150%, 12%)) / 100.0 + &
			VAL(MID(STRG$, 164%, 12%)) / 100.0
	USE
		GL_YYYY_PP::HOURS = 0.0
	END WHEN

	GL_YYYY_PP::UPDSTA	= MID(STRG$, 178%, 2%)

	IF (GL_YYYY_PP::TRANDAT <> LAST_BATCH$) AND (FUNC_ROUND(TOTAL, 2%) = 0)
	THEN
		CALL FUNC_INCREMENT(BATCH$)
		LAST_BATCH$ = GL_YYYY_PP::TRANDAT
		TOTAL = 0.0
	END IF
	GL_YYYY_PP::BTHNUM	= BATCH$
	TOTAL = TOTAL + GL_YYYY_PP::AMOUNT


5110	PUT #GL_YYYY_PP.CH%

	RETURN

5200	CLOSE #GL_YYYY_PP.CH%

	CALL ASSG_FREECHANNEL(GL_YYYY_PP.CH%)

	RETURN

6000	!
	! BUDGET file does not exist, so create it
	!
	STRG$ = EDIT$(STRG$, -1%)
	TEMP% = INSTR(1%, STRG$, "=")
	STOP IF TEMP% = 0%
	YY$ = MID(STRG$, TEMP% + 5%, 2%)

	GL_BUDGET.YEAR$ = "19" + YY$

	CALL ENTR_3MESSAGE(SCOPE, &
		"Creating new " + GL_BUDGET.YEAR$ + " file", 1%)

6050	%INCLUDE "SOURCE:[GL.OPEN]GL_BUD_YYYY.CRE"

	RETURN

6100	GL_BUD_YYYY::DOLLAR(LOOP%)	= 0.0 FOR LOOP% = 0% TO 13%
	GL_BUD_YYYY::UNIT(LOOP%)	= 0.0 FOR LOOP% = 0% TO 13%
	GL_BUD_YYYY::HOUR(LOOP%)	= 0.0 FOR LOOP% = 0% TO 13%

	GL_BUD_YYYY::ACCT		= EDIT$(MID(STRG$, 2%, 8%), 8% + 128%)

	GL_BUD_YYYY::DOLLAR(I%) = VAL(MID(STRG$, -2% + I% * 14%, 12%)) / 100. &
		FOR I% = 1% TO 13%

	PUT #GL_BUD_YYYY.CH%

	RETURN

6200	CLOSE #GL_BUD_YYYY.CH%

	CALL ASSG_FREECHANNEL(GL_BUD_YYYY.CH%)

	RETURN

7000	!
	! JOURNAL file does not exist, so create it
	!
	STRG$ = EDIT$(STRG$, -1%)
	TEMP% = INSTR(1%, STRG$, "=")
	STOP IF TEMP% = 0%
	TEMP$ = MID(STRG$, TEMP% + 4%, 1%)

	JRL_TYPE$ = "1"
	JRL_TYPE$ = "2" IF TEMP$ = "R"
	JRL_TYPE$ = "3" IF TEMP$ = "C"

	CALL ENTR_3MESSAGE(SCOPE, "Creating new " + JRL_TYPE$ + " file", 1%)

7050	%INCLUDE "SOURCE:[GL.OPEN]GL_GJ_LINE.CRE"

	RETURN

7100	GL_GJ_LINE::JOURNAL	= EDIT$(MID(STRG$, 2%, 6%), 8% + 128%)
	GL_GJ_LINE::ITEMNUM	= MID(STRG$, 10%, 4%)
	GL_GJ_LINE::SOURCE	= "GJ"
	GL_GJ_LINE::ACCT	= EDIT$(MID(STRG$, 16%, 8%), 8% + 128%)
	GL_GJ_LINE::DESCR	= MID(STRG$, 26%, 26%)
	GL_GJ_LINE::TRANDAT	= "19" + MID(STRG$, 58%, 6%)
	GL_GJ_LINE::CKNO	= EDIT$(MID(STRG$, 66%, 8%), 8% + 128%)
	GL_GJ_LINE::XREFNO	= EDIT$(MID(STRG$, 76%, 6%), 8% + 128%)
	GL_GJ_LINE::TRANKEY	= EDIT$(MID(STRG$, 84%, 6%), 8% + 128%)
	GL_GJ_LINE::SUBACC	= EDIT$(MID(STRG$, 92%, 6%), 8% + 128%)
	GL_GJ_LINE::OPERATION	= MID(STRG$, 100%, 6%)
	GL_GJ_LINE::AMOUNT	= VAL(MID(STRG$, 116%, 12%)) / 100.0
	GL_GJ_LINE::UNITS	= 0.0
	GL_GJ_LINE::POSTIM	= ""
	GL_GJ_LINE::POSDAT	= ""
	GL_GJ_LINE::BATCH	= ""

	WHEN ERROR IN
		GL_GJ_LINE::HOURS	= VAL(MID(STRG$, 130%, 12%)) / 100.0 + &
			VAL(MID(STRG$, 144%, 12%)) / 100.0
	USE
		GL_GJ_LINE::HOURS = 0.0
	END WHEN

	WHEN ERROR IN
		PUT #GL_GJ_LINE.CH%
	USE
		CONTINUE 7190
	END WHEN

7190	RETURN

7200	CLOSE #GL_GJ_LINE.CH%

	CALL ASSG_FREECHANNEL(GL_GJ_LINE.CH%)

	RETURN

	%PAGE

8000	!
	! Skip a file
	!
	STRG$ = EDIT$(STRG$, -1%)
 !	PRINT "Skipping "; STRG$

	RETURN

8100	RETURN

8200	RETURN

	%PAGE

 ExitProgram:
15000	!*******************************************************************
	! Exit program
	!*******************************************************************

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

	!===================================================================
	! Error NUMBER(ERR) cases
	!===================================================================
	SELECT ERR

	CASE 154%	! Locked Block
		SLEEP 1%
		RESUME
	END SELECT

	!===================================================================
	! Error LINE(ERL) cases
	!===================================================================
	FILENAME$ = ""
	RESUME HelpError

32767	END
