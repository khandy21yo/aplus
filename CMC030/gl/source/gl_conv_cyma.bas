1	%TITLE "Convert Cyma GL files to CMC"
	%SBTTL "GL_CONV_CYMA"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1991 BY
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
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_CONV_CYMA/LINE
	!	$ LINK/EXECUTABLE=GL_EXE: GL_CONV_CYMA, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE GL_CONV_CYMA.OBJ;*
	!
	! Author:
	!	07/17/91 - Frank F. Starman
	!
	! Modification history:
	!
	!	08/14/91 - Kevin Handy
	!		Removed A+.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	11/10/95 - Kevin Handy
	!		Lose GL_CHARTEX stuff.
	!
	!	09/04/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
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

	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.HB"
	MAP (GL_CHART)		GL_CHART_CDD	GL_CHART

	%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.HB"
	MAP (GL_PERIOD)		GL_PERIOD_CDD	GL_PERIOD

	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.HB"
	MAP (GL_YYYY_PP)	GL_YYYY_PP_CDD	GL_YYYY_PP

	%INCLUDE "SOURCE:[GL.OPEN]GL_BUD_YYYY.HB"
	MAP (GL_BUD_YYYY)	GL_BUD_YYYY_CDD	GL_BUD_YYYY

10	ON ERROR GOTO 19000

	!*******************************************************************
	! Initilize Convert
	!*******************************************************************

	CALL READ_INITIALIZE

	! Create the data display
	!
	SMG_STATUS% = SMG$CREATE_VIRTUAL_DISPLAY( 20%, 80%, DISPLAY_ID%,,, )
	CALL LIB$SIGNAL(SMG_STATUS%) IF (SMG_STATUS% AND 1%) = 0%

	SMG_STATUS% = SMG$ERASE_DISPLAY(DISPLAY_ID%)
	!
	! Paste the data display
	!
	SMG_STATUS% = SMG$PASTE_VIRTUAL_DISPLAY( DISPLAY_ID%, PASTE_ID%, 1%, 1% )
	CALL LIB$SIGNAL(SMG_STATUS%) IF (SMG_STATUS% AND 1%) = 0%
	SMG_STATUS% = SMG$SET_CURSOR_MODE(PASTE_ID%, SMG$M_CURSOR_OFF)

	!
	! PRINT BANNER
	!
	SMG_STATUS% = SMG$PUT_CHARS_WIDE(DISPLAY_ID%, "Convert Cyma GLfiles", &
		2%, 15%,SMG$M_BOLD)
	SMG_STATUS% = SMG$DRAW_LINE(DISPLAY_ID%, 4%, 1%, 4%, 80%, &
		SMG$M_BOLD)

 Password:

	EXT$="???"
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_ID%, "Company Password (XXX) :", 8%, 20%)
	EXT$ = ENTR_3STRING(SCOPE, DISPLAY_ID%, &
				"8;43", "Password", EXT$, 0%, "'E", "")

	!
	! Check for special keys typed
	!
	SELECT SCOPE::SCOPE_EXIT
	!
	! Control/C, Uparrow,
	!
	CASE 3%, SMG$K_TRM_UP, SMG$K_TRM_DOWN
		GOTO Password	! (Ignored)

	!
	! Exit key typed
	!
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO ExitProgram

	!
	! Good Keys
	!
	CASE 0%, 10%, 12%, SMG$K_TRM_DO, SMG$K_TRM_CR, &
		SMG$K_TRM_SELECT

	!
	! Bad Keys
	!
	CASE ELSE
		GOTO Password

	END SELECT

	! Ask user if they realy would like to convert all files

 ConfirmConv:
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_ID%, "Convert files :", 9%, 20%)
	CONF$ = EDIT$(ENTR_3YESNO(SCOPE, DISPLAY_ID%, &
		"9;36", "Confirm Converting", "N", 16%, "'", "N"), -1%)

	!
	! Check for special keys typed
	!
	SELECT SCOPE::SCOPE_EXIT
	!
	! Control/C, Uparrow,
	!
	CASE 3%, SMG$K_TRM_UP, SMG$K_TRM_DOWN
		GOTO ConfirmConv	! (Ignored)

	!
	! Exit key typed
	!
	CASE SMG$K_TRM_F10, SMG$K_TRM_CTRLZ
		GOTO ExitProgram

	!
	! Good Keys
	!
	CASE 0%, 10%, 12%, SMG$K_TRM_DO, SMG$K_TRM_CR, &
		SMG$K_TRM_SELECT

	!
	! Bad Keys
	!
	CASE ELSE
		GOTO ConfirmConv

	END SELECT

	SMG_STATUS% = SMG$DELETE_CHARS(DISPLAY_ID%, 30%, 9%, 20%)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_OPTION)
	SMG_STATUS% = SMG$ERASE_DISPLAY(SCOPE::SMG_MESSAGE)

	GOTO ExitProgram IF CONF$ <> "Y"

	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_ID%, "File   :", 18%, 4%)
	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_ID%, "Record :", 19%, 4%)



1000	!
	! GL_PERIOD file does not exist, so create it
	!
	CALL ENTR_3MESSAGE(SCOPE, "Creating new GL_PERIOD file", 1%)
	KILL GL_PERIOD.DEV$ + "GL_PERIOD.CTR"

1050	%INCLUDE "SOURCE:[GL.OPEN]GL_PERIOD.CRE"

1060	!
	! Create a period record
	!
	GL_PERIOD::NEWYEAR = 1%
	GL_PERIOD::LASTPERCLO = 12%
	GL_PERIOD::PERIOD(I%) = "" FOR I% = 1% TO 12%
	GL_PERIOD::PERIOD(13%) = "** NOT USED **"

	GL_PERIOD::FPFY = 12%
	GL_PERIOD::YEAR = "1991"
	GL_PERIOD::BTHNUM = ""
	GL_PERIOD::CLOSEFLAG = "0"
	GL_PERIOD::SUMMARYTOTAL = 0.0
	GL_PERIOD::SUMMARYACCT = ""
	GL_PERIOD::ENDDATE(I%) = "" FOR I% = 1% TO 13%

	PUT #GL_PERIOD.CH%, RECORD 1%

1200	CLOSE #GL_PERIOD.CH%

	CALL ASSG_FREECHANNEL(GL_PERIOD.CH%)

	!
	! CHART file does not exist, so create it
	!
	IF PASS%=0%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, "Creating new GL_CHART file", 1%)
		KILL "GL_CHART.MAS"
 !		KILL "GL_CHARTEX.MAS"
		PASS% = -1%
	ELSE
		CALL ENTR_3MESSAGE(SCOPE, "Adding records into GL_CHART file", 1%)
	END IF

2000	%INCLUDE "SOURCE:[GL.OPEN]GL_CHART.CRE"

2005 !	%INCLUDE "SOURCE:[GL.OPEN]GL_CHARTEX.CRE"

2010	CALL ASSG_CHANNEL(GLISAM.CH%,STAT%)

	OPEN "GLISAM."+EXT$ FOR INPUT AS FILE GLISAM.CH%, &
		ORGANIZATION SEQUENTIAL

	LINPUT #GLISAM.CH%, LINE$

	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_ID%, "GLISAM  ", 18%, 13%)

2020	LINPUT #GLISAM.CH%, LINE$

	GOTO 2020 IF LEFT(LINE$,1%)="1" OR VAL(MID$(LINE$,10%,4%))=0%

	SELECT VAL(MID$(LINE$,2%,8%))
	CASE 0 TO 19999999
		GL_CHART::ACCTYPE= "A"
	CASE 20000000 TO 39999999
		GL_CHART::ACCTYPE= "L"
	CASE 40000000 TO 40999999
		GL_CHART::ACCTYPE= "O"
	CASE 41000000 TO 49999999
		GL_CHART::ACCTYPE= "S"
	CASE 50000000 TO 59999999
		GL_CHART::ACCTYPE= "R"
	CASE ELSE
		GL_CHART::ACCTYPE= "E"
	END SELECT

	GL_CHART::ACCT	= MID$(LINE$,2%,4%)+"."+MID$(LINE$,6%,4%)
	GL_CHART::DESCR	= ""
	GL_CHART::SUMMARY= "1"
	GL_CHART::WORK	= ""
	GL_CHART::FLOW	= MID$(LINE$,10%,4%)
	GL_CHART::FINTYPE= ""
	GL_CHART::CPERIOD= 6%

	GL_CHART::DOLLAR(I%) = 0.0 FOR I%=0% TO 20%
	GL_CHART::UNIT(I%) = 0.0 FOR I%=0% TO 20%
	GL_CHART::HOUR(I%) = 0.0 FOR I%=0% TO 20%

	GL_CHART::RUNDOL = 0.0
	GL_CHART::RUNUNIT = 0.0
	GL_CHART::RUNHOUR = 0.0
	GL_CHART::CURDOL = 0.0
	GL_CHART::CURUNIT = 0.0
	GL_CHART::CURHOUR = 0.0

2030	PUT #GL_CHART.CH%

2040 !	GL_CHARTEX::ACCOUNT = GL_CHART::ACCT
 !	GL_CHARTEX::SYSTEM = ""
 !	GL_CHARTEX::CATEGORY = ""
 !
 !	PUT #GL_CHARTEX.CH%

	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_ID%, GL_CHART::ACCT, 19%, 13%)

	GOTO 2020

2120	CLOSE #GLMASTER.CH%

	CALL ASSG_FREECHANNEL(GLMASTER.CH%)
	CALL ASSG_CHANNEL(GLMASTER.CH%,STAT%)

2130	OPEN "GLMASTER."+EXT$ FOR INPUT AS FILE GLMASTER.CH%, &
		ORGANIZATION SEQUENTIAL

	LINPUT #GLMASTER.CH%, LINE$
	COUNTER% = 1%

	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_ID%, "GLMASTER", 18%, 13%)

2150	LINPUT #GLMASTER.CH%, LINE$
	COUNTER% = COUNTER% + 1%

2155	GET #GL_CHART.CH%, KEY#1% EQ FORMAT$(COUNTER%, "<0>###")

	GL_CHART::DESCR	= MID$(LINE$,1%,30%)
	GL_CHART::FLOW	= ""
	GL_CHART::DOLLAR(0%) = VAL(MID(LINE$,31%,13%))

	UPDATE #GL_CHART.CH%

	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_ID%, GL_CHART::ACCT, 19%, 13%)

	GOTO 2150

2160	CLOSE #GLMASTER.CH%
	CLOSE #GL_CHART.CH%

	CALL ASSG_FREECHANNEL(GLMASTER.CH%)
	CALL ASSG_FREECHANNEL(GL_CHART.CH%)

2200	CALL ENTR_3MESSAGE(SCOPE, "Creating new GL_YYYY_PP file", 1%)

	CALL ASSG_CHANNEL(GLTRANS.CH%,STAT%)

	OPEN "GLTRANS."+EXT$ FOR INPUT AS FILE GLTRANS.CH%, &
		ORGANIZATION SEQUENTIAL

	LINPUT #GLTRANS.CH%, LINE$

	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_ID%, "GLTRANS  ", 18%, 13%)

2220	LINPUT #GLTRANS.CH%, LINE$

	PERIOD$="19"+MID(LINE$,6%,2%)+"_"+MID(LINE$,2%,2%)

	FOR I%=1% TO PERIOD%
		IF PERIOD$=PERIOD.FILE$(I%)
		THEN
			PERIOD% = I%
			GOTO PeriodOpen
		END IF
	NEXT I%

	PERIOD% = PERIOD% + 1%
	PERIOD.FILE$(PERIOD%) = PERIOD$

2230	YYYY_PP$ = PERIOD$

	IF PASS%=0%
	THEN
		KILL "GL_"+YYYY_PP$+".LED"
	END IF

	%INCLUDE "SOURCE:[GL.OPEN]GL_YYYY_PP.CRE"
	GL_YYYY_PP.CH%(PERIOD%) = GL_YYYY_PP.CH%

 PeriodOpen:
	GL_YYYY_PP::ACCT	= MID(LINE$, 36%, 4%)+"."+MID(LINE$,40%,4%)
	GL_YYYY_PP::SOURCE	= ""
	GL_YYYY_PP::REFNO	= MID(LINE$, 28%, 8%)
	GL_YYYY_PP::TRANDAT	= "19"+MID(LINE$,6%,2%)+MID(LINE$,2%,4%)
	GL_YYYY_PP::DESCR	= MID(LINE$,8%,20%)
	GL_YYYY_PP::AMOUNT	= VAL(MID(LINE$,44%,12%))
	GL_YYYY_PP::XREFNO	= ""
	GL_YYYY_PP::POSTIM	= ""
	GL_YYYY_PP::POSDAT	= "19"+MID(LINE$,6%,2%)+MID(LINE$,2%,4%)
	GL_YYYY_PP::CKNO	= ""
	GL_YYYY_PP::TRANKEY	= ""
	GL_YYYY_PP::SUBACC	= ""
	GL_YYYY_PP::OPERATION	= ""
	GL_YYYY_PP::UNITS	= 0.0
	GL_YYYY_PP::HOURS	= 0.0
	GL_YYYY_PP::UPDSTA	= ""
	GL_YYYY_PP::BTHNUM	= MID(LINE$,2%,4%)+LEFT(LINE$,1%)

2240	PUT	#GL_YYYY_PP.CH%(PERIOD%)

	SMG_STATUS% = SMG$PUT_CHARS(DISPLAY_ID%, PERIOD$+" "+GL_YYYY_PP::ACCT, 19%, 13%)

	GOTO 2220

2290	CLOSE #GLTRANS.CH%

	CALL ASSG_FREECHANNEL(GLTRANS.CH%)

	GOTO Password

 ExitProgram:
15000	!*******************************************************************
	! Exit program
	!*******************************************************************

	IF CONF$ = "Y"
	THEN
		CALL ENTR_3MESSAGE(SCOPE,"Conversion Process Complete", 0%)
	ELSE
		CALL ENTR_3MESSAGE(SCOPE,"Aborting Conversion Process", 0%)
	END IF
	CALL ENTR_3MESSAGE(SCOPE, "",1%)
	SMG_STATUS% = SMG$SET_CURSOR_MODE(PASTE_ID%, SMG$M_CURSOR_ON)
	SMG_STATUS% = SMG$DELETE_PASTEBOARD(PASTE_ID%)
	SMG_STATUS% = SMG$DELETE_VIRTUAL_DISPLAY(DISPLAY_ID%)

	CALL SUBR_3EXITPROGRAM(SCOPE, "RUN CMC$ROOT:[GL]GL_MAST_CHART", "")

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

	SELECT ERL

	CASE 1000%
		RESUME 1050 IF ERR = 5%
		FILENAME$ = "GL_PERIOD"

	CASE 1200%
		RESUME 2000 IF ERR = 5%
		FILENAME$ = "GL_CHART"

	CASE 2000%
		FILENAME$ = "GL_CHART"

 !	CASE 2005%
 !		FILENAME$ = "GL_CHARTEX"

	CASE 2010%
		FILENAME$ = "GLISAM"

	CASE 2020%
		RESUME 2120 IF ERR = 11%
		FILENAME$ = "GLISAM"

	CASE 2030%
		RESUME 2020 IF ERR = 134%
		FILENAME$ = "GL_CHART"

 !	CASE 2040%
 !		FILENAME$ = "GL_CHARTEX"

	CASE 2120%
		FILENAME$ = "GLMASTER"

	CASE 2130%
		FILENAME$ = "GLMASTER"

	CASE 2150%
		RESUME 2160 IF ERR = 11%
		FILENAME$ = "GLMASTER"

	CASE 2155%
		RESUME 2150 IF ERR = 155%
		FILENAME$ = "GL_CHART"

	CASE 2200%
		FILENAME$ = "GLTRANS"

	CASE 2220%
		RESUME 2290 IF ERR = 11%
		FILENAME$ = "GLTRANS"

	CASE 2230%
		FILENAME$ = "GL_YYYY_PP"

	CASE 2240%
		FILENAME$ = "GL_YYYY_PP"

	END SELECT

	RESUME HelpError

32767
