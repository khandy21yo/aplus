1	%TITLE "File Device"
	%SBTTL "UTL_RPRT_DEVICE"
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
	! ID:UT011
	!
	! Abstract:HELP
	!	.p
	!	The ^*Device\* option provides a
	!	report which contains the following information:
	!	.b
	!	.lm +10
	!	.list 0,"*"
	!	.le
	!	File Name
	!	.le
	!	System Name
	!	.le
	!	Device
	!	.le
	!	Protection Code
	!	.els
	!	.lm -10
	!
	! Index:
	!	.x Device>Report
	!	.x Report>Device
	!
	! Compile:
	!
	!	$ BAS UTL_SOURCE:UTL_RPRT_DEVICE/LINE
	!	$ LINK/EXE=UTL_EXE: UTL_RPRT_DEVICE, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE UTL_RPRT_DEVICE.OBJ;*
	!
	! Author:
	!
	!	02/18/88 - Lance Williams
	!
	! Modification History:
	!
	!	08/12/88 - Kevin Handy
	!		Modified to open UTL_DEVICE only if not already open.
	!
	!	08/09/89 - Aaron Redd
	!		Modified to reflect changes in the entire Device setup
	!		(New file layout, different Master file, etc.)
	!
	!	05/21/90 - Frank F. Starman
	!		Added COMMAND help message.
	!
	!	03/13/92 - Kevin Handy
	!		Unrolled error trap (300)
	!
	!	06/16/93 - Kevin Handy
	!		Added REGARDLESS to TK_FILEDICT.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/16/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose excessive %PAGE
	!
	!	03/05/99 - Kevin Handy
	!		Change field 'DEVICE' to 'DEVICENAME'
	!
	!	11/06/2000 - Kevin Handy
	!		Use A"x"B
	!		Use WHEN ERROR IN
	!--

	!++
	! Abstract:COMMAND
	!	^*DEVICE/LIST\*
	!	.p
	!	^*Device/List\* prints the device file.
	!	.P
	!	^*Format: DEVICE/LIST\*
	!	.P
	!	^*Example:\*
	!	.literal
	!	Menu Command Level: /DEVICE/LIST
	!	.End literal
	!
	! Index:
	!	.x DEVICE/LIST
	!
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Included files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! CDD inclusions and related memory allocations
	!
	%INCLUDE "SOURCE:[TK.OPEN]TK_FILEDICT.HB"
	MAP	(TK_FILEDICT)	TK_FILEDICT_CDD TK_FILEDICT

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_DEVICE.HB"
	MAP	(UTL_DEVICE)	UTL_DEVICE_CDD	UTL_DEVICE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD	UTL_REPORTX

	%PAGE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

	!
	! Initialize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From System\*
	!	.p
	!	The ^*From System\* option enters the
	!	system name from which the report will begin.
	!	.p
	!	The field will accommodate a two (2) character entry,
	!	i.e. GL, AP, AR, etc.
	!
	! Index:
	!	.x From System>Device/List
	!	.x Device/List>From System
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To System\*
	!	.p
	!	The ^*To System\* option enters
	!	the system name with which the report will end printing.
	!	.p
	!	The field will accommodate a two (2) character entry,
	!	i.e. GL, AP, AR, etc.
	!
	! Index:
	!	.x To System>Device/List
	!	.x Device/List>To System
	!	.x System>To
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
	!	.p
	!	The ^*Wildcard\* field selects
	!	designated systems which are to be printed in the device
	!	report.
	!
	! Index:
	!	.x Wildcard>Device/List
	!	.x Device/List>Wildcard
	!
	!--

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_DEVICE.OPN"
	USE
		FILENAME$ = "UTL_DEVICE"
		CONTINUE HelpError
	END WHEN

	!
	! Open the TK File Dictionary (doubling as the Master Device file)
	!
310	WHEN ERROR IN
		%INCLUDE "SOURCE:[TK.OPEN]TK_FILEDICT.OPN"
	USE
		FILENAME$ = "TK_FILEDICT"
		CONTINUE HelpError
	END WHEN

	%PAGE

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "FILE  DEVICE  LISTING"
	TITLE$(2%) = "Utility System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "FileName                                Sys " + &
		"FileDescription                               FileDevice"
	TITLE$(5%) = "."

	!
	! Layouts for printed lines
	!
	LYT_LINE$ = "$FileName:030,$System:042,$Descr:089,$Device:132"

	%PAGE

17000	!***************************************************************
	! OUTPUT REPORT
	!***************************************************************
	WHEN ERROR IN
		IF (FROM_ITEM$ = "")
		THEN
			RESET #UTL_DEVICE.CH%, KEY #0%
		ELSE
			FIND #UTL_DEVICE.CH%, KEY #0% GE FROM_ITEM$, REGARDLESS
		END IF
	USE
		FILENAME$ = "UTL_DEVICE"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17100	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #UTL_DEVICE.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF (ERR = 11%)
		FILENAME$ = "UTL_DEVICE"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	GOTO ExitTotal IF &
		(UTL_DEVICE::FILENAM > TO_ITEM$) AND &
		(TO_ITEM$ <> "")

	GOTO GetNextRec IF &
		(COMP_STRING(EDIT$(UTL_DEVICE::CATAG, -1%), WLDCRD$) = 0%) AND &
		(WLDCRD$ <> "")

	!
	! Set default file description
	!
	TK_FILEDICT::DESCR = STRING$(45%, A"?"B)

	!
	! Get file description from the TK File Dictionary
	!
17200	WHEN ERROR IN
		GET #TK_FILEDICT.CH%, KEY #0% EQ UTL_DEVICE::FILENAM, REGARDLESS
	USE
		CONTINUE 17300 IF (ERR = 155%)
		FILENAME$ = "TK_FILEDICT"
		CONTINUE HelpError
	END WHEN

	!
	! Print out one line
	!
17300	TEXT$ = UTL_DEVICE::FILENAM + " " + &
		LEFT(UTL_DEVICE::CATAG, 2%) + "  " + &
		LEFT(TK_FILEDICT::DESCR, 45%) + " " + &
		LEFT(UTL_DEVICE::DEVICENAME, 42%)

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Go back to get next Device record
	!
	GOTO 17100

	%PAGE

	!***************************************************************
	! Handle end of report
	!***************************************************************

 ExitTotal:
	!
	! Do totals and things before we end
	!

 ExitProgram:
	!
	! Finish up the report
	!
	CALL OUTP_FINISH(UTL_REPORTX)

	!
	! Exit to next program or menu
	!
	IF TRM$(UTL_REPORTX::NEXTRUN) = ""
	THEN
		CALL SUBR_3EXITPROGRAM(SCOPE, "", "")
	ELSE
		CALL SUBR_3EXITPROGRAM(SCOPE, "RUN " + UTL_REPORTX::NEXTRUN, "")
	END IF

	%PAGE

 HelpError:
	!***************************************************************
	! Help Message for an error
	!***************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))

	UTL_REPORTX::STAT = -1%
	GOTO ExitProgram

19000	!***************************************************************
	! ERROR TRAPPING
	!***************************************************************

	!
	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	!***************************************************************
	! End of report UTL_RPRT_DEVICE
	!***************************************************************
	END
