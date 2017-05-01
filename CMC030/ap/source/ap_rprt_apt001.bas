1	%TITLE "Accounts Payable 1099 Table Report"
	%SBTTL "AP_RPRT_APT001"
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
	! ID:APT001
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*1099 Table Listing\* report selection
	!	prints the records in the 1099 Table file.  The report includes the
	!	following:
	!	.lm 15
	!	.b
	!	.LS 0,"*"
	!	.LE
	!	Code
	!	.LE
	!	Description
	!	.LE
	!	Base Amount
	!	.LE
	!	Form _#
	!	.LE
	!	Form Location
	!	.ELS
	!
	! Index:
	!	.x Reports>1099 Table Report
	!	.x 1099 Table Report
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_RPRT_APT001/LINE
	!	$ LINK/EXECUTABLE=AP_EXE:*.EXE AP_RPRT_APT001, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AP_RPRT_APT001.OBJ;*
	!
	! Author:
	!
	!	08/04/87 - B. Craig Larsen
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/28/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/13/97 - Kevin Handy
	!		Reformat source code
	!
	!	08/20/97 - Kevin Handy
	!		Don't need to assign channel for report
	!
	!	08/22/97 - Kevin Handy
	!		Use 'val%' instead of 'val'
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/16/98 - Kevin Handy
	!		Lose an excessive number of %PAGE's
	!
	!	10/23/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!******************************************************************
	! External modules needed
	!******************************************************************

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!******************************************************************
	! Set data storage areas (MAPs, DIMENSIONs, DECLAREs, etc.)
	!******************************************************************

	!
	! CDD inclusions and MAPs
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE		UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[AP.OPEN]AP_1099_TABLE.HB"
	MAP	(AP_1099_TABLE)	AP_1099_TABLE_CDD	AP_1099_TABLE

	!
	! Dimension arrays
	!
	DIM STRING FORM_NUMBERS(9%)

	%PAGE

	!******************************************************************
	! Take care of anything else before starting the report
	!******************************************************************

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

 Initialization:
	!******************************************************************
	! Get ready to begin
	!******************************************************************

	!
	! Initialize for output
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 80%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Code\*
	!	.b
	!	.lm +5
	!	The ^*From Code\* determines a code number from
	!	which the report will begin. If the report is to begin with the
	!	first code in the file the setting should be left blank.
	!	.lm -5
	!
	! Index:
	!	.x From Code>1099 Table List
	!	.x 1099 Table List>From Code
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Code\*
	!	.b
	!	.lm +5
	!	The ^*To Code\* determines the last code number to be
	!	printed. If the report is to end with the last code number in
	!	the file this setting should be left blank.
	!	.lm -5
	!
	! Index:
	!	.x To Code>1099 Table List
	!	.x 1099 Table List>To Code
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) WildCard Code\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard Code\* determines which code numbers will
	!	be printed. Wildcard characters (%, _*) can be used
	!	to specify selected code numbers. For example, an ^*_*\* or a blank
	!	will cause all code numbers to be selected. An ^*A%\* would cause all
	!	code numbers with an 'A' as the first character to be selected.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard Code>1099 Table List
	!	.x 1099 Table List>Wildcard Code
	!
	!--


	FORM_NUMBERS(0%) = "9"
	FORM_NUMBERS(1%) = "1  1099-A         "
	FORM_NUMBERS(2%) = "2  1099-B         "
	FORM_NUMBERS(3%) = "3  1099-DIV       "
	FORM_NUMBERS(4%) = "4  1099-G         "
	FORM_NUMBERS(5%) = "5  1099-INT       "
	FORM_NUMBERS(6%) = "6  1099-MISC      "
	FORM_NUMBERS(7%) = "7  1099-OID       "
	FORM_NUMBERS(8%) = "8  1099-PATR      "
	FORM_NUMBERS(9%) = "9  1099-R         "


300	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_1099_TABLE.OPN"
	USE
		FILENAME$ = "AP_1099_TABLE"
		CONTINUE HelpError
	END WHEN

	%PAGE

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "1099 TABLE"
	TITLE$(2%) = "Accounts Payable System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "Code   Description                BaseAmount   " + &
		"FormNumber         Location"
	TITLE$(5%) = ""

	!
	! Layouts for lines printed
	!
	LYT_LINE$ = "$Code:002,$Descr:027,VBaseAmount:044," + &
		"$FormNumber:048,$FormDescr:065,$Location:068"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #AP_1099_TABLE.CH%
		ELSE
			FIND #AP_1099_TABLE.CH%, &
				KEY #0% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		FILENAME$ = "AP_1099_TABLE"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17100	!******************************************************************
	! Main loop starts here
	!******************************************************************

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #AP_1099_TABLE.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "AP_1099_TABLE"
		CONTINUE HelpError
	END WHEN

	!
	! Check status
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Check current record
	!
	GOTO ExitTotal IF (AP_1099_TABLE::CODE > TO_ITEM$) AND (TO_ITEM$ <> "")

	GOTO GetNextRec &
		IF COMP_STRING(EDIT$(AP_1099_TABLE::CODE, -1%), WLDCRD$) = 0% &
		AND WLDCRD$ <> ""

	FORM_NUM$ = AP_1099_TABLE::FRMNUM
	FORM_NUM$ = FORM_NUMBERS(I%) &
		IF (AP_1099_TABLE::FRMNUM = LEFT(FORM_NUMBERS(I%), 1%)) &
		FOR I% = 1% TO VAL%(FORM_NUMBERS(0%))

	!
	! Print out one line
	!
	TEXT$ = AP_1099_TABLE::CODE + "     " + &
		AP_1099_TABLE::DESCR + "   " + &
		FORMAT$(AP_1099_TABLE::BASEAMT, "###,###,###.##") + "   " + &
		FORM_NUM$ + " " + &
		AP_1099_TABLE::FRMLOC

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Try for next record
	!
	GOTO GetNextRec

	%PAGE

	!******************************************************************
	! Handle totals and other items before EXITing
	!******************************************************************

 ExitTotal:
	!
	! Print out totals
	!

 ExitProgram:
	!
	! Finish up report
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
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	UTL_REPORTX::STAT = -1%
	GOTO ExitProgram

19000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	!
	! Handle untrapped errors
	!
	FILENAME$ = ""
	RESUME HelpError

32767	!******************************************************************
	! End of report AP_RPRT_APT001
	!******************************************************************
	END
