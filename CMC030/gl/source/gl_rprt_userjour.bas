1	%TITLE "User Journal Report"
	%SBTTL "GL_RPRT_USERJOUR"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1998 BY
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
	! ID:GL001
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Print Users Journal\* routine will print a report
	!	containing the following information:
	!	.table 3,25
	!	.te
	!	Line _#
	!	.te
	!	Description
	!	.te
	!	Account
	!	.te
	!	Dollars
	!	.te
	!	Units
	!	.te
	!	XRef
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Print>User  Journal
	!	.x User Journal>Report
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS GL_SOURCE:GL_RPRT_USERJOUR.BAS/LINE
	!	$ LINK/EXE=GL_EXE: GL_RPRT_USERJOUR, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE GL_RPRT_USERJOUR.OBJ;*
	!
	! Author:
	!
	!	11/23/98 - Kevin Handy
	!
	! Modification history:
	!
	!	12/18/2000 - Kevin Handy
	!		Use WHEN ERROR IN
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

	!
	! CDD inclusions and related memory allocations
	!
	%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.HB"
	MAP (AR_CONTROL)	AR_CONTROL_CDD		AR_CONTROL

 !	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
 !	MAP (AR_35CUSTOM)	AR_35CUSTOM_CDD		AR_35CUSTOM

	%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.HB"
	MAP (AR_OPEN)		AR_OPEN_CDD		AR_OPEN

	%INCLUDE "SOURCE:[AR.OPEN]AR_CLOSED.HB"
	MAP (AR_CLOSED)		AR_CLOSED_CDD		AR_CLOSED

	%INCLUDE "SOURCE:[GL.OPEN]GL_USERHEAD.HB"
	MAP (GL_USERHEAD)	GL_USERHEAD_CDD		GL_USERHEAD

	%INCLUDE "SOURCE:[GL.OPEN]GL_USERJOUR.HB"
	MAP (GL_USERJOUR)	GL_USERJOUR_CDD		GL_USERJOUR

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[GL.OPEN]GL_USERDEF.HB"
	MAP	(GL_USERDEF)	GL_USERDEF_CDD		GL_USERDEF

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION GL_OUTP_ACCTSUM

	%PAGE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

 Init:	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) Batch to Print\*
	!	.b
	!	.lm +5
	!	The ^*Batch to Print\* field refers to a particular batch number
	!	which is to be printed.
	!	.b
	!	Only one batch at a time may be printed.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Batch to Print>Print Sales Journal
	!	.x Print Sales Journal>Batch to Print
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) From Journal Code\*
	!	.b
	!	.lm +5
	!	The ^*From Journal Code\* field causes printing
	!	to begin with a specified invoice number.
	!	.b
	!	A blank will cause the report to start with the first invoice
	!	number in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Journal Code>Print User Journal
	!	.x Print User Journal>From Journal Code
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(02) To Journal Code\*
	!	.b
	!	.lm +5
	!	The ^*To Journal Code\* field causes printing
	!	to end with a specified invoice number.
	!	.b
	!	A blank causes the report to end with the last invoice
	!	number in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Journal Code>Print User Journal
	!	.x Print User Journal>To Journal Code
	!
	!--


300	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_USERHEAD.OPN"
	USE
		FILENAME$ = "GL_USERHEAD"
		CONTINUE HelpError
	END WHEN

310	!
	! Open GL_USERJOUR file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_USERJOUR.OPN"
	USE
		FILENAME$ = "GL_USERJOUR"
		CONTINUE HelpError
	END WHEN

320	!
	! Open control file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.OPN"
		GET #AR_CONTROL.CH%, RECORD 1%, REGARDLESS
	USE
		IF ERR = 5%
		THEN
			AR_CONTROL::CTITLE = "Customer"
			CONTINUE 330
		END IF

		FILENAME$ = "AR_CONTROL"
		CONTINUE HelpError
	END WHEN

330	!
	! Open Customer file
	!
 !	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.OPN"

340	!
	! User defined file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[GL.OPEN]GL_USERDEF.OPN"
	USE
		FILENAME$ = "GL_USERDEF"
		CONTINUE HelpError
	END WHEN

350	!
	! A/R Open item file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_OPEN.OPN"
	USE
		CONTINUE 360
	END WHEN

360	!
	! A/R Closed item file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_CLOSED.OPN"
	USE
		CONTINUE 370
	END WHEN

370	!

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "General Journal File List"
	TITLE$(2%) = "Batch #" + BATCH_NO$
	TITLE$(3%) = ""


	TITLE$(4%) = "   Line Description                               " + &
		"Account              Dollars       Units XRef     Invoice"
	TITLE$(5%) = "."

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #GL_USERHEAD.CH%
		ELSE
			FIND #GL_USERHEAD.CH%, KEY #0% GE FROM_ITEM$, REGARDLESS
		END IF
	USE
		CONTINUE ExitTotal
	END WHEN

	!
	! Declare some variables
	!
	TOTAL_DOLLARS = 0.0
	TOTAL_UNITS = 0.0

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #GL_USERHEAD.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal
	END WHEN

	!
	! Check current record
	!
	GOTO ExitTotal IF (GL_USERHEAD::JCODE > TO_ITEM$) AND TO_ITEM$ <> ""

17120	!
	! Print the Sales Journal line
	!
	TEXT$ = GL_USERHEAD::JCODE + "  " + &
		GL_USERHEAD::DEPOSIT + "  " + &
		PRNT_DATE(GL_USERHEAD::JDATE, 8%)

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 4%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Print out line items
	!
	GOSUB Print_Lines
	GOTO ExitProgram IF UTL_REPORTX::STAT

17200	!
	! Try for next record
	!
	GOTO GetNextRec

 ExitTotal:
	!
	! Handle end of report
	!
	TOTAL_DOLLARS = FUNC_ROUND(TOTAL_DOLLARS, 2%)
	TEXT$ = "                    " + &
		"                                 " + &
		"Grand Total:  " + FORMAT$(TOTAL_DOLLARS, "########.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Print out the Debit/Credit information
	!
	V% = GL_OUTP_ACCTSUM(OPT_SUMMARY, "", 0.0, 0.0, 0.0, &
		TITLE$(), UTL_REPORTX)

	%PAGE

 ExitProgram:
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

 Print_Lines:
17740	!****************************************************************
	!*  Subroutine for printing out Line Items			*
	!****************************************************************
	!

	WHEN ERROR IN
		FIND #GL_USERJOUR.CH%, KEY #0% GE GL_USERHEAD::JCODE, REGARDLESS
	USE
		CONTINUE LastLine
	END WHEN

 Get_Next_Line:
17750	!
	! Loop starts here
	!
	GOTO LastLine IF UTL_REPORTX::STAT

	!
	! Get next line item
	!
	WHEN ERROR IN
		GET #GL_USERJOUR.CH%, REGARDLESS
	USE
		CONTINUE LastLine
	END WHEN

	!
	! Check current line item
	!
	GOTO LastLine IF GL_USERJOUR::JCODE <> GL_USERHEAD::JCODE

	GOSUB NeedDefine

	!
	! Print the line item line
	!
	TEXT$ = "     " + &
		GL_USERJOUR::JLINE + " " + &
		GL_USERJOUR::DESCRIPTION + " " + &
		GL_USERJOUR::ACCOUNT + " " + &
		FORMAT$(GL_USERJOUR::DOLLARS, "#####.##(") + &
		GL_USERDEF::SIGNED + ") " + &
		FORMAT$(GL_USERJOUR::UNITS, "<%>####.## ") + &
		GL_USERJOUR::XREF + " " + &
		GL_USERJOUR::INVNUM

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO LastLine IF UTL_REPORTX::STAT

	GOSUB TestInvoice

	!
	! Put the Debit/Credit information into the temporary file
	!
	IF GL_USERDEF::SIGNED = "-"
	THEN
		SIGN = -1.0
	ELSE
		SIGN = 1.0
	END IF
	GOTO ExitProgram IF GL_OUTP_ACCTSUM (OPT_ADDREC, GL_USERJOUR::ACCOUNT, &
		0.0, GL_USERJOUR::DOLLARS * SIGN, GL_USERJOUR::UNITS * SIGN, &
		TITLE$(), UTL_REPORTX) <> CMC$_NORMAL

	!
	! Total in the line
	!
	TOTAL_DOLLARS = FUNC_ROUND(TOTAL_DOLLARS + &
		GL_USERJOUR::DOLLARS * SIGN, 2%)

	!
	! Try for next Line
	!
	GOTO Get_Next_Line

 LastLine:

	IF TOTAL_DOLLARS <> 0.0
	THEN
		TEXT$ = "                           " + &
			"         *** Journal does not balance" + &
			":  " + FORMAT$(TOTAL_DOLLARS, "########.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -2%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, -2%)
	END IF

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

	RETURN

	%PAGE

 NeedDefine:
18100	!*******************************************************************
	! Dig through the userdef file for the proper definition for
	! this line
	!*******************************************************************

	IF GL_USERDEF::JCODE = GL_USERJOUR::JCODE AND &
		GL_USERDEF::JLINE = GL_USERJOUR::JLINE
	THEN
		RETURN
	END IF

	WHEN ERROR IN
		GET #GL_USERDEF.CH%, &
			KEY #0% EQ GL_USERJOUR::JCODE + GL_USERJOUR::JLINE, &
			REGARDLESS
	USE
		CONTINUE 18190
	END WHEN

18190	RETURN

	%PAGE

 TestInvoice:
	!******************************************************************
	! Check to see if the invoice number is already used
	!*******************************************************************

18200	GOTO 18290 IF GL_USERJOUR::INVNUM = ""

18210	WHEN ERROR IN
		FIND #AR_OPEN.CH%, KEY #3 EQ GL_USERJOUR::INVNUM, REGARDLESS
	USE
		CONTINUE 18220
	END WHEN

	TEXT$ = "   *** WARNING *** Invoice Number " + &
		TRM$(GL_USERJOUR::INVNUM) + " exists in the AR open item file"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

18220	WHEN ERROR IN
		FIND #AR_CLOSED.CH%, KEY #3 EQ GL_USERJOUR::INVNUM, REGARDLESS
	USE
		CONTINUE 18290
	END WHEN

	TEXT$ = "   *** WARNING *** Invoice Number " + &
		TRM$(GL_USERJOUR::INVNUM) + " exists in the AR closed item file"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

18290	RETURN

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
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
