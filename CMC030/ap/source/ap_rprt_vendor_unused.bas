1	%TITLE "Accounts Payable Unused Vendor Numbers Report"
	%SBTTL "AP_RPRT_VENDOR_UNUSED"
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
	! ID:AP003
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Unused Vendor Numbers Report\* prints a list of all vendor numbers,
	!	in either the open or closed files, that contain no activity.
	!	.b
	!	The following fields are included:
	!	.lm 15
	!	.b
	!	.list 0,"*"
	!	.le
	!	Vendor Number
	!	.le
	!	Description
	!	.els
	!
	! Index:
	!	.x Report>Unused Vendor Numbers
	!	.x Unused Vendor Numbers>Report
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AP_SOURCE:AP_RPRT_VENDOR_UNUSED/LINE
	!	$ LINK/EXECUTABLE=AP_EXE: AP_RPRT_VENDOR_UNUSED, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AP_RPRT_VENDOR_UNUSED.OBJ;*
	!
	! Author:
	!
	!	06/08/89 - Aaron Redd
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/29/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/13/97 - Kevin Handy
	!		Reformat source code.
	!
	!	08/20/97 - Kevin Handy
	!		Don't need to assign channel for report
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/25/2000 - Kevin Handy
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
	! Set up data storage areas (MAPs, DIMENSIONs, DECLAREs)
	!******************************************************************

	!
	! CDD inclusions
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD	UTL_REPORTX

	%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.HB"
	MAP	(AP_VENDOR)	AP_VENDOR_CDD	AP_VENDOR

	%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.HB"
	MAP	(AP_OPEN)	AP_OPEN_CDD	AP_OPEN

	%INCLUDE "SOURCE:[AP.OPEN]AP_CLOSE.HB"
	MAP	(AP_CLOSE)	AP_CLOSE_CDD	AP_CLOSE

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
	!	^*(01) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field determines the item with
	!	which the report will begin printing. If the setting
	!	is blank, the report will begin with the first item in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Unused Vendor Numbers Report
	!	.x Unused Vendor Numbers Report>From Item
	!
	! Required:
	!--
	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field determines the item with
	!	which the report will end. If this setting is blank,
	!	the report will end with the last item in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Unused Vendor Numbers Report
	!	.x Unused Vendor Numbers Report>To Item
	!
	!--

	%PAGE


300	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_VENDOR.OPN"
	USE
		FILENAME$ = "AP_VENDOR"
		CONTINUE HelpError
	END WHEN

	!
	! Open the Accounts Receivable Closed file
	!
310	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_CLOSE.OPN"
	USE
		FILENAME$ = "AP_CLOSE"
		CONTINUE HelpError
	END WHEN

	!
	! Open the Accounts Receivable Open Item file
	!
320	WHEN ERROR IN
		%INCLUDE "SOURCE:[AP.OPEN]AP_OPEN.OPN"
	USE
		FILENAME$ = "AP_OPEN"
		CONTINUE HelpError
	END WHEN

	%PAGE

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "Unneeded Vendor Number List"
	TITLE$(2%) = "Accounts Payable System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "VendorNum  Description"
	TITLE$(5%) = "."

	!
	! Layouts for printed lines
	!
	LYT_LINE$ = "$VENDOR:010,$DESCR:061"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #AP_VENDOR.CH%
		ELSE
			FIND #AP_VENDOR.CH%, KEY #0% GE FROM_ITEM$, REGARDLESS
		END IF
	USE
		FILENAME$ = "AP_VENDOR"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17100	!******************************************************************
	! Main report loop starts here
	!******************************************************************

	!
	! Get next record from the AP Vendor file
	!
	WHEN ERROR IN
		GET #AP_VENDOR.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "AP_VENDOR"
		CONTINUE HelpError
	END WHEN

	!
	! Check status
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Check current record
	!
	GOTO ExitTotal IF (AP_VENDOR::VENNUM > TO_ITEM$) AND (TO_ITEM$ <> "")

17200	!
	! See if this Vendor Number exists in the AP Open Item file
	!
	WHEN ERROR IN
		GET #AP_OPEN.CH%, &
			KEY #0% GE (AP_VENDOR::VENNUM + "      "), &
			REGARDLESS
	USE
		CONTINUE 17300 IF (ERR = 155%)
		FILENAME$ = "AP_OPEN"
		CONTINUE HelpError
	END WHEN

	GOTO 17500 IF (AP_OPEN::VENNUM = AP_VENDOR::VENNUM)

17300	!
	! See if this Vendor Number exists in the AP Closed file
	!
	WHEN ERROR IN
		GET #AP_CLOSE.CH%, &
			KEY #0% GE (AP_VENDOR::VENNUM + "      "), &
			REGARDLESS
	USE
		CONTINUE 17400 IF (ERR = 155%)
		FILENAME$ = "AP_CLOSE"
		CONTINUE HelpError
	END WHEN

	GOTO 17500 IF (AP_CLOSE::VENNUM = AP_VENDOR::VENNUM)

17400	!
	! Since the Vendor number is unused, print it out
	!
	TEXT$ = AP_VENDOR::VENNUM + " " + &
		AP_VENDOR::VENNAM

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

17500	!
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
	! End of report AP_RPRT_VENDOR_UNUSED
	!******************************************************************
	END
