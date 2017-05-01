1	%TITLE "Support System Customer License Report"
	%SBTTL "SS_RPRT_CUSTOMLICENSE"
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
	! ID:SS001
	!
	! Abstract:HELP
	!	.p
	!	This report loops through the Accounts Recievable
	!	Customer file and prints out a list of all the customers
	!	who possess License numbers.  This list contains basic
	!	information about the customer (Customer name, number,
	!	address, etc.), as well as some stuff about the license(s):
	!	(the license number, expiration date, and so on).
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS SS_SOURCE:SS_RPRT_CUSTOMLICENSE/LINE
	!	$ LINK/EXE=SS_EXE: SS_RPRT_CUSTOMLICENSE, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE SS_RPRT_CUSTOMLICENSE.OBJ;*
	!
	! Author:
	!
	!	08/03/89 - Aaron Redd
	!
	! Modification history:
	!
	!	03/13/92 - Kevin Handy
	!		Unrolled error trapping (check)
	!
	!	04/23/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/13/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/20/97 - Kevin Handy
	!		Don't allocate channel for report
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/06/2000 - Kevin Handy
	!		Use A"x"B
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

	%PAGE

	!******************************************************************
	! Set up data storage areas (MAPs, DIMENSIONs, DECLAREs)
	!******************************************************************

	!
	! CDD inclusions
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE				UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP	(AR_35CUSTOM)		AR_35CUSTOM_CDD		AR_35CUSTOM

	%INCLUDE "SOURCE:[SS.OPEN]SS_LICENSE.HB"
	MAP	(SS_LICENSE)		SS_LICENSE_CDD		SS_LICENSE

	%INCLUDE "SOURCE:[SS.OPEN]SS_CUS_SYSMENU.HB"
	MAP	(SS_CUS_SYSMENU)	SS_CUS_SYSMENU_CDD	SS_CUS_SYSMENU

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
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = TRM$(EDIT$(UTL_REPORTX::OPTDEF(0%), 132%))
	TO_ITEM$ = TRM$(EDIT$(UTL_REPORTX::OPTDEF(1%), 132%))
	SORT_BY$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	AR_KEY% = (INSTR(1%, " N T C A ", SORT_BY$) / 2%) - 1%
	AR_KEY% = 0% IF (AR_KEY% = -1%)

	%PAGE


300	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.OPN"
	USE
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

	!
	! Open the Support System Customer License file
	!
310	WHEN ERROR IN
		%INCLUDE "SOURCE:[SS.OPEN]SS_LICENSE.OPN"
	USE
		FILENAME$ = "SS_LICENSE"
		CONTINUE HelpError
	END WHEN

	!
	! Open the SS Installed System file
	!
320	WHEN ERROR IN
		%INCLUDE "SOURCE:[SS.OPEN]SS_CUS_SYSMENU.OPN"
	USE
		FILENAME$ = "SS_CUS_SYSMENU"
		CONTINUE HelpError
	END WHEN

	%PAGE

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "List of Licensed Customer Systems"
	TITLE$(2%) = "Support System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = ""

	!
	! Layouts for printed lines
	!
	LYT_CUSLIN$ = "$CustomerNum:010,$CustomerName:051,$Address:099," + &
		"$City:115,$State:118,$ZIPCode:132"
	LYT_LICLIN$ = "$:035,$LicenseNumber:045,DExpiration:054,$Make:065," + &
		"$Model:076,$SerialNumber:097,$OperatingSystem:104," + &
		"$Version:112,PPhoneNumber:127,$PhoneExtension:132"
	LYT_SYSLIN$ = "$:035,$SystemName:037,$MenuNumber:048,DInstalled:058"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	IF FROM_ITEM$ = ""
	THEN
		RESET #AR_35CUSTOM.CH%, KEY #AR_KEY%
	ELSE
		FIND #AR_35CUSTOM.CH%, KEY #AR_KEY% GE FROM_ITEM$, REGARDLESS
	END IF

 GetNextRec:
17100	!******************************************************************
	! Main report loop starts here
	!******************************************************************
	!
	! Get next record from the AR Customer file
	!
	WHEN ERROR IN
		GET #AR_35CUSTOM.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF (ERR = 11%)
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

	!
	! Check status
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Check current record
	!
	GOTO ExitTotal IF (AR_35CUSTOM::CUSNUM > TO_ITEM$) AND (TO_ITEM$ <> "")

	!
	! Create a line of text with the customer information, and a title
	!
	TITLE$ = "CustomNum  CustomerName                       " + &
		"      Address                                         " + &
		"City            State ZIP Code"

	TEXT$ = AR_35CUSTOM::CUSNUM + " " + &
		LEFT(AR_35CUSTOM::CUSNAM, 40%) + " " + &
		AR_35CUSTOM::ADD1 + "," + AR_35CUSTOM::ADD2 + " " + &
		AR_35CUSTOM::CITY + " " + AR_35CUSTOM::STATE + "    " + &
		AR_35CUSTOM::ZIP

	!
	! Check for any licenses this customer might have
	!
17200	WHEN ERROR IN
		GET #SS_LICENSE.CH%, KEY #0% GE AR_35CUSTOM::CUSNUM, REGARDLESS
	USE
		CONTINUE 17500 IF (ERR = 155%)
		FILENAME$ = "SS_LICENSE"
		CONTINUE HelpError
	END WHEN

	GOTO 17500 IF (SS_LICENSE::CUSNUM <> AR_35CUSTOM::CUSNUM)

	!
	! License found for this customer; print out customer info
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TITLE$, 0%)
	CALL OUTP_LINE(LYT_CUSLIN$, UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Print out a blank line
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Create a title for the license information, and print it
	!
	TITLE$ = SPACE$(35%) + &
		"LicenseNum Expires  Make       Model      " + &
		"SerialNumber         OperSys Version PhoneNumber   Ext"
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TITLE$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Create a line of text from the license information
	!
17250	TEXT$ = SPACE$(35%) + SS_LICENSE::LICENSE_NUM + " " + &
		PRNT_DATE(SS_LICENSE::EXPIR_DATE, 0%) + " " + &
		SS_LICENSE::MAKE + " " + SS_LICENSE::MODEL + " " + &
		SS_LICENSE::SERIAL_NUM + " " + SS_LICENSE::OPER_SYS + "  " + &
		SS_LICENSE::VERSION + "  " + &
		PRNT_PHONE(SS_LICENSE::PHONE_NUM, 0%) + " " + SS_LICENSE::PHONE_EXT

	!
	! Print out the line of text
	!
	CALL OUTP_LINE(LYT_LICLIN$, UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get the next license record, and go back to print it out
	!
17260	WHEN ERROR IN
		GET #SS_LICENSE.CH%, REGARDLESS
	USE
		CONTINUE 17300 IF (ERR = 11%)
		FILENAME$ = "SS_LICENSE"
		CONTINUE HelpError
	END WHEN

	GOTO 17250 IF (SS_LICENSE::CUSNUM = AR_35CUSTOM::CUSNUM)

	!
	! Now, check for any systems the user might have
	!
17300	WHEN ERROR IN
		FIND #SS_CUS_SYSMENU.CH%, &
			KEY #0% EQ AR_35CUSTOM::CUSNUM, &
			REGARDLESS
	USE
		CONTINUE 17400 IF (ERR = 155%)
		FILENAME$ = "SS_CUS_SYSMENU"
		CONTINUE HelpError
	END WHEN

	!
	! Print out a blank line
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Create a title for the system information, and print it
	!
	TITLE$ = SPACE$(35%) + "System MenuNum Installed"
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TITLE$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get the (next) system record
	!
17350	WHEN ERROR IN
		GET #SS_CUS_SYSMENU.CH%, REGARDLESS
	USE
		CONTINUE 17400 IF (ERR = 11%)
		FILENAME$ = "SS_CUS_SYSMENU"
		CONTINUE HelpError
	END WHEN

	GOTO 17400 IF (SS_CUS_SYSMENU::CUSNUM <> AR_35CUSTOM::CUSNUM)

	!
	! Now, make a text line out of the system record
	!
	TEXT$ = SPACE$(35%) + SS_CUS_SYSMENU::SYSTEM + "     " + &
		SS_CUS_SYSMENU::MENNUM + "  " + &
		PRNT_DATE(SS_CUS_SYSMENU::INSDAT, 0%)

	!
	! Print out the text line
	!
	CALL OUTP_LINE(LYT_SYSLIN$, UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Go back to get the next system record
	!
	GOTO 17350

	!
	! Print out a blank line
	!
17400	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

	!
	! Print out a line of "="'s
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), STRING$(132%, A"="B), 0%)

	!
	! Print out another blank line (between customers)
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)

17500	!
	! Try for next record
	!
	GOTO GetNextRec

	%PAGE

 ExitTotal:
	!******************************************************************
	! Handle totals and other items before EXITing
	!******************************************************************
	!
	! Print out totals
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
	! End of report SS_RPRT_CUSTOMLICENSE
	!******************************************************************
	END
