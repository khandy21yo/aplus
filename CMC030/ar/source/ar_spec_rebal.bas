1	%TITLE "Recalculate balance forward Program"
	%SBTTL "AR_SPEC_REBAL"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1994 BY
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
	! ID:AR050
	!
	! Abstract:HELP
	!	.lm +5
	!	This program will recalculate the balances for all
	!	balance forward customers in the customer file.
	!	.b
	!	It assumes that the customers have not had any detail
	!	information purged from the closed file.
	!	.lm -5
	!
	! Index:
	!	.x Mail Merge>Customer
	!	.x Customer>Mail Merge
	!
	! Option:
	!
	!
	! Input:
	!
	!
	! Output:
	!
	!
	! Example:
	!
	! Author:
	!
	!	02/24/94 - Kevin Handy
	!
	! Compile:
	!
	!	$ BAS AR_SOURCE:AR_SPEC_REBAL/LINE
	!	$ LINK/EXECUTABLE=AR_EXE:*.EXE AR_SPEC_REBAL, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AR_SPEC_REBAL.OBJ;*
	!
	! Modification history:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/15/98 - Kevin Handy
	!		Lose excessive %PAGE
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

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.HB"
	MAP (AR_35CUSTOM)	AR_35CUSTOM_CDD		AR_35CUSTOM

	%INCLUDE "SOURCE:[AR.OPEN]AR_CONTROL.HB"
	MAP (AR_CONTROL)	AR_CONTROL_CDD		AR_CONTROL

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[AR.OPEN]AR_CONTACT.HB"
	MAP (AR_CONTACT)	AR_CONTACT_CDD		AR_CONTACT

	MAP (TEST) BUFFER$ = 512%

	!
	! External functions
	!
	EXTERNAL integer FUNCTION AR_FUNC_REBAL

	%PAGE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

300	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[AR.OPEN]AR_35CUSTOM.OPN"
	USE
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

330	!

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		RESET #AR_35CUSTOM.CH%
	USE
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	!
	! Get next record
	!
	WHEN ERROR IN
		GET #AR_35CUSTOM.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		CONTINUE 17025 IF ERR = 155%
		FILENAME$ = "AR_35CUSTOM"
		CONTINUE HelpError
	END WHEN

17025	!
	! Check current record
	!
	IF AR_35CUSTOM::METHOD == "B"
	THEN
		PRINT AR_35CUSTOM::CUSNUM
		V% = AR_FUNC_REBAL(AR_35CUSTOM::CUSNUM)
	END IF

17040	!
	! Try for next record
	!
	GOTO GetNextRec

 ExitTotal:
	!
	! Handle end of report
	!
	! (This forces out the last bit of the file, and fills the end
	! of the file full of NULL's, which makes WP happy)
	!

 ExitProgram:

	!
	! Exit to next program or menu
	!
	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
 !	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
 !		"E", ERN$, FILENAME$, NUM1$(ERR))
 !	UTL_REPORTX::STAT = -1%

	PRINT ERL; ERR; ERN$; " "; ERT$(ERR)

	GOTO ExitProgram

	%PAGE

19000	!******************************************************************
	! ERROR TRAPPING
	!******************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
