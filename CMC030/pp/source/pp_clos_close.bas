1	%TITLE "Pacific Pride Monthly Close"
	%SBTTL "PP_CLOS_CLOSE"
	%IDENT "V3.5"

	!
	! COPYRIGHT (C) 1993 BY
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
	! ID:PP021
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Pacific Pride Monthly Close\* programs takes
	!	care of the monthly closing routines.  It does the
	!	following procedures:
	!	.b
	!	Updates the odometer reading in the CARD file.
	!	.lm -5
	!
	! Index:
	!
	! Compile:
	!
	!	$ BAS PP_SOURCE:PP_CLOS_CLOSE/LINE
	!	$ LINK/EXE=PP_EXE: PP_CLOS_CLOSE, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PP_CLOS_CLOSE.OBJ;*
	!
	! Author:
	!
	!	12/28/92 - Dan Perkins
	!
	! Modification History:
	!
	!	02/02/93 - Kevin Handy
	!		Changed error trapping for monthly file from
	!		300 to 310.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/09/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	07/10/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include scope.com
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Include cdd
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[PP.OPEN]PP_CARD.HB"
	MAP (PP_CARD)		PP_CARD_CDD		PP_CARD

	%INCLUDE "SOURCE:[PP.OPEN]PP_MONTHLY.HB"
	MAP (PP_MONTHLY)	PP_MONTHLY_CDD		PP_MONTHLY

	%PAGE

	ON ERROR GOTO 19000

	!
 Init:	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	YYYY_PP$ = EDIT$(UTL_REPORTX::OPTDEF(0%), -1%)

	!++
	! Abstract:FLD01
	!	^*(01) Period to Close\*
	!	.b
	!	.lm +5
	!	The ^*Period to Close\* field enters a
	!	closing period for which the closing program will be effected.
	!	.b
	!	The format for entry is YYYYPP.
	!	.lm -5
	!
	! Index:
	!
	!--

300	!
	! Open Card file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PP.OPEN]PP_CARD.MOD"
	USE
		FILENAME$ = "PP_CARD"
		CONTINUE HelpError
	END WHEN

310	!
	! Open Monthly transaction file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[PP.OPEN]PP_MONTHLY.OPN"
	USE
		FILENAME$ = "PP_MONTHLY"
		CONTINUE HelpError
	END WHEN

17000	!***************************************************************
	! DO OUR THING
	!***************************************************************
	CALL ENTR_3MESSAGE(SCOPE, "Updating odometer readings.", 1% + 16%)

	WHEN ERROR IN
		RESET #PP_MONTHLY.CH%
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "PP_MONTHLY"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
	GOTO ExitProgram IF UTL_REPORTX::STAT

17020	!
	! Get record from Monthly file
	!
	WHEN ERROR IN
		GET #PP_MONTHLY.CH%, REGARDLESS
	USE
		CONTINUE ExitProgram IF ERR = 11%
		FILENAME$ = "PP_MONTHLY"
		CONTINUE HelpError
	END WHEN

	!
	! See if we need to update the card file
	!
	GOTO GetNextRec IF PP_MONTHLY::ODOM = 0.0

	IF PP_MONTHLY::VEHICLE <> ""
	THEN
		KEY$ = PP_MONTHLY::VEHICLE
	ELSE
		KEY$ = PP_MONTHLY::DRIVER
	END IF

17100	WHEN ERROR IN
		GET #PP_CARD.CH%, KEY #0% EQ PP_MONTHLY::CUSNUM + KEY$
	USE
		IF ERR = 154%
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE GetNextRec IF ERR = 155%
		FILENAME$ = "PP_CARD"
		CONTINUE HelpError
	END WHEN

	PP_CARD::ODOMETER = PP_MONTHLY::ODOM

17120	UPDATE #PP_CARD.CH%

	GOTO GetNextRec

 ExitProgram:
	!
	! Exit to next program or menu
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

	!
	! Exit from the program after showing error message
	!
	GOTO ExitProgram

19000	!***************************************************************
	! ERROR TRAPPING
	!***************************************************************

	!
	! Resume to display untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
