1	%TITLE "Asset Class File List"
	%SBTTL "AD_RPRT_CLASS"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1986, 1988 BY
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
	! ID:AD024
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This program prints the Asset Class File List.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AD_SOURCE:AD_RPRT_CLASS/LINE
	!	$ LINK/EXE=AD_EXE: AD_RPRT_CLASS, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AD_RPRT_CLASS.OBJ;*
	!
	! Author:
	!
	!	09/15/88 - J. Shad Rydalch
	!
	! Modification History:
	!
	!	03/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/10/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards.
	!
	!	08/27/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/12/97 - Kevin Handy
	!		Reformat source code
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/13/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD	UTL_REPORTX

	%INCLUDE "SOURCE:[AD.OPEN]AD_CLASS.HB"
	MAP	(AD_CLASS)	AD_CLASS_CDD	AD_CLASS

	%PAGE

	ON ERROR GOTO 19000

	!
 Init:	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)
	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)
	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)
	SORT_BY$ = EDIT$(UTL_REPORTX::OPTDEF(9%), 132%)

	SELECT SORT_BY$
	CASE "C"
		SORT_KEY% = 0%
		ADD_TITLE$ = "BY  CLASS"

	CASE "D"
		SORT_KEY% = 1%
		ADD_TITLE$ = "BY  DESCRIPTION"

	END SELECT


300	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_CLASS.OPN"
	USE
		FILENAME$ = "AD_CLASS"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "ASSET CLASS FILE LIST " + ADD_TITLE$
	TITLE$(2%) = "Asset Depreciation System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "Class  Description                            " + &
		"    Life   GDS    ADS"

	TITLE$(5%) = "."

	LYT_LINE$ = "$Class:007,$Description:050,VLife:057,VGDS:064,VADS:071"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #AD_CLASS.CH%, KEY #SORT_KEY%
		ELSE
			FIND #AD_CLASS.CH%, &
				KEY #SORT_KEY% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		CALL ENTR_3MESSAGE(SCOPE, &
			"Unable to find beginning record!", 0%)

		CONTINUE ExitProgram
	END WHEN

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #AD_CLASS.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "AD_CLASS"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	SELECT SORT_BY$

	CASE "C"
		GOTO ExitTotal IF (AD_CLASS::ASSCLASS > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(AD_CLASS::ASSCLASS, -1%), WLDCRD$) = 0%

	CASE "D"
		GOTO ExitTotal IF (AD_CLASS::DESCRIPTION > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <> "" AND &
			COMP_STRING(EDIT$(AD_CLASS::DESCRIPTION, -1%), &
			WLDCRD$) = 0%

	END SELECT

17300	!
	! Print out one line
	!
	TEXT$ = AD_CLASS::ASSCLASS + " " + &
		AD_CLASS::DESCRIPTION + "   " + &
		FORMAT$(AD_CLASS::LIFE * 0.1, "##.#") + "   " + &
		FORMAT$(AD_CLASS::GDS * 0.1, "##.#") + "   " + &
		FORMAT$(AD_CLASS::ADS * 0.1, "##.#") + "   "

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, 0%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

17350	!
	! Try for next record
	!
	GOTO GetNextRec

 ExitTotal:
17400	!
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
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
