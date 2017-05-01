1	%TITLE "Convention and Disposition List"
	%SBTTL "AD_RPRT_CONVENTION"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1986 BY
	!
	! Computer Management Center
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
	! ID:AD013
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Convention\* report prints
	!	the Convention Definition Table. This printing will contain the following
	!	fields:
	!	.lm 15
	!	.b
	!	.list 0,"*"
	!	.le
	!	Code
	!	.le
	!	Description
	!	.le
	!	Coefficient
	!	.le
	!	Specification
	!	.els
	!	.lm -5
	!
	! Index:
	!	.x Convention>Report
	!	.x Report>Convention
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS AD_SOURCE:AD_RPRT_CONVENTION/LINE
	!	$ LINK/EXE=AD_EXE: AD_RPRT_CONVENTION, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE AD_RPRT_CONVENTION.OBJ;*
	!
	! AUTHOR:
	!
	!	12/05/87 - Frank F. Starman
	!
	! MODIFICATION HISTORY:
	!
	!	03/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/10/95 - Kevin Handy
	!		(V3.6)
	!		Update source code to V3.6 standards.
	!
	!	08/28/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/19/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/17/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/15/2000 - Kevin Handy
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
	DECLARE UTL_REPORTX_CDD UTL_REPORTX

	%INCLUDE "SOURCE:[AD.OPEN]AD_CONVENTION.HB"
	MAP (AD_CONVENTION) AD_CONVENTION_CDD AD_CONVENTION

	!
	! External functions
	!
	DECLARE STRING TEXT, FROM_ITEM, TO_ITEM, WLDCRD
	DECLARE WORD CONSTANT PRINT_WIDTH = 132%

	%PAGE

	ON ERROR GOTO 19000

	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, PRINT_WIDTH)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Convention\*
	!	.b
	!	.lm +5
	!	A ^*From Convention\* value
	!	causes the report to begin with a selected convention.
	!	.b
	!	A blank setting will cause the report to start with the
	!	first convention record in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Convention>Convention List
	!	.x Convention List>From Convention
	!
	!--

	TO_ITEM = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Convention\*
	!	.b
	!	.lm +5
	!	A ^*To Convention\* value causes the
	!	report to end with a selected convention.
	!	.b
	!	A blank setting will cause the report to end with the last
	!	convention record in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Convention>Convention List
	!	.x Convention List>To Convention
	!
	!--

	WLDCRD = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* setting prints a
	!	report including selected Convention codes only using
	!	the wildcarding technique.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard>Convention List
	!	.x Convention List>Wildcard
	!
	!--


300	WHEN ERROR IN
		%INCLUDE "SOURCE:[AD.OPEN]AD_CONVENTION.OPN"
	USE
		FILENAME$ = "AD_CONVENTION"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "CONVENTION  LIST"
	TITLE$(2%) = "Asset Depreciation System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	!		1234567890123456789012345678901234567890
	TITLE$(4%) = "Code Description                        " + &
		"      Coeff Specif"

	TITLE$(5%) = "."

	LYT_LINE$ = "$Code:005,$Description:046,VCoeff:052,$Specif:059"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM = ""
		THEN
			RESET #AD_CONVENTION.CH%
		ELSE
			FIND #AD_CONVENTION.CH%, &
				KEY #0% GE FROM_ITEM, &
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
		GET #AD_CONVENTION.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "AD_CONVENTION"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	GOTO ExitTotal &
		IF (AD_CONVENTION::CONVENTION > TO_ITEM) AND TO_ITEM <> ""

	GOTO GetNextRec &
		IF COMP_STRING(EDIT$(AD_CONVENTION::CONVENTION, -1%), &
		WLDCRD) = 0% &
		AND WLDCRD <> ""

17300	!
	! Print out one line
	!
	TEXT = AD_CONVENTION::CONVENTION + "   " + &
		AD_CONVENTION::DESCRIPTION + " " + &
		FORMAT$(AD_CONVENTION::COEFF * 0.01, "##.##") + " " + &
		AD_CONVENTION::SPECIFIC

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT, 0%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

17350	!
	! Try for next record
	!
	GOTO GetNextRec

 ExitTotal:
17400	!
	! Handle end of report
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
