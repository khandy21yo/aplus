1	%TITLE "Determine Taxable/Reportable Status"
	%SBTTL "PR_READ_SUBJTAX"
	%IDENT "V3.6a Calico"

	SUB PR_READ_SUBJTAX(PR_ERNDED_DEF.CH%, &
		SUBJECT_CODE$, &
		ETYPE$, &
		CODE$, &
		TAXABLE%, &
		REPORTABLE%)

	!
	! COPYRIGHT (C) 1986 BY
	! Computer Management Center
	! Idaho Falls, Idaho.
	!
	! This software is furnished under a license and may be used and
	! copied only in accordance with terms of such license and with
	! the inclusion of the above copyright notice.  This software or
	! any other copies therof may not be provided or otherwise made
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
	! Abstract:HELP
	!	.p
	!	This subroutine determines in a payroll earning and
	!		deduction is taxable and reportable
	! Index
	!	REPORT
	!
	! Inputs
	!	PR_ERNDED_DEF.CH%
	!		Earnings/deduction definition file channel
	!	SUBJECT_CODE
	!		FWH = Federal Withholding
	!		FIE = FICA Employee
	!		FIR = FICA Employer
	!		FHE = FICA (HI) Employee
	!		FHR = FICA (HI) Employer
	!		FUI = Federal Unemployment
	!		SWH = State Withholding
	!		SUI = State Unemployment
	!		OST = Other State Taxes
	!		CWH = City Withholding
	!		DWH = County Withholding
	!		EWH = School Withholding
	!		SWC = Workmans comp
	!		WCI = Subject to WC (Only one to return subject)
	!	ETYPE$
	!		P = Pay Item
	!		D = Deduction Item
	!	CODE$
	!		Earning/Deduction code
	!	TAXABLE%
	!		0 = Taxable
	!		-1 = Not Taxable
	!	REPORTABLE%
	!		0 = Reportable
	!		-1 = Not Reportable
	!
	! Index:
	!
	! Option:
	!
	! Outputs:
	!
	!
	! Example:
	!
	!	CALL PR_READ_SUBJTAX(PR_ERNDED_DEF.CH%,
	!		'FUI', &
	!		'P', &
	!		'TX', &
	!		TAXABLE%, &
	!		REPORTABLE%)
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_READ_SUBJTAX
	!	$ LIB FUNC_LIB:CMCFUN/REP PR_READ_SUBJTAX
	!	$ DELETE PR_READ_SUBJTAX.OBJ;*
	!
	! Author:
	!
	!	11/29/87 - Robert Peterson
	!
	! Modification history:
	!
	!	4/15/87 - Robert Peterson
	!		Add flag for subject to WC.
	!
	!	03/17/89 - Kevin Handy
	!		Changed from FIND...GET... to use just GET...
	!		Places IF statement around get to try to
	!		speed calculations up.
	!
	!	06/07/89 - Kevin Handy
	!		Modified to handle SWC.  Before
	!		it ignored SWC entirely.
	!
	!	08/21/90 - Kevin Handy
	!		Modified to create an in-memory array of records
	!		in an attempt to speed up the final post (and
	!		many other programs while were at it).
	!
	!	05/10/93 - Kevin Handy
	!		Added a binary pre-scan to the lookup to make
	!		it faster to find part.
	!
	!	08/30/93 - Kevin Handy
	!		Fixed order of test in binary search.
	!
	!	04/10/95 - Kevin Handy
	!		(V3.6)
	!		Update source code to V3.6 standards.
	!		Removed parameter SUBJECT%, because it is just
	!		the taxable flag for WC.
	!
	!	07/16/96 - Kevin Handy
	!		Reformat source code.
	!
	!	03/19/97 - Kevin Handy
	!		Added FHE, FHR codes.
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[PR.OPEN]PR_ERNDED_DEF.HB"
	MAP (PR_ERNDED_DEF) PR_ERNDED_DEF_CDD PR_ERNDED_DEF

	DECLARE INTEGER CONSTANT MAX_ARRAY = 150%

	COM (PR_READ_SUBJTAX) &
		INTEGER LOADED_COUNT, &
		PR_ERNDED_DEF_CDD PR_ERNDED_ARRAY(MAX_ARRAY)

	%PAGE

	!
	! Assume taxable
	!
	TAXABLE%, REPORTABLE% = 0%

	!
	! Fill in array if this is the first time
	!
	GOSUB LoadArray if LOADED_COUNT = 0%

	!
	! Decrease bounds on loop using binary search.
	!
	LOW% = 1%
	HIGH% = LOADED_COUNT

	WHILE HIGH% - LOW% > 4%
		MIDDLE% = (LOW% + HIGH%) / 2%
		IF (PR_ERNDED_ARRAY(MIDDLE%)::ETYPE + &
			PR_ERNDED_ARRAY(MIDDLE%)::CODE) > ETYPE$ + CODE$
		THEN
			HIGH% = MIDDLE%
		ELSE
			LOW% = MIDDLE%
		END IF
	NEXT

	!
	! Scan array for this item
	!
	FOR LOOP% = LOW% TO HIGH%
		IF (PR_ERNDED_ARRAY(LOOP%)::ETYPE = ETYPE$) AND &
			(PR_ERNDED_ARRAY(LOOP%)::CODE = CODE$)
		THEN
			PR_ERNDED_DEF = PR_ERNDED_ARRAY(LOOP%)
			GOTO 8010
		END IF
	NEXT LOOP%

	!
	! Give up if no possibility it didn't fit into array
	!
	GOTO 8090 IF LOADED_COUNT <> MAX_ARRAY

8000	!
	! Have to pull it in manually from the file
	!
	IF (ETYPE$ <> PR_ERNDED_DEF::ETYPE) OR &
		(CODE$  <> PR_ERNDED_DEF::CODE)
	THEN
		WHEN ERROR IN
			GET #PR_ERNDED_DEF.CH%, KEY #0% EQ LEFT(ETYPE$, 1%) + &
				LEFT(CODE$, 2%), REGARDLESS
		USE
			CONTINUE 8090
		END WHEN
	END IF

8010	SELECT SUBJECT_CODE$

	CASE "FWH"
		TAXABLE% = (PR_ERNDED_DEF::TAXABLE_FWH = "N")
		REPORTABLE% = (PR_ERNDED_DEF::REPORTABLE_FWH = "N")

	CASE "FIE"
		TAXABLE% = (PR_ERNDED_DEF::TAXABLE_FIE = "N")
		REPORTABLE% = (PR_ERNDED_DEF::REPORTABLE_FIE = "N")

	CASE "FIR"
		TAXABLE% = (PR_ERNDED_DEF::TAXABLE_FIR = "N")
		REPORTABLE% = (PR_ERNDED_DEF::REPORTABLE_FIR = "N")

	CASE "FHE"
		TAXABLE% = (PR_ERNDED_DEF::TAXABLE_FIE = "N")
		REPORTABLE% = (PR_ERNDED_DEF::REPORTABLE_FIE = "N")

	CASE "FHR"
		TAXABLE% = (PR_ERNDED_DEF::TAXABLE_FIR = "N")
		REPORTABLE% = (PR_ERNDED_DEF::REPORTABLE_FIR = "N")

	CASE "FUI"
		TAXABLE% = (PR_ERNDED_DEF::TAXABLE_FUI = "N")
		REPORTABLE% = (PR_ERNDED_DEF::REPORTABLE_FUI = "N")

	CASE "SWH"
		TAXABLE% = (PR_ERNDED_DEF::TAXABLE_SWH = "N")
		REPORTABLE% = (PR_ERNDED_DEF::REPORTABLE_SWH = "N")

	CASE "SUI"
		TAXABLE% = (PR_ERNDED_DEF::TAXABLE_SUI = "N")
		REPORTABLE% = (PR_ERNDED_DEF::REPORTABLE_SUI = "N")

	CASE "OST"
		TAXABLE% = (PR_ERNDED_DEF::TAXABLE_OST = "N")
		REPORTABLE% = (PR_ERNDED_DEF::REPORTABLE_OST = "N")

	CASE "CWH"
		TAXABLE% = (PR_ERNDED_DEF::TAXABLE_CWH = "N")
		REPORTABLE% = (PR_ERNDED_DEF::REPORTABLE_CWH = "N")

	CASE "DWH"
		TAXABLE% = (PR_ERNDED_DEF::TAXABLE_DWH = "N")
		REPORTABLE% = (PR_ERNDED_DEF::REPORTABLE_DWH = "N")

	CASE "EWH"
		TAXABLE% = (PR_ERNDED_DEF::TAXABLE_EWH = "N")
		REPORTABLE% = (PR_ERNDED_DEF::REPORTABLE_EWH = "N")

	CASE "SWC", "WCI"
		TAXABLE% = (PR_ERNDED_DEF::SUBJ_WC = "N")
		REPORTABLE% = (PR_ERNDED_DEF::SUBJ_WC = "N")

	END SELECT

8090	EXIT SUB

	%PAGE

9000	!*******************************************************************
	! Load up array from file
	!*******************************************************************

 LoadArray:

9010	WHEN ERROR IN
		RESET #PR_ERNDED_DEF.CH%
	USE
		CONTINUE 9090
	END WHEN

9020	WHEN ERROR IN
		GET #PR_ERNDED_DEF.CH%, REGARDLESS
	USE
		CONTINUE 9090
	END WHEN

	LOADED_COUNT = LOADED_COUNT + 1%
	PR_ERNDED_ARRAY(LOADED_COUNT) = PR_ERNDED_DEF

	GOTO 9020 IF LOADED_COUNT < MAX_ARRAY

9090	RETURN

	%PAGE

 ! HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
 !	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
 !		"E", ERN$, FILENAME$, NUM1$(ERR))
 !	GOTO 8090

	END SUB
