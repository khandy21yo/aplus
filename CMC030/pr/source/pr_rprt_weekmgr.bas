1	%TITLE "PR Total Managers Report"
	%SBTTL "PR_RPRT_WEEKMGR"
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
	! ID:PR072
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Total Managers Report\* summarizes the gross earnings, bonuses, and
	!	adjusted gross earnings for each location.
	!	.b
	!	The ^*Total Manager Report\* includes the following fields:
	!	.table 3,25
	!	.te
	!	Location
	!	.te
	!	Location Name
	!	.te
	!	Gross Earnings
	!	.te
	!	Bonus
	!	.te
	!	Adjusted Gross
	!	.end table
	!
	! Index:
	!	.x Managers Total Report
	!	.x Report>Managers Total
	!
	! Option:
	!
	! Author:
	!
	!	08/23/89 - Kevin Handy
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_RPRT_WEEKMGR
	!	$ LINK/EXECUTABLE=PR_EXE:*.EXE PR_RPRT_WEEKMGR, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE PR_RPRT_WEEKMGR.OBJ;*
	!
	! Modification history:
	!
	!	06/19/90 - Aaron Redd
	!		Added line layout information so that the report could
	!		also be sent to either a spreadsheet or a DIF file.
	!
	!	12/18/91 - Kevin Handy
	!		Modified to ignore "A" records in PR_PAY.
	!
	!	04/22/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	06/14/93 - Kevin Handy
	!		Added REGARDLESS to get on UTL_LOCATION.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/12/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/12/97 - Kevin Handy
	!		Lose UTL_WORK.DEV$ variable
	!
	!	08/27/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	11/29/2000 - Kevin Handy
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

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE UTL_REPORTX_CDD UTL_REPORTX

	%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.HB"
	MAP (PR_TRN_PAY)	PR_TRN_PAY_CDD	PR_TRN_PAY
	MAP (PR_HIS_PAY)	PR_TRN_PAY_CDD	PR_HIS_PAY

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION)	UTL_LOCATION_CDD UTL_LOCATION

	!
	! Define record structures
	!
	RECORD PR_TEMP_RECORD
		STRING	REGION = 2%
		STRING	STATE = 2%
		STRING	LOCATION = 4%
		STRING	LOCNAME = 40%
		REAL	GROSS_EARN
		REAL	BONUS
	END RECORD

	!
	! Declare variables and constants
	!
	DECLARE	STRING		LYT_LINE
	DECLARE	PR_TEMP_RECORD	PR_RTOTAL, PR_TOTAL

	!
	! Dimension arrays
	!
	DIM DATA_FILE$(200%)
	DIM PR_TEMP_RECORD PR_TEMP(200%), PR_STATE(50%)

	%PAGE

	!
	! Set up error trapping
	!
	ON ERROR GOTO 19000

 Init:
	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_BATCH_NO$ = DATE_STOREDATE(UTL_REPORTX::OPTDEF(0%))

	!++
	! Abstract:FLD01
	!	^*(01) From Folder\*
	!	.b
	!	.lm +5
	!	The ^*From Folder\* field causes the printing
	!	to begin with a certain folder.
	!	.b
	!	A blank field will cause the report to begin with the first folder in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Folder>Managers Total Report
	!	.x Managers Total Report>From Folder
	!
	!--
	TO_BATCH_NO$ = DATE_STOREDATE(UTL_REPORTX::OPTDEF(1%))
	!++
	! Abstract:FLD02
	!	^*(02) To Folder\*
	!	.b
	!	.lm +5
	!	The ^*To Folder\* field causes the printing
	!	to end with a certain folder.
	!	.b
	!	A blank field will cause the report to end with the last folder in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Folder>Managers Total Report
	!	.x Managers Total Report>To Folder
	!
	!--
	DEPT_WILDCARD$ = TRM$(UTL_REPORTX::OPTDEF(2%))
	!++
	! Abstract:FLD03
	!	^*(03) Department Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Department Wildcard\* field prints
	!	a report including selected departments only using the
	!	wildcard techniques.
	!	.b
	!	Valid wildcard techniques are an asterisk (_*) or question mark (?). An
	!	asterisk (_*) indicates all departments will be considered. A question
	!	mark (?) in a field position indicates a department with any character in
	!	that equivalent position will be considered.
	!	.lm -5
	!
	! Index:
	!	.x Department Wildcard>Managers Total Report
	!	.x Managers Total Report>Department Wildcard
	!
	!--
	SHOW_LOCATION$ = LEFT(UTL_REPORTX::OPTDEF(3%), 1%)
	!++
	! Abstract:FLD04
	!	.ts 55
	!	^*(04) Show Locations	Y or N\*
	!	.b
	!	.lm +5
	!	The ^*Show Locations\* field indicates whether or not the
	!	report should show the locations.
	!	.table
	!	.te
	!	^*Y\* Yes
	!	.te
	!	^*N\* No
	!	.lm -5
	!	.end table
	!
	! Index:
	!	.x Show Locations>Managers Total Report
	!	.x Managers Total Report>Show Locations
	!
	!--

	CALL READ_DEVICE("PR_TRN_PAY", PR_TRN_PAY.DEV$, STAT%)
	CALL READ_DEVICE("PR_HIS_PAY", PR_HIS_PAY.DEV$, STAT%)

	CALL PR_FUNC_FILESCAN(FROM_BATCH_NO$, &
		TO_BATCH_NO$, &
		PR_TRN_PAY.DEV$, &
		PR_HIS_PAY.DEV$, &
		DATA_FILE$())

	DATA_FILE% = VAL%(DATA_FILE$(0%))

	GOTO ExitProgram IF DATA_FILE% = 0%

300	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.OPN"
	USE
		CONTINUE 310
	END WHEN

310	!

350	!*******************************************************************
	! Loop through all files, creating information
	!*******************************************************************

	CALL ENTR_3MESSAGE(SCOPE, "Creating work file.  Reading Pay file.", 1%)

	TOTAL_LOCATIONS% = 0%
	THIS_LOCATION% = 0%
	PR_TEMP(0%)::LOCATION = STRING$(4%, 127%)

	FOR LOOP% = 1% TO DATA_FILE%
		BATCH_NO$ = DATA_FILE$(LOOP%)

		CALL ENTR_3MESSAGE(SCOPE, "Starting " + BATCH_NO$, 1%)

		USE_HISTORY% = 0%

400		!
		! Open file
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_TRN_PAY.OPN"
		USE
			CONTINUE 402 IF ERR = 5%
			CONTINUE 460 IF ERR = 11%
			FILENAME$ = "PR_TRN_PAY"
			CONTINUE HelpError
		END WHEN

		PR_TMP_PAY.CH% = PR_TRN_PAY.CH%

		GOTO 405

402		!
		! Open pay history folder if journal not there
		!
		USE_HISTORY% = -1%

		WHEN ERROR IN
			%INCLUDE "SOURCE:[PR.OPEN]PR_HIS_PAY.OPN"
		USE
			CONTINUE 460 IF ERR = 5%
			CONTINUE 460 IF ERR = 11%
			FILENAME$ = "PR_TRN_PAY"
			CONTINUE HelpError
		END WHEN

		PR_TMP_PAY.CH% = PR_HIS_PAY.CH%

405		WHEN ERROR IN
			RESET #PR_TMP_PAY.CH%, KEY #0%
		USE
			CONTINUE 460
		END WHEN

410		WHEN ERROR IN
			GET #PR_TMP_PAY.CH%, REGARDLESS
		USE
			CONTINUE 460 IF ERR = 11%
			FILENAME$ = "PR_TRN_PAY"
			CONTINUE HelpError
		END WHEN

		!
		! If history then set history map to journal
		!
		IF USE_HISTORY%
		THEN
			PR_TRN_PAY = PR_HIS_PAY
		END IF

		GOTO 410 IF PR_TRN_PAY::PTYPE = "A"

420		!
		! Right department
		!
		IF (DEPT_WILDCARD$ <> "")
		THEN
			GOTO 410 &
				IF (COMP_STRING(PR_TRN_PAY::DEPT, DEPT_WILDCARD$) = 0%)
		END IF

		IF (PR_TRN_PAY::LOCATION <> PR_TEMP(THIS_LOCATION%)::LOCATION)
		THEN
			GOSUB SeekLocation
		END IF

		PR_TEMP(THIS_LOCATION%)::GROSS_EARN = &
			PR_TEMP(THIS_LOCATION%)::GROSS_EARN + &
			PR_TRN_PAY::GROSS

		IF (PR_TRN_PAY::CODE = "BO")
		THEN
			PR_TEMP(THIS_LOCATION%)::BONUS = &
				PR_TEMP(THIS_LOCATION%)::BONUS + &
				PR_TRN_PAY::GROSS
		END IF

		GOTO 410

460		CLOSE PR_TMP_PAY.CH%

		CALL ASSG_FREECHANNEL(PR_TRN_PAY.CH%)
		CALL ASSG_FREECHANNEL(PR_HIS_PAY.CH%)

	NEXT LOOP%

 ReportTitle:
	!
	! Set up titles
	!
	TITLE$(1%) = "Pension Report"
	TITLE$(2%) = "From Date:  " + PRNT_DATE(FROM_BATCH_NO$, 8%) + &
		" to Date " + PRNT_DATE(TO_BATCH_NO$, 8%)

	TITLE$(3%) = "For Department(s) " + DEPT_WILDCARD$
	TITLE$(3%) = "For All Departments" &
		IF (DEPT_WILDCARD$ = "") OR (DEPT_WILDCARD$ = "*")

	TITLE$(4%) = ""

	!
	! Column headings
	!
	TITLE$(5%) = "      Loc  LocationName                             " + &
		"     GrossEarn          Bonus    Adj'd Gross"
	TITLE$(6%) = "."
	TITLE$(7%) = ""

	!
	! Line layouts
	!
	LYT_LINE = "Location:010,LocationName:051,VGrossEarnings:066," + &
		"VBonus:081,VAdjustedGross:096"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	PR_TOTAL::LOCATION = ""
	PR_TOTAL::REGION = ""
	PR_TOTAL::LOCNAME = ""
	PR_TOTAL::GROSS_EARN = 0.0
	PR_TOTAL::BONUS = 0.0

	THIS_REGION$ = "67816728312"

	FOR THIS_STATE% = 1% TO TOTAL_STATES%

		IF (PR_STATE(THIS_STATE%)::REGION <> THIS_REGION$)
		THEN
			GOSUB TotalRegion

			TEXT$ = "Region: " + PR_STATE(THIS_STATE%)::REGION
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		END IF

		FOR THIS_LOCATION% = 1% TO TOTAL_LOCATIONS%

			!
			GOTO ExitProgram IF UTL_REPORTX::STAT

			IF (PR_TEMP(THIS_LOCATION%)::REGION = PR_STATE(THIS_STATE%)::REGION) AND &
				PR_TEMP(THIS_LOCATION%)::STATE = PR_STATE(THIS_STATE%)::STATE
			THEN
				IF (SHOW_LOCATION$ = "Y")
				THEN
					!
					! Set up output text
					!
					TEXT$ = &
						"      " + &
						PR_TEMP(THIS_LOCATION%)::LOCATION + " " + &
						PR_TEMP(THIS_LOCATION%)::LOCNAME + " " + &
						FORMAT$(PR_TEMP(THIS_LOCATION%)::GROSS_EARN, "###,###,###.## ") + &
						FORMAT$(PR_TEMP(THIS_LOCATION%)::BONUS, "###,###,###.## ") + &
						FORMAT$(PR_TEMP(THIS_LOCATION%)::GROSS_EARN - &
							PR_TEMP(THIS_LOCATION%)::BONUS, "###,###,###.## ")

					CALL OUTP_LINE(LYT_LINE, UTL_REPORTX, TITLE$(), TEXT$, 0%)
				END IF

				PR_STATE(THIS_STATE%)::GROSS_EARN = &
					PR_STATE(THIS_STATE%)::GROSS_EARN + &
					PR_TEMP(THIS_STATE%)::GROSS_EARN

				PR_STATE(THIS_STATE%)::BONUS = &
					PR_STATE(THIS_STATE%)::BONUS + &
					PR_TEMP(THIS_STATE%)::BONUS

				PR_TOTAL::GROSS_EARN = &
					PR_TOTAL::GROSS_EARN + &
					PR_TEMP(THIS_STATE%)::GROSS_EARN

				PR_TOTAL::BONUS = &
					PR_TOTAL::BONUS + &
					PR_TEMP(THIS_STATE%)::BONUS

				PR_RTOTAL::GROSS_EARN = &
					PR_RTOTAL::GROSS_EARN + &
					PR_TEMP(THIS_STATE%)::GROSS_EARN

				PR_RTOTAL::BONUS = &
					PR_RTOTAL::BONUS + &
					PR_TEMP(THIS_STATE%)::BONUS

			END IF

		NEXT THIS_LOCATION%

		!
		! Set up output text
		!
		TEXT$ = &
			"           " + &
			"State " + PR_STATE(THIS_STATE%)::STATE + " Total                           " + &
			FORMAT$(PR_STATE(THIS_STATE%)::GROSS_EARN, "###,###,###.## ") + &
			FORMAT$(PR_STATE(THIS_STATE%)::BONUS, "###,###,###.## ") + &
			FORMAT$(PR_STATE(THIS_STATE%)::GROSS_EARN - &
				PR_STATE(THIS_STATE%)::BONUS, "###,###,###.## ")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -2%)

	NEXT THIS_STATE%

 ExitTotal:
	GOSUB TotalRegion

	!
	! Set up output text
	!
	TEXT$ = &
		"           " + &
		"Grand Total                              " + &
		FORMAT$(PR_TOTAL::GROSS_EARN, "###,###,###.## ") + &
		FORMAT$(PR_TOTAL::BONUS, "###,###,###.## ") + &
		FORMAT$(PR_TOTAL::GROSS_EARN - &
			PR_TOTAL::BONUS, "###,###,###.## ")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)


 ExitProgram:
	CALL OUTP_FINISH(UTL_REPORTX)

17510	!
	! Exit to next program or menu
	!
	IF TRM$(UTL_REPORTX::NEXTRUN) = ""
	THEN
		CALL SUBR_3EXITPROGRAM(SCOPE, "", "")
	ELSE
		CALL SUBR_3EXITPROGRAM(SCOPE, "RUN " + UTL_REPORTX::NEXTRUN, "")
	END IF

	%PAGE

18000	!*******************************************************************
	! Subroutine to store salary amount in temp file
	!*******************************************************************
 SeekLocation:
	!
	! Search for location already in list
	!
	FOR THIS_LOCATION% = 1% TO TOTAL_LOCATIONS%

		IF (PR_TEMP(THIS_LOCATION%)::LOCATION = PR_TRN_PAY::LOCATION)
		THEN
			GOTO 18090
		END IF

		IF (PR_TEMP(THIS_LOCATION%)::LOCATION > PR_TRN_PAY::LOCATION)
		THEN
			GOTO 18020
		END IF

	NEXT THIS_LOCATION%

	THIS_LOCATION% = TOTAL_LOCATIONS% + 1%

18020	!
	! Insert a new location into the list
	!
	PR_TEMP(TEMP% + 1%) = PR_TEMP(TEMP%) &
		FOR TEMP% = TOTAL_LOCATIONS% TO THIS_LOCATION% STEP -1%

	PR_TEMP(THIS_LOCATION%)::LOCATION = PR_TRN_PAY::LOCATION
	PR_TEMP(THIS_LOCATION%)::REGION = ""
	PR_TEMP(THIS_LOCATION%)::STATE = ""
	PR_TEMP(THIS_LOCATION%)::LOCNAME = ""
	PR_TEMP(THIS_LOCATION%)::GROSS_EARN = 0.0
	PR_TEMP(THIS_LOCATION%)::BONUS = 0.0

	TOTAL_LOCATIONS% = TOTAL_LOCATIONS% + 1%

18030	WHEN ERROR IN
		GET #UTL_LOCATION.CH%, KEY #0% EQ PR_TRN_PAY::LOCATION, REGARDLESS
	USE
		CONTINUE 18040
	END WHEN

	PR_TEMP(THIS_LOCATION%)::REGION = UTL_LOCATION::REGION
	PR_TEMP(THIS_LOCATION%)::STATE = UTL_LOCATION::STATE
	PR_TEMP(THIS_LOCATION%)::LOCNAME = UTL_LOCATION::LOCNAME

18040	!
	! Search for Region already in list
	!
	FOR THIS_STATE% = 1% TO TOTAL_STATES%

		IF (PR_STATE(THIS_STATE%)::REGION + PR_STATE(THIS_STATE%)::STATE = &
			PR_TEMP(THIS_LOCATION%)::REGION + PR_TEMP(THIS_LOCATION%)::STATE)
		THEN
			GOTO 18090
		END IF

		IF (PR_STATE(THIS_STATE%)::REGION + PR_STATE(THIS_STATE%)::STATE > &
			PR_TEMP(THIS_LOCATION%)::REGION + PR_TEMP(THIS_LOCATION%)::STATE)
		THEN
			GOTO 18050
		END IF

	NEXT THIS_STATE%

	THIS_STATE% = TOTAL_STATES% + 1%

18050	!
	! Insert a new region into the list
	!
	PR_STATE(TEMP% + 1%) = PR_STATE(TEMP%) &
		FOR TEMP% = TOTAL_STATES% TO THIS_STATE% STEP -1%

	PR_STATE(THIS_STATE%)::REGION = PR_TEMP(THIS_LOCATION%)::REGION
	PR_STATE(THIS_STATE%)::STATE = PR_TEMP(THIS_LOCATION%)::STATE
	PR_STATE(THIS_STATE%)::LOCATION = ""
	PR_STATE(THIS_STATE%)::LOCNAME = ""
	PR_STATE(THIS_STATE%)::GROSS_EARN = 0.0
	PR_STATE(THIS_STATE%)::BONUS = 0.0

	TOTAL_STATES% = TOTAL_STATES% + 1%

18090	RETURN

	%PAGE

18100	!*******************************************************************
	! Print total of region
	!*******************************************************************
 TotalRegion:

	IF (PR_RTOTAL::GROSS_EARN <> 0.0) OR &
		(PR_RTOTAL::BONUS <> 0.0)
	THEN
		!
		! Set up output text
		!
		TEXT$ = &
			"           " + &
			"Region " + THIS_REGION$ + &
			" Total                          " + &
			FORMAT$(PR_RTOTAL::GROSS_EARN, "###,###,###.## ") + &
			FORMAT$(PR_RTOTAL::BONUS, "###,###,###.## ") + &
			FORMAT$(PR_RTOTAL::GROSS_EARN - &
				PR_RTOTAL::BONUS, "###,###,###.## ")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)
	END IF

	THIS_REGION$ = PR_STATE(THIS_STATE%)::REGION
	PR_RTOTAL::GROSS_EARN = 0.0
	PR_RTOTAL::BONUS = 0.0

	RETURN

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
