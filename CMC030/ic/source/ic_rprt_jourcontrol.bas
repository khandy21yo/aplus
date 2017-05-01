1	%TITLE "Cycle Count Journal"
	%SBTTL "IC_RPRT_JOURCONTROL"
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
	! ID:IC007
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_RPRT_JOURCONTROL/LINE
	!	$ LINK/EXE=IC_EXE: IC_RPRT_JOURCONTROL, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE IC_RPRT_JOURCONTROL.OBJ;*
	!
	!
	! Author:
	!
	!	10/29/98 - Kevin Handy
	!
	! Modification History:
	!
	!	11/01/98 - Kevin Handy
	!		Option to leave out blank control numbers.
	!
	!	08/10/99 - Kevin Handy
	!		Clean up (Check)
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	%INCLUDE "SOURCE:[IC.OPEN]IC_CYCLEJOUR.HB"
	MAP (IC_CYCLEJOUR)	IC_CYCLEJOUR_CDD	IC_CYCLEJOUR

	%INCLUDE "SOURCE:[IC.OPEN]IC_JOURCOUNT.HB"
	MAP (IC_JOURCOUNT)	IC_JOURCOUNT_CDD	IC_JOURCOUNT

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	DECLARE			PD_PRODUCT_CDD		PD_PRODUCT_EXAM

	!
	! External functions
	!
	EXTERNAL	LONG	FUNCTION PD_EXAM_PRODUCT
	EXTERNAL	REAL    FUNCTION PC_READ_COST

	%PAGE

	ON ERROR GOTO 19000

 Init:	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	BATCH_NO$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) Batch _#\*
	!	.b
	!	.lm +5
	!	The ^*Batch Number\* field enters the number
	!	of the batch which is to be printed.
	!	.b
	!	Only one batch at a time may be printed.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Batch Number>Cycle Count Journal
	!
	!--

	SHOW_BLANK$ = LEFT$(UTL_REPORTX::OPTDEF(1%), 1%)

	!++
	! Abstract:FLD02
	!	^*(02) Show blank Control Numbers\*
	!	.b
	!	.lm +5
	!	Should blank control numbers be listed on the report
	!	.lm -5
	!
	! Index:
	!	.x Blank Control>Cycle Count Journal
	!
	!--

	MINIMAL$ = LEFT$(UTL_REPORTX::OPTDEF(2%), 1%)

	!++
	! Abstract:FLD03
	!	^*(03) Only missing cards\*
	!	.b
	!	.lm +5
	!	Should only missing control numbers be listed on the report.
	!	.lm -5
	!
	! Index:
	!	.x Only Missing>Cycle Count Journal
	!
	!--

	CONTROL_FILE$ = TRM$(UTL_REPORTX::OPTDEF(8%))

	!++
	! Abstract:FLD09
	!	^*(08) Control Number File\*
	!	.b
	!	.lm +5
	!	Text file to store a cross reference of control numbers.
	!	.lm -5
	!
	! Index:
	!	.x Control Number File
	!
	!--

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_CYCLEJOUR.OPN"
	USE
		FILENAME$ = "IC_CYCLEJOUR"
		CONTINUE HelpError
	END WHEN

305	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_JOURCOUNT.OPN"
	USE
		FILENAME$ = "IC_JOURCOUNT"
		CONTINUE HelpError
	END WHEN

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.OPN"
	USE
		FILENAME$ = "UTL_LOCATION"
		CONTINUE HelpError
	END WHEN

320	!
	! If they specified to look at the control file, then
	! try to open it up
	!
	IF CONTROL_FILE$ <> ""
	THEN
		CALL ASSG_CHANNEL(CONTROL_FILE.CH%, STAT%)
		OPEN CONTROL_FILE$ FOR INPUT AS FILE CONTROL_FILE.CH%, &
			RECORDSIZE 256%, &
			ACCESS READ, &
			ALLOW READ
		GOSUB ReadControl
	END IF

400	!
	! Sort the cycle count into control order
	!
	CALL ASSG_CHANNEL(IC_TEMP.CH%, STAT%)
	CALL READ_DEVICE('IC_JOURCOUNT', IC_TEMP.DEV$, STAT%)

	IC_TEMP.NAME$ = IC_JOURCOUNT.DEV$ + "IC_TEMP.TEMP"

	WHEN ERROR IN
		OPEN IC_TEMP.NAME$ FOR OUTPUT AS FILE IC_TEMP.CH%, &
			ORGANIZATION INDEXED FIXED, &
			MAP IC_JOURCOUNT, &
			TEMPORARY, &
			PRIMARY KEY &
			( &
				IC_JOURCOUNT::CONTROL, &
				IC_JOURCOUNT::PRODUCT &
			)	DUPLICATES, &
			ACCESS MODIFY, ALLOW NONE
	USE
		FILENAME$ = "IC_TEMP"
		CONTINUE HelpError
	END WHEN

410	WHEN ERROR IN
		RESET #IC_JOURCOUNT.CH%
	USE
		FILENAME$ = "IC_TEMP"
		CONTINUE HelpError
	END WHEN

420	WHEN ERROR IN
		GET #IC_JOURCOUNT.CH%, REGARDLESS
	USE
		CONTINUE 500 IF ERR = 11%
		FILENAME$ = "IC_JOURCOUNT"
		CONTINUE HelpError
	END WHEN

430	WHEN ERROR IN
		PUT #IC_TEMP.CH%
	USE
		FILENAME$ = "IC_TEMP"
		CONTINUE HelpError
	END WHEN

	GOTO 420

500	!

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "CYCLE  COUNT  CONTROL  CHECKING"
	TITLE$(2%) = "Inventory Control System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "   Product#       Description                 "  + &
		"            UOM Tp Cat  PackUOM   StdCst   " + &
		"CountPack    CountUnt Control"

	TITLE$(5%) = "."

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	LAST_CONTROL$ = ""
	NEXT_CONTROL$ = ""

	WHEN ERROR IN
		RESET #IC_TEMP.CH%
	USE
		FILENAME$ = "UTL_TEMP"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	WHEN ERROR IN
		GET #IC_TEMP.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "IC_TEMP"
		CONTINUE HelpError
	END WHEN

	IF IC_JOURCOUNT::CONTROL = ""
	THEN
		IF SHOW_BLANK$ = "Y"
		THEN
			TEXT$ = "Blank Control Number"
			GOSUB PrintError
		END IF
		GOTO 17090
	END IF

	IF IC_JOURCOUNT::CONTROL = LAST_CONTROL$
	THEN
		TEXT$ = "Duplicate Control Number: " + LAST_CONTROL$
		GOSUB PrintError
		GOTO 17050
	END IF

	IF IC_JOURCOUNT::CONTROL <> NEXT_CONTROL$
	THEN
		PREV_CONTROL$ = IC_JOURCOUNT::CONTROL
		STAT% = FUNC_NUMBERADD(PREV_CONTROL$, -1%)

		TEXT$ = "Missing Control Numbers: " + &
			NEXT_CONTROL$ + " to " + &
			PREV_CONTROL$

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, LIN%)

		GOSUB MissingControl
	END IF

17050	!
	! Validate part number if possible
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT
	GOTO 17090 IF MINIMAL$ = "Y"

	IF (CONTROL_FILE.CH% <> 0%)
	THEN
		!
		! Search for the starting point
		!
		WHILE (CONTROL_CONTROL$ < IC_JOURCOUNT::CONTROL)

			GOSUB ReadControl

		NEXT

		!
		! If this control number is undefined, then complain
		!
		IF CONTROL_CONTROL$ <> IC_JOURCOUNT::CONTROL
		THEN
			TEXT$ = "Unallocated Control Number: " + &
				IC_JOURCOUNT::CONTROL
			GOSUB PrintError
		ELSE
			IF CONTROL_PRODUCT$ <> IC_JOURCOUNT::PRODUCT OR &
				CONTROL_PRODUCT$ = "______________"
			THEN
				TEXT$ = "Product Number Difference"

				GOSUB PrintError

				TEXT$ = "   -->" + CONTROL_LINE$
				CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
			END IF
		END IF

	END IF

17090	!
	! Set up for next pass through theloop
	!
	LAST_CONTROL$ = IC_JOURCOUNT::CONTROL
	NEXT_CONTROL$ = IC_JOURCOUNT::CONTROL
	STAT% = FUNC_NUMBERADD(NEXT_CONTROL$, 1%)

	GOTO 17020

	%PAGE

 PrintError:
17100	!*******************************************************************
	! Print an error message, and the record that has a problem
	!*******************************************************************

	!
	! TEXT$ should come in with an error message
	!
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, LIN%)

	!
	! Read Product file
	!
	V% = PD_EXAM_PRODUCT(IC_JOURCOUNT::PRODUCT, PD_PRODUCT_EXAM)

	!
	! Print out one line
	!
	COST = PC_READ_COST(IC_JOURCOUNT::PRODUCT, UTL_LOCATION::LOCATION, &
		IC_CYCLEJOUR::COUNTDATE, "")

	EXTCOST = FUNC_ROUND( COST * IC_JOURCOUNT::QUANTITY, 2%)

	IF PD_PRODUCT_EXAM::PRODUCT_FACTOR
	THEN
		PACK.QTY = IC_JOURCOUNT::QUANTITY / &
			PD_PRODUCT_EXAM::PRODUCT_FACTOR &
			IF PD_PRODUCT_EXAM::PRODUCT_FACTOR <> 0.0
	ELSE
		PACK.QTY = IC_JOURCOUNT::QUANTITY
	END IF

	TEXT$ = "   " + &
		IC_JOURCOUNT::PRODUCT + " " + &
		PD_PRODUCT_EXAM::DESCRIPTION + " " + &
		PD_PRODUCT_EXAM::UOM + "  " + &
		PD_PRODUCT_EXAM::PROD_TYPE + " " + &
		PD_PRODUCT_EXAM::CATEGORY + "     " + &
		PD_PRODUCT_EXAM::BOMUOM + "  " + &
		FORMAT$(PD_PRODUCT_EXAM::PRODUCT_FACTOR, "######.#") + " " + &
		FORMAT$(PACK.QTY, "###,###.###") + " " + &
		FORMAT$(IC_JOURCOUNT::QUANTITY, "###,###,###") + " " + &
		IC_JOURCOUNT::CONTROL

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

	!
	! Try for next record
	!
	RETURN

	%PAGE

 MissingControl:
17200	!*******************************************************************
	! Display whatever information we can get on missing control numbers
	!*******************************************************************

	!
	! Search for the starting point
	!
	WHILE (CONTROL_CONTROL$ < NEXT_CONTROL$) AND (CONTROL_FILE.CH% <> 0%)

		GOSUB ReadControl

	NEXT

	!
	! Process all of the other information
	!
17220	IF (CONTROL_CONTROL$ <= PREV_CONTROL$) AND (CONTROL_FILE.CH% <> 0%)
	THEN
		RETURN IF UTL_REPORTX::STAT

		TEXT$ = "   -->" + CONTROL_LINE$
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

		GOSUB ReadControl
		GOTO 17220
	END IF

	RETURN

	%PAGE

 ReadControl:
17300	!*******************************************************************
	! Read one line from the text control file
	!*******************************************************************

	WHEN ERROR IN
		LINPUT #CONTROL_FILE.CH%, CONTROL_LINE$
	USE
		CLOSE CONTROL_FILE.CH%
		CONTROL_FILE.CH% = 0%
		CONTINUE 17390
	END WHEN

	GOTO 17300 IF CONTROL_LINE$ = ""

	CONTROL_CONTROL$ = MID$(CONTROL_LINE$, 1%, 6%)
	CONTROL_LOCATION$ = MID$(CONTROL_LINE$, 10%, 4%)
	CONTROL_BIN$ = MID$(CONTROL_LINE$, 15%, 6%)
	CONTROL_PRODUCT$ = MID$(CONTROL_LINE$, 22%, 14%)

17390	RETURN

	%PAGE

 ExitTotal:
	!
	! Handle end of report
	!
	PREV_CONTROL$ = "~~~~~~"

	TEXT$ = "Missing Control Numbers: " + &
		NEXT_CONTROL$ + " to <end>"

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, LIN%)

	GOSUB MissingControl

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
