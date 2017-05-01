1	%TITLE "Product Usage Report by Percentage"
	%SBTTL "IC_RPRT_AUDITBALANCE"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 2002 BY
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
	! ID:IC133
	!
	! Abstract:HELP
	!
	!	This report is used for repairing problems with the
	!	inventory balance files.  It requires that all of the
	!	history files exist, and will use them to calculate
	!	what should be in the beginning and posted balance
	!	columns.
	!	.p
	!	This report is not meant for normal use, as the
	!	information probably does not have meaning to the
	!	casual reader.  It is meant for recovering information
	!	in the balance file when that file is broken.
	!
	! Index:
	!
	!	Inventory Balance>Audit
	!	Audit>Inventory Balance
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_RPRT_AUDITBALANCE/LINE
	!	$ LINK/EXE=IC_EXE: IC_RPRT_AUDITBALANCE, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE IC_RPRT_AUDITBALANCE.OBJ;*
	!
	! Author:
	!
	!	02/06/2002 - Kevin Handy
	!
	! Modification History:
	!
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

	%INCLUDE "SOURCE:[IC.OPEN]IC_CONTROL.HB"
	MAP (IC_CONTROL)	IC_CONTROL_CDD		IC_CONTROL

	%INCLUDE "SOURCE:[IC.OPEN]IC_35HISTORY.HB"
	MAP (IC_35HISTORY)	IC_35HISTORY_CDD	IC_35HISTORY

	%INCLUDE "SOURCE:[IC.OPEN]IC_35BALANCE.HB"
	MAP (IC_35BALANCE) IC_35BALANCE_CDD IC_35BALANCE

	RECORD WORK_FILE_CDD
		STRING PRODUCT = 14%
		STRING LOCATION = 4%
		STRING TRANSTYPE = 2%
		GFLOAT BALANCE
		GFLOAT POSTED
	END RECORD

	MAP (WORK_FILE) WORK_FILE_CDD WORK_FILE

	!
	! Dimension arrays
	!
	DIM IC_35HISTORY_FILE$(240%)

	%PAGE

	ON ERROR GOTO 19000

 Init:	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 80%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field causes the
	!	report to begin printing with a selected item number.
	!	The value entered must be in agreement with the value in field
	!	(01) Sort by.
	!	.b
	!	A blank field will cause the report to begin with the first
	!	item in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Product Usage Report
	!	.x Product Usage RepoIrt>From Item
	!	.x Item>From
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field causes
	!	the report to end printing with a selected item number.
	!	The value entered must be in agreement with the value in field (01)
	!	Sort by.
	!	.b
	!	A blank setting will cause the report to end with the
	!	last item in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Product Usage Report
	!	.x Product Usage Report>To Item
	!	.x Item>To
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects designated items
	!	to be printed by entering a "wildcard" value.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard>Product Usage Report
	!	.x Product Usage Report>Wildcard
	!
	!--

	LOCATION$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	^*(05) Location\*
	!	.b
	!	.lm +5
	!	The ^*Location\* field selects a designated
	!	product location for which the report will be printed.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Location>Product Usage Report
	!	.x Product Usage Report>Location
	!
	!--

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_CONTROL.CRE"

		GET #IC_CONTROL.CH%, RECORD 1%
	USE
		FILENAME$ = "IC_CONTROL"
		CONTINUE HelpError
	END WHEN

	!
	! Assign the current period
	!
	PCLOSED% = VAL%(IC_CONTROL::PERIOD)

305	CALL READ_DEVICE('IC_35HISTORY',IC_35HISTORY.DEV$, STAT%)

	CALL FIND_FILE(IC_35HISTORY.DEV$ + "IC_35HISTORY*.HIS", &
		IC_35HISTORY_FILE$(), 16%, "", "")

	IC_35HISTORY_FILE% = VAL%(IC_35HISTORY_FILE$(0%))

	IF IC_35HISTORY_FILE% = 0%
	THEN
		CALL ENTR_3MESSAGE(SCOPE, &
			"Inventory History files do not exist", 0%)
		GOTO ExitProgram
	END IF

	IC_35HISTORY_FILE$(I%) = &
		MID(IC_35HISTORY_FILE$(I%), 14%, 4%) &
		FOR I% = 1% TO IC_35HISTORY_FILE%

800	!
	! Create work file
	!
	CALL ASSG_CHANNEL(WORK_FILE.CH%, STAT%)

	OPEN "IC_WORK.TMP" AS FILE WORK_FILE.CH%, &
		ORGANIZATION INDEXED FIXED, &
		TEMPORARY, &
		MAP WORK_FILE, &
		BUFFER 32%, &
		PRIMARY KEY &
		( &
			WORK_FILE::PRODUCT, &
			WORK_FILE::LOCATION, &
			WORK_FILE::TRANSTYPE &
		), &
		ACCESS MODIFY, ALLOW NONE

900	!
	! Process all years
	!
	FOR LOOP% = 1% TO IC_35HISTORY_FILE%

		YYYY$ = IC_35HISTORY_FILE$(LOOP%)
		CALL ENTR_3MESSAGE(SCOPE, YYYY$, 1%)

		THISPER% = VAL%(YYYY$) * 100%

		WHEN ERROR IN
			%INCLUDE "SOURCE:[IC.OPEN]IC_35HISTORY.OPN"
		USE
			CALL ENTR_3MESSAGE(SCOPE, &
				"Inventory Hisyory file won't open" + &
				YYYY$, 0%)
			CONTINUE 990
		END WHEN

		WHEN ERROR IN
			IF FROM_ITEM$ = ""
			THEN
				RESET #IC_35HISTORY.CH%
			ELSE
				FIND #IC_35HISTORY.CH%, KEY #0% GE FROM_ITEM$
			END IF
		USE
			CONTINUE 980
		END WHEN

910		WHEN ERROR IN
			GET #IC_35HISTORY.CH%, REGARDLESS
		USE
			CONTINUE 980
		END WHEN

		GOTO 910 IF IC_35HISTORY::PRODUCT < FROM_ITEM$
		GOTO 980 IF IC_35HISTORY::PRODUCT > TO_ITEM$ AND TO_ITEM$ <> ""

		SUBTOTAL = 0.0
		SUBPOSTED = 0.0

		FOR I% = 1% TO 12%
			IF THISPER% + I% >= PCLOSED%
			THEN
				SUBPOSTED = FUNC_ROUND(SUBPOSTED + &
					IC_35HISTORY::PQUANTITY(I%), 4%)
			ELSE
				SUBTOTAL = FUNC_ROUND(SUBTOTAL + &
					IC_35HISTORY::PQUANTITY(I%), 4%)
			END IF
		NEXT I%

		GOTO 910 IF SUBTOTAL = 0.0 AND SUBPOSTED = 0.0

		WHEN ERROR IN
			GET #WORK_FILE.CH%, &
				KEY #0% EQ IC_35HISTORY::PRODUCT + &
				IC_35HISTORY::LOCATION + &
				IC_35HISTORY::TRANSTYPE

		USE
			CONTINUE 950
		END WHEN

		!
		! Add to existing subtotal
		!
		WORK_FILE::BALANCE = FUNC_ROUND(WORK_FILE::BALANCE + &
			SUBTOTAL, 4%)
		WORK_FILE::POSTED = FUNC_ROUND(WORK_FILE::POSTED + &
			SUBPOSTED, 4%)

		UPDATE #WORK_FILE.CH%

		GOTO 910

950		!
		! Create new subtotal
		!
		WORK_FILE::PRODUCT = IC_35HISTORY::PRODUCT
		WORK_FILE::LOCATION = IC_35HISTORY::LOCATION
		WORK_FILE::TRANSTYPE = IC_35HISTORY::TRANSTYPE
		WORK_FILE::BALANCE = SUBTOTAL
		WORK_FILE::POSTED = SUBPOSTED

		PUT #WORK_FILE.CH%

		GOTO 910

980		CLOSE IC_35HISTORY.CH%

990	NEXT LOOP%

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "IC BALANCE AUDIT REPORT"
	TITLE$(2%) = "FOR  LOCATION  " + LOCATION$ + &
		" FOR " + NUM1$(IC_35BALANCE_FILE%) + " PERIODS"
	TITLE$(3%) = "Inventory Control System"
	TITLE$(4%) = ""

	!
	! Heading
	!
	TITLE$(5%) = "Product#       Loca  Trans       Beginning     Posted"

	TITLE$(6%) = "."

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		RESET #WORK_FILE.CH%
	USE
		CONTINUE ExitTotal
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
		GET #WORK_FILE.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "WORK_FILE"
		CONTINUE HelpError
	END WHEN

	!
	! Print out one line
	!
	TEXT1$ = WORK_FILE::PRODUCT + " " + &
		WORK_FILE::LOCATION + " " + &
		WORK_FILE::TRANSTYPE + " " + &
		FORMAT$(WORK_FILE::BALANCE, "###,###,###.## ") + &
		FORMAT$(WORK_FILE::POSTED, "###,###,###.##")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT1$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Try for next record
	!
	GOTO GetNextRec

 ExitTotal:

 ExitProgram:
	CLOSE WORK_FILE.CH%

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
