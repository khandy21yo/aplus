1	%TITLE "ABC Stock Value Report"
	%SBTTL "IC_RPRT_ABC"
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
	! ID:IC001
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*ABC Stock Value Report\* contains the inventory report based on the
	!	ABC flags. The A flag consists of the largest single unit dollar amount,
	!	the B flag the middle single dollar amount, and C the lowest single dollar
	!	amount. This report is used in cycle counting to indicate how often the item
	!	must be counted. This report contains the following fields:
	!	.table 3,25
	!	.te
	!	ABC Type
	!	.te
	!	Store
	!	.te
	!	Product Number
	!	.te
	!	Description
	!	.te
	!	Location
	!	.te
	!	Count time indications
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x ABC>Report
	!	.x Report>ABC
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_RPRT_ABC/LINE
	!	$ LINK/EXE=IC_EXE: IC_RPRT_ABC, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE IC_RPRT_ABC.OBJ;*
	!
	! Author:
	!
	!	07/29/88 - Frank Starman
	!
	! Modification History:
	!
	!	01/16/92 - Dan Perkins
	!		Changed quantities to display integer values.
	!
	!	04/24/92 - Kevin Handy
	!		Clean up (check)
	!
	!	04/01/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	01/26/96 - Kevin Handy
	!		Reformat source code.
	!		Change STRING$(...,ASCII(" ")) to "" in several
	!		places.
	!
	!	10/18/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	07/30/97 - Kevin Handy
	!		Change XAGE parameter to READ_PERIOD to integer.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	06/20/2000 - Kevin Handy
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
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	%INCLUDE "SOURCE:[IC.OPEN]IC_CONTROL.HB"
	MAP	(IC_CONTROL)	IC_CONTROL_CDD		IC_CONTROL

	%INCLUDE "SOURCE:[IC.OPEN]IC_BINMAP.HB"
	MAP	(IC_BINMAP)	IC_BINMAP_CDD		IC_BINMAP

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP	(PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP	(UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	MAP (IC_ABC) &
		DECIMAL(11%, 3%)	IC_ABC.EXTCOST, &
		STRING		IC_ABC.ABC = 1%, &
		GFLOAT		IC_ABC.BEGBAL, &
		GFLOAT		IC_ABC.ENDBAL, &
		RFA		IC_ABC.PRODRFA

	!
	! External functions
	!
	EXTERNAL REAL    FUNCTION PC_READ_COST
	EXTERNAL LONG    FUNCTION IC_READ_35BALANCE

	%PAGE

	ON ERROR GOTO 19000

 Init:	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	LOCATION$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	^*(05) Locations\*
	!	.b
	!	.lm +5
	!	The ^*Locations\* field enters the location
	!	codes (which have been established in the Utility system) that are
	!	to be printed.
	!	.lm -5
	!
	! Index:
	!	.x Locations>ABC Stock Value Report
	!	.x ABC Stock Value Report>Locations
	!
	!--

	SORT_KEY% = 0%

	CALL ASSG_CHANNEL(IC_ABC.CH%, STAT%)
	CALL ASSG_CHANNEL(IC_ABCABC.CH%, STAT%)
	CALL READ_DEVICE("UTL_WORK", UTL_WORK.DEV$, STAT%)

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_CONTROL.OPN"
		GET #IC_CONTROL.CH%, RECORD 1%, REGARDLESS
		CLOSE IC_CONTROL.CH%
	USE
		FILENAME$ = "IC_CONTROL"
		CONTINUE HelpError
	END WHEN

	V% = READ_PERIOD("READ", IC_CONTROL::ERA, IC_CONTROL::PERIOD, &
		PERIOD_DESCR$, STAT$, START_DATE$, END_DATE$, 0%)

	YYYYPP$ = IC_CONTROL::PERIOD

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"
	USE
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

320	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.OPN"
	USE
		FILENAME$ = "UTL_LOCATION"
		CONTINUE HelpError
	END WHEN

330	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_BINMAP.OPN"
	USE
		FILENAME$ = "IC_BINMAP"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	TITLE$(1%) = "ABC  STOCK  VALUE  "
	TITLE$(3%) = "Inventory Control System"
	TITLE$(4%) = ""

	TITLE$(5%) = "Product#       Description              " + &
		"       UOM Tp Cat  Mthd         BegBal     " + &
		"    EndBal          Cost ABC  PercBal"

	TITLE$(6%) = "."

	LYT_LINE$ = "$Product#:015,$Description:047,$UOM:051,$Tp:054," + &
		"$Cat:059,$Mthd:072,VBegBal:087,VEndBal:103," + &
		"VCost:108,$ABC:113,VPercBal:131"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		RESET #UTL_LOCATION.CH%
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "UTL_LOCATION"
		CONTINUE HelpError
	END WHEN

 NextLocation:
	WHEN ERROR IN
		GET #UTL_LOCATION.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "UTL_LOCATION"
		CONTINUE HelpError
	END WHEN

	GOTO NextLocation IF LOCATION$ <> "" AND &
		COMP_STRING(EDIT$(UTL_LOCATION::LOCATION, -1%), LOCATION$) = 0%

	TITLE$(2%) = IN_TITLE$ + "  IN  " + YYYYPP$ + "  " + &
		TRM$(PERIOD_DESCR$) + &
		"  AT LOCATION " + UTL_LOCATION::LOCATION + " " + &
		UTL_LOCATION::LOCNAME

17003	RESET #PD_PRODUCT.CH%, KEY #SORT_KEY%

17005	CALL ENTR_3MESSAGE(SCOPE, "Creating temporary file for location " + &
		UTL_LOCATION::LOCATION, 1%)

	CLOSE #IC_ABCABC.CH%

	WHEN ERROR IN
		OPEN UTL_WORK.DEV$ + "IC_TEMP.TMP" FOR OUTPUT AS FILE IC_ABCABC.CH%, &
			ORGANIZATION INDEXED FIXED, &
			MAP IC_ABC, &
			PRIMARY KEY (IC_ABC.EXTCOST) DUPLICATES, &
			TEMPORARY, &
			BUFFER 32%, &
			ACCESS MODIFY, ALLOW MODIFY
	USE
		FILENAME$ = "IC_ABC.TMP"
		CONTINUE HelpError
	END WHEN

17010	WHEN ERROR IN
		GET #PD_PRODUCT.CH%, REGARDLESS
	USE
		CONTINUE EndTemp IF ERR = 11%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

	!
	! Check for a inactive item
	!
	GOTO 17010 IF PD_PRODUCT::SSTATUS <> "A"

	GOTO 17010 &
		IF 1% AND IC_READ_35BALANCE(PD_PRODUCT::PRODUCT_NUM, &
			UTL_LOCATION::LOCATION, &
		BALANCE(,)) = 0%

	BEGBAL = BALANCE(1%, 1%)
	ENDBAL = BEGBAL + BALANCE(1%, 2%)

	GOTO 17010 IF ENDBAL = 0.0

	IF PD_PRODUCT::METHOD = "STD"
	THEN
		COST = PC_READ_COST(PD_PRODUCT::PRODUCT_NUM, &
			UTL_LOCATION::LOCATION, END_DATE$, "")
	ELSE
		COST = 0.0
	END IF

	EXTCOST = FUNC_ROUND(COST * ENDBAL, 2%)
	RUN_AMOUNT = RUN_AMOUNT + EXTCOST

17013	IC_BINMAP::ABC = ""

	WHEN ERROR IN
		FIND #IC_BINMAP.CH%, &
			KEY #0% EQ PD_PRODUCT::PRODUCT_NUM + &
			UTL_LOCATION::LOCATION, &
			REGARDLESS

		GET #IC_BINMAP.CH%, REGARDLESS
	USE
		CONTINUE AddTemp IF ERR = 155%
		FILENAME$ = "IC_BINMAP"
		CONTINUE HelpError
	END WHEN

 AddTemp:
	IC_ABC.EXTCOST = -EXTCOST
	IC_ABC.ABC = IC_BINMAP::ABC
	IC_ABC.BEGBAL = BEGBAL
	IC_ABC.ENDBAL = ENDBAL
	IC_ABC.PRODRFA = GETRFA(PD_PRODUCT.CH%)

	WHEN ERROR IN
		PUT #IC_ABCABC.CH%
	USE
		FILENAME$ = "IC_BINMAP"
		CONTINUE HelpError
	END WHEN

	GOTO 17010

 EndTemp:
	CLOSE #IC_ABC.CH%

	OPEN UTL_WORK.DEV$ + "IC_TEMP2.TMP" FOR OUTPUT AS FILE IC_ABC.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP IC_ABC, &
		PRIMARY KEY (IC_ABC.ABC) DUPLICATES, &
		TEMPORARY, &
		BUFFER 32%, &
		ACCESS MODIFY, ALLOW MODIFY

	RESET #IC_ABCABC.CH%

 NextTemp:
17014	WHEN ERROR IN
		GET #IC_ABCABC.CH%, REGARDLESS
		PUT #IC_ABC.CH%
	USE
		CONTINUE 17015 IF ERR = 11%
		FILENAME$ = "IC_TEMP"
		CONTINUE HelpError
	END WHEN

	GOTO NextTemp

17015	RESET #IC_ABC.CH%

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #IC_ABC.CH%, REGARDLESS
	USE
		CONTINUE NextLocation IF ERR = 11%
		FILENAME$ = "IC_ABC"
		CONTINUE HelpError
	END WHEN

17030	WHEN ERROR IN
		GET #PD_PRODUCT.CH%, RFA IC_ABC.PRODRFA, REGARDLESS
	USE
		CONTINUE NextLocation IF ERR = 11%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

17300	!
	! Print out one line
	!
	PERTOTAL   = 0.0
	PERTOTAL   = -IC_ABC.EXTCOST / RUN_AMOUNT * 100% &
		IF RUN_AMOUNT <> 0.0

	SPAC$ = PD_PRODUCT::UOM + "  " + &
		PD_PRODUCT::PROD_TYPE + " " + &
		PD_PRODUCT::CATEGORY + " " + &
		PD_PRODUCT::METHOD + " " + &
		FORMAT$(IC_ABC.BEGBAL, "##,###,###,###") + " " + &
		FORMAT$(IC_ABC.ENDBAL, "##,###,###,###") + " "

	TEXT_LIN$ = PD_PRODUCT::PRODUCT_NUM + " " + &
		LEFT(PD_PRODUCT::DESCRIPTION, 31%) + " " + &
		SPAC$ + &
		FORMAT$(-IC_ABC.EXTCOST, "##,###,###.##") + " " + &
		IC_ABC.ABC + "   " + &
		FORMAT$(PERTOTAL, "####.##%")

	LIN% = 0%

	IF LAST_LOCATION$ <> UTL_LOCATION::LOCATION
	THEN
		IF UTL_REPORTX::PAGENO
		THEN
			GOSUB 18100
			LIN% = 999%
		END IF
		GOSUB 18000
		LAST_LOCATION$ = UTL_LOCATION::LOCATION
	ELSE
		GOSUB 18100 IF TEST_ABC$ <> IC_ABC.ABC
	END IF

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT_LIN$, LIN%)

	TOTAL_COST = TOTAL_COST - IC_ABC.EXTCOST
	TOTAL_ABC = TOTAL_ABC  - IC_ABC.EXTCOST

	TEST_ABC$ = IC_ABC.ABC

17350	!
	! Try for next record
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT
	GOTO GetNextRec

 ExitTotal:
17400	!
	! Handle end of report
	!
	GOSUB 18100
	GOSUB 18000

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

18000	IF UTL_REPORTX::PAGENO
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
		!
		! Print total cost
		!
		TEXT$ = SPACE$(LEN(PD_PRODUCT::PRODUCT_NUM)) + " " + &
			"T O T A L" + SPACE$(23% + LEN(SPAC$)) + &
			FORMAT$(TOTAL_COST, "##,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	END IF

	RETURN

18100	IF UTL_REPORTX::PAGENO
	THEN
		!
		! Print total cost
		!
		TEXT$ = SPACE$(LEN(PD_PRODUCT::PRODUCT_NUM)) + " " + &
			"TOTAL    " + SPACE$(23% + LEN(SPAC$)) + &
			FORMAT$(TOTAL_ABC, "##,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	END IF

	TOTAL_ABC = 0.0

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
