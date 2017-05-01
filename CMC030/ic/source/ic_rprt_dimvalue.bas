1	%TITLE "Diminishing Stock Value Report"
	%SBTTL "IC_RPRT_DIMVALUE"
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
	! ID:IC008
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Diminishing Stock Value Report\* consists of the stock value report
	!	in a value order. The order of the stock proceeds from the largest total
	!	dollar amount to the smallest total dollar amount. Included in this report
	!	are the following fields:
	!	.table 3,25
	!	.te
	!	Product Name	Description
	!	.te
	!	Unit of Measure	Type
	!	.te
	!	Category	Method
	!	.te
	!	Ending Balance	Cost
	!	.te
	!	ABC	Percent Balance
	!	.te
	!	Percent Total
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Diminishing Stock Value>Report
	!	.x Report>Diminishing Stock Value
	!
	! Option:
	!
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_RPRT_DIMVALUE/LINE
	!	$ LINK/EXE=IC_EXE: IC_RPRT_DIMVALUE, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE IC_RPRT_DIMVALUE.OBJ;*
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
	!	11/30/94 - Kevin Handy
	!		Fixed bug where total was not zeroed between
	!		locations.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	12/05/95 - Kevin Handy
	!		Reformat closer to 80 columns.
	!
	!	01/26/96 - Kevin Handy
	!		Reformat source code.
	!		Change STRING$(...,ASCII(" ")) to "" in
	!		several places.
	!
	!	05/09/96 - Kevin Handy
	!		Modified to not print out 0 quanities.
	!		Remove some commented out code.
	!
	!	10/19/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	07/30/97 - Kevin Handy
	!		Change XAGE parameter of READ_PERIOD to integer.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/18/2000 - Kevin Handy
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
	MAP (IC_CONTROL)	IC_CONTROL_CDD		IC_CONTROL

	%INCLUDE "SOURCE:[IC.OPEN]IC_BINMAP.HB"
	MAP (IC_BINMAP)		IC_BINMAP_CDD		IC_BINMAP

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	MAP (IC_DIMVALUE) &
		DECIMAL(11,3)	IC_DIMVALUE.EXTCOST, &
		GFLOAT		IC_DIMVALUE.BEGBAL, &
		GFLOAT		IC_DIMVALUE.ENDBAL, &
		RFA		IC_DIMVALUE.PRODRFA

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
	!	.x Locations>Diminishing Stock Value Report
	!	.x Diminishing Stock Value Report>Locations
	!
	!--

	SORT_KEY% = 0%

	CALL ASSG_CHANNEL(IC_TEMP.CH%, STAT%)
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
		CONTINUE ReportTitle IF ERR = 5%
		FILENAME$ = "IC_BINMAP"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	TITLE$(1%) = "INVENTORY  DIMINISHING  VALUE  "
	TITLE$(3%) = "Inventory Control System"
	TITLE$(4%) = ""

	TITLE$(5%) = "Product#       Description              " + &
		"       UOM Tp Cat  Mthd         BegBal     " + &
		"    EndBal          Cost ABC  PercBal  PercTot"

	TITLE$(6%) = "."

	LYT_LINE$ = "$Product#:015,$Description:047,$UOM:051,$Tp:054," + &
		"$Cat:059,$Mthd:072,VBegBal:087,VEndBal:103," + &
		"VCost:108,$ABC:113,VPercBal:122,VPercTot:132"

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

	TITLE$(2%) = IN_TITLE$ + "  IN  " + YYYYPP$ + "  " + TRM$(PERIOD_DESCR$) + &
		"  AT LOCATION " + UTL_LOCATION::LOCATION + " " + &
		UTL_LOCATION::LOCNAME

17003	RESET #PD_PRODUCT.CH%, KEY #SORT_KEY%

17005	CALL ENTR_3MESSAGE(SCOPE, "Creating temporary file for location " + &
		UTL_LOCATION::LOCATION, 1%)

	CLOSE #IC_TEMP.CH%

	WHEN ERROR IN
		OPEN UTL_WORK.DEV$ + "IC_TEMP.TMP" FOR OUTPUT AS FILE IC_TEMP.CH%, &
			ORGANIZATION INDEXED FIXED, &
			MAP IC_DIMVALUE, &
			PRIMARY KEY (IC_DIMVALUE.EXTCOST) DUPLICATES, &
			TEMPORARY, &
			BUFFER 32%, &
			ACCESS MODIFY, ALLOW MODIFY
	USE
		FILENAME$ = "IC_DIMVALUE"
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
	! Check to see if inactive item
	!
	GOTO 17010 IF PD_PRODUCT::SSTATUS <> "A"

	GOTO 17010 IF 1% AND IC_READ_35BALANCE(PD_PRODUCT::PRODUCT_NUM, &
		UTL_LOCATION::LOCATION, BALANCE(,)) = 0%

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

	EXTCOST = FUNC_ROUND( COST * ENDBAL, 2%)
	RUN_AMOUNT = RUN_AMOUNT + EXTCOST

	IC_DIMVALUE.EXTCOST = -EXTCOST
	IC_DIMVALUE.BEGBAL = BEGBAL
	IC_DIMVALUE.ENDBAL = ENDBAL
	IC_DIMVALUE.PRODRFA = GETRFA(PD_PRODUCT.CH%)

	PUT #IC_TEMP.CH%

	GOTO 17010

 EndTemp:
17015	RESET #IC_TEMP.CH%

	PERBALENCE = 100.0

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #IC_TEMP.CH%, REGARDLESS
	USE
		CONTINUE NextLocation IF ERR = 11%
		FILENAME$ = "IC_TEMP"
		CONTINUE HelpError
	END WHEN

17030	WHEN ERROR IN
		GET #PD_PRODUCT.CH%, RFA IC_DIMVALUE.PRODRFA, REGARDLESS
	USE
		CONTINUE NextLocation IF ERR = 11%
		FILENAME$ = "PD_PRODUCE"
		CONTINUE HelpError
	END WHEN

17200	IC_BINMAP::ABC = ""

	WHEN ERROR IN
		FIND #IC_BINMAP.CH%, &
			KEY #0% EQ PD_PRODUCT::PRODUCT_NUM + &
			UTL_LOCATION::LOCATION, &
			REGARDLESS

		GET #IC_BINMAP.CH%, REGARDLESS
	USE
		CONTINUE 17300 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "BINMAP"
		CONTINUE HelpError
	END WHEN

17300	!
	! Print out one line
	!
	PERTOTAL   = 0.0
	PERTOTAL   = -IC_DIMVALUE.EXTCOST / RUN_AMOUNT * 100% &
		IF RUN_AMOUNT <> 0.0

	PERBALANCE = PERBALANCE - PERTOTAL

	!
	! Don't print if nothing in inventory
	!
	IF (IC_DIMVALUE.ENDBAL <> 0.0)
	THEN
		SPAC$ = PD_PRODUCT::UOM + "  " + &
			PD_PRODUCT::PROD_TYPE + " "  + &
			PD_PRODUCT::CATEGORY + " "  + &
			PD_PRODUCT::METHOD + " "  + &
			FORMAT$(IC_DIMVALUE.BEGBAL, "##,###,###,###") + " " + &
			FORMAT$(IC_DIMVALUE.ENDBAL, "##,###,###,###") + " "

		TEXT_LIN$ = PD_PRODUCT::PRODUCT_NUM + " " + &
			LEFT(PD_PRODUCT::DESCRIPTION, 31%) + " " + &
			SPAC$ + &
			FORMAT$(-IC_DIMVALUE.EXTCOST, "##,###,###.##") + " " + &
			IC_BINMAP::ABC + "   " + &
			FORMAT$(PERTOTAL, "####.##%") + " " + &
			FORMAT$(-PERBALANCE, "####.##%")

		LIN% = 0%

		IF LAST_LOCATION$ <> UTL_LOCATION::LOCATION
		THEN
			LIN% = 999% IF UTL_REPORTX::PAGENO
			GOSUB 18000
			LAST_LOCATION$ = UTL_LOCATION::LOCATION
		END IF

		CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), &
			TEXT_LIN$, LIN%)

		TOTAL_COST = TOTAL_COST - IC_DIMVALUE.EXTCOST
	END IF

17350	!
	! Try for next record
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT
	GOTO GetNextRec

 ExitTotal:
17400	!
	! Handle end of report
	!
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
		!
		! Print total cost
		!
		TEXT$ = SPACE$(LEN(PD_PRODUCT::PRODUCT_NUM)) + " " + &
			"T O T A L" + SPACE$(23% + LEN(SPAC$)) + &
			FORMAT$(TOTAL_COST, "##,###,###.##")

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	END IF

	TOTAL_COST = 0.0

	RETURN

	%Page

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
