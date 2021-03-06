1	%TITLE "Set ABC flag based on $ Stock"
	%SBTTL "IC_SPEC_SETABC"
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
	! ID:IC008
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This program will set the ABC flags on all inventory
	!	products for selected locations based on the dollar
	!	value (cost) in inventory.
	!	.b
	!	The breakdown between A/B/C is determined by the
	!	amounts entered in the two break fields.
	!	Anything with a dollar value greater than the first
	!	break will be set to "A". Anything between the two breaks
	!	will be set to "B", and anything less than the second
	!	break will be set to "C".
	!	.lm -5
	!
	! Index:
	!	.x ABC>Set
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_SPEC_SETABC/LINE
	!	$ LINK/EXE=IC_EXE: IC_SPEC_SETABC, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE IC_SPEC_SETABC.OBJ;*
	!
	! Author:
	!
	!	11/28/94 - Kevin Handy
	!		Created from IC_SPEC_DIMVALUE
	!
	! Modification History:
	!
	!	11/30/94 - Kevin Handy
	!		Modified to ignore ABC types "X", and to blank
	!		the cycle count map.
	!
	!	01/18/95 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	09/05/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/12/97 - Kevin Handy
	!		Lose unused variables IC_TEMP.CH% and UTL_WORK.DEV$
	!
	!	07/30/97 - Kevin Handy
	!		Change XAGE parameter of READ_PERIOD to integer
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/01/2000 - Kevin Handy
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
		DECIMAL(11, 3)	IC_DIMVALUE.EXTCOST, &
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


	!++
	! Abstract:FLD01
	!	^*(01) A More Than\*
	!	.b
	!	.lm +5
	!	Any inventory amount greater than this amount
	!	will be classified as "A".
	!	.lm -5
	!
	! Index:
	!
	!--
	BREAK_1 = VAL(TRM$(UTL_REPORTX::OPTDEF(0%)))

	!++
	! Abstract:FLD02
	!	^*(02) B More Than\*
	!	.b
	!	.lm +5
	!	Any inventory amount greater than this amount
	!	but less than the "A More Than" field
	!	will be classified as "B".
	!	.b
	!	Anything less than this amount will be clssified as
	!	"C".
	!	.b
	!	This field should be less than the "A Less Than" field.
	!	.lm -5
	!
	! Index:
	!
	!--
	BREAK_2 = VAL(TRM$(UTL_REPORTX::OPTDEF(1%)))

	!++
	! Abstract:FLD03
	!	^*(03) Only set blank\*
	!	.b
	!	.lm +5
	!	A Yes here will cause only the undefined ABC codes
	!	to be set, otherwise it will adjust all products.
	!	.lm -5
	!
	! Index:
	!
	!--
	BLANK_ONLY$ = LEFT(UTL_REPORTX::OPTDEF(2%), 1%)

	!++
	! Abstract:FLD05
	!	^*(05) Locations\*
	!	.b
	!	.lm +5
	!	The ^*Locations\* field enters the location
	!	codes (which have been established in the Utility system) that are
	!	to be reset.
	!	.lm -5
	!
	! Index:
	!	.x Locations>Set ABC
	!	.x Set ABC>Locations
	!
	!--
	LOCATION$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

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
		%INCLUDE "SOURCE:[IC.OPEN]IC_BINMAP.MOD"
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

	TITLE$(2%) = IN_TITLE$ + "  IN  " + YYYYPP$ + "  " + &
		TRM$(PERIOD_DESCR$) + &
		"  AT LOCATION " + UTL_LOCATION::LOCATION + " " + &
		UTL_LOCATION::LOCNAME

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), "", LIN%)

	TEXT_LIN$ = "Location: " + UTL_LOCATION::LOCATION

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT_LIN$, LIN%)

17003	RESET #PD_PRODUCT.CH%

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

	GOTO 17010 IF IC_READ_35BALANCE(PD_PRODUCT::PRODUCT_NUM, &
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

	EXTCOST = FUNC_ROUND(COST * ENDBAL, 2%)
	RUN_AMOUNT = RUN_AMOUNT + EXTCOST

	IC_DIMVALUE.EXTCOST = EXTCOST
	IC_DIMVALUE.BEGBAL = BEGBAL
	IC_DIMVALUE.ENDBAL = ENDBAL
	IC_DIMVALUE.PRODRFA = GETRFA(PD_PRODUCT.CH%)

17020	GETFLAG% = 0%

	WHEN ERROR IN
		GET #IC_BINMAP.CH%, &
			KEY #0% EQ PD_PRODUCT::PRODUCT_NUM + &
			UTL_LOCATION::LOCATION
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE HelpError
	END WHEN

	GETFLAG% = -1%

17030	!************************************************************

	!
	! Calculate what new ABC flag should be
	!
	IF IC_DIMVALUE.EXTCOST >= BREAK_1
	THEN
		NEW_ABC$ = "A"
	ELSE
		IF IC_DIMVALUE.EXTCOST >= BREAK_2
		THEN
			NEW_ABC$ = "B"
		ELSE
			NEW_ABC$ = "C"
		END IF
	END IF

	IF (GETFLAG% = 0%)
	THEN
		IC_BINMAP::PRODUCT = PD_PRODUCT::PRODUCT_NUM
		IC_BINMAP::LOCATION = UTL_LOCATION::LOCATION
		IC_BINMAP::BIN(I%) = "" FOR I% = 0% TO 3%
		IC_BINMAP::SAFETY = 0.0
		IC_BINMAP::MAXLEVEL = 0.0
		IC_BINMAP::ABC = NEW_ABC$
		IC_BINMAP::CYCLEMAP = STRING$(8%, 0%)

		PUT #IC_BINMAP.CH%
	ELSE
		GOTO 17010 IF (BLANK_ONLY$ = "Y") AND (IC_BINMAP::ABC <> " ")
		GOTO 17010 IF IC_BINMAP::ABC = "X"

		IF (IC_BINMAP::ABC <> NEW_ABC$)
		THEN
			IC_BINMAP::ABC = NEW_ABC$
			IC_BINMAP::CYCLEMAP = STRING$(8%, 0%)

			UPDATE #IC_BINMAP.CH%
		ELSE
			GOTO 17010
		END IF
	END IF

	PERTOTAL = 0.
	PERTOTAL = IC_DIMVALUE.EXTCOST / RUN_AMOUNT * 100% &
		IF RUN_AMOUNT <> 0.0

	PERBALANCE = PERBALANCE - PERTOTAL

	SPAC$ = PD_PRODUCT::UOM + "  " + &
		PD_PRODUCT::PROD_TYPE + " "  + &
		PD_PRODUCT::CATEGORY + " "  + &
		PD_PRODUCT::METHOD + " "  + &
		FORMAT$(IC_DIMVALUE.BEGBAL, "##,###,###,###") + " "  + &
		FORMAT$(IC_DIMVALUE.ENDBAL, "##,###,###,###") + " "

	TEXT_LIN$ = PD_PRODUCT::PRODUCT_NUM + " " + &
		LEFT(PD_PRODUCT::DESCRIPTION, 31%) + " " + &
		SPAC$ + &
		FORMAT$(IC_DIMVALUE.EXTCOST, "##,###,###.##") + " " + &
		IC_BINMAP::ABC + "   " + &
		FORMAT$(PERTOTAL, "####.##%") + " " + &
		FORMAT$(-PERBALANCE, "####.##%")

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT_LIN$, LIN%)

	!************************************************************

	GOTO 17010

 EndTemp:
	GOTO NextLocation

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
