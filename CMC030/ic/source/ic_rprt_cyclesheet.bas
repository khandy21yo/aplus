1	%TITLE "Cycle Count Worksheet"
	%SBTTL "IC_RPRT_CYCLESHEET"
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
	! ID:IC006
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	Accessing the ^*Cycle Count Worksheet\*
	!	prints a report to be used as a turnaround document to cycle
	!	count inventory items. The report contains the following
	!	categories:
	!	.table 3,25
	!	.te
	!	Product _#	Bin Location
	!	.te
	!	Description	Unit Of Measure
	!	.te
	!	Type	Category
	!	.te
	!	Pack	Form
	!	.te
	!	Unit Of Measure	Pack Form
	!	.te
	!	Product Form	Standard Cost
	!	.te
	!	Physical Count	Pack Unit
	!	.te
	!	Extend
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Cycle Count Worksheet>Report
	!	.x Cycle Count Worksheet>Report
	!	.x Report>Cycle Count Worksheet
	!	.x Report>Cycle Count Worksheet
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_RPRT_CYCLESHEET/LINE
	!	$ LINK/EXE=IC_EXE: IC_RPRT_CYCLESHEET, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE IC_RPRT_CYCLESHEET.OBJ;*
	!
	!
	! Author:
	!
	!	08/27/87 - Frank F. Starman
	!
	! Modification History:
	!
	!	02/26/92 - Kevin Handy
	!		Removed references to "PD_PACK" file, which Frank
	!		removed so programs couldn't compile.
	!
	!	01/20/93 - Frank F. Starman
	!		Speed up program.
	!
	!	04/01/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	01/31/95 - Kevin Handy
	!		Double Space worksheet.
	!
	!	03/29/95 - Kevin Handy
	!		(V3.6) Calico
	!		Fixed bug where wasn't calling OUTP_FINISH
	!		to close down report.
	!
	!	09/05/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/28/97 - Kevin Handy
	!		Use integer for #key
	!
	!	10/03/97 - Kevin Handy
	!		Added BARCODE option
	!
	!	10/13/97 - Kevin Handy
	!		Modifications to be able to handle alternate
	!		printer types in the Code39 function.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/31/2000 - Kevin Handy
	!		Use A"x"B
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

	%INCLUDE "SOURCE:[IC.OPEN]IC_BINMAP.HB"
	MAP (IC_BINMAP)		IC_BINMAP_CDD		IC_BINMAP

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	%INCLUDE "FUNC_INCLUDE:PRINT35.INC"
	MAP (PRINTX) PRINTX_CDD PRINTX

	MAP (IC_CYCLESHEET) &
		STRING IC_CYCLESHEET.BIN = 6%, &
		RFA    IC_CYCLESHEET.STORERFA

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION READ_BIT
	EXTERNAL REAL   FUNCTION PC_READ_COST
	EXTERNAL STRING FUNCTION OUTP_CODE39
	EXTERNAL LONG   FUNCTION FIND_3PRINTGROUPITEM

	%PAGE

	ON ERROR GOTO 19000

 Init:	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Bin _#\*
	!	.b
	!	.lm +5
	!	The ^*From Bin _#\* setting enters a
	!	bin _# with which the report will begin printing.
	!	.b
	!	A blank setting will cause the report to begin with the
	!	first bin _# in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Bin Number>Cycle Count Worksheet
	!	.x Cycle Count Worksheet>From Bin Number
	!	.x Bin Number>From
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Bin _#\*
	!	.b
	!	.lm +5
	!	The ^*To Bin _#\* field enters a bin _#
	!	with which the report will end printing.
	!	.b
	!	A blank field will cause the report to end with the last bin _#
	!	in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Bin Number>Cycle Count Worksheet
	!	.x Cycle Count Worksheet>To Bin Number
	!	.x Bin Number>To
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects designated bin
	!	_#'s to be printed by entering a "wildcard" value in this
	!	field.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard>Cycle Count Worksheet
	!	.x Cycle Count Worksheet>Wildcard
	!
	!--

	LOCATION$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	^*(05) Locations\*
	!	.b
	!	.lm +5
	!	The ^*Locations\* field enters the locations
	!	codes (which have been established in the Utility system) that are
	!	to be printed.
	!	.b
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Locations>Cycle Count Worksheet
	!	.x Cycle Count Worksheet>Locations
	!
	!--

	TO_WEEK% = VAL%(UTL_REPORTX::OPTDEF(5%))

	!++
	! Abstract:FLD06
	!	^*(06) Week Number\*
	!	.b
	!	.lm +5
	!	The ^*Week Number\* field enters the number which will print
	!	a report of the items counted for this week. The flags for the count weeks
	!	are set in the cycle journal. This field is most effective if all inventory
	!	is not counted in the same week, otherwise all reports will be printed.
	!	.lm -5
	!
	! Index:
	!	.x Week Number
	!
	!--

	SHOW_STD$ = LEFT(UTL_REPORTX::OPTDEF(6%), 1%)

	!++
	! Abstract:FLD07
	!	^*(07) Show STD\*
	!	.b
	!	.lm +5
	!	This field determines if the standard cost will be printed
	!	on the worksheet.
	!	.lm -5
	!
	! Index:
	!	.x Week Number
	!
	!--

	BARCODE$ = LEFT(UTL_REPORTX::OPTDEF(7%), 1%)

	!++
	! Abstract:FLD08
	!	^*(08) Print Barcode\*
	!	.b
	!	.lm +5
	!	Should a barcode be printed?
	!	.lm -5
	!
	! Index:
	!	.x Week Number
	!
	!--

	HEADER_LINE$ = "COUNT ON WEEK NUMBER " + NUM1$(TO_WEEK%)
	HEADER_LINE$ = "COUNT OVER ALL WEEKS " IF TO_WEEK% = 0%

	!
	! Get type of printer
	!
	IF PRINTX::ITEMS == 0%
	THEN
		CALL OUTP_INITIALIZE(UTL_REPORTX::PRINTTYPE)
	END IF

	LOOP% = FIND_3PRINTGROUPITEM("BC", "*", PRINTX)
	IF LOOP% > 0%
	THEN
		BARPRINT% = VAL%(PRINTX::SEQU(LOOP%))
	ELSE
		BARPRINT% = 0%
	END IF

	CALL ASSG_CHANNEL(IC_TEMP.CH%, STAT%)
	CALL READ_DEVICE("UTL_WORK", UTL_WORK.DEV$, STAT%)

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_BINMAP.OPN"
	USE
		FILENAME$ = "IC_BINMAP"
		CONTINUE HelpError
	END WHEN

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.OPN"
	USE
		FILENAME$ = "UTL_LOCATION"
		CONTINUE HelpError
	END WHEN

320	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"
	USE
		CONTINUE 330 IF ERR = 5%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

330	!

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "CYCLE  COUNT  WORKSHEET  " + HEADER_LINE$
	TITLE$(3%) = "Inventory Control System"
	TITLE$(4%) = ""

	!
	! Heading
	!
	IF (SHOW_STD$ = "Y")
	THEN
		TITLE$(5%) = "Product#       Description                 " + &
			"              Std Cost Ty Cat  Bin    UOM  " + &
			"PhysCount"

		IF BARCODE$ <> "Y"
		THEN
			TITLE$(5%) = TITLE$(5%) + " PackUnt Extend"
		END IF
	ELSE
		TITLE$(5%) = "Product#       Description                 " + &
			"             Ty Cat  Bin    UOM  " + &
			"PhysCount"

		IF BARCODE$ <> "Y"
		THEN
			TITLE$(5%) = TITLE$(5%) + " PackUnt Extend"
		END IF
	END IF
	TITLE$(6%) = "."

	LYT_LINE$ = "$Product#:015,$Description:053,$UOM:057,$Ty:060," + &
		"$Cat:065,$Pack:070,$Form:075,$UOM:082," + &
		"VPackF:089,VProdF:097,VStdCost:105," + &
		"$PhysCount:115,$PackUnt:123,$Extend:132"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		RESET #UTL_LOCATION.CH%
	USE
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

	TITLE$(2%) = "  AT LOCATION " + TRM$(UTL_LOCATION::LOCATION) + " " + &
		TRM$(UTL_LOCATION::LOCNAME)

17003	WHEN ERROR IN
		FIND #IC_BINMAP.CH%, &
			KEY #1% EQ UTL_LOCATION::LOCATION, &
			REGARDLESS
	USE
		CONTINUE NextLocation IF ERR = 155%
		FILENAME$ = "IC_BINMAP"
		CONTINUE HelpError
	END WHEN

17005	CALL ENTR_3MESSAGE(SCOPE, "Creating temporary file for location " + &
		UTL_LOCATION::LOCATION, 1% + 16%)

	CLOSE #IC_TEMP.CH%

	WHEN ERROR IN
		OPEN UTL_WORK.DEV$ + "IC_TEMP.TMP" FOR OUTPUT AS FILE IC_TEMP.CH%, &
			ORGANIZATION INDEXED FIXED, &
			MAP IC_CYCLESHEET, &
			PRIMARY KEY (IC_CYCLESHEET.BIN) DUPLICATES, &
			TEMPORARY, &
			BUFFER 32%, &
			ACCESS MODIFY, ALLOW NONE
	USE
		FILENAME$ = "IC_CYCLESHEET.TMP"
		CONTINUE HelpError
	END WHEN

17010	WHEN ERROR IN
		GET #IC_BINMAP.CH%, REGARDLESS
	USE
		CONTINUE CreateBin IF ERR = 11%
		FILENAME$ = "IC_BINMAP"
		CONTINUE HelpError
	END WHEN

	GOTO CreateBin IF (IC_BINMAP::LOCATION <> UTL_LOCATION::LOCATION)

	GOTO 17010 IF READ_BIT(8%, IC_BINMAP::CYCLEMAP, TO_WEEK%) = 0% &
		AND TO_WEEK% <> 0%

	FOR I% = 0% TO 3%
		IF IC_BINMAP::BIN(I%) <> "" OR I% = 0%
		THEN
			IC_CYCLESHEET.BIN       = IC_BINMAP::BIN(I%)
			IC_CYCLESHEET.STORERFA  = GETRFA(IC_BINMAP.CH%)
			PUT #IC_TEMP.CH%
		END IF
	NEXT I%

	GOTO 17010

 CreateBin:
17015	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #IC_TEMP.CH%
		ELSE
			FIND #IC_TEMP.CH%, KEY #0% GE FROM_ITEM$, REGARDLESS
		END IF
	USE
		FILENAME$ = "IC_CYCLESHEET.TMP"
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
		CONTINUE NextLocation IF ERR = 11%
		FILENAME$ = "IC_TEMP"
		CONTINUE HelpError
	END WHEN

	GOTO NextLocation IF (IC_CYCLESHEET.BIN > TO_ITEM$) AND TO_ITEM$ <> ""

	GOTO GetNextRec &
		IF COMP_STRING(EDIT$(IC_CYCLESHEET.BIN, -1%), WLDCRD$) = 0% &
		AND WLDCRD$ <> ""

	GET #IC_BINMAP.CH%, RFA IC_CYCLESHEET.STORERFA, REGARDLESS

17200	!
	! Read Product file
	!
	WHEN ERROR IN
		GET #PD_PRODUCT.CH%, KEY #0% EQ IC_BINMAP::PRODUCT, REGARDLESS
	USE
		PD_PRODUCT::DESCRIPTION = &
			STRING$(LEN(PD_PRODUCT::DESCRIPTION), A"?"B)
		PD_PRODUCT::CATEGORY = &
			STRING$(LEN(PD_PRODUCT::CATEGORY), A"?"B)
		PD_PRODUCT::PROD_TYPE = &
			STRING$(LEN(PD_PRODUCT::PROD_TYPE), A"?"B)
		PD_PRODUCT::UOM = &
			STRING$(LEN(PD_PRODUCT::UOM), A"?"B)
		PD_PRODUCT::SSTATUS = &
			STRING$(LEN(PD_PRODUCT::SSTATUS), A"?"B)

		CONTINUE 17210
	END WHEN

17210	GOTO GetNextRec IF PD_PRODUCT::SSTATUS <> "A"

17300	!
	! Print out one line
	!
	TEXT_LIN$ = IC_BINMAP::PRODUCT + " " + &
		PD_PRODUCT::DESCRIPTION

	IF (SHOW_STD$ = "Y")
	THEN
		COST = PC_READ_COST(IC_BINMAP::PRODUCT, &
			UTL_LOCATION::LOCATION, "", "")

		TEXT_LIN$ = TEXT_LIN$ + &
			FORMAT$(COST, " ######.##")
	END IF

	TEXT_LIN$ = TEXT_LIN$ + &
		" " + &
		PD_PRODUCT::PROD_TYPE + " " + &
		PD_PRODUCT::CATEGORY + " " + &
		IC_CYCLESHEET.BIN + " " + &
		PD_PRODUCT::UOM + " " + &
		"!_________!"

		IF BARCODE$ = "Y"
		THEN
			TEXT_LIN$ = TEXT_LIN$ + "  " + &
				OUTP_CODE39(TRM$(IC_BINMAP::PRODUCT), &
				UTL_REPORTX::PRINTTO, BARPRINT%)
		ELSE
			TEXT_LIN$ = TEXT_LIN$ + &
				"_______!_______!"
		END IF

	LIN% = 0%

	IF LAST.LOCATION$ <> UTL_LOCATION::LOCATION
	THEN
		IF UTL_REPORTX::PAGENO
		THEN
			LIN% = 999%
		END IF
		LAST.LOCATION$ = UTL_LOCATION::LOCATION
		GOSUB 18600
		TEST_BIN$ = ""
	ELSE
		GOSUB 18600 IF TEST_BIN$ <> IC_CYCLESHEET.BIN
	END IF

	IF TEST_BIN$ <> IC_CYCLESHEET.BIN
	THEN
		TEXT1$ = "     Bin Location: " + IC_CYCLESHEET.BIN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT1$, LIN%)
		LIN% = 0%
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
	END IF

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT_LIN$, LIN%)
	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), "", -1%)

	TEST_BIN$ = IC_CYCLESHEET.BIN

17350	!
	! Try for next record
	!
	GOTO GetNextRec

 ExitTotal:
17400	!
	! Handle end of report
	!

 ExitProgram:

	GOSUB 18600

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

18600	!
	! Print totals
	!
	IF TEST_BIN$<>""
	THEN
		TEXT$ = SPACE$(15%) + &
			"T O T A L " + &
			SPACE$(102% - 9%) + &
			"________!"

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), " ", 0%)
	END IF

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
