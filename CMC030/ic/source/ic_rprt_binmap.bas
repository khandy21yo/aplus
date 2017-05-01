1	%TITLE "Product Bin Location List"
	%SBTTL "IC_RPRT_BINMAP"
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
	! ID:IC002
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Bin and Cycle Count Map\* option provides access
	!	to a report which includes the following information:
	!	.table 3,25
	!	.te
	!	Product _#	Description
	!	.te
	!	Location	ABC
	!	.te
	!	Bin 1	Bin 2
	!	.te
	!	Bin 3	Bin 4
	!	.te
	!	Week Number	Maximum Level
	!	.te
	!	Safety Stock
	!	.end taBLE
	!
	! Index:
	!	.x Report>Bin and Cycle Count Map
	!	.x Bin and Cycle Count Map>Report
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_RPRT_BINMAP/LINE
	!	$ LINK/EXE=IC_EXE: IC_RPRT_BINMAP, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE IC_RPRT_BINMAP.OBJ;*
	!
	!
	! Author:
	!
	!	04/07/88 - Frank F. Starman
	!
	! Modification History:
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/14/96 - Kevin Handy
	!		Lose extra '&'.
	!		Reformat source code.
	!
	!	05/15/97 - Kevin Handy
	!		Reformat source code.
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
	DECLARE			UTL_REPORTX_CDD	UTL_REPORTX

	%INCLUDE "SOURCE:[IC.OPEN]IC_BINMAP.HB"
	MAP	(IC_BINMAP)	IC_BINMAP_CDD	IC_BINMAP

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP	(PD_PRODUCT)	PD_PRODUCT_CDD	PD_PRODUCT

	DECLARE STRING TEST_PRINTING

	!
	! External functions
	!
	EXTERNAL	STRING  FUNCTION FUNC_BITSTRING

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
	!	^*(01) From Item\*
	!	.b
	!	.lm +5
	!	The ^*From Item\* field
	!	begins the report with a selected
	!	item.
	!	.b
	!	A blank setting will cause the report to begin with
	!	the first item in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Item>Product Bin Location
	!	.x Product Bin Location>From Item
	!	.x Item>From
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Item\*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field enters a selected
	!	item number with which the report will end.
	!	.b
	!	A blank setting will cause the report to end with the last
	!	item in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Item>Product Bin Location
	!	.x Product Bin Location>To Item
	!	.x Item>To
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field prints only
	!	selected items on the report by using the "wildcarding"
	!	technique.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard>Product Bin Location
	!	.x Product Bin Location>Wildcard
	!
	!--

	SORT_BY$ = EDIT$(UTL_REPORTX::OPTDEF(9%), -1%)

	!++
	! Abstract:FLD10
	!	^*(10) Sort\*
	!	.b
	!	.lm +5
	!	The ^*Sort\* field prints the report
	!	in a selected order.
	!	.b
	!	Valid settings are:
	!	.table 3,25
	!	.te
	!	^*L\* - Location
	!	.te
	!	^*P\* - Product Type
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Sort>Product Bin Location
	!	.x Product Bin Location>Sort
	!
	!--

	SELECT SORT_BY$
	CASE "L"
		SORT_KEY% = 1%

	CASE "P"
		SORT_KEY% = 0%
	END SELECT

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_BINMAP.OPN"
	USE
		FILENAME$ = "IC_BINMAP"
		CONTINUE HelpError
	END WHEN

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"
	USE
		CONTINUE ReportTitle IF ERR = 5%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "BIN  LOCATION  AND  CYCLE  COUNT  LIST"
	TITLE$(2%) = "Inventory control system"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "Product#       Description           Loc   " + &
		"ABC Bin1   Bin2  " + &
		" Bin3   Bin4    ::123456789-123456789-123" + &
		"456789-123456789-123456789-12::"

	TITLE$(5%) = SPACE$(44%) + "  MaximumLevel   SafetyStock"
	TITLE$(6%) = "."

	LYT.LINE$ = "$Product#:015,$Description:037,Loc:043,$ABC:047," + &
		"$Bin1:054,$Bin2:061,$Bin3:068,$Bin4:077,$CYCLEMAP:132"

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #IC_BINMAP.CH%
		ELSE
			FIND #IC_BINMAP.CH%, &
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
		GET #IC_BINMAP.CH%, REGARDLESS
	USE
		CONTINUE ExitTotal IF ERR = 11%
		FILENAME$ = "IC_BINMAP"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	SELECT SORT_KEY%

	CASE 0%

		GOTO ExitTotal IF (IC_BINMAP::PRODUCT > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <>"" AND &
			COMP_STRING(EDIT$(IC_BINMAP::PRODUCT, -1%), &
			WLDCRD$) = 0%

	CASE 1%

		GOTO ExitTotal IF (IC_BINMAP::LOCATION > TO_ITEM$) AND &
			TO_ITEM$ <> ""

		GOTO GetNextRec IF WLDCRD$ <>"" AND &
			COMP_STRING(EDIT$(IC_BINMAP::LOCATION, -1%), &
			WLDCRD$) = 0%

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), " ", 0%) &
			IF TEST_PRINTING <> IC_BINMAP::LOCATION  AND &
			TEST_PRINTING <> ""

		TEST_PRINTING = IC_BINMAP::LOCATION
	END SELECT

17200	!
	! Read product description
	!
	WHEN ERROR IN
		FIND #PD_PRODUCT.CH%, KEY #0% EQ IC_BINMAP::PRODUCT, REGARDLESS
		GET #PD_PRODUCT.CH%, REGARDLESS
	USE
		PD_PRODUCT::DESCRIPTION = &
			STRING$(LEN(PD_PRODUCT::DESCRIPTION), A"?"B)

		CONTINUE 17300 IF ERR = 155%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN


17300	!
	! Print out one line
	!
	TEXT$ = IC_BINMAP::PRODUCT + " " + &
		LEFT(PD_PRODUCT::DESCRIPTION, 21%) + " " + &
		IC_BINMAP::LOCATION + "  " + &
		IC_BINMAP::ABC + "   " + &
		IC_BINMAP::BIN(0%) + " " + &
		IC_BINMAP::BIN(1%) + " " + &
		IC_BINMAP::BIN(2%) + " " + &
		IC_BINMAP::BIN(3%) + "  ::" + &
		FUNC_BITSTRING(8%,IC_BINMAP::CYCLEMAP, 52%, "*") + &
		"::"

	CALL OUTP_LINE(LYT.LINE$, UTL_REPORTX, TITLE$(), TEXT$, 1%)

	TEXT$ = SPACE$(44%) + &
		FORMAT$(IC_BINMAP::MAXLEVEL, "##,###,###.###") + &
		FORMAT$(IC_BINMAP::SAFETY, "##,###,###.###")

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)

17350	!
	! Try for next record
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT
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
