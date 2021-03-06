1	%TITLE "Product Bin Locations"
	%SBTTL "IC_RPRT_BIN"
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
	! ID:IC003
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Bin Location\* report option will provide access to a report
	!	which will include the following information:
	!	.table 3,25
	!	.te
	!	Bin Location
	!	.te
	!	Product _#
	!	.te
	!	Description
	!	.te
	!	ABC
	!	.te
	!	Week Number
	!	.end table
	!	.lm -5
	!
	! Index:
	!	.x Bin Location>Report
	!	.x Bin Location>Report
	!	.x Report>Bin Location
	!	.x Bin Location>Report
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_RPRT_BIN/LINE
	!	$ LINK/EXE=IC_EXE: IC_RPRT_BIN, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE IC_RPRT_BIN.OBJ;*
	!
	! Author:
	!
	!	06/15/88 - Frank Starman
	!
	! Modification History:
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
	!	08/14/96 - Kevin Handy
	!		Lose extra '&' before 'then'.
	!		Reformat source code.
	!		Lose commented out code.
	!
	!	10/18/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	05/27/97 - Kevin Handy
	!		Use integer for #key
	!
	!	11/12/97 - Kevin Handy
	!		Make FROM_ITEM work
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/19/99 - Kevin Handy
	!		Use "WHEN ERROR IN"
	!
	!	10/31/2000 - Kevin Handy
	!		Use A"x"B
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

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	MAP (IC_TEMP) &
		STRING IC_TEMP.LOC = 4%, &
		STRING IC_TEMP.BIN = 6%, &
		RFA    IC_TEMP.RFA

	DECLARE STRING	V_STR_BIN

	!
	! External functions
	!
	EXTERNAL	INTEGER FUNCTION READ_BIT
	EXTERNAL	STRING  FUNCTION FUNC_BITSTRING

	%PAGE

	ON ERROR GOTO 19000

	!
	! Declare channels
	!
	CALL ASSG_CHANNEL(IC_TEMP.CH%, STAT%)

	!
	! Look up device
	!
	CALL READ_DEVICE("UTL_WORK", UTL_WORK.DEV$, STAT%)

 Init:	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) From Bin Location\*
	!	.b
	!	.lm +5
	!	The ^*From Bin Location\* field
	!	causes the report to begin printing with a selected Bin
	!	Location.
	!	.b
	!	A blank setting will cause the report to begin with the
	!	first Bin Location in the file.
	!	.lm -5
	!
	! Index:
	!	.x From Bin Location>Bin Location Report
	!	.x Bin Location Report>From Bin Location
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) To Bin Location\*
	!	.b
	!	.lm +5
	!	The ^*To Bin Location\* field
	!	causes the report to end printing with a selected Bin Location.
	!	.b
	!	A blank setting will cause the report to end with the
	!	last Bin Location in the file.
	!	.lm -5
	!
	! Index:
	!	.x To Bin Location>Bin Location Report
	!	.x Bin Location Report>To Bin Location
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(2%), -1%)

	!++
	! Abstract:FLD03
	!	^*(03) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects designated items
	!	to be printed by entering a "wildcard" value in this field.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard>Bin Location Report
	!	.x Bin Location Report>Wildcard
	!
	!--

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
	!	.x Locations>Bin Location Report
	!	.x Bin Location Report>Locations
	!
	!--

	FROM_WEEK% = VAL%(UTL_REPORTX::OPTDEF(5%))

	!++
	! Abstract:FLD06
	!	^*(06) From Week\*
	!	.b
	!	.lm +5
	!	The ^*From Week\* field causes the
	!	report to begin printing with a selected week.
	!	.b
	!	A blank setting will cause the report to begin with the first
	!	week in the file.
	!	.b
	!	The entry will be a two digit number ranging from 01 to 52.
	!	.lm -5
	!
	! Index:
	!	.x From Week>Bin Location Report
	!	.x Bin Location Report>From Week
	!
	!--

	TO_WEEK% = VAL%(UTL_REPORTX::OPTDEF(6%))

	!++
	! Abstract:FLD07
	!	^*(07) To Week\*
	!	.b
	!	.lm +5
	!	The ^*To Week\* field will cause the
	!	report to end with a selected week.
	!	.b
	!	A blank setting will cause the report to end with the
	!	last week in the file.
	!	.b
	!	The format for entry is two digits ranging from 01 to 52.
	!	.lm -5
	!
	! Index:
	!	.x To Week>Bin Locations Report
	!	.x Bin Locations Report>To Week
	!	.x Week>To
	!
	!--

	FROM_WEEK% = 1%  IF FROM_WEEK% <= 0%
	TO_WEEK% = 52% IF TO_WEEK% = 0% OR TO_WEEK% > 52%

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_BINMAP.OPN"
	USE
		FILENAME$ = "IC_BINMAP"
		CONTINUE HelpError
	END WHEN

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"
	USE
		CONTINUE 320 IF ERR = 5%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

320	WHEN ERROR IN
		%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.OPN"
	USE
		FILENAME$ = "UTL_LOCATION"
		CONTINUE HelpError
	END WHEN

330	CALL ENTR_3MESSAGE(SCOPE, "Creating temporary file", 1%)

	WHEN ERROR IN
		OPEN UTL_WORK.DEV$ + "IC_TEMP.TMP" FOR OUTPUT AS FILE IC_TEMP.CH%, &
			ORGANIZATION INDEXED FIXED, &
			MAP IC_TEMP, &
			PRIMARY KEY (IC_TEMP.LOC, IC_TEMP.BIN) DUPLICATES, &
			TEMPORARY, &
			BUFFER 32%, &
			ACCESS MODIFY, ALLOW MODIFY
	USE
		FILENAME$ = "IC_TEMP.TMP"
		CONTINUE HelpError
	END WHEN

400	RESET #IC_BINMAP.CH%

 ReadBinMap:
410	WHEN ERROR IN
		GET #IC_BINMAP.CH%, REGARDLESS
	USE
		CONTINUE ReportTitle IF ERR = 11%
		FILENAME$ = "IC_BINMAP"
		CONTINUE HelpError
	END WHEN

	GOTO ReadBinMap &
		IF COMP_STRING(EDIT$(IC_BINMAP::LOCATION, -1%), &
		LOCATION$) = 0% &
		AND LOCATION$ <> ""

	FOR I% = 0% TO 3%
		IF IC_BINMAP::BIN(I%) <> ""
		THEN
			IF (FROM_ITEM$ <= IC_BINMAP::BIN(I%)) AND &
				((TO_ITEM$ = "") OR &
				(TO_ITEM$ >= IC_BINMAP::BIN(I%)))
			THEN
				IC_TEMP.LOC = IC_BINMAP::LOCATION
				IC_TEMP.BIN = IC_BINMAP::BIN(I%)
				IC_TEMP.RFA = GETRFA(IC_BINMAP.CH%)
				PUT #IC_TEMP.CH%
			END IF
		END IF
	NEXT I%

	GOTO ReadBinMap

 ReportTitle:
	!
	! Title
	!
	TITLE$(2%) = "Inventory control system"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "BinLoc  Product#       Description          " + &
		"                         ABC  " + &
		"::123456789-123456789-123" + &
		"456789-123456789-123456789-12::"

	TITLE$(5%) = " "

	LYT_LINE$ = "$BinLoc:008,$Product#:023,$Description:069,$ABC:074,$CYCLE:131"

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

	TITLE$(1%) = "BIN  LOCATION  AT  " + IC_TEMP.LOC + "  " + &
		UTL_LOCATION::LOCNAME

17010	WHEN ERROR IN
		FIND #IC_TEMP.CH%, KEY #0% EQ UTL_LOCATION::LOCATION, REGARDLESS
	USE
		CONTINUE NextLocation IF ERR = 155%
		FILENAME$ = "IC_TEMP"
		CONTINUE HelpError
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
		GET #IC_TEMP.CH%, REGARDLESS
	USE
		CONTINUE NextLocation IF ERR = 11%
		FILENAME$ = "IC_TEMP"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	GOTO NextLocation IF IC_TEMP.LOC <> UTL_LOCATION::LOCATION

 !	GOTO NextLocation IF (IC_TEMP.BIN > TO_ITEM$) AND TO_ITEM$ <> ""

	GOTO GetNextRec IF COMP_STRING(EDIT$(IC_TEMP.BIN, -1%), WLDCRD$) = 0% &
		AND WLDCRD$ <> ""

17100	WHEN ERROR IN
		GET #IC_BINMAP.CH%, RFA IC_TEMP.RFA, REGARDLESS
	USE
		CONTINUE 17350 IF ERR = 155%
		FILENAME$ = "IC_BINMAP"
		CONTINUE HelpError
	END WHEN

	GOTO 17200 IF FROM_WEEK% = 1% AND TO_WEEK% = 52%

	FOR I% = FROM_WEEK% TO TO_WEEK%
		GOTO 17200 IF READ_BIT(8%, IC_BINMAP::CYCLEMAP, I%)
	NEXT I%

	GOTO GetNextRec

17200	PD_PRODUCT::DESCRIPTION = STRING$(LEN(PD_PRODUCT::DESCRIPTION), A"?"B)

	WHEN ERROR IN
		GET #PD_PRODUCT.CH%, KEY #0% EQ IC_BINMAP::PRODUCT, REGARDLESS
	USE
		CONTINUE 17300 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

17300	!
	! Print out one line
	!
	TEXT$ = IC_TEMP.BIN + "  " + &
		IC_BINMAP::PRODUCT + " " + &
		PD_PRODUCT::DESCRIPTION + "      " + &
		IC_BINMAP::ABC + "    ::" + &
		FUNC_BITSTRING(8%, IC_BINMAP::CYCLEMAP, 52%, "*") + &
		"::" &

	V_INT_LIN% = 0%

	IF LAST_LOCATION$ <> UTL_LOCATION::LOCATION
	THEN
		LAST_LOCATION$ = UTL_LOCATION::LOCATION
		V_STR_BIN = ""
	ELSE
		IF V_STR_BIN <> IC_TEMP.BIN AND V_STR_BIN <>""
		THEN
			CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
			V_STR_BIN = IC_TEMP.BIN
		END IF
	END IF

	CALL OUTP_LINE(LYT_LINE$, UTL_REPORTX, TITLE$(), TEXT$, V_INT_LIN%)

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
17510	CALL OUTP_FINISH(UTL_REPORTX)

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
 !	SELECT ERR
 !	CASE 154%	* Locked record
 !		SLEEP 5%
 !		RESUME
 !	END SELECT

	FILENAME$ = ""

	!
	! Untrapped error
	!
	RESUME HelpError

32767	END
