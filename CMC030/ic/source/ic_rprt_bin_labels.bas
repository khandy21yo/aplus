1	%TITLE "Product Bin Labels"
	%SBTTL "IC_RPRT_BIN_LABELS"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1997 BY
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
	! ID:IC003
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This program will print labels with barcodes (code39) for inventory
	!	bin locations.
	!	.note
	!	This program will only print properly on a HP LaserJet II
	!	compatible laser printer (such as the OKIDATA 820).
	!	.end note
	!	.lm -5
	!
	! Index:
	!	.x Bin Location>Report
	!	.x Report>Bin Location
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_RPRT_BIN_LABELS/LINE
	!	$ LINK/EXE=IC_EXE: IC_RPRT_BIN_LABELS, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE IC_RPRT_BIN_LABELS.OBJ;*
	!
	! Author:
	!
	!	10/07/97 - Kevin Handy
	!
	! Modification History:
	!
	!	11/12/97 - Kevin Handy
	!		Make FROM_ITEM work
	!
	!	11/13/97 - Kevin Handy
	!		Do more comparisons at time of temp file
	!		generation, instead of during scan.
	!
	!	11/25/97 - Kevin Handy
	!		Clean up (Check)
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

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT)	PD_PRODUCT_CDD		PD_PRODUCT

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION

	%INCLUDE "FUNC_INCLUDE:PRINT35.INC"
	MAP (PRINTX) PRINTX_CDD PRINTX

	MAP (IC_TEMP) &
		STRING IC_TEMP.LOC = 4%, &
		STRING IC_TEMP.BIN = 6%, &
		RFA    IC_TEMP.RFA

	DECLARE STRING	V_STR_BIN

	!
	! External functions
	!
	EXTERNAL INTEGER FUNCTION READ_BIT
	EXTERNAL STRING FUNCTION OUTP_CODE39
	EXTERNAL LONG   FUNCTION FIND_3PRINTGROUPITEM

	DECLARE INTEGER CONSTANT LASER_INCH = 300%

	%PAGE

	!
	! This function is used to generate an escape sequence
	! to position to a specified line on a specified label
	!
	DEF FNLASER_POSITION$(LABEL%, LINE%, SPACING%, COFFSET%, ROFFSET%)

		LLABEL% = LABEL%

		!
		! Make sure we are in the range of 0 to 9
		!
		LLABEL% = LLABEL% - (LLABEL% / 10% * 10%)

		!
		! Figure out left/right which label it goes to (0 to 4)
		!
		LCOLUMN% = LLABEL% / 5%

		!
		! Figure out which label (up/down) we need (0 to 1)
		!
		LROW% = LLABEL% - LCOLUMN% * 5%

		!
		! Convert the row/column to 1/300" positions
		!
		LCOLUMN% = LCOLUMN% * (40% * LASER_INCH / 10%) + COFFSET%
		LROW% = LROW% * (LASER_INCH * 2%) + &
			LINE% * (LASER_INCH / SPACING%) + ROFFSET%

		!
		! Generate the escape sequence
		!
		FNLASER_POSITION$ = '27'C + "*p" + NUM1$(LCOLUMN%) + "x" + &
			"*p" + NUM1$(LROW%) + "Y"

	FNEND

	DECLARE STRING CONSTANT HP_COUR10 = &
		"27"C + "&l0O" + "27"C + "(8U" + &
		"27"C + "(s0p10h12v0s0b3T"
	DECLARE STRING CONSTANT HP_COUR12 = &
		"27"C + "&l0O" + "27"C + "(8U" + &
		"27"C + "(s0p12h10v0s0b3T"
	DECLARE STRING CONSTANT HP_TIME12 = &
		"27"C + "&l0O" + "27"C + "(8U" + &
		"27"C + "(s1p12v0s0b5T"
	DECLARE STRING CONSTANT HP_HELV14 = &
		"27"C + "&l0O" + "27"C + "(0U" + &
		"27"C + "(s1p14.4v0s3b4T"

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
	!	.x From Bin Location>Bin Location
	!	.x Bin Location>From Bin Location
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
	!	.x To Bin Location>Bin Location
	!	.x Bin Location>To Bin Location
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
	!	.x Wildcard>Bin Location
	!	.x Bin Location>Wildcard
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
	!	.lm -5
	!
	! Index:
	!	.x Locations>Bin Location
	!	.x Bin Location>Locations
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
	!	.x From Week>Bin Location
	!	.x Bin Location>From Week
	!
	!--

	TO_WEEK% = VAL%(UTL_REPORTX::OPTDEF(6%))

	!++
	! Abstract:FLD07
	!	^*(07) To Week\*
	!	.b
	!	.lm +5
	!	The ^*To Week\* field causes the
	!	report to end with a selected week.
	!	.b
	!	A blank setting will cause the report to end with the
	!	last week in the file.
	!	.b
	!	The format for entry is two digits ranging from 01 to 52.
	!	.lm -5
	!
	! Index:
	!	.x To Week>Bin Locations
	!	.x Bin Locations>To Week
	!	.x Week>To
	!
	!--

	FROM_WEEK% = 1%  IF FROM_WEEK% <= 0%
	TO_WEEK% = 52% IF TO_WEEK% = 0% OR TO_WEEK% > 52%

	ROW_OFFSET% = VAL%(UTL_REPORTX::OPTDEF(8%))

	!++
	! Abstract:FLD09
	!	^*(09) Row Offset\*
	!	.lm +5
	!	.b
	!	Determines additional space from the top of the page
	!	to position the labels.
	!	Based on 1 inch = 300.
	!
	! Index:
	!	.x Row Offset>Bin Locations
	!	.x Bin Locations>Column Row
	!
	!--

	COL_OFFSET% = VAL%(UTL_REPORTX::OPTDEF(9%))

	!++
	! Abstract:FLD10
	!	^*(10) Column Offset\*
	!	.lm +5
	!	.b
	!	Determines additional space from the left margin of the page
	!	to position the labels.
	!	Based on 1 inch = 300.
	!
	! Index:
	!	.x Column Offset>Bin Locations
	!	.x Bin Locations>Column Offset
	!
	!--

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
		CONTINUE HelpError
	END WHEN

	GOTO ReadBinMap &
		IF COMP_STRING(EDIT$(IC_BINMAP::LOCATION, -1%), &
		LOCATION$) = 0% AND LOCATION$ <> ""

	FOR I% = 0% TO 3%
		IF IC_BINMAP::BIN(I%) <> ""
		THEN
			IF (FROM_ITEM$ <= IC_BINMAP::BIN(I%)) AND &
				((TO_ITEM$ = "") OR &
				(TO_ITEM$ >= IC_BINMAP::BIN(I%)))
			THEN
				IF COMP_STRING(EDIT$(IC_BINMAP::BIN(I%), -1%), &
					WLDCRD$) <> 0% OR WLDCRD$ <> ""
				THEN
					IC_TEMP.LOC = IC_BINMAP::LOCATION
					IC_TEMP.BIN = IC_BINMAP::BIN(I%)
					IC_TEMP.RFA = GETRFA(IC_BINMAP.CH%)
					PUT #IC_TEMP.CH%
				END IF
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
	TITLE$(4%) = ""
	TITLE$(5%) = ""

	LYT_LINE$ = ""

	LABEL% = 0%

	IF PRINTX::ITEMS == 0
	THEN
		CALL OUTP_INITIALIZE(UTL_REPORTX::PRINTTYPE)
	END IF

	LOOP% = FIND_3PRINTGROUPITEM("BC", "*", PRINTX)
	IF LOOP% > 0%
	THEN
		INTRO% = VAL%(PRINTX::SEQU(LOOP%))
	ELSE
		INTRO% = 0%
	END IF

	!
	! So that the first line printed isn't so long
	!
	CALL OUTP_RAWPRINT(LYT_LINE$, UTL_REPORTX, TITLE$(), "", 0%)

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

17100	WHEN ERROR IN
		GET #IC_BINMAP.CH%, RFA IC_TEMP.RFA,REGARDLESS
	USE
		CONTINUE 17350 IF ERR = 155%
		FILENAME$ = "IC_BINMAP"
		CONTINUE HelpError
	END WHEN

	GOTO 17200 IF FROM_WEEK% = 1% AND TO_WEEK% = 52%

	FOR I% = FROM_WEEK% TO TO_WEEK%
		GOTO 17200 IF READ_BIT(8%,IC_BINMAP::CYCLEMAP, I%)
	NEXT I%

	GOTO GetNextRec

17200	WHEN ERROR IN
		GET #PD_PRODUCT.CH%, KEY #0% EQ IC_BINMAP::PRODUCT, REGARDLESS
	USE
		PD_PRODUCT::DESCRIPTION = &
			STRING$(LEN(PD_PRODUCT::DESCRIPTION), A"?"B)

		CONTINUE 17300 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "PD_PRODUCT"
		CONTINUE HelpError
	END WHEN

17300	!
	! Force out a page?
	!
	IF (LABEL% <> 0%) AND ((LABEL% / 10%) * 10%) = LABEL%
	THEN
		CALL OUTP_FORMFF(UTL_REPORTX)
	END IF

	!
	! Slap out one label
	!
	!	Barcode
	!	Barcode
	!	Product Number
	!	Description
	!	Bin Number
	!
	TEXT$ = OUTP_CODE39(PD_PRODUCT::PRODUCT_NUM, &
		UTL_REPORTX::PRINTTO, INTRO%)

	CALL OUTP_RAWPRINT(LYT_LINE$, UTL_REPORTX, TITLE$(), &
		FNLASER_POSITION$(LABEL%, 0%, 6%, COL_OFFSET%, ROW_OFFSET%) + &
		TEXT$, &
		0%)

	CALL OUTP_RAWPRINT(LYT_LINE$, UTL_REPORTX, TITLE$(), &
		FNLASER_POSITION$(LABEL%, 1%, 6%, COL_OFFSET%, ROW_OFFSET%) + &
		TEXT$, &
		0%)

	CALL OUTP_RAWPRINT(LYT_LINE$, UTL_REPORTX, TITLE$(), &
		FNLASER_POSITION$(LABEL%, 3%, 6%, COL_OFFSET%, ROW_OFFSET%) + &
		HP_HELV14 + PD_PRODUCT::PRODUCT_NUM, &
		0%)

	TEXT$ = TRM$(LEFT(PD_PRODUCT::DESCRIPTION, 26%))
	CALL OUTP_RAWPRINT(LYT_LINE$, UTL_REPORTX, TITLE$(), &
		FNLASER_POSITION$(LABEL%, 4%, 5%, COL_OFFSET%, ROW_OFFSET%) + &
		TEXT$, &
		0%)

	TEXT$ = TRM$(RIGHT(PD_PRODUCT::DESCRIPTION, 27%))
	IF TEXT$ <> ""
	THEN
		CALL OUTP_RAWPRINT(LYT_LINE$, UTL_REPORTX, TITLE$(), &
			FNLASER_POSITION$(LABEL%, 5%, 5%, COL_OFFSET%, ROW_OFFSET%) + &
			TEXT$, &
			0%)
	END IF

	CALL OUTP_RAWPRINT(LYT_LINE$, UTL_REPORTX, TITLE$(), &
		FNLASER_POSITION$(LABEL%, 6%, 5%, COL_OFFSET%, ROW_OFFSET%) + &
		"Bin#: " + IC_TEMP.BIN + &
		"       Loc: " + IC_TEMP.LOC + &
		HP_COUR10, &
		0%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	LABEL% = LABEL% + 1%

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

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
