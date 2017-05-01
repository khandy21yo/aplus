1	%TITLE "Document a Structures"
	%SBTTL "TK_SPEC_DOCUSTRUCT"
	%IDENT "V3.6a Calico"

	!
	!	COPYRIGHT (C) 1987 BY
	!	Computer Management Center, Idaho Falls, Idaho.
	!
	! This software is furnished under a license and may be used and
	! copied only in accordance with terms of such license and with
	! the inclusion of the above copyright notice.  This software or
	! any other copies therof may not be provided or otherwise made
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
	! Abstract:HELP
	!	.p
	!	This program will read a menu source file, and from
	!	that, will create record structure documentation.
	!
	! Index:
	!
	! Option:
	!
	! Input:
	!
	!	Name of file to document
	!
	!	Name of LaTeX file to create.
	!
	! Output:
	!
	!	LaTeX documentation file
	!
	! Example:
	!
	!
	! Compile:
	!
	!	$ BAS TK_SOURCE:TK_SPEC_DOCUSTRUCT
	!	$ LINK/EXE=TK_EXE: TK_SPEC_DOCUSTRUCT, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE TK_SPEC_DOCUSTRUCT.OBJ;*
	!
	! Author:
	!
	!	10/26/87 - Kevin Handy
	!
	! Modification history:
	!
	!	06/14/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	11/20/95 - Kevin Handy
	!		Lose /NOWARN on compile statement.
	!
	!	08/14/96 - Kevin Handy
	!		Lose extra '&' before 'end if'
	!
	!	06/05/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/28/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/09/99 - Kevin Handy
	!		Use BASIC$STARLET for LIB$
	!
	!	06/09/99 - Kevin Handy
	!		Remove FNLEVEL$ (Dead code)
	!
	!	10/17/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:LIBRARY.COM"

	%INCLUDE "LIB$ROUTINES" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET.TLB"

	!
	! External functions
	!
	EXTERNAL LONG   TK_DOCU_GETMODULES_ALL
 !	EXTERNAL STRING FUNCTION TK_FUNC_TEXSTRING
	EXTERNAL LONG    FUNCTION SYS$FILESCAN

	!
	! Map statements
	!
	%INCLUDE "SOURCE:[TK.OPEN]TK_MODULE.HB"
	MAP (TK_MODULE) TK_MODULE_CDD TK_MODULE

	%INCLUDE "SOURCE:[TK.OPEN]TK_RELATION.HB"
	MAP (TK_RELATION) TK_RELATION_CDD TK_RELATION

	%INCLUDE "SOURCE:[TK.OPEN]TK_FILE.HB"
	MAP (TK_FILE) TK_FILE_CDD TK_FILE

	%INCLUDE "SOURCE:[TK.OPEN]TK_FOREIGN.HB"
	MAP (TK_FOREIGN) TK_FOREIGN_CDD TK_FOREIGN

	%INCLUDE "SOURCE:[TK.OPEN]TK_FILEDICT.HB"
	MAP (TK_FILEDICT) TK_FILEDICT_CDD TK_FILEDICT

	MAP (IOBUF) LONG IO_BUF(6%)
	MAP (IOBUF) WORD IO_BUF_W(12%)
	MAP (IOBUF1) NAME.BUFFER$ = 50%
	MAP (LBR_JUNKJUNK) TEXT$ = 132%, TEXT1$ = 256%		! Read buffer

	MAP (TK_DOCUALL) &
		MODCOUNTALL%, &
		MODNAMEALL$(1500%) = 50%, &
		MODFLAGALL%(1500%)

	!
	! Declare variables
	!
	DECLARE RFA TXRFA, NULL.RFA, WIN_RFA, WIN1_RFA
	DECLARE LONG CONSTANT FSCN$_NAME = 6
	DECLARE INTEGER CONSTANT MAX_COMMAND = 100%
	DECLARE INTEGER CONSTANT MAX_LIST = 3000%

	DIM LOC_CMDCODE$(MAX_COMMAND), &
		LOC_CMDOP%(MAX_COMMAND), &
		LOC_MODNAME$(MAX_LIST), &
		LOC_BOMEXE$(MAX_LIST), &
		LOC_BOMLVL%(MAX_LIST), &
		LOC_BOMMOD$(MAX_LIST), &
		LOC_BOMSTR$(MAX_LIST), &
		LOC_STRLIST$(MAX_LIST), &
		LOC_HIDPRG$(MAX_LIST), &
		RFA RFA_LEVEL(100%), &
		STRING TEST_MODULE(100%)

	%PAGE

	!*******************************************************************
	! Allocate channels
	!*******************************************************************
	CALL ASSG_CHANNEL(LOC_SYSTEM.CH%, STAT%)
	CALL ASSG_CHANNEL(LOC_FILE.CH%, STAT%)
	CALL ASSG_CHANNEL(LOC_CMDTBL.CH%, STAT%)

	!
	! Set up file name for menu look up
	!
	TLB_DEVICE$ = "REF:"

300	!
	! Open module file
	!
	%INCLUDE "SOURCE:[TK.OPEN]TK_MODULE.OPN"

310	!
	! Open module relation file
	!
	%INCLUDE "SOURCE:[TK.OPEN]TK_RELATION.OPN"

320	!
	! Open structure and data item file
	!
	%INCLUDE "SOURCE:[TK.OPEN]TK_FILE.OPN"

340	!
	! Open the foreign key file
	!
	%INCLUDE "SOURCE:[TK.OPEN]TK_FOREIGN.OPN"

350	!
	! Open the file dictionary
	!
	%INCLUDE "SOURCE:[TK.OPEN]TK_FILEDICT.OPN"

360	!*******************************************************************
	! Initilize for conversion
	!*******************************************************************
	!
	! Initialize command list
	!
	LOC_CMDCNT% = 0%

	OPEN "TK_DSRCMD" FOR INPUT AS FILE LOC_CMDTBL.CH%, &
		ACCESS READ, &
		ALLOW MODIFY, &
		DEFAULTNAME "CMC:.TEMPLATE"

370	!
	! Read in one line
	!
	WHEN ERROR IN
		LINPUT #LOC_CMDTBL.CH%, LOC_LINE$
	USE
		CONTINUE 380
	END WHEN

	IF LOC_LINE$ <> ""
	THEN
		LOC_QUOTE% = INSTR(1%, LOC_LINE$, '"')
		IF LOC_QUOTE%
		THEN
			LOC_CMDCNT% = LOC_CMDCNT% + 1%
			LOC_CMDOP%(LOC_CMDCNT%) = VAL%(LEFT(LOC_LINE$, &
				LOC_QUOTE% - 1%))
			LOC_LINE$ = RIGHT(LOC_LINE$, LOC_QUOTE% + 1%)
			LOC_QUOTE% = INSTR(1%, LOC_LINE$, '"')
			LOC_QUOTE% = LEN(LOC_LINE$) + 1% IF LOC_QUOTE% = 0%
			LOC_CMDCODE$(LOC_CMDCNT%) = LEFT(LOC_LINE$, &
				LOC_QUOTE% - 1%)

		END IF
	END IF

	GOTO 370

380	!*******************************************************************
	! Get menu source file
	!*******************************************************************
	FILE_NAME$ = ""

1200	LINPUT "Name of menu file (No account or externsion) "; SOURCE.NAME$

	SOURCE.NAME$ = EDIT$(SOURCE.NAME$, 2% + 32% + 256%)

	OPEN SOURCE.NAME$ FOR INPUT AS FILE LOC_SYSTEM.CH%, &
		ACCESS READ, &
		ALLOW MODIFY, &
		DEFAULTNAME "CMC:.MNU"

1300	!
	! Get base name of input file
	!
	NAME.BUFFER$ = SOURCE.NAME$

	!
	! Strip off all but the program name
	!
	IO_BUF_W(1%) = FSCN$_NAME
	IO_BUF_W(0%) = 0%
	IO_BUF(1%) = 0%
	IO_BUF(2%) = 0%
	IO_BUF(3%) = 0%
	SYS_STATUS% = SYS$FILESCAN( &
		NAME.BUFFER$ BY DESC, &
		IO_BUF() BY REF, 0%)
	TEMP_LONG% = IO_BUF(1%)
	TEMP1_LONG% = LOC(NAME.BUFFER$)
	TEMP_LONG% = TEMP_LONG% - TEMP1_LONG% + 1%
	SOURCE.BASE$ = MID(NAME.BUFFER$, &
		TEMP_LONG%, IO_BUF_W(0%))

1400	!*******************************************************************
	! Get output file name
	!*******************************************************************

	LINPUT "Name of output file <.TXT> "; LOC_FILE$

	OPEN LOC_FILE$ FOR OUTPUT AS FILE LOC_FILE.CH%, &
		DEFAULTNAME ".TXT", &
		RECORDSIZE 255%, &
		ALLOW READ

	!
	! Create documentation
	!
	PRINT #LOC_FILE.CH%, "^*Visual Table of Content\*"
	PRINT #LOC_FILE.CH%, ".b"
	PRINT #LOC_FILE.CH%, ".lt"

2000	!*******************************************************************
	! Scan through file, handling one line at a time
	!*******************************************************************

	QUOTE_FLAG% = 0%	! Which way does the current quote marker point

	!
	! Read in one line
	!
	WHEN ERROR IN
		LINPUT #LOC_SYSTEM.CH%, INLINE$
	USE
		CONTINUE 3000 IF ERR = 11%
		EXIT HANDLER
	END WHEN

	GOTO 2000 IF INLINE$ = ""

2200	!*******************************************************************
	! Parse one line
	!*******************************************************************
 ParseLine:

	LOC_FIRSTCHAR$ = LEFT(INLINE$, 1%)

	SELECT LOC_FIRSTCHAR$

	CASE "!"
		LOC_EXE% = INSTR(1%, INLINE$, ".EXE")
		IF LOC_EXE%
		THEN
			LOC_TEST% = INSTR(1%, INLINE$, "]")
			LOC_COLON% = INSTR(1%, INLINE$, ":")

			LOC_TEST% = LOC_COLON% IF LOC_COLON% > LOC_TEST%
			INLINE$ = LEFT(INLINE$, LOC_EXE% - 1%)
			INLINE$ = RIGHT(INLINE$, LOC_TEST% + 1%)

			GOTO EndofParse &
				IF LOC_MODNAME$(LOC_MODLOOP%) = TLB_PROGRAM$ &
				FOR LOC_MODLOOP% = 1% TO LOC_MODCNT%

			!
			! Item not found, create it
			!
			LOC_LOOP%, LOC_MODCNT% = LOC_MODCNT% + 1%

			WHILE (LOC_LOOP% > 1%) AND &
				(LOC_MODNAME$(LOC_LOOP% - 1%) > TLB_PROGRAM$)

				LOC_MODNAME$(LOC_LOOP%) = &
					LOC_MODNAME$(LOC_LOOP% - 1%)
				LOC_LOOP% = LOC_LOOP% - 1%
			NEXT

			LOC_MODNAME$(LOC_LOOP%) = INLINE$
		END IF

	CASE ELSE
		!
		! Pull off dots
		!
		DOT_COUNTER% = 0%
		WHILE MID(INLINE$, DOT_COUNTER% + 1%, 1%) = "."
			DOT_COUNTER% = DOT_COUNTER% + 1%
		NEXT

		INLINE$ = RIGHT(INLINE$, DOT_COUNTER% + 1%)

		!
		! Pull apart line
		!
		FIRST_SPACE% = INSTR(1%, INLINE$, " ")

		FIRST_ARROW% = INSTR(FIRST_SPACE% + 1%, INLINE$, ">")
		FIRST_ARROW% = INSTR(FIRST_SPACE% + 1%, INLINE$, "<") &
			IF FIRST_ARROW% = 0%

		IF FIRST_ARROW% = 0%
		THEN
			PRINT "Unable to parse input line from menu '"; &
				INLINE$; "'"
			GOTO 2000
		END IF

		SECOND_ARROW% = INSTR(FIRST_ARROW% + 1%, INLINE$, ">")
		SECOND_ARROW% = INSTR(FIRST_ARROW% + 1%, INLINE$, "<") &
			IF SECOND_ARROW% = 0%

		IF SECOND_ARROW% = 0%
		THEN
			PRINT "Unable to parse input line from menu '"; &
				INLINE$; "'"
			GOTO 2000
		END IF

		!
		! Seperate parts
		!
		MENU_OPTION$ = LEFT(INLINE$, FIRST_SPACE% - 1%)
		MENU_DESCR$ = SEG$(INLINE$, FIRST_SPACE% + 1%, &
			FIRST_ARROW% - 2%)
		MENU_IDENT$ = SEG$(INLINE$, FIRST_ARROW% + 1%, &
			SECOND_ARROW% - 1%)
		MENU_FILE$ = RIGHT(INLINE$, SECOND_ARROW% + 1%)

		!
		! Yank off system name if possible
		!
		IF DOT_COUNTER% = 0%
		THEN
			MENU_SYSTEM$ = MENU_OPTION$
		END IF

		IF MID(INLINE$, FIRST_ARROW%, 1%) = ">"
		THEN
			MENU_TYPE$ = "P"
		ELSE
			MENU_TYPE$ = "H"
		END IF

		!
		! Handle help file for this line
		!
		CALL HELP_MENUHELPKEY(MENU_FILE$, &
			MENU_TYPE$, &
			MENU_SYSTEM$, &
			TLB_IDENT$, &
			TLB_PROGRAM$, &
			TLB_ITEM$, &
			TLB_DEVICE$, &
			1%)

		!
		! Record visual table of content
		!
		LOC_VTOC$ = LEFT(SPACE$(DOT_COUNTER%) + &
			LEFT(MENU_OPTION$ + SPACE$(7%), 7%) + &
			MENU_DESCR$ + SPACE$(45%), 45%)

		IF MENU_TYPE$ = "P"
		THEN
			LOC_VTOC$ = &
				LOC_VTOC$ + LEFT(TLB_PROGRAM$, 20%)
		END IF

		!
		! Print the visual table of content
		!
		PRINT #LOC_FILE.CH%, LOC_VTOC$

		!
		! Store in module name array if this is a process
		!
		IF MENU_TYPE$ = "P"
		THEN
			GOTO EndofParse &
				IF LOC_MODNAME$(LOC_MODLOOP%) = TLB_PROGRAM$ &
				FOR LOC_MODLOOP% = 1% TO LOC_MODCNT%

			!
			! Item not found, create it
			!
			LOC_LOOP%, LOC_MODCNT% = LOC_MODCNT% + 1%

			WHILE (LOC_LOOP% > 1%) AND &
				(LOC_MODNAME$(LOC_LOOP% - 1%) > TLB_PROGRAM$)

				LOC_MODNAME$(LOC_LOOP%) = &
					LOC_MODNAME$(LOC_LOOP% - 1%)
				LOC_LOOP% = LOC_LOOP% - 1%
			NEXT

			LOC_MODNAME$(LOC_LOOP%) = TLB_PROGRAM$
		END IF

	END SELECT

 EndOfParse:
	!
	! Read in next line
	!
	GOTO 2000

	%PAGE

3000	!
	! Print end of vtoc
	!
	PRINT #LOC_FILE.CH%, ".el"

	!*******************************************************************
	! Find out what structures are being used in the list of program
	! that have been created.
	!*******************************************************************
	LOC_BOMCNT%, LOC_STRCNT% = 0%

	FOR LOC_MODLOOP% = 1% TO LOC_MODCNT%

3010		!
		! Look up module in module relation file to find all structures
		!
		WHEN ERROR IN
			FIND #TK_RELATION.CH%, KEY #0% EQ &
				LOC_MODNAME$(LOC_MODLOOP%), REGARDLESS
		USE
			CONTINUE 3090
		END WHEN

3020		!
		! Get module relation record
		!
		WHEN ERROR IN
			GET #TK_RELATION.CH%, REGARDLESS
		USE
			CONTINUE 3090
		END WHEN

		!
		! Jump out if Module name does not match test
		!
		IF LOC_MODNAME$(LOC_MODLOOP%) <> TK_RELATION::PARENT
		THEN
			GOTO 3090
		END IF

		LEVEL% = 1%

 GoDownModTree:
		!
		TEST_MODULE(LEVEL%) = TK_RELATION::PARENT
		RFA_LEVEL(LEVEL%) = GETRFA(TK_RELATION.CH%)

		!
		! Check to see if this level becomes recursive
		!
		GOTO GoUpModTree &
			IF TEST_MODULE(LOOP%) = TEST_MODULE(LEVEL%) &
				FOR LOOP% = 1% TO LEVEL% - 1%

		!
		! If this is a cmc function for CMCFUN
		!
3030		WHEN ERROR IN
			GET #TK_MODULE.CH%, KEY #0% EQ TK_RELATION::PARENT, REGARDLESS
		USE
			CONTINUE 3040 IF ERR = 155%
			EXIT HANDLER
		END WHEN

		!
		! If this is a cmc base function then skip to next level
		!
		IF TK_MODULE::CATEGORY = "CMCFUN"
		THEN
			GOTO 3060
		END IF

3040		!
		! Select the type of definition this is
		!
		SELECT TK_RELATION::DEFREF
		!
		! This is a structure
		!
		CASE "3"
			!
			! Store structure name
			!
			LOC_BOMCNT% = LOC_BOMCNT% + 1%
			LOC_BOMEXE$(LOC_BOMCNT%) = LOC_MODNAME$(LOC_MODLOOP%)
			LOC_BOMLVL%(LOC_BOMCNT%) = LEVEL%
			LOC_BOMMOD$(LOC_BOMCNT%) = TK_RELATION::PARENT
			LOC_BOMSTR$(LOC_BOMCNT%) = TK_RELATION::CHILD

			GOTO 3050 IF TK_RELATION::CHILD = &
				LOC_STRLIST$(LOC_STRLOOP%) &
				FOR LOC_STRLOOP% = 1% TO LOC_STRCNT%

			!
			! Item not found, create it
			!
			LOC_LOOP%, LOC_STRCNT% = LOC_STRCNT% + 1%

			WHILE (LOC_LOOP% > 1%) AND &
				(LOC_STRLIST$(LOC_LOOP% - 1%) > &
				TK_RELATION::CHILD)
				LOC_STRLIST$(LOC_LOOP%) = &
				LOC_STRLIST$(LOC_LOOP% - 1%)
				LOC_LOOP% = LOC_LOOP% - 1%
			NEXT

			LOC_STRLIST$(LOC_LOOP%) = TK_RELATION::CHILD

		END SELECT

3050		WHEN ERROR IN
			GET #TK_RELATION.CH%, KEY #0% EQ TK_RELATION::CHILD, REGARDLESS
		USE
			CONTINUE 3060 IF ERR = 155%
			EXIT HANDLER
		END WHEN

		LEVEL% = LEVEL% + 1%
		GOTO GoDownModTree

 GoUpModTree:
3055		!
		LEVEL% = LEVEL% - 1%
		GOTO 3070 IF LEVEL% = 0%

3060		WHEN ERROR IN
			GET #TK_RELATION.CH%, RFA RFA_LEVEL(LEVEL%), REGARDLESS
			GET #TK_RELATION.CH%, REGARDLESS
		USE
			CONTINUE 3055 IF ERR = 155% OR ERR = 11%
			EXIT HANDLER
		END WHEN

		IF TK_RELATION::PARENT <> TEST_MODULE(LEVEL%)
		THEN
			GOTO GoUpModTree
		ELSE
			GOTO GoDownModTree
		END IF

3070		!
		! Try for next record
		!
		WHEN ERROR IN
			GET #TK_RELATION.CH%, RFA RFA_LEVEL(1%), REGARDLESS
		USE
			CONTINUE 3090 IF ERR = 155%
			EXIT HANDLER
		END WHEN

		GOTO 3020

3090	NEXT LOC_MODLOOP%

3100	!*****************************************************************
	! Print brief structure description
	!*****************************************************************
	PRINT #LOC_FILE.CH%, ".page"
	PRINT #LOC_FILE.CH%, "^*Record Structure Table\*"
	PRINT #LOC_FILE.CH%, ".b"
	PRINT #LOC_FILE.CH%, ".lt"

	FOR LOC_STRLOOP% = 1% TO LOC_STRCNT%
		TK_FILE::DESCRIPTION = "?????????????????????????????????"
		TK_FILE::DATABASE = ""
		TK_FILE::DATETYPE = ""
		TK_FILE::DATASIZE = 0%

		!
		! Look for _cdd on end of name
		!
		LOC_CDD% = INSTR(1%, LOC_STRLIST$(LOC_STRLOOP%), "_CDD")

		IF LOC_CDD%
		THEN
			LOC_STRNAME$ = LEFT(LOC_STRLIST$(LOC_STRLOOP%), &
				LOC_CDD% - 1%)
		ELSE
			LOC_STRNAME$ = LOC_STRLIST$(LOC_STRLOOP%)
		END IF

		!
		! Look up structure description
		!
		WHEN ERROR IN
			GET #TK_FILE.CH%, KEY #0% EQ LOC_STRNAME$, REGARDLESS
		USE
			CONTINUE 3110 IF ERR = 155%
			EXIT HANDLER
		END WHEN

3110		LOC_TEXT$ = LEFT(LOC_STRNAME$ + SPACE$(18%), 18%) + " " + &
			LEFT(TK_FILE::DESCRIPTION, 40%) + " " + &
			TK_FILE::DATABASE + " " + &
			FORMAT$(TK_FILE::DATASIZE, "####")

		PRINT #LOC_FILE.CH%, LOC_TEXT$

	NEXT LOC_STRLOOP%

	PRINT #LOC_FILE.CH%, ".el"

3200	!*****************************************************************
	! Structure hierarchy
	!*****************************************************************
	PRINT #LOC_FILE.CH%, ".page"
	PRINT #LOC_FILE.CH%, "^*Database Hierarchy\*"
	PRINT #LOC_FILE.CH%, ".b"
	PRINT #LOC_FILE.CH%, ".lt"

	FOR LOC_STRLOOP% = 1% TO LOC_STRCNT%

		!
		! Look for _cdd on end of name
		!
		LOC_CDD% = INSTR(1%, LOC_STRLIST$(LOC_STRLOOP%), "_CDD")

		IF LOC_CDD%
		THEN
			LOC_STRNAME$ = LEFT(LOC_STRLIST$(LOC_STRLOOP%), &
				LOC_CDD% - 1%)
		ELSE
			LOC_STRNAME$ = LOC_STRLIST$(LOC_STRLOOP%)
		END IF

		!
		! Set structure print test
		!
		LOC_PRINTTEST% = 0%

3210		!
		! Look up foreign structure
		!
		WHEN ERROR IN
			FIND #TK_FOREIGN.CH%, KEY #0% EQ LOC_STRNAME$, REGARDLESS
		USE
			CONTINUE 3290
		END WHEN

3220		!
		! Get foreign structure record
		!
		WHEN ERROR IN
			GET #TK_FOREIGN.CH%, REGARDLESS
		USE
			CONTINUE 3290
		END WHEN


		!
		! Jump out if Module name does not match test
		!
		IF LOC_STRNAME$ <> TK_FOREIGN::STRUCT
		THEN
			GOTO 3290
		END IF

		LEVEL% = 1%

 GoDownStrTree:
		!
		TEST_MODULE(LEVEL%) = TK_FOREIGN::STRUCT
		RFA_LEVEL(LEVEL%) = GETRFA(TK_FOREIGN.CH%)

		!
		! Check to see if this level becomes recursive
		!
		GOTO GoUpStrTree &
			IF TEST_MODULE(LOOP%) = TEST_MODULE(LEVEL%) &
				FOR LOOP% = 1% TO LEVEL% - 1%

3230		!
		! Look up structure description
		!
		IF LOC_PRINTTEST% = 0%
		THEN
			WHEN ERROR IN
				GET #TK_FILE.CH%, &
					KEY #0% EQ TRM$(TK_FOREIGN::STRUCT), &
					REGARDLESS
			USE
				LOC_STRUCTDESC$ = "????????????????????????????????"
				CONTINUE 3240
			END WHEN

			LOC_STRUCTDESC$ = TK_FILE::DESCRIPTION
		END IF

3240		!
		! Look up sub structure description
		!
		LOC_SUBSTRUCTDESC$ = "???????????????????????????????????"

		WHEN ERROR IN
			GET #TK_FILE.CH%, &
				KEY #0% EQ TRM$(TK_FOREIGN::SUBSTRUCT), &
				REGARDLESS

		USE
			CONTINUE 3245
		END WHEN

		LOC_SUBSTRUCTDESC$ = TK_FILE::DESCRIPTION

3245		!
		! Print structure hierarchy
		!
		IF LOC_PRINTTEST% = 0%
		THEN
			LOC_TEXT$ = LEFT(TK_FOREIGN::STRUCT, 18%) + " " + &
				LOC_STRUCTDESC$

			PRINT #LOC_FILE.CH%, LOC_TEXT$
			LOC_PRINTTEST% = -1%
		END IF

		LOC_TEXT$ = SPACE$(10%) + &
			FORMAT$(LEVEL%, "### ") + &
			LEFT(TK_FOREIGN::SUBSTRUCT, 18%) + " " + &
			LOC_SUBSTRUCTDESC$

		PRINT #LOC_FILE.CH%, LOC_TEXT$

3250		WHEN ERROR IN
			GET #TK_FOREIGN.CH%, &
				KEY #0% EQ TK_FOREIGN::SUBSTRUCT, &
				REGARDLESS
		USE
			CONTINUE 3260 IF ERR = 155%
			EXIT HANDLER
		END WHEN

		LEVEL% = LEVEL% + 1%
		GOTO GoDownStrTree

 GoUpStrTree:
3255		!
		LEVEL% = LEVEL% - 1%
		GOTO 3270 IF LEVEL% = 0%

3260		WHEN ERROR IN
			GET #TK_FOREIGN.CH%, RFA RFA_LEVEL(LEVEL%), REGARDLESS

			GET #TK_FOREIGN.CH%, REGARDLESS
		USE
			CONTINUE 3255 IF ERR = 155% OR ERR = 11%
			EXIT HANDLER
		END WHEN

		IF TK_FOREIGN::STRUCT <> TEST_MODULE(LEVEL%)
		THEN
			GOTO GoUpStrTree
		ELSE
			GOTO GoDownStrTree
		END IF

3270		!
		! Try for next record
		!
		WHEN ERROR IN
			GET #TK_FOREIGN.CH%, RFA RFA_LEVEL(1%), REGARDLESS
		USE
			CONTINUE 3290 IF ERR = 155%
			EXIT HANDLER
		END WHEN

		GOTO 3220

3290		IF LOC_PRINTTEST%
		THEN
			PRINT #LOC_FILE.CH%
		END IF

	NEXT LOC_STRLOOP%

	PRINT #LOC_FILE.CH%, ".el"

3300	!*******************************************************************
	! Print bill of modules using structures
	!*******************************************************************
	PRINT #LOC_FILE.CH%, ".page"
	PRINT #LOC_FILE.CH%, "^*Bill of Modules Using Structures\*"
	PRINT #LOC_FILE.CH%, ".b"
	PRINT #LOC_FILE.CH%, ".lt"

	FOR LOC_BOMLOOP% = 1% TO LOC_BOMCNT%
		IF LOC_BOMEXE$(LOC_BOMLOOP%) <> LOC_BOMEXE$(LOC_BOMLOOP% - 1%)
		THEN
			PRINT #LOC_FILE.CH%

			LOC_TEXT$ = LEFT(LOC_BOMEXE$(LOC_BOMLOOP%) + &
				SPACE$(18%), 18%) + &
				FORMAT$(LOC_BOMLVL%(LOC_BOMLOOP%), " #### ") + &
				LEFT(LOC_BOMMOD$(LOC_BOMLOOP%) + &
				SPACE$(18%), 18%) + " " + &
				LEFT(LOC_BOMSTR$(LOC_BOMLOOP%) + &
				SPACE$(18%), 18%)
		ELSE
			LOC_TEXT$ = SPACE$(18%) + &
				FORMAT$(LOC_BOMLVL%(LOC_BOMLOOP%), " #### ") + &
				LEFT(LOC_BOMMOD$(LOC_BOMLOOP%) + &
				SPACE$(18%), 18%) + " " + &
				LEFT(LOC_BOMSTR$(LOC_BOMLOOP%) + &
				SPACE$(18%), 18%)
		END IF

		PRINT #LOC_FILE.CH%, LOC_TEXT$

	NEXT LOC_BOMLOOP%

	PRINT #LOC_FILE.CH%, ".el"

3400	!*******************************************************************
	! Where structures are used
	!*******************************************************************
	PRINT #LOC_FILE.CH%, ".page"
	PRINT #LOC_FILE.CH%, "^*Where Structures are Used in Modules\*"
	PRINT #LOC_FILE.CH%, ".b"
	PRINT #LOC_FILE.CH%, ".lt"

	FOR LOC_STRLOOP% = 1% TO LOC_STRCNT%
		LOC_PRINTTEST% = 0%
		!
		! Loop thru BOM
		!
		FOR LOC_BOMLOOP% = 1% TO LOC_BOMCNT%
			IF LOC_STRLIST$(LOC_STRLOOP%) = &
				LOC_BOMSTR$(LOC_BOMLOOP%)
			THEN
				IF LOC_PRINTTEST%
				THEN
					LOC_TEXT$ = SPACE$(18%) + " " + &
						LEFT(LOC_BOMMOD$(LOC_BOMLOOP%) + &
						SPACE$(18%), 18%)
				ELSE
					LOC_TEXT$ = LEFT(LOC_STRLIST$(LOC_STRLOOP%) + &
						SPACE$(18%), 18%) + " " + &
						LEFT(LOC_BOMMOD$(LOC_BOMLOOP%) + &
						SPACE$(18%), 18%)
				END IF

				LOC_PRINTTEST% = -1%
				PRINT #LOC_FILE.CH%, LOC_TEXT$
			END IF

		NEXT LOC_BOMLOOP%
		!
		! Put in one line feed if text has been printed
		!
		IF LOC_PRINTTEST%
		THEN
			PRINT #LOC_FILE.CH%
		END IF
	NEXT LOC_STRLOOP%

	PRINT #LOC_FILE.CH%, ".el"

3500	!*******************************************************************
	! Structure Composition
	!*******************************************************************
	PRINT #LOC_FILE.CH%, ".page"
	PRINT #LOC_FILE.CH%, "^*Record Structure Composition\*"
	PRINT #LOC_FILE.CH%, ".b"
	PRINT #LOC_FILE.CH%, ".lt"

	FOR LOC_STRLOOP% = 1% TO LOC_STRCNT%
		!
		! Look for _cdd on end of name
		!
		LOC_CDD% = INSTR(1%, LOC_STRLIST$(LOC_STRLOOP%), "_CDD")

		IF LOC_CDD%
		THEN
			LOC_STRNAME$ = LEFT(LOC_STRLIST$(LOC_STRLOOP%), &
				LOC_CDD% - 1%)
		ELSE
			LOC_STRNAME$ = LOC_STRLIST$(LOC_STRLOOP%)
		END IF

		!
		! Look up structure description
		!
		WHEN ERROR IN
			FIND #TK_FILE.CH%, KEY #0% EQ LOC_STRNAME$
		USE
			CONTINUE 3590 IF ERR = 155%
			EXIT HANDLER
		END WHEN

3510		WHEN ERROR IN
			GET #TK_FILE.CH%, REGARDLESS
		USE
			CONTINUE 3590 IF ERR = 11%
			EXIT HANDLER
		END WHEN

		IF LOC_STRNAME$ = TK_FILE::STRUCT
		THEN
			!
			! Print name of record structure
			!
			IF TK_FILE::SEQUENCE = "000"
			THEN
				LOC_TEXT$ = LEFT(LOC_STRNAME$ + &
					SPACE$(18%), 18%) + " " + &
					LEFT(TK_FILE::DESCRIPTION, 40%) + " "
			!
			! Print field name
			!
			ELSE
				LOC_TEXT$ = SPACE$(10%) + &
					TK_FILE::SEQUENCE + " " + &
					LEFT(TK_FILE::FLDNAME, 18%) + " " + &
					LEFT(TK_FILE::DESCRIPTION, 40%)
			END IF

			PRINT #LOC_FILE.CH%, LOC_TEXT$

			GOTO 3510
		END IF

		PRINT #LOC_FILE.CH%

3590	NEXT LOC_STRLOOP%

	PRINT #LOC_FILE.CH%, ".el"

3600	!*******************************************************************
	! Full structure/field description
	!*******************************************************************

	FOR LOC_STRLOOP% = 1% TO LOC_STRCNT%
		!
		! Look for _cdd on end of name
		!
		LOC_CDD% = INSTR(1%, LOC_STRLIST$(LOC_STRLOOP%), "_CDD")

		IF LOC_CDD%
		THEN
			LOC_STRNAME$ = LEFT(LOC_STRLIST$(LOC_STRLOOP%), &
				LOC_CDD% - 1%)
		ELSE
			LOC_STRNAME$ = LOC_STRLIST$(LOC_STRLOOP%)
		END IF

		!
		! Look up structure description
		!
		WHEN ERROR IN
			GET #TK_FILEDICT.CH%, KEY #1% EQ LOC_STRNAME$, REGARDLESS
		USE
			CONTINUE 3690 IF ERR = 155%
			EXIT HANDLER
		END WHEN

		!
		! Set up temporary file name
		!
		LOC_FILEDOCU$ = "TEMP" + READ_SYSJOB + ".TMP"

		CALL TK_SUBR_FILEDOCU(TK_FILEDICT, LOC_FILEDOCU$)

3610		!
		! Open temporary file
		!
		CALL ASSG_CHANNEL(LOC_FILEDOCU.CH%, STAT%)
		WHEN ERROR IN
			OPEN LOC_FILEDOCU$ FOR INPUT AS FILE LOC_FILEDOCU.CH%, &
				RECORDSIZE 255%
		USE
			CONTINUE 3690
		END WHEN

		!
		! Print a new page
		!
		PRINT #LOC_FILE.CH%, ".PAGE"

3620		!
		! Read temporary file
		!
		WHEN ERROR IN
			LINPUT #LOC_FILEDOCU.CH%, LOC_TEXT$
		USE
			CONTINUE 3630 IF ERR = 11%
			EXIT HANDLER
		END WHEN

		PRINT #LOC_FILE.CH%, LOC_TEXT$

		GOTO 3620

3630		!
		! Close temp file
		!
		CLOSE LOC_FILEDOCU.CH%
		CALL ASSG_FREECHANNEL(LOC_FILEDOCU.CH%)

		! Kill temp file
		!
		SMG_STATUS% = LIB$DELETE_FILE( &
			LOC_FILEDOCU$ + ";*")

		IF (SMG_STATUS% AND 1%) = 0%
		THEN
			CALL ENTR_3MESSAGE(SCOPE, "Error deleting OPENS - " &
				+ NUM1$(SMG_STATUS%), 0%)
		END IF

3690	NEXT LOC_STRLOOP%

	CLOSE LOC_FILE.CH%

	!*******************************************************************
	! Finish up file
	!*******************************************************************

 ExitProgram:
	GOTO 20000

	!*******************************************************************
	! Print level titles
	!*******************************************************************

 !	DEF FNLEVEL$(DOT_COUNTER%, TITLE$)
 !
 !	SELECT DOT_COUNTER%
 !
	!
	! Start of a chapter
	!
 !	CASE 0%, 1%
 !		FNLEVEL$ = "\chapter {" + TK_FUNC_TEXSTRING(TITLE$) + "}"
 !
	!
	! Start of a section
	!
 !	CASE 2%
 !		FNLEVEL$ = "\pagebreak[3]\section {" + &
 !			TK_FUNC_TEXSTRING(TITLE$) + "}"
 !
	!
	! Start of a subsection
	!
 !	CASE 3%
 !		FNLEVEL$ = "\pagebreak[2]\subsection {" + &
 !			TK_FUNC_TEXSTRING(TITLE$) + "}"
 !
	!
	! Start of a subsubsection
	!
 !	CASE 4%
 !		FNLEVEL$ = "\pagebreak[1]\subsubsection {" + &
 !			TK_FUNC_TEXSTRING(TITLE$) + "}"
 !
	!
	! Start of a paragraph
	!
 !	CASE 5%
 !		FNLEVEL$ = "\paragraph {" + TK_FUNC_TEXSTRING(TITLE$) + "}"
 !
	!
	! Start of a subparagraph
	!
 !	CASE ELSE
 !		FNLEVEL$ = "\subparagraph {" + TK_FUNC_TEXSTRING(TITLE$) + "}"
 !
 !	END SELECT
 !
 !	FNEND

	%PAGE

20000	END

21100	FUNCTION LONG TK_DOCU_GETMODULES_ALL(MODKEY$, RFA MODRFA)

	!
	! This function graps the names passed to it from the
	! LIB$SEARCH call
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	MAP (TK_DOCUALL) &
		MODCOUNTALL%, &
		MODNAMEALL$(1500%) = 50%, &
		MODFLAGALL%(1500%)

	!
	! Don't add name if already have a key to that item,
	! or we have amassed too many items already.
	!
	GOTO ExitFunction &
		IF MODCOUNTALL% >= 1500%

	!
	! Add to list
	!
	MODCOUNTALL% = MODCOUNTALL% + 1%
	MODNAMEALL$(MODCOUNTALL%) = MODKEY$
	MODFLAGALL%(MODCOUNTALL%) = 0%


 ExitFunction:
	TK_DOCU_GETMODULES_ALL = 1%

	END FUNCTION
