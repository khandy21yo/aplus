1	%TITLE "Element Definition Maintenance"
	%SBTTL "TK_MAIN_ELEMENT"
	%IDENT "V3.6a Calico"

	FUNCTION LONG TK_MAIN_ELEMENT(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1987 BY
	!
	! Computer Management Center
	! Idaho Falls, Idaho.
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
	!	Maintains the Element definition file.
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS TK_SOURCE:TK_MAIN_ELEMENT/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN TK_MAIN_ELEMENT
	!	$ DELETE TK_MAIN_ELEMENT.OBJ;*
	!
	! Author:
	!
	!	09/10/87 - Kevin Handy
	!
	! Modification history:
	!
	!	02/13/88 - Frank F. Starman
	!		A new File layout of TK_ELEMENT
	!
	!	08/05/91 - Kevin Handy
	!		Modified to use ACCESS READ on opens so they
	!		don't mark file as having been modified.
	!
	!	06/11/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/14/96 - Kevin Handy
	!		Lose extra '&' before 'than'.
	!		Reformat source code.
	!
	!	10/29/96 - Kevin Handy
	!		Clean up (Check)
	!
	!	06/04/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/28/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/10/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	12/12/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[TK.OPEN]TK_ELEMENT.HB"
	MAP (TK_ELEMENT)	TK_ELEMENT_CDD	TK_ELEMENT
	MAP (TK_ELEMENT_OLD)	TK_ELEMENT_CDD	TK_ELEMENT_OLD, TK_ELEMENT2

	COM (CH_TK_ELEMENT) &
		TK_ELEMENT.CH%, &
		DATA_TYPE%, &
		DATA_TYPE$(30%) = 20%, &
		DATA_FIELD_SIZE$(30%) = 4%

	%PAGE

	ON ERROR GOTO 29000

	DATA_TYPE_FILE$ = "CMC:TK_DATA_TYPE.TEMPLATE"

	SELECT MOPTION

	CASE OPT_INIT
		!
		! Define window
		!
		SMG_WINDOW::DESCR = "Element definition maintenance"
		SMG_WINDOW::NHELP = "TK_MAIN_ELEMENT"
		SMG_WINDOW::HSIZE = 78%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HVIEW = 130%
		SMG_WINDOW::VVIEW = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::NITEMS= 5%
		SMG_WINDOW::FLAGS = 0%

		SMG_WINDOW::NKEYS = 2%
		SMG_WINDOW::KNAME(0%) = "Element_name"
			SMG_WINDOW::KFIELD(0%, 0%) = 1%
			SMG_WINDOW::KFIELD(0%, 1%) = 1%
		SMG_WINDOW::KNAME(1%) = "DateType"
			SMG_WINDOW::KFIELD(1%, 0%) = 2%
			SMG_WINDOW::KFIELD(1%, 1%) = 3%
			SMG_WINDOW::KFIELD(1%, 2%) = 4%


		!
		! Load in defaults for chart
		!
		CALL READ_DEFAULTS(SMG_WINDOW)

700		!
		! Get info required for main file
		!
		GOTO 750 IF TK_ELEMENT.CH% > 0%

		GOSUB LoadTemplateArrays

		!
		! Open main file (existing) for modification
		!
		%INCLUDE "SOURCE:[TK.OPEN]TK_ELEMENT.CRE"

750		SMG_WINDOW::CHAN  = TK_ELEMENT.CH%

		WHEN ERROR IN
			RESET #TK_ELEMENT.CH%
			GET #TK_ELEMENT.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

	!
	! Display the background
	!
20100	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		DATA	5, 05, "(01) Element Name", &
			6, 05, "(02) Description", &
			7, 05, "(03) Date Type", &
			8, 05, "(04) Size", &
			9, 05, "(05) Test Structure", &
			0,  0, ""

		RESTORE

		READ XPOS%, YPOS%, XSTR$
		I% = 0%
		WHILE (XPOS% <> 0%)
			I% = I% + 1%
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				XSTR$, XPOS%, YPOS%) &
				IF (SMG_WINDOW::HFLAG(I%) AND 2%) = 0%
			READ XPOS%, YPOS%, XSTR$
		NEXT

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

	!
	! Enter/Display/Default
	!
20200	CASE OPT_ENTRY
		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View"

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

 Reenter:	SELECT MLOOP

		CASE 1%
			TK_ELEMENT::ELEMENT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "5;30", TEMP$, &
				TK_ELEMENT::ELEMENT, MFLAG, "'E", MVALUE)

		CASE 2%
			TK_ELEMENT::DESCR = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "6;30", TEMP$, &
				TK_ELEMENT::DESCR, MFLAG, "'E", MVALUE)

		CASE 3%
			TK_ELEMENT::ETYPE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "7;30", TEMP$, &
				TK_ELEMENT::ETYPE, MFLAG, "'E", MVALUE)

			IF SCOPE::SCOPE_EXIT = SMG$K_TRM_F14
			THEN
				X% = ENTR_3CHOICE(SCOPE, "", "", DATA_TYPE$(), &
					TK_ELEMENT::ETYPE, 8% + 64%, &
					" DATA TYPES ", "", 0%)

				TK_ELEMENT::ETYPE = DATA_TYPE$(X%) IF X% > 0%
				GOTO Reenter
			END IF

		CASE 4%
			TK_ELEMENT::ESIZE = ENTR_3NUMBER(SCOPE, &
				SMG_WINDOW::WNUMBER, "8;30", TEMP$, &
				TK_ELEMENT::ESIZE * 1.0, MFLAG, "#####", MVALUE)

		CASE 5%
			TK_ELEMENT::TESTSTRUCT = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, "9;30", TEMP$, &
				TK_ELEMENT::TESTSTRUCT, MFLAG, "'E", MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

20300	CASE OPT_TESTENTRY
		TK_MAIN_ELEMENT = 0%

		SELECT MLOOP

		CASE 1%
			IF TK_ELEMENT::ELEMENT = ""
			THEN
				TK_MAIN_ELEMENT = 1%
			ELSE
				IF (MVALUE = "ADD")
				THEN
					WHEN ERROR IN
						GET #TK_ELEMENT.CH%, &
							KEY #0% EQ TK_ELEMENT::ELEMENT + "", &
							REGARDLESS
					USE
						CONTINUE 32767 IF ERR = 155%
						EXIT HANDLER
					END WHEN

					TK_MAIN_ELEMENT = 2%
					CALL ENTR_3MESSAGE(SCOPE, &
						"Record Already Exists", 1%)
				END IF
			END IF

		CASE 3%

			IF TK_ELEMENT::ETYPE <> ""
			THEN
				TEMP% = -1%
				TEMP% =  0% IF TK_ELEMENT::ETYPE = &
					TRM$(DATA_TYPE$(LOOP%)) &
						FOR LOOP% = 1% TO DATA_TYPE%

				IF TEMP%
				THEN
					X% = ENTR_3CHOICE(SCOPE, "", "", &
						DATA_TYPE$(), &
						TK_ELEMENT::ETYPE, &
						8% + 64%, " DATA TYPES ", &
						"", 0%)

					TK_ELEMENT::ETYPE = &
						DATA_TYPE$(X%) IF X% > 0%
					TK_MAIN_ELEMENT = 1%
					GOTO ExitFunction
				END IF
			END IF

			FOR LOOP% = 1% TO DATA_TYPE%
				IF DATA_TYPE$(LOOP%) = TK_ELEMENT::ETYPE AND &
					TK_ELEMENT::ETYPE <> "TEXT"
				THEN
					TK_ELEMENT::ESIZE = &
						VAL%(DATA_FIELD_SIZE$(LOOP%))

					TK_ELEMENT::ESIZE = ENTR_3NUMBER(SCOPE, &
						SMG_WINDOW::WNUMBER, "8;25", TEMP$, &
						TK_ELEMENT::ESIZE * 1.0, 1%, &
						"#####", MVALUE)
				END IF
			NEXT LOOP%

		END SELECT

20500	CASE OPT_SETOLD
		TK_ELEMENT_OLD = TK_ELEMENT

	CASE OPT_RESETOLD
		TK_ELEMENT = TK_ELEMENT_OLD

	CASE OPT_SETDEFAULT
		TK_ELEMENT2 = TK_ELEMENT

	CASE OPT_RESETDEFAULT
		TK_ELEMENT = TK_ELEMENT2

	CASE OPT_VIEW
		SELECT MLOOP

		CASE 1%
			MVALUE = "  ElementName                            " + &
				"Description                               " + &
				"Datatype"

		CASE 2%
			MVALUE = "041,083"

		CASE 3%
			MVALUE = TK_ELEMENT::ELEMENT + " " + &
				LEFT$(TK_ELEMENT::DESCR, 40%) + " " + &
				TK_ELEMENT::ETYPE

		END SELECT

	CASE OPT_FIND
		SELECT MLOOP
		CASE 0%
			FIND #TK_ELEMENT.CH%, &
				KEY #0% GE TK_ELEMENT::ELEMENT + "", &
				REGARDLESS
		CASE 1%
			FIND #TK_ELEMENT.CH%, &
				KEY #1% GE TK_ELEMENT::ETYPE + &
				TK_ELEMENT::ELEMENT, &
				REGARDLESS
		END SELECT

	END SELECT

 ExitFunction:
	EXIT FUNCTION

 LoadTemplateArrays:

28020	!
	CALL ASSG_CHANNEL(TEXT_FILE.CH%, STAT%)

	WHEN ERROR IN
		OPEN DATA_TYPE_FILE$ FOR INPUT AS FILE TEXT_FILE.CH%, &
			ACCESS READ, ALLOW MODIFY, &
			RECORDSIZE 132%
	USE
		CONTINUE 28040
	END WHEN

	DATA_TYPE% = 0%

28030	!
	WHEN ERROR IN
		LINPUT #TEXT_FILE.CH%, INP$
	USE
		CONTINUE 28040
	END WHEN

	INP$ = EDIT$(INP$, 4% + 8% + 16% + 32% + 128%)

	IF INP$ <> ""
	THEN
		TEMP% = INSTR(1%, INP$, "SIZE>")
		IF TEMP%
		THEN
			DATA_TYPE% = DATA_TYPE% + 1%
			DATA_FIELD_SIZE$(DATA_TYPE%) = RIGHT(INP$, TEMP% + 5%)
			DATA_TYPE$(DATA_TYPE%) = TRM$(LEFT(INP$, TEMP% - 1%))
		END IF
	END IF

	GOTO 28030

28040	!
	CLOSE TEXT_FILE.CH%

	DATA_TYPE$(0%) = NUM1$(DATA_TYPE%)

	CALL ASSG_FREECHANNEL(TEXT_FILE.CH%)

	RETURN

29000	!
	ON ERROR GO BACK

32767	END FUNCTION
