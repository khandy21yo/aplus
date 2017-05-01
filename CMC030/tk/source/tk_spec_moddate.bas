1	%TITLE "Read Dates Modified with Descriptions"
	%SBTTL "TK_SPEC_MODDATE"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1987, 1988 BY
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
	! ID:TK020
	!
	! Abstract:HELP
	!	.p
	!	Read Dates Modified with Descriptions
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS TK_SOURCE:TK_SPEC_MODDATE/LINE
	!	$ LINK/EXECUTABLE=TK_EXE: TK_SPEC_MODDATE, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE TK_SPEC_MODDATE.OBJ;*
	!
	! Author:
	!
	!	07/11/89 - J. Shad Rydalch
	!
	! Modification history:
	!
	!	08/05/91 - Kevin Handy
	!		Added ACCESS READ to open statements.
	!
	!	06/14/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	06/23/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	10/29/96 - Kevin Handy
	!		Reformat source code
	!
	!	08/25/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	09/11/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!		Change "[AP.-]" to "[000000]"
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE		UTL_REPORTX_CDD	UTL_REPORTX

	%INCLUDE "SOURCE:[TK.OPEN]TK_MODDATE.HB"
	MAP	(TK_MODDATE) TK_MODDATE_CDD TK_MODDATE

	!
	! Dimension variables
	!
	DIM FILE_NAME$(1000%), DIR_NAME$(100%)

	!
	! Handle output file
	!
	COM_FILE.CH% = 5%
	READ_FILE.CH% = 6%

	ON ERROR GOTO 19000

	!
	! Initilize Report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	SORT_BY$ = EDIT$(UTL_REPORTX::OPTDEF(0%), -1%)
	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)
	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)
	WLDCRD$	= EDIT$(UTL_REPORTX::OPTDEF(3%), 132%)

	DIRECT$ = EDIT$(UTL_REPORTX::OPTDEF(5%), -1%)
	MODULE$ = EDIT$(UTL_REPORTX::OPTDEF(6%), -1%)
	MOD_ONLY$ = EDIT$(UTL_REPORTX::OPTDEF(7%), -1%)

	SELECT SORT_BY$
	CASE "A"
		KEY_NUM% = 0%
		ADD_TITLE$ = "Author"

	CASE "P"
		KEY_NUM% = 1%
		ADD_TITLE$ = "Program  Name"

	END SELECT

300	%INCLUDE "SOURCE:[TK.OPEN]TK_MODDATE.TEN"

 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "Check  Modification  History  by  " + ADD_TITLE$
	TITLE$(2%) = "TK System"
	TITLE$(3%) = ""

	!
	! Heading
	!	      123456789012345678901234567890 1234567890
	!		 1234567890123456789012345678901234567890
	TITLE$(4%) = "Program Author                 Date Created " + &
			"Program Name                             "

	!	           123456789012345678901234567890 1234567890
	!		 1234567890123456789012345678901234567890
	!		 1234567890123456789012345678901234567890
	TITLE$(5%) = "     Program Editor                 Date Modified " + &
			"Description of Modification"
	TITLE$(6%) = "."

	CALL FIND_FILE("SOURCE:[000000]*.DIR", DIR_NAME$(), 16%, "", "")
	DIR_LOOP% = VAL%(DIR_NAME$(0%))

500	!
	! Write to report file
	!
	! Look up one file
	!
	FOR J% = 1% TO DIR_LOOP%

		GOTO NextJ IF COMP_STRING(DIR_NAME$(J%), DIRECT$) = 0%

		PREFIX$ = "SOURCE:[" + DIR_NAME$(J%) + ".SOURCE]"
		CALL FIND_FILE(PREFIX$ + TRM$(MODULE$) + ".BAS", &
			FILE_NAME$(), 16%, "", "")

		I_LOOP% = VAL%(FILE_NAME$(0%))

		FOR I% = 1% TO I_LOOP%
505			OPEN PREFIX$ + FILE_NAME$(I%) + ".BAS" &
				FOR INPUT AS FILE READ_FILE.CH%, &
				ACCESS READ, ALLOW MODIFY

			START_FLAG% = 0%
			MOD_HISTORY$ = "N"

			TK_MODDATE::PROG_NAME = PREFIX$ + FILE_NAME$(I%) + &
				".BAS"
			CALL ENTR_3MESSAGE &
				(SCOPE, "Extacting from: " + &
				TK_MODDATE::PROG_NAME, 1%)

 GetLine:
510			WHEN ERROR IN
				LINPUT #READ_FILE.CH%, TEXT$
			USE
				CONTINUE 520
			END WHEN

			TEXT$ = EDIT$(TEXT$, 8% + 16% + 128%)
			IF TEXT$ = "!"
			THEN
				HAVE_MOD$ = "N"
				GOTO GetLine
			END IF
			GOTO 520 IF TEXT$ = "!--"
			IF TEXT$ = "!++"
			THEN
				START_FLAG% = 1%
				GOTO GetLine
			END IF
			GOTO GetLine IF START_FLAG% = 0%

			SELECT EDIT$(TEXT$, 32%)
			CASE "! ABSTRACT:"
				SELECT_ITEM% = 1%

			CASE "! INDEX:"
				SELECT_ITEM% = 2%

			CASE "! INPUT:"
				SELECT_ITEM% = 3%

			CASE "! OUTPUT:"
				SELECT_ITEM% = 4%

			CASE "! EXAMPLE:"
				SELECT_ITEM% = 5%

			CASE "! ENVIRONMENT:"
				SELECT_ITEM% = 6%

			CASE "! COMPILE:"
				SELECT_ITEM% = 7%

			CASE "! AUTHOR:"
				SELECT_ITEM% = 8%

			CASE "! MODIFICATION HISTORY:"
				SELECT_ITEM% = 9%

			END SELECT

			SELECT SELECT_ITEM%

			CASE 8%
				IF EDIT$(TEXT$, 32%) <> "! AUTHOR:"
				THEN
					SPLIT% = INSTR(1%, TEXT$, "-")
					TK_MODDATE::DATE = &
						DATE_STOREDATE(MID(TEXT$, 2%, SPLIT% - 2%))
					TK_MODDATE::PROG_EDITOR = RIGHT(TEXT$, SPLIT% + 2%)
				END IF

				TK_MODDATE::MOD_EDITOR	= SPACE$(30%)
				TK_MODDATE::MOD_DATE	= SPACE$(8%)
				TK_MODDATE::MOD_DESCRIPTION = SPACE$(40%)

				COUNTER% = 0%

			CASE 9%
			IF EDIT$(TEXT$, 32%) <> "! MODIFICATION HISTORY:"
			THEN
				IF HAVE_MOD$ = "N"
				THEN
					SPLIT% = INSTR(1%, TEXT$, "-")
					TK_MODDATE::MOD_DATE = &
					DATE_STOREDATE(MID(TEXT$, 2%, SPLIT% - 2%))
					TK_MODDATE::MOD_EDITOR = RIGHT(TEXT$, SPLIT% + 2%)
					HAVE_MOD$ = "Y"
				ELSE

					COUNTER% = COUNTER% + 1%

					TK_MODDATE::XX_FIELD = FORMAT$(COUNTER%, &
						"<0>##")

					TK_MODDATE::MOD_DESCRIPTION = &
						RIGHT(TEXT$, 2%)

					PUT #TK_MODDATE.CH%
					MOD_HISTORY$ = "Y"
				END IF
				END IF
			END SELECT

			GOTO GetLine

520			CLOSE READ_FILE.CH%

			IF MOD_ONLY$ = "N"
			THEN
				PUT #TK_MODDATE.CH% IF MOD_HISTORY$ = "N"
			END IF

525		NEXT I%

 NextJ:
	NEXT J%

17000	!***********************************************************************
	! OUTPUT REPORT Print report file created above
	!***********************************************************************

	IF FROM_ITEM$ = ""
	THEN
		RESET #TK_MODDATE.CH%, KEY #KEY_NUM%
	ELSE
		FIND #TK_MODDATE.CH%, KEY #KEY_NUM% GE FROM_ITEM$
	END IF

	CHECK_KEY$ = ""

 GetNextRec:
17020	!
	! Main loop starts here
	!
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Get next record
	!
	WHEN ERROR IN
		GET #TK_MODDATE.CH%
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 1%
			RETRY
		END IF

		CONTINUE ExitTotal
	END WHEN

	!
	! Check current record
	!
	SELECT SORT_BY$
	CASE "A"
		GOTO ExitTotal IF (TK_MODDATE::PROG_EDITOR > TO_ITEM$) &
			AND (TO_ITEM$ <> "")
		GOTO GetNextRec IF COMP_STRING &
			(EDIT$(TK_MODDATE::PROG_EDITOR, -1%), WLDCRD$) &
			AND WLDCRD$ <> ""
	CASE "P"
		GOTO ExitTotal IF (TK_MODDATE::PROG_NAME > TO_ITEM$) &
			AND (TO_ITEM$ <> "")
		GOTO GetNextRec IF COMP_STRING &
			(EDIT$(TK_MODDATE::PROG_NAME, -1%), WLDCRD$) &
			AND WLDCRD$ <> ""
	END SELECT

	IF (TK_MODDATE::PROG_EDITOR + &
		TK_MODDATE::DATE + &
		TK_MODDATE::PROG_NAME <> CHECK_KEY$)
	THEN
		!
		! Print key
		!
		TEXT$ = TK_MODDATE::PROG_EDITOR	+ " " + &
			PRNT_DATE(TK_MODDATE::DATE, 8%)	+ "   " + &
			TK_MODDATE::PROG_NAME

		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
		GOTO Exitprogram IF UTL_REPORTX::STAT

		OLD_MOD_EDITOR$ = "..."
	END IF

	IF TK_MODDATE::MOD_EDITOR <> SPACE$(LEN(TK_MODDATE::MOD_EDITOR))
	THEN
		!
		! Print modification history if any
		!
		IF (TK_MODDATE::MOD_EDITOR + TK_MODDATE::MOD_DATE = &
			OLD_MOD_EDITOR$)
		THEN
			TEXT$ = SPACE$(50%) + TK_MODDATE::MOD_DESCRIPTION

		ELSE
			TEXT$ = "     " + TK_MODDATE::MOD_EDITOR + " " + &
				PRNT_DATE(TK_MODDATE::MOD_DATE, 8%) + "    " + &
				TK_MODDATE::MOD_DESCRIPTION
		END IF

		CALL OUTP_LINE("", UTL_REPORTX,TITLE$(), TEXT$, 0%)
		GOTO Exitprogram IF UTL_REPORTX::STAT

		OLD_MOD_EDITOR$ = TK_MODDATE::MOD_EDITOR + TK_MODDATE::MOD_DATE

	END IF

	CHECK_KEY$ = TK_MODDATE::PROG_EDITOR + &
		TK_MODDATE::DATE + &
		TK_MODDATE::PROG_NAME

17350	!
	! Try for next record
	!
	GOTO GetNextRec

 ExitTotal:
17400	!
	!
	!

 ExitProgram:
18000	!*******************************************************************
	! Exit program
	!*******************************************************************
	CLOSE #COM_FILE.CH%
	CLOSE #READ_FILE.CH%

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
	!*******************************************************************
	! Help Message for an Error
	!*******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	UTL_REPORTX::STAT = -1%

	GOTO ExitProgram

19000	!*******************************************************************
	! Error trapping
	!*******************************************************************

	FILENAME$ = "TK_SPEC_MODDATE"
	RESUME HelpError

32767	END
