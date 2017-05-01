1	%TITLE "Transfer and Update Files"
	%SBTTL "UT_SPEC_TRANSFER"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1990 BY
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
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This program transfers data sent from a satellite
	!	location into this locations database.
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS UTL_SOURCE:UT_SPEC_TRANSFER/LINE
	!	$ LINK/EXE=UTL_EXE: UT_SPEC_TRANSFER, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE UT_SPEC_TRANSFER.OBJ;*
	!
	! Author:
	!
	!	11/18/93 - Frank F. Starman
	!
	! Modification history:
	!
	!	01/05/94 - Kevin Handy
	!		Modified open for line file so that it will
	!		create line file if it doesn't exist so transfer
	!		will not die.
	!
	!	01/24/94 - Kevin Handy
	!		Clean up (Check)
	!
	!	08/24/94 - Kevin Handy
	!		Add log file to keep track of when transfers
	!		actually run, and what gets transfered.
	!
	!	08/29/94 - Kevin Handy
	!		Added a line to log the starting date & time
	!		in case no files transfer down.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 Calico coding standards
	!
	!	07/17/95 - Kevin Handy
	!		Adjust source closer to 80 columns.
	!
	!	07/28/95 - Kevin Handy
	!		Clean up (Check)
	!
	!	07/16/96 - Kevin Handy
	!		Reformat source code.
	!
	!	02/14/97 - Kevin Handy
	!		Added a few 'REGARDLESS's
	!		Bring closer to 80 columns.
	!
	!	08/28/97 - Kevin Handy
	!		Change 'val' to 'val%'
	!
	!	11/10/97 - Kevin Handy
	!		If IC_COMTROL file is locked, wait for it
	!		to unlock.
	!
	!	06/10/98 - Kevin Handy
	!		Handle cases where the files were transfered
	!		badly by skipping over that file and trying
	!		the next one.
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/20/98 - Kevin Handy
	!		Lose IC_JOURCOUNT map, which was never used.
	!
	!	06/09/99 - Kevin Handy
	!		Lose lines 1600, 1610 (Dead code)
	!
	!	10/19/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	!
	! Map statements
	!
	%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERJOUR.HB"
	MAP (OE_ORDERJOUR)	OE_ORDERJOUR_CDD	OE_ORDERJOUR

	%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERLINE.HB"
	MAP (OE_ORDERLINE)	OE_ORDERLINE_CDD	OE_ORDERLINE

	%INCLUDE "SOURCE:[IC.OPEN]IC_CONTROL.HB"
	MAP (IC_CONTROL)	IC_CONTROL_CDD		IC_CONTROL

	%INCLUDE "SOURCE:[IC.OPEN]IC_CYCLEJOUR.HB"
	MAP (IC_CYCLEJOUR)	IC_CYCLEJOUR_CDD	IC_CYCLEJOUR

	%INCLUDE "SOURCE:[IC.OPEN]IC_JOURADJUST.HB"
	MAP (IC_JOURADJUST)	IC_JOURADJUST_CDD	IC_JOURADJUST

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"
	DECLARE			UTL_REPORTX_CDD		UTL_REPORTX

	DECLARE LONG	EXIT_STATUS
	DECLARE STRING	TITLE(10%)
	DECLARE STRING	BATCH_NUMBER

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION ASSG_POSTBATCH
	EXTERNAL LONG	FUNCTION IC_WRIT_35BALANCE

	DIM ARRAY_FILE$(100%)
	DIM ARRAY_NEW_FILE$(100%)

	%PAGE

	ON ERROR GOTO 19000

	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)
	GOTO Aborted IF UTL_REPORTX::STAT

	TITLE(1%) = "DAILY TRANSFER PROTOCOL"
	TITLE(2%) = "Utility System"
	TITLE(3%) = ""

	!
	! Heading
	!
	TITLE(4%) = "."

	!
	! Open up secret log file to keep track of what files have been
	! transfered to the main system, and on what date.
	!
	CALL ASSG_CHANNEL(LOGFILE.CH%, EXIT_STATUS%)

	OPEN "FILERECEIVED.TXT" AS FILE LOGFILE.CH%, &
		ACCESS APPEND

	PRINT #LOGFILE.CH%, DATE$(0%); " "; TIME$(0%); " "; &
		"Transfer Started"

	!******************************************************************
	! Check if process has been interrupted
	!******************************************************************
	!
	! Open up batch control file and check if interrupted
	!
	EXIT_STATUS = ASSG_POSTBATCH(OPT_RESTART, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "UT_TRANSFER", "", "", "")

	%PAGE

300	!
	! Open IC control file
	!
	WHEN ERROR IN
		%INCLUDE "SOURCE:[IC.OPEN]IC_CONTROL.OPN"
		GET #IC_CONTROL.CH%, RECORD 1%, REGARDLESS
	USE
		IF ERR = 138%
		THEN
			SLEEP 30%
			CONTINUE 300
		END IF

		FILENAME$ = "IC_CONTROL"
		CONTINUE HelpError
	END WHEN

	CUR.PERIOD$, YYYYPP$ = IC_CONTROL::PERIOD

	EXIT_STATUS = CMC$_NORMAL

	SELECT IC_CONTROL::CONTROLFLAG

	CASE "0"
		!
		! the control flag seems to be O.K.
		!

	CASE ELSE
		TEXT$ = "%IC Control flag = " + IC_CONTROL::CONTROLFLAG

		CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
		EXIT_STATUS = CMC$_WARNING

	END SELECT

	!******************************************************************
	! Assign batch number
	!******************************************************************
	!
	! Open up batch control file and get a batch number
	!
	GOTO Aborted IF ASSG_POSTBATCH(OPT_ASSIGN, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "UT_TRANSFER", "TRANSFER", &
		"", "") <> CMC$_NORMAL

	GOTO Aborted IF EXIT_STATUS = CMC$_WARNING

	!*******************************************************************
	! Do the OE_ORDERJOUR files
	!*******************************************************************

	OE_ORDERJOUR.DEV$ = "CMC_INBOX:"
	OE_ORDERLINE.DEV$ = "CMC_INBOX:"

	CALL FIND_FILE(OE_ORDERJOUR.DEV$ + "OE_ORDERJOUR_*_%%.JRL", &
		ARRAY_FILE$(), 16%, "", "")

	ARRAY_FILE% = VAL%(ARRAY_FILE$(0%))

	FOR LOOP% = 1% TO ARRAY_FILE%

		BATCH_NO$ = EDIT$(RIGHT(ARRAY_FILE$(LOOP%), 14%), -1%)
		RECORDS% = 0%

1000		!
		! Remember that we sent this file
		!
		PRINT #LOGFILE.CH%, DATE$(0%); " "; TIME$(0%); " "; &
			OE_ORDERJOUR.NAME$

		!
		! Open journal file
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERJOUR.OPN"
		USE
			FILENAME$ = "OE_ORDERJOUR"
			PRINT #LOGFILE.CH%, DATE$(0%); " "; TIME$(0%); " "; &
				"*** This file is BAD ***"
			CONTINUE 1390
		END WHEN

1010		!
		! Remember that we sent this file
		!
		PRINT #LOGFILE.CH%, DATE$(0%); " "; TIME$(0%); " "; &
			OE_ORDERLINE.NAME$

		!
		! Open journal line file
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[OE.OPEN]OE_ORDERLINE.CRE"
		USE
			FILENAME$ = "OE_ORDERLINE"
			PRINT #LOGFILE.CH%, DATE$(0%); " "; TIME$(0%); " "; &
				"*** This file is BAD ***"
			CONTINUE 1390
		END WHEN

		!
		! Initialize the posting
		!
		EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "OE_ORDERJOUR_" + BATCH_NO$ + &
			".JRL", "", "", "")

		EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", "",  "")

		RESET #OE_ORDERJOUR.CH%

1100		WHEN ERROR IN
			GET #OE_ORDERJOUR.CH%, REGARDLESS
		USE
			CONTINUE 1300 IF ERR = 11%
			FILENAME$ = "OE_ORDERJOUR"
			CONTINUE HelpError
		END WHEN

1150		WHEN ERROR IN
			FIND #OE_ORDERLINE.CH%, KEY #0% EQ OE_ORDERJOUR::ORDNUM
		USE
			CONTINUE 1100 IF ERR = 155% OR ERR = 11%
			FILENAME$ = "OE_ORDERLINE"
			CONTINUE HelpError
		END WHEN

 Orderline:
		WHEN ERROR IN
			GET #OE_ORDERLINE.CH%, REGARDLESS
		USE
			CONTINUE 1100 IF ERR = 155% OR ERR = 11%
			FILENAME$ = "OE_ORDERLINE"
			CONTINUE HelpError
		END WHEN

		GOTO 1100 IF OE_ORDERLINE::ORDNUM <> OE_ORDERJOUR::ORDNUM

		CALL SUBR_TRANTYPE(OE_ORDERJOUR::CUSNUM,OE_ORDERLINE::LIN, &
			OE_ORDERLINE::ORDQTY, &
			OE_ORDERLINE::SHPQTY, OE_ORDERLINE::BCKQTY, &
			TRANTYPE$(), TRANQTY())

		FOR I% = 1% TO VAL%(TRANTYPE$(0%))

			V% = IC_WRIT_35BALANCE(OE_ORDERLINE::PRODUCT, &
				OE_ORDERJOUR::LOCATION, &
				LEFT(TRANTYPE$(I%), 2%), TRANQTY(I%))

			IF LEFT(TRANTYPE$(I%), 2%) = "TR"
			THEN
				V% = IC_WRIT_35BALANCE(OE_ORDERLINE::PRODUCT, &
					OE_ORDERJOUR::SHIPLIN, &
					LEFT(TRANTYPE$(I%), 2%), -TRANQTY(I%))
			END IF
		NEXT I%
		RECORDS% = RECORDS% + 1%

		GOTO Orderline

1300		CLOSE OE_ORDERJOUR.CH%
		CLOSE OE_ORDERLINE.CH%

		EXIT_STATUS = ASSG_POSTBATCH(OPT_CLOSEFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", "",  "")

		OE_ORDERJOUR.DEV$ = ""
		CALL READ_DEVICE("OE_ORDERJOUR",OE_ORDERJOUR.DEV$, STAT%)

		CALL FIND_FILE(OE_ORDERJOUR.DEV$ + "OE_ORDERJOUR_*_%%.JRL", &
			ARRAY_NEW_FILE$(), 16%, "", "")

		FILE% = VAL%(ARRAY_NEW_FILE$(0%))
		NEW_BATCH_NO$ = BATCH_NO$

1310		FOR I% = 1% TO FILE%

			IF NEW_BATCH_NO$ = EDIT$(RIGHT(ARRAY_NEW_FILE$(I%), &
				14%), -1%)
			THEN
				DELKA% = LEN(BATCH_NO$)
				NEW_BATCH_NO$ = LEFT(BATCH_NO$, DELKA% - 2%) + &
					FORMAT$(I% + 1%, "<0>#")
				GOTO 1310
			END IF
		NEXT I%

		NAME "CMC_INBOX:OE_ORDERJOUR_" + BATCH_NO$ + ".JRL" AS &
			OE_ORDERJOUR.DEV$ + &
			"OE_ORDERJOUR_" + NEW_BATCH_NO$ + ".JRL"
		NAME "CMC_INBOX:OE_ORDERLINE_" + BATCH_NO$ + ".JRL" AS &
			OE_ORDERJOUR.DEV$ + &
			"OE_ORDERLINE_" + NEW_BATCH_NO$ + ".JRL"

		IF BATCH_NO$ <> NEW_BATCH_NO$
		THEN
			EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, &
				BATCH_NUMBER, &
				TITLE(), UTL_REPORTX, &
				"OE_ORDERJOUR_" + NEW_BATCH_NO$ + &
				".JRL", "", "", "")
			TEXT$ = SPACE$(9%) + FORMAT$(RECORDS%, "######## ") + &
				"Line records in journal and file renamed"
			CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
		ELSE
			TEXT$ = SPACE$(9%) + FORMAT$(RECORDS%, "######## ") + &
				"Line records in journal"

			CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
		END IF

1390		OE_ORDERJOUR.DEV$ = "CMC_INBOX:"

		!
		! In case of error, make sure they are really closed
		!
		CLOSE OE_ORDERJOUR.CH%
		CLOSE OE_ORDERLINE.CH%

	NEXT LOOP%

	!*******************************************************************
	! Do the cycle count journals
	!*******************************************************************
	IC_CYCLEJOUR.DEV$ = "CMC_INBOX:"
	IC_JOURADJUST.DEV$ = "CMC_INBOX:"

	CALL FIND_FILE(IC_CYCLEJOUR.DEV$ + "IC_CYCLEJOUR_*.JRL", &
		ARRAY_FILE$(), 16%, "", "")

	ARRAY_FILE% = VAL%(ARRAY_FILE$(0%))

	FOR LOOP% = 1% TO ARRAY_FILE%

		BATCH_NO$ = EDIT$(MID(ARRAY_FILE$(LOOP%), 14%, 2%), -1%)
		RECORDS% = 0%

1400		!
		! Remember that we sent this file
		!
		PRINT #LOGFILE.CH%, DATE$(0%); " "; TIME$(0%); " "; &
			IC_CYCLEJOUR.NAME$

		!
		! Open journal file
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[IC.OPEN]IC_CYCLEJOUR.OPN"
		USE
			FILENAME$ = "IC_CYCLEJOUR"
			PRINT #LOGFILE.CH%, DATE$(0%); " "; TIME$(0%); " "; &
				"*** This file is BAD ***"
			CONTINUE 1690
		END WHEN

1410		!
		! Open journal line file
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[IC.OPEN]IC_JOURADJUST.OPN"
		USE
			FILENAME$ = "IC_JOURADJUST"
			PRINT #LOGFILE.CH%, DATE$(0%); " "; TIME$(0%); " "; &
				"*** This file is BAD ***"
			CONTINUE 1690
		END WHEN

		!
		! Remember that we sent this file
		!
		PRINT #LOGFILE.CH%, DATE$(0%); " "; TIME$(0%); " "; &
			IC_JOURADJUST.NAME$

		EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "IC_CYCLEJOUR_" + BATCH_NO$ + &
			".JRL", "", "", "")

		EXIT_STATUS = ASSG_POSTBATCH(OPT_OPENFILE, BATCH_NUMBER, &
			TITLE(), UTL_REPORTX, "", "", "",  "")

		RESET #IC_CYCLEJOUR.CH%

1500		WHEN ERROR IN
			GET #IC_CYCLEJOUR.CH%, REGARDLESS
		USE
			CONTINUE 1600 IF ERR = 11%
			FILENAME$ = "IC_CYCLEJOUR"
			CONTINUE HelpError
		END WHEN

1550		WHEN ERROR IN
			FIND #IC_JOURADJUST.CH%, &
				KEY #0% EQ IC_CYCLEJOUR::LOCATION
		USE
			CONTINUE 1500 IF ERR = 155% OR ERR = 11%
			FILENAME$ = "IC_JOURADJUST"
			CONTINUE HelpError
		END WHEN

 Adjustline:
		WHEN ERROR IN
			GET #IC_JOURADJUST.CH%, REGARDLESS
		USE
			CONTINUE 1500 IF ERR = 155% OR ERR = 11%
			FILENAME$ = "IC_JOURADJUST"
			CONTINUE HelpError
		END WHEN

		GOTO 1500 IF IC_JOURADJUST::LOCATION <> IC_CYCLEJOUR::LOCATION

		V% = IC_WRIT_35BALANCE(IC_JOURADJUST::PRODUCT, &
			IC_JOURADJUST::LOCATION, &
			IC_CYCLEJOUR::TRANSTYPE,IC_JOURADJUST::QUANTITY)

		RECORDS% = RECORDS% + 1%

		GOTO Adjustline

1600	!
 !1600		CLOSE IC_CYCLEJOUR.CH%
 !		CLOSE IC_JOURADJUST.CH%
 !
 !		EXIT_STATUS = ASSG_POSTBATCH(OPT_CLOSEFILE, BATCH_NUMBER, &
 !			TITLE(), UTL_REPORTX, "", "", "",  "")
 !
 !		IC_CYCLEJOUR.DEV$ = ""
 !		CALL READ_DEVICE("IC_CYCLEJOUR",IC_CYCLEJOUR.DEV$, STAT%)
 !
 !		CALL FIND_FILE(IC_CYCLEJOUR.DEV$ + "IC_CYCLEJOUR_*.JRL", &
 !			ARRAY_NEW_FILE$(), 16%, "", "")
 !
 !		FILE% = VAL%(ARRAY_NEW_FILE$(0%))
 !		NEW_BATCH_NO$ = BATCH_NO$
 !
 !1610		FOR I% = 1% TO FILE%
 !
 !			IF NEW_BATCH_NO$ = &
 !				EDIT$(RIGHT(ARRAY_NEW_FILE$(I%), 14%), -1%)
 !			THEN
 !				DELKA% = LEN(BATCH_NO$)
 !				NEW_BATCH_NO$ = LEFT(BATCH_NO$, DELKA% - 2%) + &
 !					FORMAT$(I% + 1%, "<0>#")
 !				GOTO 1610
 !			END IF
 !		NEXT I%
 !
 !		NAME "CMC_INBOX:IC_CYCLEJOUR_" + BATCH_NO$ + ".JRL" AS &
 !			IC_CYCLEJOUR.DEV$ + "IC_CYCLEJOUR_" + NEW_BATCH_NO$ + ".JRL"
 !		NAME "CMC_INBOX:IC_JOURADJUST_"+BATCH_NO$ + ".JRL" AS &
 !			IC_CYCLEJOUR.DEV$ + "IC_JOURADJUST_" + NEW_BATCH_NO$ + ".JRL"
 !
 !		IF BATCH_NO$ <> NEW_BATCH_NO$
 !		THEN
 !			EXIT_STATUS = ASSG_POSTBATCH(OPT_MARKFILE, &
 !				BATCH_NUMBER, &
 !				TITLE(), UTL_REPORTX, &
 !				"IC_CYCLEJOUR_" + NEW_BATCH_NO$ + &
 !				".JRL", "", "", "")
 !			TEXT$ = SPACE$(9%) + FORMAT$(RECORDS%, "######## ") + &
 !				"Line records in journal and file renamed"
 !			CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
 !		ELSE
 !			TEXT$ = SPACE$(9%) + FORMAT$(RECORDS%, "######## ") + &
 !				"Line records in journal"
 !
 !			CALL OUTP_LINE("", UTL_REPORTX, TITLE(), TEXT$, 0%)
 !		END IF

1690		IC_CYCLEJOUR.DEV$ = "CMC_INBOX:"

		!
		! In case of error, make sure they are really closed
		!
		CLOSE IC_CYCLEJOUR.CH%
		CLOSE IC_JOURADJUST.CH%

	NEXT LOOP%

 Completed:
	!
	! Update period file
	!
	CLOSE LOGFILE.CH%

	!
	! Complete closing process and remove batch control
	!
	EXIT_STATUS = ASSG_POSTBATCH(OPT_COMPLETE, BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "", "", "", "")

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

 Aborted:
	!******************************************************************
	! Abort process
	!******************************************************************
	EXIT_STATUS = ASSG_POSTBATCH(OPT_ABORT,	BATCH_NUMBER, TITLE(), &
		UTL_REPORTX, "", "", CUR.PERIOD$, PERIOD$)

	GOTO ExitProgram

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_PRINTMESS(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR), UTL_REPORTX, TITLE(), 0%)

	GOTO Aborted

19000	!******************************************************************
	! Error trapping
	!******************************************************************

	FILENAME$ = ""
	RESUME HelpError

32767	END
