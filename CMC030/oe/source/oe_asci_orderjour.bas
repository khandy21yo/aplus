1	%TITLE "Convert Journals into Text File"
	%SBTTL "OE_ASCI_ORDERJOUR"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 2000 BY
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
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	.lm -5
	!
	! Index:
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS OE_SOURCE:OE_ASCI_ORDERJOUR/LINE
	!	$ LINK/EXECUTABLE=OE_EXE:*.EXE OE_ASCI_ORDERJOUR, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE OE_ASCI_ORDERJOUR.OBJ;*
	!
	! Author:
	!
	!	08/29/2000 - Kevin Handy
	!		Original version
	!
	! Modification history:
	!
	!	10/30/2000 - Kevin Handy
	!		Merge in OE_ASCI_ORDERLINE
	!--

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	%INCLUDE "SOURCE:[OE.open]OE_ORDERJOUR.HB"
	MAP (OE_ORDERJOUR) OE_ORDERJOUR_cdd OE_ORDERJOUR

	%INCLUDE "SOURCE:[OE.open]OE_ORDERLINE.HB"
	MAP (OE_ORDERLINE) OE_ORDERLINE_cdd OE_ORDERLINE

	DECLARE INTEGER CONSTANT MAX_FILES = 2000%
	DIM FILE_NAMES$(MAX_FILES)


	FMT% = 132%

	!
	! Function to clean up text fields so that it won't confuse
	! simplistic CSV handlers
	!
	DEF FNFORMAT$(A$, FMTFLG%)
		SELECT FMTFLG%
		CASE 0%
			FNFORMAT$ = edit$(A$, 140%)
		CASE 132%
			B$ = A$
			QBZ% = INSTR(1%, B$, '"')
			WHILE(QBZ%)
				MID(B$, QBZ%, 1%) = " "
				QBZ% = INSTR(QBZ%, B$, '"')
			NEXT
			QBZ% = INSTR(1%, B$, ',')
			WHILE(QBZ%)
				MID(B$, QBZ%, 1%) = " "
				QBZ% = INSTR(QBZ%, B$, ',')
			NEXT
			FNFORMAT$ = '"' + edit$(B$, 140%) + '"'
		END SELECT
	FNEND

	SELECT FMT%
	CASE 132%
		FMTFLAG$ = ", "
	CASE ELSE
		FMTFLAG$ = "	"
	END SELECT

1000	INPUT "Root Output File Name: "; output.name$

	INPUT "Wildcard Batch number: "; batch_no$

	!
	! Look up all file names
	!
	CALL FIND_FILE("CMC_OUTBOX:OE_ORDERJOUR_" + BATCH_NO$ + ".JRL", &
		FILE_NAMES$(), 16%, "", "")

	FILE_COUNT% = VAL%(FILE_NAMES$(0%))

	!
	! Strip file names out to just the batch number
	!
	FOR LOOP% = 1% TO FILE_COUNT%

		I% = INSTR(1%, FILE_NAMES$(LOOP%), "OE_")
		I% = INSTR(I% + 4%, FILE_NAMES$(LOOP%), "_")
		FILE_NAMES$(LOOP%) = RIGHT(FILE_NAMES$(LOOP%), I% + 1%) &
			IF I%

		I% = INSTR(1%, FILE_NAMES$(LOOP%), ".")
		FILE_NAMES$(LOOP%) = LEFT(FILE_NAMES$(LOOP%), I% - 1%) &
			IF I%

	NEXT LOOP%

1100	OPEN OUTPUT.NAME$ + ".HEAD" FOR OUTPUT AS FILE 1%, &
		RECORDSIZE 511%

	TEXT$ = "ORDNUM" + FMTFLAG$ + &
		"ORDDATE" + FMTFLAG$ + &
		"ORDTYPE" + FMTFLAG$ + &
		"ORDCAT" + FMTFLAG$ + &
		"CUSNUM" + FMTFLAG$ + &
		"DISC" + FMTFLAG$ + &
		"MISC" + FMTFLAG$ + &
		"SHIPNAM" + FMTFLAG$ + &
		"ADD1" + FMTFLAG$ + &
		"ADD2" + FMTFLAG$ + &
		"ADD3" + FMTFLAG$ + &
		"CITY" + FMTFLAG$ + &
		"STATE" + FMTFLAG$ + &
		"ZIP" + FMTFLAG$ + &
		"COUNTRY" + FMTFLAG$ + &
		"DEPOSIT" + FMTFLAG$ + &
		"OLDCUSTPO" + FMTFLAG$ + &
		"SHIPDATE" + FMTFLAG$ + &
		"SHIPVIA" + FMTFLAG$ + &
		"TERMS" + FMTFLAG$ + &
		"SALESTAX" + FMTFLAG$ + &
		"LOCATION" + FMTFLAG$ + &
		"OPERATOR" + FMTFLAG$ + &
		"COMMAMT" + FMTFLAG$ + &
		"COMMPERC" + FMTFLAG$ + &
		"SALESMAN" + FMTFLAG$ + &
		"CREASON" + FMTFLAG$ + &
		"SALCOMM" + FMTFLAG$ + &
		"HANDLING" + FMTFLAG$ + &
		"AMTPAID" + FMTFLAG$ + &
		"CHECK" + FMTFLAG$ + &
		"NOTES1" + FMTFLAG$ + &
		"NOTES2" + FMTFLAG$ + &
		"MISCACCT" + FMTFLAG$ + &
		"TRANDATE" + FMTFLAG$ + &
		"TRANTIME" + FMTFLAG$ + &
		"INVNUM" + FMTFLAG$ + &
		"FREIGHT" + FMTFLAG$ + &
		"TAXCODE" + FMTFLAG$ + &
		"TAXFLAG" + FMTFLAG$ + &
		"SHIPLIN" + FMTFLAG$ + &
		"PAYMNT" + FMTFLAG$ + &
		"REG_FLAG" + FMTFLAG$ + &
		"CUSTPO"

	PRINT #1%, TEXT$

	OPEN OUTPUT.NAME$ + ".LINE" FOR OUTPUT AS FILE 2%, &
		RECORDSIZE 511%

	TEXT$ = "ORDNUM" + FMTFLAG$ + &
		"PRODUCT" + FMTFLAG$ + &
		"ORDQTY" + FMTFLAG$ + &
		"SHPQTY" + FMTFLAG$ + &
		"PRICE" + FMTFLAG$ + &
		"DISCOUNT" + FMTFLAG$ + &
		"COST" + FMTFLAG$ + &
		"REQDATE" + FMTFLAG$ + &
		"PROMO" + FMTFLAG$ + &
		"MISCH" + FMTFLAG$ + &
		"BCKQTY" + FMTFLAG$ + &
		"NOTES1" + FMTFLAG$ + &
		"NOTES2" + FMTFLAG$ + &
		"SUBACCT" + FMTFLAG$ + &
		"LIN" + FMTFLAG$ + &
		"MISCH2" + FMTFLAG$ + &
		"NETLINE"

	print #2%, text$

	!
	! Process each batch
	!
	FOR LOOP% = 1% TO FILE_COUNT%

		BATCH_NO$ = FILE_NAMES$(LOOP%)
		PRINT "Processing: "; BATCH_NO$

		GOSUB 2000
		GOSUB 3000

1190	NEXT LOOP%

	GOTO 9000

2000	!*******************************************************************
	! Process one file
	!*******************************************************************

	WHEN ERROR IN
		%INCLUDE "source:[OE.open]OE_ORDERJOUR.opn"
	USE
		PRINT "Cannot open batch "; BATCH_NO$
		CONTINUE 2900
	END WHEN

	WHEN ERROR IN
		RESET #OE_ORDERJOUR.CH%
	USE
		CONTINUE 2900
	END WHEN

2100	WHEN ERROR IN
		GET #OE_ORDERJOUR.CH%, REGARDLESS
	USE
		CONTINUE 2900 IF ERR = 11%
		EXIT HANDLER
	END WHEN

	TEXT$ = &
		FNFORMAT$(OE_ORDERJOUR::ORDNUM, FMT%) + FMTFLAG$ + &
		FNFORMAT$(OE_ORDERJOUR::ORDDATE, FMT%) + FMTFLAG$ + &
		FNFORMAT$(OE_ORDERJOUR::ORDTYPE, FMT%) + FMTFLAG$ + &
		FNFORMAT$(OE_ORDERJOUR::ORDCAT, FMT%) + FMTFLAG$ + &
		FNFORMAT$(OE_ORDERJOUR::CUSNUM, FMT%) + FMTFLAG$ + &
		num1$(OE_ORDERJOUR::DISC) + FMTFLAG$ + &
		num1$(OE_ORDERJOUR::MISC) + FMTFLAG$ + &
		FNFORMAT$(OE_ORDERJOUR::SHIPNAM, FMT%) + FMTFLAG$ + &
		FNFORMAT$(OE_ORDERJOUR::ADD1, FMT%) + FMTFLAG$ + &
		FNFORMAT$(OE_ORDERJOUR::ADD2, FMT%) + FMTFLAG$ + &
		FNFORMAT$(OE_ORDERJOUR::ADD3, FMT%) + FMTFLAG$ + &
		FNFORMAT$(OE_ORDERJOUR::CITY, FMT%) + FMTFLAG$ + &
		FNFORMAT$(OE_ORDERJOUR::STATE, FMT%) + FMTFLAG$ + &
		FNFORMAT$(OE_ORDERJOUR::ZIP, FMT%) + FMTFLAG$ + &
		FNFORMAT$(OE_ORDERJOUR::COUNTRY, FMT%) + FMTFLAG$ + &
		FNFORMAT$(OE_ORDERJOUR::DEPOSIT, FMT%) + FMTFLAG$ + &
		FNFORMAT$(OE_ORDERJOUR::OLDCUSTPO, FMT%) + FMTFLAG$ + &
		FNFORMAT$(OE_ORDERJOUR::SHIPDATE, FMT%) + FMTFLAG$ + &
		FNFORMAT$(OE_ORDERJOUR::SHIPVIA, FMT%) + FMTFLAG$ + &
		FNFORMAT$(OE_ORDERJOUR::TERMS, FMT%) + FMTFLAG$ + &
		num1$(OE_ORDERJOUR::SALESTAX) + FMTFLAG$ + &
		FNFORMAT$(OE_ORDERJOUR::LOCATION, FMT%) + FMTFLAG$ + &
		FNFORMAT$(OE_ORDERJOUR::OPERATOR, FMT%) + FMTFLAG$ + &
		num1$(OE_ORDERJOUR::COMMAMT) + FMTFLAG$ + &
		num1$(OE_ORDERJOUR::COMMPERC) + FMTFLAG$ + &
		FNFORMAT$(OE_ORDERJOUR::SALESMAN, FMT%) + FMTFLAG$ + &
		FNFORMAT$(OE_ORDERJOUR::CREASON, FMT%) + FMTFLAG$ + &
		num1$(OE_ORDERJOUR::SALCOMM) + FMTFLAG$ + &
		num1$(OE_ORDERJOUR::HANDLING) + FMTFLAG$ + &
		num1$(OE_ORDERJOUR::AMTPAID) + FMTFLAG$ + &
		FNFORMAT$(OE_ORDERJOUR::CHECK, FMT%) + FMTFLAG$ + &
		FNFORMAT$(OE_ORDERJOUR::NOTES(1), FMT%) + FMTFLAG$ + &
		FNFORMAT$(OE_ORDERJOUR::NOTES(2), FMT%) + FMTFLAG$ + &
		FNFORMAT$(OE_ORDERJOUR::MISCACCT, FMT%) + FMTFLAG$ + &
		FNFORMAT$(OE_ORDERJOUR::TRANDATE, FMT%) + FMTFLAG$ + &
		FNFORMAT$(OE_ORDERJOUR::TRANTIME, FMT%) + FMTFLAG$ + &
		FNFORMAT$(OE_ORDERJOUR::INVNUM, FMT%) + FMTFLAG$ + &
		num1$(OE_ORDERJOUR::FREIGHT) + FMTFLAG$ + &
		FNFORMAT$(OE_ORDERJOUR::TAXCODE, FMT%) + FMTFLAG$ + &
		FNFORMAT$(OE_ORDERJOUR::TAXFLAG, FMT%) + FMTFLAG$ + &
		FNFORMAT$(OE_ORDERJOUR::SHIPLIN, FMT%) + FMTFLAG$ + &
		NUM1$(OE_ORDERJOUR::PAYMNT) + FMTFLAG$ + &
		FNFORMAT$(OE_ORDERJOUR::REG_FLAG, FMT%) + FMTFLAG$ + &
		FNFORMAT$(OE_ORDERJOUR::CUSTPO, FMT%)

	PRINT #1%, TEXT$

	GOTO 2100

2900	CLOSE OE_ORDERJOUR.CH%
	RETURN


3000	!*******************************************************************
	! Handle line file
	!*******************************************************************

	%include "source:[OE.open]OE_ORDERLINE.opn"

	WHEN ERROR IN
		reset #OE_ORDERLINE.ch%
	use
		continue 3900
	end when

3100	WHEN ERROR IN
		GET #OE_ORDERLINE.ch%, REGARDLESS
	USE
		CONTINUE 3900 IF ERR = 11%
		EXIT HANDLER
	END WHEN

	NET = FUNC_ROUND(OE_ORDERLINE::SHPQTY * &
		(OE_ORDERLINE::PRICE - OE_ORDERLINE::PROMO) * &
		(1 - OE_ORDERLINE::DISCOUNT / 100.0) + &
		OE_ORDERLINE::MISCH + &
		OE_ORDERLINE::MISCH2, 2%)

	TEXT$ = &
		FNFORMAT$(OE_ORDERLINE::ORDNUM, FMT%) + FMTFLAG$ + &
		FNFORMAT$(OE_ORDERLINE::PRODUCT, FMT%) + FMTFLAG$ + &
		num1$(OE_ORDERLINE::ORDQTY) + FMTFLAG$ + &
		num1$(OE_ORDERLINE::SHPQTY) + FMTFLAG$ + &
		num1$(OE_ORDERLINE::PRICE) + FMTFLAG$ + &
		num1$(OE_ORDERLINE::DISCOUNT) + FMTFLAG$ + &
		num1$(OE_ORDERLINE::COST) + FMTFLAG$ + &
		FNFORMAT$(OE_ORDERLINE::REQDATE, FMT%) + FMTFLAG$ + &
		num1$(OE_ORDERLINE::PROMO) + FMTFLAG$ + &
		num1$(OE_ORDERLINE::MISCH) + FMTFLAG$ + &
		num1$(OE_ORDERLINE::BCKQTY) + FMTFLAG$ + &
		FNFORMAT$(OE_ORDERLINE::NOTES1, FMT%) + FMTFLAG$ + &
		FNFORMAT$(OE_ORDERLINE::NOTES2, FMT%) + FMTFLAG$ + &
		FNFORMAT$(OE_ORDERLINE::SUBACCT, FMT%) + FMTFLAG$ + &
		FNFORMAT$(OE_ORDERLINE::LIN, FMT%) + FMTFLAG$ + &
		num1$(OE_ORDERLINE::MISCH2) + FMTFLAG$ + &
		NUM1$(NET)

	print #2%, text$

	goto 3100

3900	close OE_ORDERLINE.ch%
	RETURN

9000	!*******************************************************************
	! Finish up
	!*******************************************************************

	CLOSE #1%, 2%

32767	END
