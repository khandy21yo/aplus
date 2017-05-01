1	%TITLE "Load PERCON file into a Cycle Count Journal"
	%SBTTL "IC_SPEC_PERCONCOUNT"
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
	! ID:IC008
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	This program loads a text file into a cycle count journal.
	!	.lm -5
	!
	! Compile:
	!
	!	$ BAS IC_SOURCE:IC_SPEC_PERCONCOUNT/LINE
	!	$ LINK/EXE=IC_EXE: IC_SPEC_PERCONCOUNT, FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE IC_SPEC_PERCONCOUNT.OBJ;*
	!
	! Author:
	!
	!	10/07/97 - Kevin Handy
	!
	! Modification History:
	!
	!	10/23/97 - Kevin Handy
	!		Added PO Receiver capturing
	!
	!	11/17/97 - Kevin Handy
	!		Check first line of data if we haven't gotten a
	!		file type. 2 commas means PO Reveiver.
	!
	!	12/01/97 - Kevin Handy
	!		Strip quotes from entries (").
	!		Lose half of the messaged dumped while running
	!
	!	12/02/97 - Kevin Handy
	!		Ask for batch number and location.
	!
	!	12/02/97 - Kevin Handy
	!		Added code for WP Issue journal.
	!
	!	12/24/97 - Kevin Handy
	!		Clean up (Check)
	!
	!	12/29/97 - Kevin Handy
	!		Add in WP Buyoff stuff
	!
	!	01/12/97 - Kevin Handy
	!		Added in PO By line number business.
	!		Cancels as "Cqty"
	!
	!	04/22/98 - Kevin Handy
	!		Clean up (Check)
	!
	!	06/10/98 - Kevin Handy
	!		Handle duplicating buyoff lines (usually
	!		one buyoff, and the next a cancel)
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	10/20/98 - Kevin Handy
	!		Changes for new CONTROL field, and new keys
	!
	!	03/23/99 - Kevin Handy
	!		Change rounding to 4 digits from 2.
	!
	!	09/29/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!
	!	05/03/2001 - Kevin Handy
	!		Read files that were transfered in binary mode.
	!
	!	05/04/2001 - Kevin Handy
	!		Instead of pulling in entire file into a string
	!		and processing all at once, pull in one block
	!		and process it then loop.
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	%INCLUDE "SOURCE:[IC.OPEN]IC_CYCLEJOUR.HB"
	MAP (IC_CYCLEJOUR) IC_CYCLEJOUR_CDD IC_CYCLEJOUR

	%INCLUDE "SOURCE:[IC.OPEN]IC_JOURCOUNT.HB"
	MAP (IC_JOURCOUNT) IC_JOURCOUNT_CDD IC_JOURCOUNT

	%INCLUDE "SOURCE:[PO.OPEN]PO_RECJOUR.HB"
	MAP (PO_RECJOUR) PO_RECJOUR_CDD PO_RECJOUR

	%INCLUDE "SOURCE:[PO.OPEN]PO_RECLINE.HB"
	MAP (PO_RECLINE) PO_RECLINE_CDD PO_RECLINE

	%INCLUDE "SOURCE:[PO.OPEN]PO_REG_LINE.HB"
	MAP (PO_REG_LINE) PO_REG_LINE_CDD PO_REG_LINE

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT) PD_PRODUCT_CDD PD_PRODUCT
	DECLARE PD_PRODUCT_CDD PD_PRODUCT_EXAM

	%INCLUDE "SOURCE:[WP.OPEN]WP_ISSJOUR.HB"
	MAP (WP_ISSJOUR) WP_ISSJOUR_CDD WP_ISSJOUR

	%INCLUDE "SOURCE:[WP.OPEN]WP_ISSLINE.HB"
	MAP (WP_ISSLINE) WP_ISSLINE_CDD WP_ISSLINE

	%INCLUDE "SOURCE:[WP.OPEN]WP_REQREGISTER.HB"
	MAP (WP_REQREGISTER) WP_REQREGISTER_CDD WP_REQREGISTER

	%INCLUDE "SOURCE:[WP.OPEN]WP_BUYOFF.HB"
	MAP (WP_BUYOFF) WP_BUYOFF_CDD WP_BUYOFF

	%INCLUDE "SOURCE:[WP.OPEN]WP_BUYOFFLINE.HB"
	MAP (WP_BUYOFFLINE) WP_BUYOFFLINE_CDD WP_BUYOFFLINE

	%INCLUDE "SOURCE:[WP.OPEN]WP_REGLINE.HB"
	MAP (WP_REGLINE)	WP_REGLINE_CDD		WP_REGLINE
	COM (WP_REGLINE_READ)	WP_REGLINE_CDD		WP_REGLINE_READ

	%INCLUDE "SOURCE:[PD.OPEN]PD_ACCOUNT.HB"
	DECLARE			PD_ACCOUNT_CDD		PD_ACCOUNT_READ

	%INCLUDE "SOURCE:[JC.OPEN]JC_JOB.HB"
	MAP (SB_SUBACCOUNT)	JC_JOB_CDD		JC_JOB

	%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.HB"
	MAP (SB_SUBACCOUNT)	SB_SUBACCOUNT_CDD	SB_SUBACCOUNT

	!
	! External Functions
	!
	EXTERNAL LONG FUNCTION IC_WRIT_35BALANCE
	EXTERNAL REAL FUNCTION PC_READ_COST
	EXTERNAL LONG   FUNCTION WP_READ_REGLINE
	EXTERNAL LONG   FUNCTION PD_READ_ACCOUNT
	EXTERNAL LONG   FUNCTION PD_EXAM_PRODUCT
	EXTERNAL STRING FUNCTION READ_SET

	%PAGE

	ON ERROR GOTO 19000

	CALL ASSG_CHANNEL(PERCON.CH%, STATUS%)

100	!*******************************************************************
	! Open text file read from Percon
	!
	PRINT "File to Load: ";
	LINPUT PERCON$

	PRINT "Batch Number To Use: ";
	LINPUT USE_BATCH_NO$

	PRINT "Location To Use: ";
	LINPUT LOCATION$

	WHEN ERROR IN
		OPEN PERCON$ FOR INPUT AS FILE PERCON.CH%, &
			ACCESS READ, &
			ALLOW READ
	USE
		CONTINUE 1100 IF ERR = 228%
		PRINT ERR, ERL, ERT$(ERR)
		STOP
	END WHEN

	FILE_TYPE% = 0%

1000	!*********************************************************************
	! Scan through the text file
	!
	WHEN ERROR IN
		LINPUT #PERCON.CH%, READ_LINE$
	USE
		CONTINUE 1500 IF ERR = 11%
		CONTINUE HelpError
	END WHEN

	GOTO 1000 IF READ_LINE$ = ""
	KEEP_READLINE$ = READ_LINE$

	GOSUB 1400

	GOTO 1000

1100	!*******************************************************************
	! Binary file?
	!*******************************************************************

	WHEN ERROR IN
		OPEN PERCON$ FOR INPUT AS FILE PERCON.CH%, &
			RECORDTYPE NONE, &
			ACCESS READ, &
			ALLOW READ
	USE
		PRINT ERR, ERL, ERT$(ERR)
		STOP
	END WHEN

	FILE_TYPE% = 0%
	BUFFER$ = ""
	OFFSET% = 0%

1110	!
	! Grab next chunk of the file
	!
	WHEN ERROR IN
		GET #PERCON.CH%
	USE
		CONTINUE 1500 IF ERR = 11%
		STOP
	END WHEN

	FIELD #PERCON.CH%, RECOUNT AS X$
	BUFFER$ = BUFFER$ + X$

1190	!
	! Get next binary line
	!
	I% = INSTR(OFFSET% + 1%, BUFFER$, '13'C)
	IF I%
	THEN
		READ_LINE$ = EDIT$(SEG$(BUFFER$, OFFSET% + 1%, I% - 1%), 4%)
		OFFSET% = I%
	ELSE
		!
		! No complete lines, reset and grab another chunk of the file
		!
		BUFFER$ = RIGHT(BUFFER$, OFFSET% + 1%)
		OFFSET% = 0%
		GOTO 1110
	END IF

	GOTO 1190 IF READ_LINE$ = ""
	KEEP_READLINE$ = READ_LINE$

	GOSUB 1400

	GOTO 1190


1400	!
	! Handle file type records
	!
	SELECT READ_LINE$

	CASE "CC,PC"
		!
		! Cycle count journal
		!
		FILE_TYPE% = 1%
		PRINT "Reading Cycle Count file"
		GOTO 1000

	CASE "PORE,PC"
		!
		! Purchase Order Receivers
		!
		FILE_TYPE% = 2%
		PRINT "Reading PO Receiver file"
		GOTO 1000

	CASE "WPIS,PC"
		!
		! Work In Process, Issue
		!
		FILE_TYPE% = 3%
		PRINT "Reading WP Issue file"
		GOTO 1000

	CASE "WPBO,PC"
		!
		! Work in process, Buyoff
		!
		FILE_TYPE% = 4%
		PRINT "Reading WP Buyoff file"
		GOTO 1000

	END SELECT

	!
	! If it says it is a default file, and there are two commas
	! in the record, then assume it is a PO Receiver journal
	!
	IF (FILE_TYPE% = 0%)
	THEN
		IF INSTR(INSTR(1%, READ_LINE$, ",") + 1%, READ_LINE$, ",")
		THEN
			PRINT "Assuming PO Receiver file"
			FILE_TYPE% = 2%
		END IF
	END IF

	!
	! Split up the record
	!
	RBUFF$(I%) = "" FOR I% = 1% TO 10%
	RBUFF% = 0%
	I% = INSTR(1%, READ_LINE$, ",")
	WHILE I%
		RBUFF% = RBUFF% + 1%
		RBUFF$ = LEFT(READ_LINE$, I% - 1%)
		IF LEFT(RBUFF$, 1%) == '"' AND RIGHT(RBUFF$, LEN(RBUFF$)) = '"'
		THEN
			RBUFF$ = MID(RBUFF$, 2%, LEN(RBUFF$) - 2%)
		END IF
		RBUFF$(RBUFF%) = RBUFF$
		READ_LINE$ = RIGHT(READ_LINE$, I% + 1%)
		I% = INSTR(1%, READ_LINE$, ",")
	NEXT
	RBUFF% = RBUFF% + 1%
	RBUFF$(RBUFF%) = READ_LINE$

	!
	! Process Cycle Count
	!
	SELECT FILE_TYPE%

	CASE 0%, 1%
		GOSUB 2000

	CASE 2%
		GOSUB 3000

	CASE 3%
		GOSUB 4000

	CASE 4%
		GOSUB 5000

	CASE ELSE
		PRINT "Internal program error!!!"

	END SELECT

	RETURN

1500	!*********************************************************************
	! Done with file
	!
	CLOSE #IC_CYCLEJOUR.CH%
	CLOSE #IC_JOURCOUNT.CH%
	CLOSE #PO_RECJOUR.CH%
	CLOSE #PO_RECLINE.CH%
	CLOSE #PD_PRODUCT.CH%

	GOTO ExitProgram

2000	!*********************************************************************
	! Load a line into the Cycle count file
	!
	GOSUB 2800	! Are the files open already?

	IF RBUFF% = 1%
	THEN
		PRODUCT$ = RBUFF$(1%)
		QUANTITY = 1.0
	ELSE
		PRODUCT$ = RBUFF$(1%)
		QUANTITY = VAL(RBUFF$(2%))
	END IF

2100	IC_JOURCOUNT::LOCATION	= LOCATION$
	IC_JOURCOUNT::PRODUCT	= PRODUCT$
	IC_JOURCOUNT::QUANTITY	= QUANTITY
	IC_JOURCOUNT::CONTROL	= ""

	PUT #IC_JOURCOUNT.CH%

	PRINT "Cycle Count: "; TRM$(IC_JOURCOUNT::PRODUCT)

	RETURN

2800	!
	! Open up cycle count journal
	!
	RETURN IF IC_CYCLEJOUR.CH% <> 0%

	BATCH_NO$ = USE_BATCH_NO$
	%INCLUDE "SOURCE:[IC.OPEN]IC_CYCLEJOUR.CRE"

2810	WHEN ERROR IN
		GET #IC_CYCLEJOUR.CH%, KEY #0% EQ LOCATION$, REGARDLESS
	USE
		CONTINUE 2815
	END WHEN

	GOTO 2820

2815	IC_CYCLEJOUR::LOCATION	= LOCATION$
	IC_CYCLEJOUR::COUNTDATE	= DATE_TODAY
	IC_CYCLEJOUR::TRANSTYPE	= "CC"
	IC_CYCLEJOUR::PRIMREF	= ""
	IC_CYCLEJOUR::SECREF	= ""
	IC_CYCLEJOUR::CROSSREF	= ""
	IC_CYCLEJOUR::SUBACCT	= ""
	IC_CYCLEJOUR::STATIONMAN = ""

	PUT #IC_CYCLEJOUR.CH%

2820	%INCLUDE "SOURCE:[IC.OPEN]IC_JOURCOUNT.CRE"

2890	RETURN

	%PAGE

3000	!******************************************************************
	! Purchase Order Receiver
	!*******************************************************************

	PRINT "PO Receiver Data: "; KEEP_READLINE$

	IF RBUFF% < 3%
	THEN
		PRINT "Bad entry: "; KEEP_READLINE$
		RETURN
	END IF

3010	!
	! Open PO register header if is isn't open yet
	!
	IF PO_REG_LINE.CH% = 0%
	THEN
		%INCLUDE "SOURCE:[PO.OPEN]PO_REG_LINE.OPN"
	END IF

3020	!
	! Open Receiver journal
	!
	IF PO_RECJOUR.CH% = 0%
	THEN
		BATCH_NO$ = USE_BATCH_NO$
		%INCLUDE "SOURCE:[PO.OPEN]PO_RECJOUR.CRE"
		%INCLUDE "SOURCE:[PO.OPEN]PO_RECLINE.CRE"
	END IF

3100	!
	! Let's work on the header first. Does it already exist?
	!
	RSET PO_RECJOUR::PO = RBUFF$(1%)
	PONUM$ = PO_RECJOUR::PO
	INVLOCATION$ = LOCATION$

	WHEN ERROR IN
		GET #PO_RECJOUR.CH%, KEY #0% EQ PO_RECJOUR::PO, REGARDLESS
	USE
		CONTINUE 3120
	END WHEN

	GOTO 3200

3120	!
	! It doesn't exist, so let's create it
	!
	RSET PO_RECJOUR::PO	= PONUM$
	PO_RECJOUR::RECDATE	= DATE_TODAY
	PO_RECJOUR::REFNO	= ""
	PO_RECJOUR::OPERATOR	= "PC"

	PUT #PO_RECJOUR.CH%

3200	!
	! Ok, we now have a header, so lets see if we can find a proper
	! match for it in to PO register
	!
	LINEFLAG$ = "N"
	RSET PO_REG_LINE::PO	= PONUM$
	PO_REG_LINE::PRODUCT	= RBUFF$(2%)

	WHEN ERROR IN
		GET #PO_REG_LINE.CH%, &
			KEY #4% EQ PO_REG_LINE::PRODUCT + PO_REG_LINE::PO, &
			REGARDLESS
	USE
		CONTINUE 3205
	END WHEN

	LINEFLAG$ = "Y"
	INVLOCATION$ = PO_REG_LINE::FROMLOCATION

	GOTO 3300

3205	!
	! No match by part number, lets try it by line number
	!
	IF LEN(RBUFF$(2%)) < 4%
	THEN
		RSET PO_REG_LINE::PO = PONUM$
		PO_REG_LINE::PO_LINE = FORMAT$(VAL(RBUFF$(2%)), "<0>###")

		WHEN ERROR IN
			GET #PO_REG_LINE.CH%, &
				KEY #0% EQ PO_REG_LINE::PO + PO_REG_LINE::PO_LINE, &
				REGARDLESS

		USE
			CONTINUE 3210
		END WHEN

		LINEFLAG$ = "Y"
		INVLOCATION$ = PO_REG_LINE::FROMLOCATION

		GOTO 3300
	END IF

3210	!
	! Don't have this product, so lets see about creating a new
	! line for it. Let's look in our journal for an existing line
	!
	RSET PO_RECLINE::PO = PONUM$
	LAST_LINE$ = "0000"

	WHEN ERROR IN
		GET #PO_RECLINE.CH%, KEY #0% GE PO_RECLINE::PO, REGARDLESS
	USE
		CONTINUE 3220
	END WHEN

3215	WHILE PO_RECLINE::PO = PONUM$

		LAST_LINE$ = PO_RECLINE::PO_LINE &
			IF LAST_LINE$ < PO_RECLINE::PO_LINE

		WHEN ERROR IN
			GET #PO_RECLINE.CH%
		USE
			IF ERR = 154%	! Locked Block
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE 3220
		END WHEN
	NEXT

3220	!
	! And see if there is a bigger key in the register
	!
	RSET PO_REG_LINE::PO	= PONUM$

	WHEN ERROR IN
		GET #PO_REG_LINE.CH%, &
			KEY #0% GT PO_REG_LINE::PO + LAST_LINE$, &
			REGARDLESS
	USE
		CONTINUE 3230
	END WHEN

3225	WHILE PO_REG_LINE::PO = PONUM$

		INVLOCATION$ = PO_REG_LINE::FROMLOCATION
		LAST_LINE$ = PO_REG_LINE::PO_LINE &
			IF LAST_LINE$ < PO_REG_LINE::PO_LINE

		WHEN ERROR IN
			GET #PO_REG_LINE.CH%
		USE
			IF ERR = 154%	! Locked Block
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE 3230
		END WHEN
	NEXT

3230	!
	! Now, fake up a PO line that we can use as a basis
	! of creating a nerw line
	!
	STATUS% = FUNC_INCREMENT(LAST_LINE$)

	RSET PO_REG_LINE::PO	= PONUM$
	PO_REG_LINE::PRODUCT	= RBUFF$(2%)
	PO_REG_LINE::PO_LINE	= LAST_LINE$
	PO_REG_LINE::DESCRIPTION = ""
	PO_REG_LINE::UOM	= ""
	PO_REG_LINE::FROMLOCATION = INVLOCATION$

3240	!
	! Can we find the description of the part?
	!
	IF PD_PRODUCT.CH% = 0%
	THEN
		%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"
	END IF

3250	WHEN ERROR IN
		GET #PD_PRODUCT.CH%, KEY #0% EQ PO_REG_LINE::PRODUCT, REGARDLESS
	USE
		CONTINUE 3300
	END WHEN

	PO_REG_LINE::DESCRIPTION = PD_PRODUCT::PRODUCT_NUM
	PO_REG_LINE::UOM	= PD_PRODUCT::UOM

3300	!
	! Does the line already exist in the journal?
	!
	WHEN ERROR IN
		GET #PO_RECLINE.CH%, &
			KEY #0% EQ PONUM$ + PO_REG_LINE::PO_LINE
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE 3400
	END WHEN

	IF LEFT(RBUFF$(3%), 1%) = "C"
	THEN
		CANQTY = VAL(RIGHT(RBUFF$(3%), 2%))
		RECQTY = 0.0
		PO_RECLINE::CANQTY = PO_RECLINE::CANQTY + CANQTY
	ELSE
		CANQTY = 0.0
		RECQTY = VAL(RBUFF$(3%))
		PO_RECLINE::RECQTY = PO_RECLINE::RECQTY + RECQTY
	END IF

	UPDATE #PO_RECLINE.CH%

	GOTO 3900

3400	!
	! Now we can add the line
	!
	RSET PO_RECLINE::PO	= PONUM$
	PO_RECLINE::PO_LINE	= PO_REG_LINE::PO_LINE
	PO_RECLINE::DESCRIPTION	= PO_REG_LINE::DESCRIPTION
	PO_RECLINE::PRODUCT	= PO_REG_LINE::PRODUCT
	PO_RECLINE::UOM		= PO_REG_LINE::UOM
	IF LEFT(RBUFF$(3%), 1%) = "C"
	THEN
		CANQTY = VAL(RIGHT(RBUFF$(3%), 2%))
		RECQTY = 0.0
	ELSE
		CANQTY = 0.0
		RECQTY = VAL(RBUFF$(3%))
	END IF
	PO_RECLINE::RECQTY	= RECQTY
	PO_RECLINE::CANQTY	= CANQTY
	PO_RECLINE::LINEFLAG	= LINEFLAG$

	PUT #PO_RECLINE.CH%

3900	!
	! Adjust the inventory balances
	!
	IF PO_RECLINE::LINEFLAG = "Y"
	THEN
		V% = IC_WRIT_35BALANCE(PO_RECLINE::PRODUCT, &
			INVLOCATION$, "PO", -RECQTY)
	END IF

	V% = IC_WRIT_35BALANCE (PO_RECLINE::PRODUCT, &
		INVLOCATION$, "RE", RECQTY)

	RETURN

4000	!******************************************************************
	! WP Issue Journal
	!*******************************************************************

	PRINT "WP Issue Data: "; KEEP_READLINE$

	IF RBUFF% < 3%
	THEN
		PRINT "Bad entry: "; KEEP_READLINE$
		RETURN
	END IF

4010	!
	! Open PO register header if is isn't open yet
	!
	IF WP_ISSJOUR.CH% = 0%
	THEN
		BATCH_NO$ = USE_BATCH_NO$
		%INCLUDE "SOURCE:[WP.OPEN]WP_ISSJOUR.CRE"
	END IF

4020	!
	! Open Receiver journal
	!
	IF WP_ISSLINE.CH% = 0%
	THEN
		BATCH_NO$ = USE_BATCH_NO$
		%INCLUDE "SOURCE:[WP.OPEN]WP_ISSLINE.CRE"
	END IF

4030	!
	! Requisition register
	!
	IF WP_REQREGISTER.CH% = 0%
	THEN
		%INCLUDE "SOURCE:[WP.OPEN]WP_REQREGISTER.OPN"
	END IF

4100	!
	! Let's work on the header first. Does it already exist?
	!
	RSET WP_ISSJOUR::REQNUM = RBUFF$(1%)

	WHEN ERROR IN
		GET #WP_ISSJOUR.CH%, KEY #0% EQ WP_ISSJOUR::REQNUM, REGARDLESS
	USE
		CONTINUE 4120
	END WHEN

4110	!
	! We still need to get the location from the register
	!
	WHEN ERROR IN
		GET #WP_REQREGISTER.CH%, &
			KEY #1% EQ WP_ISSJOUR::REQNUM, REGARDLESS
	USE
		WP_REQREGISTER::JOB = ""
		WP_REQREGISTER::LLINE = ""
		WP_REQREGISTER::LOCATION = LOCATION$
		CONTINUE 4200
	END WHEN

	GOTO 4200

4120	!
	! Check if in register
	!
	WHEN ERROR IN
		RSET WP_ISSJOUR::REQNUM = RBUFF$(1%)

		GET #WP_REQREGISTER.CH%, &
			KEY #1% EQ WP_ISSJOUR::REQNUM, REGARDLESS
	USE
		WP_REQREGISTER::JOB = ""
		WP_REQREGISTER::LLINE = ""
		WP_REQREGISTER::LOCATION = LOCATION$
		CONTINUE 4140
	END WHEN

4140	!
	! It doesn't exist, so let's create it
	!
	RSET WP_ISSJOUR::REQNUM	= RBUFF$(1%)
	WP_ISSJOUR::JOB		= WP_REQREGISTER::JOB
	WP_ISSJOUR::LLINE	= WP_REQREGISTER::LLINE
	WP_ISSJOUR::OPERATOR	= ""

	PUT #WP_ISSJOUR.CH%

4200	!
	! Ok, we now have a header, so lets see if we can find a proper
	! match for the line number in the WP register
	!
	IF LEN(RBUFF$(2%)) <= LEN(WP_REQREGISTER::REQLIN)
	THEN
		WP_REQREGISTER::REQLIN = FORMAT$(VAL%(RBUFF$(2%)), "<0>###")

		WHEN ERROR IN
			GET #WP_REQREGISTER.CH%, &
				KEY #0% EQ WP_ISSJOUR::JOB + &
				WP_ISSJOUR::LLINE + &
				WP_ISSJOUR::REQNUM + &
				WP_REQREGISTER::REQLIN, &
				REGARDLESS
		USE
			CONTINUE 4210
		END WHEN

		GOTO 4230
	END IF

4210	!
	! No match by line number, lets try searching for a product number
	!
	WP_REQREGISTER::PRODUCT = RBUFF$(2%)

	WHEN ERROR IN
		GET #WP_REQREGISTER.CH%, &
			KEY #2% EQ WP_REQREGISTER::PRODUCT + &
				WP_REQREGISTER::LOCATION + &
				WP_ISSJOUR::JOB + &
				WP_ISSJOUR::REQNUM, &
			REGARDLESS
	USE
		WP_REQREGISTER::REQLIN		= "NEWL"
		WP_REQREGISTER::PRODUCT		= RBUFF$(2%)
		CONTINUE 4250
	END WHEN

	GOTO 4230

4230	!
	! Does this line already exist in the issue journal?
	!
	WHEN ERROR IN
		GET #WP_ISSLINE.CH%, KEY #0% EQ &
			WP_ISSJOUR::REQNUM + &
			WP_ISSJOUR::JOB + &
			WP_ISSJOUR::LLINE + &
			WP_REQREGISTER::REQLIN
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE 4280 IF ERR = 155%
	END WHEN

	IF (LEFT(RBUFF$(3%), 1%) = "C")
	THEN
		CANQTY = VAL(RIGHT(RBUFF$(3%), 2%))
		ISSQTY = 0.0
		WP_ISSLINE::QTYCANCEL = FUNC_ROUND(WP_ISSLINE::QTYCANCEL + &
			CANQTY, 4%)
	ELSE
		CANQTY = 0.0
		ISSQTY = VAL(RBUFF$(3%))
		WP_ISSLINE::QTYISSUE = FUNC_ROUND(WP_ISSLINE::QTYISSUE + &
			ISSQTY, 4%)
	END IF

	WHEN ERROR IN
		UPDATE #WP_ISSLINE.CH%
	USE
		CONTINUE 4280 IF ERR = 155%
	END WHEN

	GOTO 4900

4250	!
	! Now, fake up a new line, since there isn't a match at the moment
	!
	! First we try to come up with an available line number
	!
	WP_REQREGISTER::JOB = WP_ISSJOUR::JOB
	WP_REQREGISTER::LLINE = WP_ISSJOUR::LLINE
	WP_REQREGISTER::LLINE = WP_ISSJOUR::REQNUM
	WP_REQREGISTER::REQLIN = "0000"
	LAST_LINE$ = WP_REQREGISTER::REQLIN

	WHILE WP_REQREGISTER::JOB = WP_ISSJOUR::JOB AND &
		WP_REQREGISTER::LLINE = WP_ISSJOUR::LLINE AND &
		WP_REQREGISTER::LLINE = WP_ISSJOUR::REQNUM

		LAST_LINE$ = WP_REQREGISTER::REQLIN &
			IF LAST_LINE$ < WP_REQREGISTER::REQLIN

		WHEN ERROR IN
			GET #WP_REQREGISTER.CH%, &
				KEY #0% GT WP_ISSJOUR::JOB + &
					WP_ISSJOUR::LLINE + &
					WP_ISSJOUR::REQNUM + &
					WP_REQREGISTER::REQLIN, &
				REGARDLESS
		USE
			CONTINUE 4260
		END WHEN
	NEXT

4260	!
	! Ok, we've gotten the max line from the register,
	! now we need to see if there is a max line in the journal
	!
	WP_ISSLINE::REQNUM	= WP_ISSJOUR::REQNUM
	WP_ISSLINE::JOB		= WP_ISSJOUR::JOB
	WP_ISSLINE::LLINE	= WP_ISSJOUR::LLINE
	WP_ISSLINE::REQLINE	= LAST_LINE$

	WHILE WP_ISSLINE::REQNUM = WP_ISSJOUR::REQNUM AND &
		WP_ISSLINE::JOB = WP_ISSJOUR::JOB AND &
		WP_ISSLINE::LLINE = WP_ISSJOUR::LLINE

		LAST_LINE$ = WP_ISSLINE::REQLINE &
			IF LAST_LINE$ < WP_ISSLINE::REQLINE

		WHEN ERROR IN
			GET #WP_ISSLINE.CH%, KEY #0% GT &
				WP_ISSLINE::REQNUM + &
				WP_ISSLINE::JOB + &
				WP_ISSLINE::LLINE + &
				WP_ISSLINE::REQLINE
		USE
			IF ERR = 154%	! Locked Block
			THEN
				SLEEP 5%
				RETRY
			END IF

			CONTINUE 4270
		END WHEN
	NEXT

4270	!
	! Increment the line number
	!
	V% = FUNC_INCREMENT(LAST_LINE$)
	WP_REQREGISTER::REQLIN = LAST_LINE$

4280	!
	! At last, we can create a new line
	!
	WP_ISSLINE::REQNUM	= WP_ISSJOUR::REQNUM
	WP_ISSLINE::JOB		= WP_ISSJOUR::JOB
	WP_ISSLINE::LLINE	= WP_ISSJOUR::LLINE
	WP_ISSLINE::REQLINE	= WP_REQREGISTER::REQLIN
	WP_ISSLINE::PRODUCT	= WP_REQREGISTER::PRODUCT
	WP_ISSLINE::COST	= PC_READ_COST(WP_REQREGISTER::PRODUCT, &
		WP_REQREGISTER::LOCATION, &
		DATE_TODAY, "")
	IF (LEFT(RBUFF$(3%), 1%) = "C")
	THEN
		CANQTY = VAL(RIGHT(RBUFF$(3%), 2%))
		ISSQTY = 0.0
	ELSE
		CANQTY = 0.0
		ISSQTY = VAL(RBUFF$(3%))
	END IF
	WP_ISSLINE::QTYISSUE	= ISSQTY
	WP_ISSLINE::QTYCANCEL	= CANQTY
	WP_ISSLINE::ISSDATE	= DATE_TODAY
	WP_ISSLINE::QTYRUN	= 0.0
	WP_ISSLINE::PROD_FLAG	= "Y"

	PUT #WP_ISSLINE.CH%

4900	!
	! Adjust the inventory balances
	!
	V% = IC_WRIT_35BALANCE(WP_ISSLINE::PRODUCT, &
		LOCATION$, "RQ", &
		-ISSQTY)

	V% = IC_WRIT_35BALANCE(WP_ISSLINE::PRODUCT, &
		LOCATION$, "IS", &
		-ISSQTY)

	RETURN

5000	!*******************************************************************
	! WP Buyoff Journals
	!*******************************************************************

	PRINT "WP Issue Data: "; KEEP_READLINE$

	IF RBUFF% < 3%
	THEN
		PRINT "Bad entry: "; KEEP_READLINE$
		RETURN
	END IF

5005	!
	! Open SB_SUBACCOUNT file
	!
	IF (SB_SUBACCOUNT.CH% = 0%)
	THEN
		%INCLUDE "SOURCE:[SB.OPEN]SB_SUBACCOUNT.OPN"
	END IF

5010	!
	! Open PO register header if is isn't open yet
	!
	IF WP_BUYOFF.CH% = 0%
	THEN
		BATCH_NO$ = USE_BATCH_NO$
		%INCLUDE "SOURCE:[WP.OPEN]WP_BUYOFF.CRE"
	END IF

5020	!
	! Open PO register header if is isn't open yet
	!
	IF WP_BUYOFFLINE.CH% = 0%
	THEN
		BATCH_NO$ = USE_BATCH_NO$
		%INCLUDE "SOURCE:[WP.OPEN]WP_BUYOFFLINE.CRE"
	END IF

5100	!
	! Let's work on the header first. Does it already exist?
	!
	WP_BUYOFF::JOB = RBUFF$(1%)

	WHEN ERROR IN
		GET #WP_BUYOFF.CH%, KEY #0% EQ WP_BUYOFF::JOB, REGARDLESS
	USE
		CONTINUE 5150 IF ERR = 155%
		CONTINUE HelpError
	END WHEN

	GOTO 5200

5150	!
	! It doesn't exist, so let's create it
	!
	WP_BUYOFF::JOB		= RBUFF$(1%)
	WP_BUYOFF::ACCT		= READ_SET("WP_MAIN_BUYOFF", "2")
	WP_BUYOFF::OPERATOR	= ""

	PUT #WP_BUYOFF.CH%

5200	!
	! OK, Header now exists, so let's play with lines
	! Create initial information
	!
	WP_BUYOFFLINE::JOB	= WP_BUYOFF::JOB
	WP_BUYOFFLINE::LLINE	= FORMAT$(VAL(RBUFF$(3%)), "<0>###")
	IF LEFT(RBUFF$(4%), 1%) = "C"
	THEN
		COMQTY = 0.0
		CANQTY = VAL(RIGHT(RBUFF$(4%), 2%))
	ELSE
		COMQTY = VAL(RBUFF$(4%))
		CANQTY = 0.0
	END IF
	WP_BUYOFFLINE::COMPQTY	= COMQTY
	WP_BUYOFFLINE::CANCELQTY = CANQTY
	WP_BUYOFFLINE::COST	= 0.0
	WP_BUYOFFLINE::BDATE	= DATE_TODAY
	WP_BUYOFFLINE::ACCT	= ""
	WP_BUYOFFLINE::FG_ID_NO	= RBUFF$(2%)

	!
	! Look up the product number in the WP_REGLINE file,
	! if we can
	!
	WP_REGLINE_READ::ITEMCODE = ""
	WP_REGLINE_READ::DESCR =  ""

	V% = WP_READ_REGLINE(WP_BUYOFFLINE::JOB, &
		WP_BUYOFFLINE::LLINE, "EQ", &
		WP_REGLINE_READ, QTY())

	GOTO 5290 IF WP_REGLINE_READ::LLINE <> WP_BUYOFFLINE::LLINE

5210	!
	! Get the job file information
	!
	JC_JOB::SSTATUS = ""
	JC_JOB::TTYPE = ""
	JC_JOB::CLASS = ""
	JC_JOB::DESCR = ""
	JC_JOB::LOCATION = LOCATION$

	WHEN ERROR IN
		GET #SB_SUBACCOUNT.CH%, KEY #0% EQ "J" + &
			WP_BUYOFF::JOB, REGARDLESS
	USE
		CONTINUE 5220
	END WHEN

5220	!
	! Pull in other bits from wherever we cab
	!
	V% = PD_EXAM_PRODUCT(WP_REGLINE_READ::ITEMCODE, &
		PD_PRODUCT_EXAM)

	V% = PD_READ_ACCOUNT(JC_JOB::LOCATION, &
		PD_PRODUCT_EXAM::PROD_TYPE, PD_ACCOUNT_READ)

	WP_BUYOFFLINE::ACCT = PD_ACCOUNT_READ::INVACCT

	WP_BUYOFFLINE::COST = PC_READ_COST( &
		WP_REGLINE_READ::ITEMCODE, &
		JC_JOB::LOCATION, &
		WP_BUYOFFLINE::BDATE, "")

	IF WP_REGLINE_READ::TTYPE = "M"
	THEN
		V% = IC_WRIT_35BALANCE (WP_REGLINE_READ::ITEMCODE, &
			JC_JOB::LOCATION, "MA", &
			WP_BUYOFFLINE::COMPQTY)
	END IF

5290	!
	! Does this line already exist
	!
	WHEN ERROR IN
		GET #WP_BUYOFFLINE.CH%, &
			KEY #1% EQ WP_BUYOFFLINE::LLINE + &
			WP_BUYOFFLINE::JOB+ &
			WP_BUYOFFLINE::FG_ID_NO
	USE
		IF ERR = 154%	! Locked Block
		THEN
			SLEEP 5%
			RETRY
		END IF

		CONTINUE 5295
	END WHEN

	WP_BUYOFFLINE::COMPQTY	= WP_BUYOFFLINE::COMPQTY + COMQTY
	WP_BUYOFFLINE::CANCELQTY = WP_BUYOFFLINE::CANCELQTY + CANQTY

	WHEN ERROR IN
		UPDATE #WP_BUYOFFLINE.CH%
	USE
		CONTINUE 5295
	END WHEN

	GOTO 5299

5295	!
	! And, finally, add the record
	!
	PUT #WP_BUYOFFLINE.CH%

5299	RETURN

 ExitProgram:
	GOTO 32767

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	PRINT "Error: " + NUM1$(ERL) + " " + ERT$(ERR)
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
