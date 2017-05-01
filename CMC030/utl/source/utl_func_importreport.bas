1	%TITLE "Batch Monitor Function"
	%SBTTL "UTL_FUNC_IMPORTREPORT"
	%IDENT "V3.6a Calico"

	FUNCTION LONG UTL_FUNC_IMPORTREPORT(LONG IMPORT.CH, LONG UTL_REPORT.CH)

	!
	! COPYRIGHT (C) 1998 BY
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
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	Used to load in import files.
	!	.lm -5
	!
	! Index:
	!
	! Compile:
	!
	!	$ BAS UTL_SOURCE:UTL_FUNC_IMPORTREPORT
	!	$ LIB FUNC_LIB:CMCFUN/REP UTL_FUNC_IMPORTREPORT
	!	$ DELETE UTL_FUNC_IMPORTREPORT.OBJ;*
	!
	! Author:
	!
	!	12/15/98 - Kevin Handy
	!
	! Modification history:
	!
	!	09/17/99 - Kevin Handy
	!		Modified so that it will not change the user
	!		entry fields if the record already exists.
	!		(HAVE_RECORD% business)
	!
	!	09/17/99 - Kevin Handy
	!		Use WHEN ERROR IN instead of ON ERROR GOTO
	!--
	%PAGE

	!
	! Define options
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORT.HB"
	MAP (UTL_REPORT)	UTL_REPORT_CDD	UTL_REPORT

	%PAGE

2000	!
	! Grab one line from the source file
	!
	WHEN ERROR IN
		LINPUT #IMPORT.CH, INLINE$
	USE
		CONTINUE 2900
	END WHEN

	I% = INSTR(1%, INLINE$, ">")

	GOTO 2000 IF I% = 0%

	INCOUNT% = INCOUNT% + 1%

	!
	! Split up the record
	!
	CODE$ = LEFT(INLINE$, I% - 1%)
	VALUE$ = RIGHT(INLINE$, I% + 1%)

2100	!
	! Process the data item
	!
	SELECT CODE$
	CASE "RN"
		!
		! This MUST be the first field of a record
		!
		GOSUB DumpRecord
		UTL_REPORT::REPNUM = VALUE$
		WHEN ERROR IN
			GET #UTL_REPORT.CH, KEY #0% EQ UTL_REPORT::REPNUM
			HAVE_RECORD% = -1%
		USE
			HAVE_RECORD% = 0%
		END WHEN

	CASE "PD"
		UTL_REPORT::REPYN = VALUE$
	CASE "SL"
		UTL_REPORT::SPOOL = VALUE$
	CASE "OD"
		IF HAVE_RECORD% = 0%
		THEN
			UTL_REPORT::DEFOUT = VALUE$
		END IF
	CASE "U1"
		IF HAVE_RECORD% = 0%
		THEN
			UTL_REPORT::OPTDEF(0%) = VALUE$
		END IF
	CASE "U2"
		IF HAVE_RECORD% = 0%
		THEN
			UTL_REPORT::OPTDEF(1%) = VALUE$
		END IF
	CASE "U3"
		IF HAVE_RECORD% = 0%
		THEN
			UTL_REPORT::OPTDEF(2%) = VALUE$
		END IF
	CASE "U4"
		IF HAVE_RECORD% = 0%
		THEN
			UTL_REPORT::OPTDEF(3%) = VALUE$
		END IF
	CASE "U5"
		IF HAVE_RECORD% = 0%
		THEN
			UTL_REPORT::OPTDEF(4%) = VALUE$
		END IF
	CASE "U6"
		IF HAVE_RECORD% = 0%
		THEN
			UTL_REPORT::OPTDEF(5%) = VALUE$
		END IF
	CASE "U7"
		IF HAVE_RECORD% = 0%
		THEN
			UTL_REPORT::OPTDEF(6%) = VALUE$
		END IF
	CASE "U8"
		IF HAVE_RECORD% = 0%
		THEN
			UTL_REPORT::OPTDEF(7%) = VALUE$
		END IF
	CASE "U9"
		IF HAVE_RECORD% = 0%
		THEN
			UTL_REPORT::OPTDEF(8%) = VALUE$
		END IF
	CASE "U0"
		IF HAVE_RECORD% = 0%
		THEN
			UTL_REPORT::OPTDEF(9%) = VALUE$
		END IF
	CASE "SF"
		IF HAVE_RECORD% = 0%
		THEN
			UTL_REPORT::SPOOLFORM = VALUE$
		END IF
	CASE "PT"
		IF HAVE_RECORD% = 0%
		THEN
			UTL_REPORT::PRINTTYPE = VALUE$
		END IF
	CASE "?SY"
		UTL_REPORT::SYSTEM = VALUE$
	CASE "?SB"
		UTL_REPORT::SUBSYS = VALUE$
	CASE "?RD"
		UTL_REPORT::REPDES = VALUE$
	CASE "?DV"
		UTL_REPORT::PRODEV = VALUE$
	CASE "?PN"
		UTL_REPORT::PRONAM = VALUE$
	CASE "?CS"
		UTL_REPORT::CANSPOOL = VALUE$
	CASE "?CD"
		UTL_REPORT::CANDISP = VALUE$
	CASE "?CV"
		UTL_REPORT::CANDEV = VALUE$
	CASE "?CF"
		UTL_REPORT::CANFILE = VALUE$
	CASE "?CE"
		UTL_REPORT::CANDET = VALUE$
	CASE "?YN"
		UTL_REPORT::REPYN = VALUE$

	CASE "?DE(0)"
		UTL_REPORT::DESCR(0%) = VALUE$
	CASE "?OT(0)"
		UTL_REPORT::OPTTYPE(0%) = VALUE$
	CASE "?OL(0)"
		UTL_REPORT::OPTLEN(0%) = VAL(VALUE$)
	CASE "?VL(0)"
		UTL_REPORT::VALID(0%) = VALUE$
	CASE "?RQ(0)"
		UTL_REPORT::REQUIRE(0%) = VALUE$
	CASE "?IG(0)"
		UTL_REPORT::ITEMGROUP(0%) = VALUE$
	CASE "?IT(0)"
		UTL_REPORT::ITEM(0%) = VALUE$

	CASE "?DE(1)"
		UTL_REPORT::DESCR(1%) = VALUE$
	CASE "?OT(1)"
		UTL_REPORT::OPTTYPE(1%) = VALUE$
	CASE "?OL(1)"
		UTL_REPORT::OPTLEN(1%) = VAL(VALUE$)
	CASE "?VL(1)"
		UTL_REPORT::VALID(1%) = VALUE$
	CASE "?RQ(1)"
		UTL_REPORT::REQUIRE(1%) = VALUE$
	CASE "?IG(1)"
		UTL_REPORT::ITEMGROUP(1%) = VALUE$
	CASE "?IT(1)"
		UTL_REPORT::ITEM(1%) = VALUE$

	CASE "?DE(2)"
		UTL_REPORT::DESCR(2%) = VALUE$
	CASE "?OT(2)"
		UTL_REPORT::OPTTYPE(2%) = VALUE$
	CASE "?OL(2)"
		UTL_REPORT::OPTLEN(2%) = VAL(VALUE$)
	CASE "?VL(2)"
		UTL_REPORT::VALID(2%) = VALUE$
	CASE "?RQ(2)"
		UTL_REPORT::REQUIRE(2%) = VALUE$
	CASE "?IG(2)"
		UTL_REPORT::ITEMGROUP(2%) = VALUE$
	CASE "?IT(2)"
		UTL_REPORT::ITEM(2%) = VALUE$

	CASE "?DE(3)"
		UTL_REPORT::DESCR(3%) = VALUE$
	CASE "?OT(3)"
		UTL_REPORT::OPTTYPE(3%) = VALUE$
	CASE "?OL(3)"
		UTL_REPORT::OPTLEN(3%) = VAL(VALUE$)
	CASE "?VL(3)"
		UTL_REPORT::VALID(3%) = VALUE$
	CASE "?RQ(3)"
		UTL_REPORT::REQUIRE(3%) = VALUE$
	CASE "?IG(3)"
		UTL_REPORT::ITEMGROUP(3%) = VALUE$
	CASE "?IT(3)"
		UTL_REPORT::ITEM(3%) = VALUE$

	CASE "?DE(4)"
		UTL_REPORT::DESCR(4%) = VALUE$
	CASE "?OT(4)"
		UTL_REPORT::OPTTYPE(4%) = VALUE$
	CASE "?OL(4)"
		UTL_REPORT::OPTLEN(4%) = VAL(VALUE$)
	CASE "?VL(4)"
		UTL_REPORT::VALID(4%) = VALUE$
	CASE "?RQ(4)"
		UTL_REPORT::REQUIRE(4%) = VALUE$
	CASE "?IG(4)"
		UTL_REPORT::ITEMGROUP(4%) = VALUE$
	CASE "?IT(4)"
		UTL_REPORT::ITEM(4%) = VALUE$

	CASE "?DE(5)"
		UTL_REPORT::DESCR(5%) = VALUE$
	CASE "?OT(5)"
		UTL_REPORT::OPTTYPE(5%) = VALUE$
	CASE "?OL(5)"
		UTL_REPORT::OPTLEN(5%) = VAL(VALUE$)
	CASE "?VL(5)"
		UTL_REPORT::VALID(5%) = VALUE$
	CASE "?RQ(5)"
		UTL_REPORT::REQUIRE(5%) = VALUE$
	CASE "?IG(5)"
		UTL_REPORT::ITEMGROUP(5%) = VALUE$
	CASE "?IT(5)"
		UTL_REPORT::ITEM(5%) = VALUE$

	CASE "?DE(6)"
		UTL_REPORT::DESCR(6%) = VALUE$
	CASE "?OT(6)"
		UTL_REPORT::OPTTYPE(6%) = VALUE$
	CASE "?OL(6)"
		UTL_REPORT::OPTLEN(6%) = VAL(VALUE$)
	CASE "?VL(6)"
		UTL_REPORT::VALID(6%) = VALUE$
	CASE "?RQ(6)"
		UTL_REPORT::REQUIRE(6%) = VALUE$
	CASE "?IG(6)"
		UTL_REPORT::ITEMGROUP(6%) = VALUE$
	CASE "?IT(6)"
		UTL_REPORT::ITEM(6%) = VALUE$

	CASE "?DE(7)"
		UTL_REPORT::DESCR(7%) = VALUE$
	CASE "?OT(7)"
		UTL_REPORT::OPTTYPE(7%) = VALUE$
	CASE "?OL(7)"
		UTL_REPORT::OPTLEN(7%) = VAL(VALUE$)
	CASE "?VL(7)"
		UTL_REPORT::VALID(7%) = VALUE$
	CASE "?RQ(7)"
		UTL_REPORT::REQUIRE(7%) = VALUE$
	CASE "?IG(7)"
		UTL_REPORT::ITEMGROUP(7%) = VALUE$
	CASE "?IT(7)"
		UTL_REPORT::ITEM(7%) = VALUE$

	CASE "?DE(8)"
		UTL_REPORT::DESCR(8%) = VALUE$
	CASE "?OT(8)"
		UTL_REPORT::OPTTYPE(8%) = VALUE$
	CASE "?OL(8)"
		UTL_REPORT::OPTLEN(8%) = VAL(VALUE$)
	CASE "?VL(8)"
		UTL_REPORT::VALID(8%) = VALUE$
	CASE "?RQ(8)"
		UTL_REPORT::REQUIRE(8%) = VALUE$
	CASE "?IG(8)"
		UTL_REPORT::ITEMGROUP(8%) = VALUE$
	CASE "?IT(8)"
		UTL_REPORT::ITEM(8%) = VALUE$

	CASE "?DE(9)"
		UTL_REPORT::DESCR(9%) = VALUE$
	CASE "?OT(9)"
		UTL_REPORT::OPTTYPE(9%) = VALUE$
	CASE "?OL(9)"
		UTL_REPORT::OPTLEN(9%) = VAL(VALUE$)
	CASE "?VL(9)"
		UTL_REPORT::VALID(9%) = VALUE$
	CASE "?RQ(9)"
		UTL_REPORT::REQUIRE(9%) = VALUE$
	CASE "?IG(9)"
		UTL_REPORT::ITEMGROUP(9%) = VALUE$
	CASE "?IT(9)"
		UTL_REPORT::ITEM(9%) = VALUE$

	CASE "?CT"
		UTL_REPORT::CHAINTO = VALUE$
	CASE "?LR"
		IF HAVE_RECORD% = 0%
		THEN
			UTL_REPORT::LASTRUNDATE = VALUE$
		END IF
	CASE "?LT"
		IF HAVE_RECORD% = 0%
		THEN
			UTL_REPORT::LASTRUNTIME = VALUE$
		END IF
	CASE "?BR"
		IF HAVE_RECORD% = 0%
		THEN
			UTL_REPORT::BASERUNDATE = VALUE$
		END IF
	CASE "?RF"
		UTL_REPORT::RUNFREQ = VALUE$
	CASE "?RW"
		UTL_REPORT::REPWID = VAL(VALUE$)
	CASE ELSE
		!
		! I don't understand what you're giving me
		!
		INCOUT% = INCOUNT% - 1%
	END SELECT

	GOTO 2000

2900	!
	! Finish up
	!
	GOSUB DumpRecord

	EXIT FUNCTION

	%PAGE

	!*******************************************************************
	! Try to add the record to the file
	!*******************************************************************
 DumpRecord:
3000	!
	! Make sure we have something to work with
	!
	GOTO 3100 IF HAVE_RECORD%

	IF INCOUNT% > 5%
	THEN
		WHEN ERROR IN
			PUT #UTL_REPORT.CH
		USE
			!
			! Record already exists, so try to update it
			!
			CONTINUE 3100
		END WHEN
	END IF

	INCOUNT% = 0%

	GOTO 3900

3100	!
	! Try to update the record
	!
	FIND #UTL_REPORT.CH, KEY #0% EQ UTL_REPORT::REPNUM

	UPDATE #UTL_REPORT.CH

3900	RETURN

	END FUNCTION
