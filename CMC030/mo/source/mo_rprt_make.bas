1	%TITLE "Manufacture Order Make Report"
	%SBTTL "MO_RPRT_MAKE"
	%IDENT "V3.6a Calico"

	!
	! COPYRIGHT (C) 1991, BY
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
	! ID:MO0001
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Make\* report contains the following fields of information:
	!	.table 3,25
	!	.te
	!	Make	Description
	!	.te
	!	Model Code	Description
	!	.te
	!	Year	Type
	!	.te
	!	Size	Class
	!	.te
	!	Tubing	Slant
	!	.te
	!	Overall	Narrow Front
	!	.te
	!	Narrow Back
	!	.end table
	!	The means to print only a partial list of the Make file is provided by
	!	using the from and to fields.
	!	.lm -5
	!
	! Index:
	!	.x Make>Report
	!	.x Report>Make
	!
	! Author:
	!
	!	03/22/91 - Craig Tanner
	!
	! Compile:
	!
	!	$ BAS MO_SOURCE:MO_RPRT_MAKE
	!	$ LINK/EXECUTABLE=MO_EXE:*.EXE MO_RPRT_MAKE, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE MO_RPRT_MAKE.OBJ;*
	!
	! Modification history:
	!
	!	10/03/91 - Dan Perkins
	!		Cleaned program code.  Checked error trapping.
	!
	!	11/02/92 - Dan Perkins
	!		Fixed error trapping on 17210 to trap ERR 155 instead
	!		of ERR 11.
	!
	!	05/25/93 - Dan Perkins
	!		Added code to print down through Model options.
	!
	!	06/11/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	09/24/93 - Kevin Handy
	!		Added cost and two prices to output.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	06/26/96 - Kevin Handy
	!		Reformat source code.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	09/22/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!		Split up soem IF statements
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

	%INCLUDE "SOURCE:[MO.OPEN]MO_MAKE.HB"
	MAP (MO_MAKE)		MO_MAKE_CDD		MO_MAKE

	%INCLUDE "SOURCE:[MO.OPEN]MO_MAKELINE.HB"
	MAP (MO_MAKELINE)	MO_MAKELINE_CDD		MO_MAKELINE

	%INCLUDE "SOURCE:[MO.OPEN]MO_MODEL.HB"
	MAP (MO_MODEL)		MO_MODEL_CDD		MO_MODEL

	%INCLUDE "SOURCE:[MO.OPEN]MO_MODELCODE.HB"
	MAP (MO_MODELCODE)	MO_MODELCODE_CDD	MO_MODELCODE

	%INCLUDE "SOURCE:[MO.OPEN]MO_MODELLINE.HB"
	MAP (MO_MODELLINE)	MO_MODELLINE_CDD	MO_MODELLINE

	%INCLUDE "SOURCE:[MO.OPEN]MO_OPTGROUP.HB"
	MAP (MO_OPTGROUP)	MO_OPTGROUP_CDD		MO_OPTGROUP

	%INCLUDE "SOURCE:[MO.OPEN]MO_OPTION.HB"
	MAP (MO_OPTION)		MO_OPTION_CDD		MO_OPTION

	!
	! External functions
	!
	EXTERNAL REAL FUNCTION PC_READ_COST
	EXTERNAL REAL FUNCTION PC_READ_PRICE

	%PAGE

	ON ERROR GOTO 19000

 Init:	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 132%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	SORTBY$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) Sort by\*
	!	.b
	!	.lm +5
	!	The ^*Sort By\* field causes the report
	!	to be printed in a specific order.
	!	.b
	!	Valid settings are:
	!	.table 3,25
	!	.te
	!	^*M\* - Make
	!	.te
	!	^*Y\* - Year
	!	.te
	!	^*T\* - Type
	!	.te
	!	^*S\* - Size
	!	.te
	!	^*C\* - Class
	!	.end table
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x From>Option Group
	!	.x Option Group>From
	!
	!--

	FROM_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(1%), 132%)

	!++
	! Abstract:FLD02
	!	^*(02) From Item _#\*
	!	.b
	!	.lm +5
	!	The ^*From Item _#\* field causes the report
	!	to begin with a particular Group _#.
	!	.b
	!	A blank field will cause the report to start with the first
	!	Group _# in the file.
	!	.lm -5
	!
	! Index:
	!	.x From>Option Group
	!	.x Option Group>From
	!
	!--

	TO_ITEM$ = EDIT$(UTL_REPORTX::OPTDEF(2%), 132%)

	!++
	! Abstract:FLD03
	!	^*(03) To Item _#\*
	!	.b
	!	.lm +5
	!	The ^*To Item _#\* field causes the printing
	!	to end with a particular Group _#.
	!	.b
	!	A blank field will cause the report to end with the last Group _#
	!	in the file.
	!	.lm -5
	!
	! Index:
	!	.x To>Option Group
	!	.x Option Group>To
	!
	!--

	WLDCRD$ = EDIT$(UTL_REPORTX::OPTDEF(3%), -1%)

	!++
	! Abstract:FLD04
	!	^*(04) Wildcard\*
	!	.b
	!	.lm +5
	!	The ^*Wildcard\* field selects
	!	designated items to be printed by entering a "wildcard"
	!	for wildcarding technique.
	!	.lm -5
	!
	! Index:
	!	.x Wildcard
	!
	!--

	OPTIONS$ = EDIT$(UTL_REPORTX::OPTDEF(4%), -1%)

	!++
	! Abstract:FLD05
	!	^*(05) Print Options (Y/N)\*
	!	.b
	!	.lm +5
	!	The ^*Print Options\* field chooses
	!	whether model options will be printed.
	!	.lm -5
	!
	! Index:
	!
	!--

	FORLOCATION$ = SPACE$(4%)
	LSET FORLOCATION$ = EDIT$(UTL_REPORTX::OPTDEF(5%), -1%)

	!++
	! Abstract:FLD06
	!	^*(06) Location\*
	!
	! Index:
	!
	!--

	FORPRICE1$ = "  "
	LSET FORPRICE1$ = EDIT$(UTL_REPORTX::OPTDEF(6%), -1%)

	!++
	! Abstract:FLD07
	!	^*(07) Price Type 1\*
	!
	! Index:
	!
	!--

	FORPRICE2$ = "  "
	LSET FORPRICE2$ = EDIT$(UTL_REPORTX::OPTDEF(7%), -1%)

	!++
	! Abstract:FLD08
	!	^*(08) Price Type 2\*
	!
	! Index:
	!
	!--

	!
	! Open files
	!
300	WHEN ERROR IN
		%INCLUDE "SOURCE:[MO.OPEN]MO_MAKE.OPN"
	USE
		FILENAME$ = "MO_MAKE"
		CONTINUE HelpError
	END WHEN

310	WHEN ERROR IN
		%INCLUDE "SOURCE:[MO.OPEN]MO_MAKELINE.OPN"
	USE
		CONTINUE 320 IF ERR = 5%
		FILENAME$ = "MO_MAKELINE"
		CONTINUE HelpError
	END WHEN

320	WHEN ERROR IN
		%INCLUDE "SOURCE:[MO.OPEN]MO_MODEL.OPN"
	USE
		CONTINUE 330 IF ERR = 5%
		FILENAME$ = "MO_MODEL"
		CONTINUE HelpError
	END WHEN

330	WHEN ERROR IN
		%INCLUDE "SOURCE:[MO.OPEN]MO_MODELCODE.OPN"
	USE
		CONTINUE 340 IF ERR = 5%
		FILENAME$ = "MO_MODELCODE"
		CONTINUE HelpError
	END WHEN

340	WHEN ERROR IN
		%INCLUDE "SOURCE:[MO.OPEN]MO_MODELLINE.OPN"
	USE
		CONTINUE 350 IF ERR = 5%
		FILENAME$ = "MO_MODELLINE"
		CONTINUE HelpError
	END WHEN

350	WHEN ERROR IN
		%INCLUDE "SOURCE:[MO.OPEN]MO_OPTGROUP.OPN"
	USE
		CONTINUE 360 IF ERR = 5%
		FILENAME$ = "MO_OPTGROUP"
		CONTINUE HelpError
	END WHEN

360	WHEN ERROR IN
		%INCLUDE "SOURCE:[MO.OPEN]MO_OPTION.OPN"
	USE
		CONTINUE ReportTitle IF ERR = 5%
		FILENAME$ = "MO_OPTION"
		CONTINUE HelpError
	END WHEN


 ReportTitle:
	!
	! Title
	!
	TITLE$(1%) = "MAKE REPORT"

	SELECT SORTBY$

	CASE "M"
		K_NUM% = 0%
		TITLE$(1%) = TITLE$(1%) + " BY MAKE"

	CASE "Y"
		K_NUM% = 1%
		TITLE$(1%) = TITLE$(1%) + " BY YEAR"

	CASE "T"
		K_NUM% = 2%
		TITLE$(1%) = TITLE$(1%) + " BY TYPE"

	CASE "S"
		K_NUM% = 3%
		TITLE$(1%) = TITLE$(1%) + " BY SIZE"

	CASE "C"
		K_NUM% = 4%
		TITLE$(1%) = TITLE$(1%) + " BY CLASS"
	END SELECT

	TITLE$(2%) = "Manufacturing Order System"

	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "Make       Description                    Year Type " + &
		"Size Class Tubing   Slant    Overall   Narrow Front  Narrow Back"

	TITLE$(5%) = "  ModelCode  Description                     " + &
		"Product#        BoxProduct#"

	IF OPTIONS$ = "Y"
	THEN
		TITLE$(6%) = "    OptGrp  Description                " + &
			"Seq   Opt   Description                " + &
			"  Product#        Cost        " + &
			FORPRICE1$ + "        " + FORPRICE2$

		TITLE$(7%) = "."
	ELSE
		TITLE$(6%) = "."
	END IF

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************
	PAGE% = 0%

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #MO_MAKE.CH%, KEY #K_NUM%
		ELSE
			FIND #MO_MAKE.CH%, KEY #K_NUM% GE FROM_ITEM$, REGARDLESS
		END IF
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "MO_MAKE"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	GOTO ExitProgram IF UTL_REPORTX::STAT

	WHEN ERROR IN
		GET #MO_MAKE.CH%, REGARDLESS
	USE
		CONTINUE ExitProgram IF ERR = 11%
		FILENAME$ = "MO_MAKE"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	SELECT SORTBY$

	CASE "M"
		GOTO ExitProgram IF (MO_MAKE::MAKE > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_ARRAY &
			(EDIT$(MO_MAKE::MAKE, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "Y"
		GOTO ExitProgram IF (MO_MAKE::YEAR > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_ARRAY &
			(EDIT$(MO_MAKE::YEAR, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "T"
		GOTO ExitProgram IF (MO_MAKE::MTYPE > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_ARRAY &
			(EDIT$(MO_MAKE::MTYPE, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "S"
		GOTO ExitProgram IF (MO_MAKE::MSIZE > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_ARRAY &
			(EDIT$(MO_MAKE::MSIZE, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""

	CASE "C"
		GOTO ExitProgram IF (MO_MAKE::CLASS > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_ARRAY &
			(EDIT$(MO_MAKE::CLASS, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""
	END SELECT

	!
	! Print out one line
	!
	TEXT$ = MO_MAKE::MAKE + " " + &
		LEFT$(MO_MAKE::DESCR, 30%) + " " + &
		MO_MAKE::YEAR + " " + &
		MO_MAKE::MTYPE + "   " + &
		MO_MAKE::MSIZE + " " + &
		MO_MAKE::CLASS + "  " + &
		MO_MAKE::TUBING + " " + &
		MO_MAKE::SLANT + " " + &
		MO_MAKE::OVERALL + "  " + &
		MO_MAKE::NFRONT + "             " + &
		MO_MAKE::NBACK

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, PAGE%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	PAGE% = 0%

	!
	! Get Make line items
	!
17100	WHEN ERROR IN
		FIND #MO_MAKELINE.CH%, &
			KEY #0% GE MO_MAKE::MAKE + MO_MAKE::YEAR + &
			MO_MAKE::MTYPE + MO_MAKE::MSIZE, &
			REGARDLESS
	USE
		CONTINUE GetNextRec IF ERR = 155% OR ERR = 9%
		FILENAME$ = "MO_MAKELINE"
		CONTINUE HelpError
	END WHEN

 GetMakeLine:
17120	WHEN ERROR IN
		GET #MO_MAKELINE.CH%, REGARDLESS
	USE
		CONTINUE ExitMakeLine IF ERR = 11%
		FILENAME$ = "MO_MAKELINE"
		CONTINUE HelpError
	END WHEN

	GOTO ExitMakeLine IF MO_MAKELINE::MAKE <> MO_MAKE::MAKE OR &
		MO_MAKELINE::YEAR <> MO_MAKE::YEAR OR &
		MO_MAKELINE::MTYPE <> MO_MAKE::MTYPE OR &
		MO_MAKELINE::MSIZE <> MO_MAKE::MSIZE

	!
	! Get the model code description
	!
17200	WHEN ERROR IN
		GET #MO_MODELCODE.CH%, &
			KEY #0% EQ MO_MAKELINE::MODELCODE, &
			REGARDLESS
	USE
		MO_MODELCODE::DESCR = &
			STRING$(LEN(MO_MODELCODE::DESCR), A"?"B)

		CONTINUE 17300 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "MO_MODELCODE"
		CONTINUE HelpError
	END WHEN

	!
	! See if there are any product numbers
	!
17300	WHEN ERROR IN
		GET #MO_MODEL.CH%, &
			KEY #0% EQ MO_MAKELINE::MODELCODE + &
			MO_MAKELINE::MSIZE + MO_MAKELINE::MSIZE, &
			REGARDLESS
	USE
		MO_MODEL::PRODUCT = ""
		MO_MODEL::BPRODUCT = ""

		CONTINUE PrintCodeLine IF ERR = 155% OR ERR = 9%
		FILENAME$ = "MO_MODEL"
		CONTINUE HelpError
	END WHEN

 PrintCodeLine:
	!
	! Print out the line
	!
	TEXT$ = "  " + &
		MO_MAKELINE::MODELCODE + "       " + &
		LEFT(MO_MODELCODE::DESCR, 30%) + "  " + &
		MO_MODEL::PRODUCT + "  " + &
		MO_MODEL::BPRODUCT

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	GOTO GetMakeLine IF OPTIONS$ <> "Y"

	!
	! See if there are any options
	!
17400	WHEN ERROR IN
		FIND #MO_MODELLINE.CH%, &
			KEY #0% EQ MO_MAKELINE::MODELCODE + &
			MO_MAKE::MSIZE + MO_MAKE::CLASS, &
			REGARDLESS
	USE
		CONTINUE GetMakeLine IF ERR = 155% OR ERR = 9%
		FILENAME$ = "MO_MODELLINE"
		CONTINUE HelpError
	END WHEN

	TEST_GROUP$ = ""

 GetModelLine:
17420	WHEN ERROR IN
		GET #MO_MODELLINE.CH%, REGARDLESS
	USE
		CONTINUE GetMakeLine IF ERR = 11%
		FILENAME$ = "MO_MODELLINE"
		CONTINUE HelpError
	END WHEN

	GOTO GetMakeLine &
		IF MO_MODELLINE::MODELCODE <> MO_MAKELINE::MODELCODE OR &
		MO_MODELLINE::MSIZE <> MO_MAKE::MSIZE OR &
		MO_MODELLINE::CLASS <> MO_MAKE::CLASS

17500	WHEN ERROR IN
		GET #MO_OPTGROUP.CH%, &
			KEY #0% EQ MO_MODELLINE::OPTGROUP, &
			REGARDLESS
	USE
		CONTINUE 17600 IF ERR = 155% OR ERR = 9%
		FILENAME$ = "MO_OPTGROUP"
		CONTINUE HelpError
	END WHEN

17600	WHEN ERROR IN
		GET #MO_OPTION.CH%, &
			KEY #0% EQ MO_MODELLINE::OPTGROUP + &
			MO_MODELLINE::OPTN, &
			REGARDLESS
	USE
		MO_OPTION::DESCR = ""
		MO_OPTION::PRODUCT = ""

		CONTINUE PrintOptionLine IF ERR = 155% OR ERR = 9%
		FILENAME$ = "MO_OPTION"
		CONTINUE HelpError
	END WHEN

 PrintOptionLine:
	IF MO_MODELLINE::OPTGROUP = TEST_GROUP$
	THEN
		MO_OPTGROUP::DESCR     = ""
		MO_OPTGROUP::SEQUENCE  = ""
	ELSE
		TEST_GROUP$ = MO_MODELLINE::OPTGROUP
	END IF

	COST = PC_READ_COST(MO_OPTION::PRODUCT, FORLOCATION$, &
		DATE_TODAY, EFFDATE$)
	PRICE1 = PC_READ_PRICE(MO_OPTION::PRODUCT, FORLOCATION$, FORPRICE1$, &
		DATE_TODAY, "", EFFDATE$, EFFTIME$)
	PRICE2 = PC_READ_PRICE(MO_OPTION::PRODUCT, FORLOCATION$, FORPRICE2$, &
		DATE_TODAY, "", EFFDATE$, EFFTIME$)

	TEXT$ = "    " + &
		MO_MODELLINE::OPTGROUP + "      " + &
		LEFT(MO_OPTGROUP::DESCR, 25%) + "  " + &
		MO_OPTGROUP::SEQUENCE + "  " + &
		MO_MODELLINE::OPTN + "  " + &
		LEFT(MO_OPTION::DESCR, 25%) + "  " + &
		MO_OPTION::PRODUCT + " " + &
		FORMAT$(COST, "<%>##,###.## ") + &
		FORMAT$(PRICE1, "<%>##,###.## ") + &
		FORMAT$(PRICE2, "<%>##,###.## ")


	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	GOTO GetModelLine

 ExitMakeLine:
	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", -1%)

	PAGE% = 999% IF OPTIONS$ = "Y"

	GOTO GetNextRec

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
