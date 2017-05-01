1	%TITLE "Option Group Report"
	%SBTTL "MO_RPRT_OPTGROUP"
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
	! ID:MO0044
	!
	! Abstract:HELP
	!	.b
	!	.lm +5
	!	The ^*Option Group\* report contains the following information:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	Option Group
	!	.le
	!	Description
	!	.le
	!	Sequence
	!	.le
	!	Option
	!	.le
	!	Description
	!	.le
	!	Product
	!	.els
	!	.lm -5
	!	The means to print only a partial list of the Option Group is provided,
	!	along with the means to print the Group file without the line items.
	!	.lm -5
	!
	! Index:
	!	.x Option Group>Report
	!	.x Report>Option Group
	!
	! Author:
	!
	!	03/11/91 - Craig Tanner
	!
	! Compile:
	!
	!	$ BAS MO_SOURCE:MO_RPRT_OPTGROUP
	!	$ LINK/EXECUTABLE=MO_EXE:*.EXE MO_RPRT_OPTGROUP, -
	!		FUNC_LIB:CMCLINK/OPTION
	!	$ DELETE MO_RPRT_OPTGROUP.OBJ;*
	!
	! Modification history:
	!
	!	10/04/91 - Dan Perkins
	!		Cleaned up program code.
	!		Checked error trapping.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	07/18/96 - Kevin Handy
	!		Reformat source code.
	!
	!	05/15/97 - Kevin Handy
	!		Reformat source code.
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	12/17/2000 - Kevin Handy
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
	DECLARE UTL_REPORTX_CDD UTL_REPORTX

	%INCLUDE "SOURCE:[MO.OPEN]MO_OPTGROUP.HB"
	MAP	(MO_OPTGROUP)	MO_OPTGROUP_CDD	MO_OPTGROUP

	%INCLUDE "SOURCE:[MO.OPEN]MO_OPTION.HB"
	MAP	(MO_OPTION)	MO_OPTION_CDD	MO_OPTION

	%PAGE

	ON ERROR GOTO 19000

 Init:	!
	! Initilize report
	!
	CALL OUTP_INITFROMFILE(UTL_REPORTX, 80%)

	GOTO ExitProgram IF UTL_REPORTX::STAT

	SORT_BY$ = EDIT$(UTL_REPORTX::OPTDEF(0%), 132%)

	!++
	! Abstract:FLD01
	!	^*(01) Sort By \*
	!	.b
	!	.lm +5
	!	The ^*Sort by\* field sorts the Option
	!	Group file by either the Option Group Code or the Sequence Number.
	!	.b
	!	The following are valid:
	!	.b
	!	.lm 15
	!	.list 0,"*"
	!	.le
	!	G = Group
	!	.le
	!	S = Sequence
	!	.els
	!	.b
	!	.lm -5
	!	An entry is required in this field.
	!	.lm -5
	!
	! Index:
	!	.x Sortby>Option Group
	!	.x Option Group>Sortby
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
	!	^*(03) To Item \*
	!	.b
	!	.lm +5
	!	The ^*To Item\* field causes the printing
	!	to end with a particular Group.
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

	OPTIONS$ = EDIT$(UTL_REPORTX::OPTDEF(4%), 132%)

	!++
	! Abstract:FLD05
	!	^*(05) Print Options \*
	!	.b
	!	.lm +5
	!	The ^*Print Options\* prints the report
	!	with or without the Option line items under each Option
	!	Group.  A ^*Y\* (Yes) or ^*N\* (No) entry is required.
	!
	! Index:
	!	.x Options>Option Group
	!	.x Option Group>Options
	!
	!--

300	WHEN ERROR IN
		%INCLUDE "SOURCE:[MO.OPEN]MO_OPTGROUP.OPN"
	USE
		FILENAME$ = "MO_OPTGROUP"
		CONTINUE HelpError
	END WHEN

310	WHEN ERROR IN
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
	SELECT SORT_BY$
	CASE "G"
		K_NUM% = 0%
		TITLE$(1%) = "OPTION REPORT BY GROUP"
	CASE "S"
		K_NUM% = 1%
		TITLE$(1%) = "OPTION REPORT BY SEQUENCE"
	END SELECT

	TITLE$(2%) = "Manufacturing Order System"
	TITLE$(3%) = ""

	!
	! Heading
	!
	TITLE$(4%) = "OptGroup Description                              Sequence"

	IF OPTIONS$ = "Y"
	THEN
		TITLE$(5%) = "    Option Description                              Product"
		TITLE$(6%) = "."
	ELSE
		TITLE$(5%) = "."
	END IF

	%PAGE

17000	!******************************************************************
	! OUTPUT REPORT
	!******************************************************************

	WHEN ERROR IN
		IF FROM_ITEM$ = ""
		THEN
			RESET #MO_OPTGROUP.CH%, KEY #K_NUM%
		ELSE
			FIND #MO_OPTGROUP.CH%, &
				KEY #K_NUM% GE FROM_ITEM$, &
				REGARDLESS
		END IF
	USE
		CONTINUE ExitProgram IF ERR = 155%
		FILENAME$ = "MO_OPTGROUP"
		CONTINUE HelpError
	END WHEN

 GetNextRec:
17020	GOTO ExitProgram IF UTL_REPORTX::STAT

	WHEN ERROR IN
		GET #MO_OPTGROUP.CH%, REGARDLESS
	USE
		CONTINUE ExitProgram IF ERR = 11%
		FILENAME$ = "MO_OPTGROUP"
		CONTINUE HelpError
	END WHEN

	!
	! Check current record
	!
	SELECT SORT_BY$

	CASE "G"
		GOTO ExitProgram IF (MO_OPTGROUP::OPTGROUP > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING &
			(EDIT$(MO_OPTGROUP::OPTGROUP, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""
	CASE "S"
		GOTO ExitProgram IF (MO_OPTGROUP::SEQUENCE > TO_ITEM$) &
			AND TO_ITEM$ <> ""

		GOTO GetNextRec IF COMP_STRING &
			(EDIT$(MO_OPTGROUP::SEQUENCE, -1%), WLDCRD$) = 0% &
			AND WLDCRD$ <> ""
	END SELECT

	!
	! Print out one line
	!
	TEXT$ = MO_OPTGROUP::OPTGROUP + "       " + &
		MO_OPTGROUP::DESCR + " " + &
		MO_OPTGROUP::SEQUENCE

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	!
	! Try for line items
	!
	IF OPTIONS$ = "Y"
	THEN
		GOSUB GetOptions
	END IF

	GOTO GetNextRec

 GetOptions:
17200	!
	! Get Option line items
	!
	WHEN ERROR IN
		GET #MO_OPTION.CH%, KEY #0% GE MO_OPTGROUP::OPTGROUP
	USE
		CONTINUE GetNextRec IF ERR = 155% OR ERR = 11% OR ERR = 9%
		FILENAME$ = "MO_OPTION"
		CONTINUE HelpError
	END WHEN

 GetNextOpt:
	IF MO_OPTION::OPTGROUP <> MO_OPTGROUP::OPTGROUP
	THEN
		CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), "", 0%)
		RETURN
	END IF

	!
	! Print out the line
	!
	TEXT$ = MO_OPTGROUP::OPTGROUP + " " + &
		MO_OPTION::OPTN + "   " + &
		MO_OPTION::DESCR + " " + &
		MO_OPTION::PRODUCT

	CALL OUTP_LINE("", UTL_REPORTX, TITLE$(), TEXT$, 0%)
	GOTO ExitProgram IF UTL_REPORTX::STAT

	GET #MO_OPTION.CH%, REGARDLESS
	GOTO GetNextOpt

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
