1	%TITLE "Print Out a Section of the Form"
	%SBTTL "OUTP_FORMLINECOUNT"
	%IDENT "V3.6a Calico"

	FUNCTION LONG OUTP_FORMLINECOUNT(LONG PRINT_GROUP, &
		STRING FORM_TEXT, &
		LONG FORM_GROUPS, &
		FORM_GROUP_CDD FORM_GROUP())

	!
	!	COPYRIGHT (C) 1998 BY
	!	Software Solutions, Inc.
	!	Idaho Falls, Idaho.
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
	! Software Solutions, Inc assumes no responsibility for the use
	! or reliability of its software on equipment which is not
	! supported by Software Solutions, Inc
	!
	!++
	!
	! Abstract:HELP
	!	.p
	!	This function is used to determine how many lines would
	!	be printed with the formatting embeded in them.
	!
	! Parameters:
	!
	!	PRINT_GROUP
	!		Passed pointer to the proper group to print.
	!
	!	FORM_TEXT$
	!		The actual form to print.
	!
	!	FORM_GROUPS
	!		The passed number of groups on the form.
	!
	!	FORM_GROUP()
	!		The definition of the groups.
	!
	!	Returs number of lines that would be printed.
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:OUTP_FORMLINECOUNT/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP OUTP_FORMLINECOUNT
	!	$ DELETE OUTP_FORMLINECOUNT.OBJ;*
	!
	! Author:
	!
	!	01/06/98 - Kevin Handy
	!
	! Modification history:
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/15/99 - Kevin Handy
	!		Lose commented out code
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include necessary files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"
	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:PRINT35.INC"
	MAP (PRINTX) PRINTX_CDD PRINTX

	!
	! Define map variables
	!
	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REPORTX.HB"

	%INCLUDE "SOURCE:[UTL.OPEN]FORM_GROUP.HB"

	%PAGE

	!
	! Leave if nothing to print
	!
	IF PRINT_GROUP = 0%
	THEN
		OUTP_FORMLINECOUNT = 0%
		EXIT FUNCTION
	END IF

	!
	! Strip out the group for this part
	!
	THIS_PART$ = SEG$(FORM_TEXT, FORM_GROUP(PRINT_GROUP)::POINTER, &
		FORM_GROUP(PRINT_GROUP + 1%)::POINTER - 1%)

	OUTP_FORMLINECOUNT = LEN(XLATE(THIS_PART$, STRING$(10%, 0%) + "X"))

	END FUNCTION
