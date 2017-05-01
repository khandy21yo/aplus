1	%TITLE "Initilize to Use a Form"
	%SBTTL "OUTP_FORMINIT"
	%IDENT "V3.6a Calico"

	FUNCTION LONG OUTP_FORMINIT(FORM_LIB$, FORM_NAME$, FORM_TEXT$, &
		FORM_GROUP%, FORM_GROUP_CDD FORM_GROUP())
	!
	! COPYRIGHT (C) 1996 BY
	! Software Solutions, Inc.
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
	! or reliability of its software on equipment which is not supported
	! by Software Solutions, Inc.
	!
	!++
	!
	! Abstract:HELP
	!	.p
	!	This function is used to load in a definition of a form, and do
	!	preliminary processing on it.
	!
	! Parameters:
	!
	!	FORM_LIB$
	!		The passed form library
	!
	!	FORM_NAME$
	!		The name of the form to load in.
	!
	!	FORM_TEXT$
	!		The passed text of the form.
	!
	!	FORM_GROUP
	!		Passed pointers to the start of each group
	!		of the form.
	!
	!	FORM_GROUP%
	!		Number of groups that make up this form.
	!
	! Example:
	!
	!
	! Compile:
	!
	!	$ BAS FUNC_SOURCE:OUTP_FORMINIT
	!	$ LIB FUNC_LIB:CMCFUN/REP OUTP_FORMINIT
	!	$ DELETE OUTP_FORMINIT.OBJ;*
	!
	! Author:
	!
	!	10/20/96 - Kevin Handy
	!		Moved original source to OUTP_36FORMINIT, and
	!		replaced this with a stub to call that version.
	!
	! Modification history:
	!
	!	08/19/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	04/08/99 - Kevin Handy
	!		Added definition of SCOPE to avoid crashes
	!--
	%PAGE

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"
	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[UTL.OPEN]FORM_GROUP.HB"

	EXTERNAL LONG FUNCTION OUTP_36FORMINIT

	OUTP_FORMINIT = OUTP_36FORMINIT(SCOPE, &
		FORM_LIB$, FORM_NAME$, FORM_TEXT$, &
		FORM_GROUP%, FORM_GROUP())

32767	END FUNCTION
