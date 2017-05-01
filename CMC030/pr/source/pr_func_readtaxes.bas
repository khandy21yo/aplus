1	%TITLE "Calculate Taxes Taxable Reportable"
	%SBTTL "PR_FUNC_READTAXES"
	%IDENT "V3.6a Calico"

	FUNCTION LONG PR_FUNC_READTAXES( &
		THIS_EMPNUM$, &
		PR_REG_TAXES.CH%, &
		PR_TAXES%, &
		PR_TAXES_STRUCT PR_TAXES())

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
	!	This function reads tax information from the quarterly
	!	register.
	!	.lm -5
	!
	! Index:
	!	.X Read Taxes>Register
	!	.x Register>Read Taxes
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS PR_SOURCE:PR_FUNC_READTAXES/LINE
	!	$ LIB FUNC_LIB:CMCFUN/REP PR_FUNC_READTAXES
	!	$ DELETE PR_FUNC_READTAXES.OBJ;*
	!
	! Author:
	!
	!	11/27/87 - Robert Peterson
	!
	! Modification history:
	!
	!	12/31/90 - Kevin Handy
	!		Taken from the program PR_FUNC_CALCTAXES and
	!		modified for new file layout of PR_REG_TAXES.
	!		This made it an increadibly much smaller function.
	!
	!	01/17/90 - Kevin Handy
	!		Modified to fix bug where it wasn't filling the
	!		totals into (4).
	!
	!	06/03/91 - Kevin Handy
	!		Removed debug code in error trapping.
	!
	!	07/15/91 - Kevin Handy
	!		Removed PR_REG_ERNDED.CH% and PR_ERNDED_DEF.CH%
	!		from parameter list.
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	08/21/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	09/13/2000 - Kevin Handy
	!		Use WHEN ERROR IN
	!--
	%PAGE

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:PR_STRUCTURE.INC"

	%INCLUDE "SOURCE:[PR.OPEN]PR_REG_TAXES.HB"
	MAP (PR_REG_TAXES) PR_REG_TAXES_CDD PR_REG_TAXES

	%PAGE

	!
	! Initialize
	!
	PR_TAXES% = 0%

1000	!*******************************************************************
	! Read in TAXWH file
	!*******************************************************************

	WHEN ERROR IN
		FIND #PR_REG_TAXES.CH%, KEY #0% GE THIS_EMPNUM$, REGARDLESS
	USE
		CONTINUE 32767
	END WHEN

1010	WHEN ERROR IN
		GET #PR_REG_TAXES.CH%, REGARDLESS
	USE
		CONTINUE 32767
	END WHEN

	IF PR_REG_TAXES::EMPNUM = THIS_EMPNUM$
	THEN
		PR_TAXES% = PR_TAXES% + 1%

		PR_TAXES(PR_TAXES%)::TTYPE		= PR_REG_TAXES::TTYPE
		PR_TAXES(PR_TAXES%)::CODE		= PR_REG_TAXES::CODE
		PR_TAXES(PR_TAXES%)::TAXABLE(4%) = 0.0
		PR_TAXES(PR_TAXES%)::REPORTABLE(4%) = 0.0
		PR_TAXES(PR_TAXES%)::TAX(4%)	= 0.0
		PR_TAXES(PR_TAXES%)::WKWRK(4%)	= 0.0
		FOR I% = 0% TO 3%
			PR_TAXES(PR_TAXES%)::TAXABLE(I%) = &
				PR_REG_TAXES::TAXABLE(I%)
			PR_TAXES(PR_TAXES%)::TAXABLE(4%) = &
				PR_TAXES(PR_TAXES%)::TAXABLE(4%) + &
				PR_REG_TAXES::TAXABLE(I%)
			PR_TAXES(PR_TAXES%)::REPORTABLE(I%) = &
				PR_REG_TAXES::REPORTABLE(I%)
			PR_TAXES(PR_TAXES%)::REPORTABLE(4%) = &
				PR_TAXES(PR_TAXES%)::REPORTABLE(4%) + &
				PR_REG_TAXES::REPORTABLE(I%)
			PR_TAXES(PR_TAXES%)::TAX(I%) = PR_REG_TAXES::TAX(I%)
			PR_TAXES(PR_TAXES%)::TAX(4%) = &
				PR_TAXES(PR_TAXES%)::TAX(4%) + &
				PR_REG_TAXES::TAX(I%)
			PR_TAXES(PR_TAXES%)::WKWRK(I%) = PR_REG_TAXES::WKWRK(I%)
			PR_TAXES(PR_TAXES%)::WKWRK(4%) = &
				PR_TAXES(PR_TAXES%)::WKWRK(4%) + &
				PR_REG_TAXES::WKWRK(I%)
		NEXT I%

		GOTO 1010
	END IF

	GOTO 32767

32767	END FUNCTION
