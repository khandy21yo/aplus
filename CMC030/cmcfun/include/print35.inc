	!------------------------------------------------------------------
	! Record definition for printer control sequences
	!------------------------------------------------------------------

	DECLARE INTEGER CONSTANT PRINT.MAXGRP = 30
	DECLARE INTEGER CONSTANT PRINT.MAXITM = 100

	RECORD PRINTX_CDD
		LONG	GROUPS				! Number of groups
		STRING	GROUPX(PRINT.MAXGRP) = 2%	! Groups
		LONG	GROUPP(PRINT.MAXGRP)		! Pointer to lines
		STRING	DEFLT(PRINT.MAXGRP) = 6%	! Default value
		STRING	DESCR(PRINT.MAXGRP) = 20%	! Description
		LONG	ITEMS				! Number of items
		STRING	ITEM(PRINT.MAXITM) = 6%		! items
		STRING	SEQU(PRINT.MAXITM) = 32%	! Sequences
	END RECORD

