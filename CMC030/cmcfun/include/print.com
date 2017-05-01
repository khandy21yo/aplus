	!------------------------------------------------------------------
	! Common file for printer control sequences
	!------------------------------------------------------------------

	DECLARE INTEGER CONSTANT PRINT.MAXGRP = 30
	DECLARE INTEGER CONSTANT PRINT.MAXITM = 100

 !	%INCLUDE "FUNC_SOURCE:PRINT35.INC"

	COM (PRINTX) &
		PRINT.GROUPS%,				! Number of groups &
		PRINT.GROUP$(PRINT.MAXGRP) = 2%,	! Groups &
		PRINT.GROUP%(PRINT.MAXGRP),		! Pointer to lines &
		PRINT.DEF$(PRINT.MAXGRP) = 6%,		! Default value &
		PRINT.DESC$(PRINT.MAXGRP) = 20%,	! Description &
		PRINT.ITEMS%,				! Number of items &
		PRINT.ITEM$(PRINT.MAXITM) = 6%,		! items &
		PRINT.SEQ$(PRINT.MAXITM) = 32%		! Sequences

 !	COM (PRINTX) PRINTX_CDD PRINTX
