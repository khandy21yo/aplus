1	!
	!++
	! Description:
	!
	!	Special program to change 'MP' type items in the
	!	inventory master file to 'DR' when they have no
	!	components under them.
	!
	! History:
	!
	!	12/29/2003 - Kevin Handy
	!		Original version
	!
	!--

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT) PD_PRODUCT_CDD PD_PRODUCT

	%INCLUDE "SOURCE:[BM.OPEN]BM_RELATION.HB"
	MAP (BM_RELATION) BM_RELATION_CDD BM_RELATION

100	!
	! Open up product master file
	!
	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.MOD"

	RESET #PD_PRODUCT.CH%

200	%INCLUDE "SOURCE:[BM.OPEN]BM_RELATION.OPN"

1000	!
	! Search for the next "O" category item
	!
	GET #PD_PRODUCT.CH%

	IF PD_PRODUCT::PROD_TYPE <> "SA" OR PD_PRODUCT::SSTATUS <> "A"
	THEN
		GOTO 1000
	END IF

	WHEN ERROR IN
		GET #BM_RELATION.CH%, KEY #0% EQ PD_PRODUCT::PRODUCT_NUM
	USE
		!
		! No children, change it.
		!
		CONTINUE 1100 IF ERR = 155%
		PRINT ERR, ERL
		STOP
		CONTINUE 1000
	END WHEN

	GOTO 1000

1100	PRINT PD_PRODUCT::PRODUCT_NUM

	PD_PRODUCT::PROD_TYPE = "DR"
	UPDATE #PD_PRODUCT.CH%

	GOTO 1000

32767	END
