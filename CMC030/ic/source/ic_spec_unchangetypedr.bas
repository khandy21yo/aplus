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

100	!
	! Open up product master file
	!
	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.MOD"

	RESET #PD_PRODUCT.CH%

200	OPEN "UNCHANGEDR.TXT" FOR INPUT AS FILE 1%

1000	!
	! Search for the next "O" category item
	!
	LINPUT #1%, PARTNO$

	PARTNO$ = LEFT(PARTNO$ + "              ", 14%)

	WHEN ERROR IN
		GET #PD_PRODUCT.CH%, KEY #0% EQ PARTNO$
	USE
		CONTINUE 1000
	END WHEN

1100	PRINT PD_PRODUCT::PRODUCT_NUM

	PD_PRODUCT::PROD_TYPE = "MP"
	UPDATE #PD_PRODUCT.CH%

	GOTO 1000

32767	END
