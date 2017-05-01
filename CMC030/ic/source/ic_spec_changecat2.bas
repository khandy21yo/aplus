1	!
	!++
	! Description:
	!
	!	Special program to change Hoses from the HYD category
	!	to a new HOSE category.
	!
	! History:
	!
	!	02/27/2000 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!--

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT) PD_PRODUCT_CDD PD_PRODUCT

100	!
	! Open up product master file
	!
	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.MOD"

	RESET #PD_PRODUCT.CH%

1000	!
	! Search for the next "O" category item
	!
	GET #PD_PRODUCT.CH%

	GOTO 1000 UNLESS PD_PRODUCT::CATEGORY = "HYD" AND &
		LEFT(PD_PRODUCT::DESCRIPTION, 5%) = "HOSE,"

	PRINT PD_PRODUCT::PRODUCT_NUM; " "; PD_PRODUCT::DESCRIPTION

1010	PD_PRODUCT::CATEGORY = "HOSE"
	UPDATE #PD_PRODUCT.CH%

	GOTO 1000

32767	END
