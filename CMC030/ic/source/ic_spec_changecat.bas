1	!
	!++
	! Description:
	!
	!	Special program to change 'O' category items in the
	!	inventory master file to 'Z'.
	!
	! History:
	!
	!	09/02/97 - Kevin Handy
	!		Original version
	!
	!	08/19/98 - Kevin Handy
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

1000	!
	! Search for the next "O" category item
	!
	GET #PD_PRODUCT.CH%, KEY #2% EQ "O   "

	PRINT PD_PRODUCT::PRODUCT_NUM; " "; PD_PRODUCT::DESCRIPTION

1010	PD_PRODUCT::CATEGORY = "Z"
	UPDATE #PD_PRODUCT.CH%

	GOTO 1000

32767	END
