1	!
	!++
	! Description:
	!
	!	MERGE two PD_PRODUCT master files together.
	!	BE CAREFUL THAT THIS IS WHAT YOU REALLY WANT TO DO
	!
	! History:
	!
	!	04/01/2002 - Kevin Handy
	!--

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT) PD_PRODUCT_CDD PD_PRODUCT

100	!
	! Open up product master file
	!
	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"
	PD_PRODUCT_PLAZA.CH% = PD_PRODUCT.CH%
	RESET #PD_PRODUCT_PLAZA.CH%
	PD_PRODUCT.CH% = 0%

200	!
	! Open up other product file
	!
	PD_PRODUCT.DEV$ = "$DISK2:[ROBSON]"
	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.MOD"
	PD_PRODUCT_MARCO.CH% = PD_PRODUCT.CH%
	PD_PRODUCT.CH% = 0%

1000	!
	! Search for the next "O" category item
	!
	WHEN ERROR IN
		GET #PD_PRODUCT_PLAZA.CH%, REGARDLESS
	USE
		CONTINUE 2000
	END WHEN

1010	WHEN ERROR IN
		PUT #PD_PRODUCT_MARCO.CH%
	USE
		PRINT "FAILED: " + PD_PRODUCT::PRODUCT_NUM
		CONTINUE 1900
	END WHEN

	PRINT "ADDED: " + &
		PD_PRODUCT::PRODUCT_NUM + " " + &
		PD_PRODUCT::DESCRIPTION

1900	GOTO 1000

2000	!

32767	END
