1	!
	!++
	! Description:
	!
	!	Generate a file containing a list of belted chain parts that
	!	are to be modified to drop them from BM
	!
	! History:
	!
	!	06/04/2002 - Kevin Handy
	!--

	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.HB"
	MAP (PD_PRODUCT) PD_PRODUCT_CDD PD_PRODUCT

100	!
	! Open up product master file
	!
	%INCLUDE "SOURCE:[PD.OPEN]PD_PRODUCT.OPN"

190	!
	! Open text file for results
	!
	OUTFILE.CH% = 2%
	OPEN "CHBT.TXT" FOR OUTPUT AS FILE OUTFILE.CH%

1000	!
	! Locate first record of interest
	!
	WHEN ERROR IN
		FIND #PD_PRODUCT.CH%, KEY #2% GE "CHBT", REGARDLESS
	USE
		CONTINUE 1900
	END WHEN

1100	!
	! Get next record
	!
	WHEN ERROR IN
		GET #PD_PRODUCT.CH%, REGARDLESS
	USE
		CONTINUE 1900
	END WHEN

	GOTO 1900 IF PD_PRODUCT::CATEGORY <> "CHBT"

	IF LEFT(PD_PRODUCT::DESCRIPTION, 8%) = "CHN BLTD"
	THEN
		PRINT #OUTFILE.CH%, PD_PRODUCT::PRODUCT_NUM
	END IF

	GOTO 1100

1900	!
	! DONE
	!
	CLOSE OUTFILE.CH%
	CLOSE PD_PRODUCT.CH%

32767	END
