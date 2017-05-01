	!
	! File Layout for: BM.BM_MAXMIN on 02-Jun-00
	!
	! File to define MAX/MIN build information for reports
	!

	RECORD BM_MAXMIN_CDD
		! Element = PRODUCT
		!   Description = Built Product Number
		STRING PRODUCT = 14
		! Element =
		!   Description = Grouping Indicator
		STRING MGROUP = 4
		! Element = QUANTITY
		!   Description = Maximum Quantity to build
		GFLOAT MAXQTY
		! Element = QUANTITY
		!   Description = Minimum Quantity To Build
		GFLOAT MINQTY
	END RECORD
