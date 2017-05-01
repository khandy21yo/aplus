	!
	! File Layout for: BM.BM_PRODOPER on 21-May-01
	!
	! Product operation table
	!

	RECORD BM_PRODOPER_CDD
		! Element = PRODUCT
		!   Description = Product Number
		STRING PRODUCT = 14
		! Element = ITEMNUM
		!   Description = Oparation seq. number
		STRING ITEMNUM = 4
		! Element = OPERATION
		!   Description = Operation
		STRING OPERATION = 8
		! Element =
		!   Description = Number of hours
		GFLOAT HOURS
		! Element = DATE
		!   Description = Effective Date (YYYYMMDD)
		STRING EFFDATE = 8
		! Element =
		!   Description = Status (A,O..)
		STRING STAT = 1
		! Element =
		!   Description = Number of hours on this level
		GFLOAT THISHOURS
	END RECORD
