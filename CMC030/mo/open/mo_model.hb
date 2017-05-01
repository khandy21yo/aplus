	!
	! File Layout for: MO.MO_MODEL on 21-May-01
	!
	! Model Base Definition File
	!

	RECORD MO_MODEL_CDD
		! Element =
		!   Description = Model Code
		STRING MODELCODE = 4
		! Element =
		!   Description = Make Size
		STRING MSIZE = 4
		! Element =
		!   Description = Make Class
		STRING CLASS = 4
		! Element = PRODUCT
		!   Description = Product Number
		STRING PRODUCT = 14
		! Element = PRODUCT
		!   Description = Box Product Number
		STRING BPRODUCT = 14
	END RECORD
