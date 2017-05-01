	!
	! File Layout for: MO.MO_OPTION on 21-May-01
	!
	! Option Definition Master File
	!

	RECORD MO_OPTION_CDD
		! Element =
		!   Description = Option Group
		STRING OPTGROUP = 2
		! Element =
		!   Description = Option Code
		STRING OPTN = 4
		! Element =
		!   Description = Option Description
		STRING DESCR = 40
		! Element = PRODUCT
		!   Description = Product Number
		STRING PRODUCT = 14
	END RECORD
