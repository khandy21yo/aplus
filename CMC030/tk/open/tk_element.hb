	!
	! File Layout for: TK.TK_ELEMENT on 21-May-01
	!
	! Element Definition
	!

	RECORD TK_ELEMENT_CDD
		! Element =
		!   Description = Element
		STRING ELEMENT = 39
		! Element =
		!   Description = Element description
		STRING DESCR = 60
		! Element =
		!   Description = Type
		STRING ETYPE = 20
		! Element =
		!   Description = Size
		WORD ESIZE
		! Element =
		!   Description = Test file structure name
		STRING TESTSTRUCT = 39
	END RECORD
