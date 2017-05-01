	!
	! File Layout for: PP.PP_CARDEXEMPT on 21-May-01
	!
	! Pacific Pride Card Tax Exemption File
	!

	RECORD PP_CARDEXEMPT_CDD
		! Element = CUSTOMER
		!   Description = Customer Number
		STRING CUSNUM = 10
		! Element = CARD
		!   Description = Card Number
		STRING CARD = 8
		! Element = STATE
		!   Description = State
		STRING STATE = 2
		! Element =
		!   Description = Authority
		STRING AUTHORITY = 5
		! Element = PRODUCT
		!   Description = Product Number
		STRING PRODUCT = 14
	END RECORD
