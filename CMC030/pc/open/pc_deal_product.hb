	!
	! File Layout for: PC.PC_DEAL_PRODUCT on 22-Apr-02
	!
	! Products in a deal
	!

	RECORD PC_DEAL_PRODUCT_CDD
		! Element =
		!   Description = Deal number
		STRING DEAL =   20
		! Element = PRODUCT
		!   Description = Product Number
		STRING PRODUCT = 14
		! Element = AMOUNT
		!   Description = Price in deal
		GFLOAT PRICE
		! Element = PERCENTAGE
		!   Description = Percentage Discount
		GFLOAT PERCENT
		! Element = ACCOUNT
		!   Description = General Ledger Account Number
		STRING ACCOUNT = 18
	END RECORD
