	!
	! File Layout for: TK.TK_FOREIGN on 21-May-01
	!
	! Foreign Key Definition
	!

	RECORD TK_FOREIGN_CDD
		! Element =
		!   Description = Sub-record structure
		STRING SUBSTRUCT = 50
		! Element =
		!   Description = Field names used in the association
		STRING FLDNAMES = 60
		! Element =
		!   Description = Record structure
		STRING STRUCT = 50
		! Element =
		!   Description = Sub-record structure association
		STRING SUBASSOCIATE = 1
		! Element =
		!   Description = Record Structure association
		STRING ASSOCIATE = 1
	END RECORD
