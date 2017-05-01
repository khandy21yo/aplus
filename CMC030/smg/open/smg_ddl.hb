	!
	! File Layout for: SMG.SMG_DDL on 11-Mar-99
	!
	! Data Definition Array Communication
	!

	RECORD SMG_DDL_CDD
		! Element =
		!   Description = Description for file
		STRING DESCR = 80
		! Element =
		!   Description = Number of fields in file
		WORD FIELD_NUM
		! Element =
		!   Description = Field Name
		STRING FIELD_NAME(255) = 20
		! Element =
		!   Description = Field attribute
		STRING FIELD_ATTRIBUTE(255) = 20
		! Element =
		!   Description = Field type
		STRING FIELD_TYPE(255) = 20
		! Element =
		!   Description = Field element
		STRING FIELD_ELEMENT(255) = 20
		! Element =
		!   Description = Field size
		STRING FIELD_SIZE(255) = 4
		! Element =
		!   Description = Field description
		STRING FIELD_DESC(255) = 40
	END RECORD
