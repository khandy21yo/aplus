	!
	! File Layout for: PD.PD_PACK on 21-May-01
	!
	! Product Pack Description
	!

	RECORD PD_PACK_CDD
		! Element =
		!   Description = Pack Code
		STRING CODE = 4
		! Element =
		!   Description = Packaging form
		STRING FORM = 3
		! Element =
		!   Description = Packaging material
		STRING MATERIAL = 2
		! Element =
		!   Description = Volume unit of measure
		STRING UOM = 2
		! Element =
		!   Description = Units in one pack
		GFLOAT PACK_FACTOR
		! Element =
		!   Description = Weight of one unit in case
		GFLOAT WEIGHT_FACTOR
		! Element =
		!   Description = Product units in one pack
		GFLOAT PRODUCT_FACTOR
	END RECORD
