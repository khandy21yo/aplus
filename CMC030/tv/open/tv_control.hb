	!
	! File Layout for: TV.TV_CONTROL on 21-May-01
	!
	! Control File for TV Traffic
	!

	RECORD TV_CONTROL_CDD
		! Element =
		!   Description = Program number
		STRING PROGNUM = 10
		! Element =
		!   Description = Weight for priority
		GFLOAT WEIGHT_PRIORITY
		! Element =
		!   Description = Weight for spot length
		GFLOAT WEIGHT_LENGTH
		! Element =
		!   Description = Weight for spot cost
		GFLOAT WEIGHT_COST
		! Element =
		!   Description = Weight for spot randomness
		GFLOAT WEIGHT_RANDOM
		! Element =
		!   Description = Badness for each priority point
		GFLOAT BADNESS_PRIORITY
		! Element =
		!   Description = Badness for each spot in break already
		GFLOAT BADNESS_PER_SPOT
		! Element =
		!   Description = Badness for each confl. spot in break
		GFLOAT BADNESS_CONFLICT
		! Element =
		!   Description = Badness if break match code matches
		GFLOAT BADNESS_MATCH
		! Element =
		!   Description = Random badness amount
		GFLOAT BADNESS_RANDOM
		! Element =
		!   Description = Worst badness in break that can sched.
		GFLOAT BADNESS_WORST
		! Element =
		!   Description = Default spot seperation
		STRING DEFAULT_SPOT_SEPER = 6
		! Element =
		!   Description = Default product seperation
		STRING DEFAULT_PROD_SEPER = 6
		! Element =
		!   Description = Maximum spot/product seperation
		STRING MAXIMUM_SEPER = 6
	END RECORD
