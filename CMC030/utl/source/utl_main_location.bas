1	%TITLE "Location Profile"
	%SBTTL "UTL_MAIN_LOCATION"
	%IDENT "V3.6a Calico"

	FUNCTION LONG UTL_MAIN_LOCATION(CDD_WINDOW_CDD SMG_WINDOW, &
		LONG MOPTION, LONG MLOOP, LONG MFLAG, STRING MVALUE)

	!
	! COPYRIGHT (C) 1987, 1988 BY
	!
	! Computer Management Center, Inc.
	! Idaho Falls, Idaho.
	!
	! This software is furnished under a license and may be used and
	! copied only in accordance with terms of such license and with
	! the inclusion of the above copyright notice.  This software or
	! any other copies thereof may not be provided or otherwise made
	! available to any other person.  No title to and ownership of
	! the software is hereby transferred.
	!
	! The information in this software is subject to change without
	! notice and should not be construed as a commitment by
	! Computer Management Center, Inc.
	!
	! CMC assumes no responsibility for the use or reliability of
	! its software on equipment which is not supported by CMC.
	!
	!++
	! ID:0165
	!
	! Abstract:HELP
	!	.p
	!	The ^*Location Profile\* program maintains the Location Address File.
	!
	! Index:
	!	.x Location Address File
	!
	! Option:
	!
	! Compile:
	!
	!	$ BAS UTL_SOURCE:UTL_MAIN_LOCATION/LINE
	!	$ LIB/REP FUNC_LIB:CMCFUN UTL_MAIN_LOCATION
	!	$ DELETE UTL_MAIN_LOCATION.OBJ;*
	!
	! Author:
	!
	!	07/23/87 - Frank Starman
	!
	! Modification history:
	!
	!	05/30/88 - Lance Williams
	!		Modified to allow R/O open of file if R/W fails.
	!
	!	02/04/89 - Frank Starman
	!		New layout
	!
	!	05/13/92 - Dan Perkins
	!		Use FUNC_TESTENTRY to test input.
	!
	!	08/01/92 - Frank Starman
	!		Allow maintain Region thru F17.
	!
	!	06/16/93 - Kevin Handy
	!		Clean up (Check)
	!
	!	04/15/95 - Kevin Handy
	!		(V3.6)
	!		Update to V3.6 coding standards
	!
	!	01/29/96 - Kevin Handy
	!		Reformat source code.
	!		Change STRING$(...,ASCII(" ")) to "" in
	!		several places.
	!
	!	06/06/97 - Kevin Handy
	!		Use integer for #key
	!
	!	08/22/98 - Kevin Handy
	!		(V3.6a Calico)
	!		Update to V3.6a coding standards
	!
	!	03/10/99 - Kevin Handy
	!		Fix FIND bug
	!
	!	11/06/2000 - Kevin Handy
	!		Use A"x"B
	!		Use WHEN ERROR IN
	!--
	%PAGE

	!
	! Set up compiling options
	!
	OPTION SIZE = (REAL GFLOAT, INTEGER LONG)

	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "FUNC_INCLUDE:UTL_WINDOW.INC"
	%INCLUDE "FUNC_INCLUDE:MAIN_WINDOW.COM"

	%INCLUDE "SOURCE:[CDD.OPEN]CDD_WINDOW.HB"

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_REGION.HB"
	MAP (UTL_REGION)	UTL_REGION_CDD          UTL_REGION

	%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.HB"
	MAP (UTL_LOCATION)	UTL_LOCATION_CDD	UTL_LOCATION
	MAP (UTL_LOCATION_OLD)	UTL_LOCATION_CDD	UTL_LOCATION_OLD
	MAP (UTL_LOCATION_DEF)	UTL_LOCATION_CDD	UTL_LOCATION_DEF

	! Common areas
	!
	! These areas store information that is re-used between
	! calls to these functions.
	!
	COM (CH_UTL_LOCATION) &
		UTL_LOCATION.CH%, &
		UTL_LOCATION.READONLY%

	!
	! External functions
	!
	EXTERNAL LONG	FUNCTION MAIN_WINDOW
	EXTERNAL LONG	FUNCTION FUNC_TESTENTRY

	%PAGE

	ON ERROR GOTO 29000

	SELECT MOPTION

	!
	! Initilization
	!
	CASE OPT_INIT

		!*******************************************************
		! Set up information
		!*******************************************************

		!
		! Define SMG_WINDOW
		!
		SMG_WINDOW::DESCR = "Location Profile"
		SMG_WINDOW::NHELP = "UTL_MAIN_LOCATION"
		SMG_WINDOW::HSIZE = 76%
		SMG_WINDOW::VSIZE = 18%
		SMG_WINDOW::HPOS  = 2%
		SMG_WINDOW::VPOS  = 2%
		SMG_WINDOW::FLAGS = 0%
		SMG_WINDOW::NITEMS= 12%

		SMG_WINDOW::NKEYS = 4%
		SMG_WINDOW::KNAME(0%) = "Location_num"
		SMG_WINDOW::KFIELD(0%, 0%) = 1%
		SMG_WINDOW::KFIELD(0%, 1%) = 1%
		SMG_WINDOW::KNAME(1%) = "Region"
		SMG_WINDOW::KFIELD(1%, 0%) = 2%
		SMG_WINDOW::KFIELD(1%, 1%) = 3%
		SMG_WINDOW::KFIELD(1%, 2%) = 1%
		SMG_WINDOW::KNAME(2%) = "Group"
		SMG_WINDOW::KFIELD(2%, 0%) = 2%
		SMG_WINDOW::KFIELD(2%, 1%) = 4%
		SMG_WINDOW::KFIELD(2%, 2%) = 1%
		SMG_WINDOW::KNAME(3%) = "State"
		SMG_WINDOW::KFIELD(3%, 0%) = 2%
		SMG_WINDOW::KFIELD(3%, 1%) = 8%
		SMG_WINDOW::KFIELD(3%, 2%) = 1%

		SMG_WINDOW::HVIEW = 130%
		SMG_WINDOW::VVIEW = 18%

		CALL READ_DEFAULTS(SMG_WINDOW) &
			IF INSTR(1%, " QV", MVALUE) <= 1%

700		!
		! Declare channels
		!
		IF UTL_LOCATION.CH% > 0%
		THEN
			!
			! Already open, set flag to read-only if
			! was that way from last time.
			!
			SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%) &
				IF UTL_LOCATION.READONLY%
			GOTO 790
		END IF

750		WHEN ERROR IN
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.CRE"
		USE
			CONTINUE 760 IF ERR = 10%
			UTL_MAIN_LOCATION = ERR
			CONTINUE 770
		END WHEN

		UTL_LOCATION.READONLY% = 0%
		GOTO 790

760		!
		! If unable to open for modify, try to open
		! with read access only.
		!
		WHEN ERROR IN
			%INCLUDE "SOURCE:[UTL.OPEN]UTL_LOCATION.OPN"
		USE
			UTL_MAIN_LOCATION = ERR
			CONTINUE 770
		END WHEN

		SMG_WINDOW::FLAGS = (SMG_WINDOW::FLAGS OR 2%)
		UTL_LOCATION.READONLY% = -1%

		GOTO 790

770		!
		! File not able to open, so reset channel
		!
		CALL ASSG_FREECHANNEL(UTL_LOCATION.CH%)

		EXIT FUNCTION

790		SMG_WINDOW::CHAN  = UTL_LOCATION.CH%
		WHEN ERROR IN
			RESET #UTL_LOCATION.CH%
			GET #UTL_LOCATION.CH%, REGARDLESS
		USE
			CONTINUE 32767
		END WHEN

	!
	! Display SMG_WINDOW background
	!
	CASE OPT_BACKGROUND

		SMG_STATUS% = SMG$BEGIN_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)
		SMG_STATUS% = SMG$ERASE_DISPLAY(SMG_WINDOW::WNUMBER)

		DATA	02,02, "(01) Location #", &
			03,02, "(02) Name", &
			05,02, "(03) Address 1 ", &
			06,02, "(04) Address 2 ", &
			07,02, "(05) City      ", &
			08,02, "(06) State     ", &
			09,02, "(07) Zip       ", &
			10,02, "(08) County    ", &
			11,02, "(09) Country   ", &
			12,02, "(10) Region", &
			13,02, "(11) Loc Group", &
			14,02, "(12) Phone     ", &
			04,18, "Mailing Address", &
			04,48, "Shipping Address", &
			0, 0, ""

		RESTORE

		READ XPOS%, YPOS%, XSTR$
		I% = 0%
		WHILE (XPOS% <> 0%)
			I% = I% + 1%
			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				XSTR$, XPOS%, YPOS%) &
				IF (SMG_WINDOW::HFLAG(I%) AND 2%) = 0%
			READ XPOS%, YPOS%, XSTR$
		NEXT

		SMG_STATUS% = SMG$END_DISPLAY_UPDATE(SMG_WINDOW::WNUMBER)

	!
	! Enter/Display/Default
	!
20200	CASE OPT_ENTRY

		TEMP$, TEMP1$ = TRM$(SCOPE::PRG_ITEM)
		TEMP$ = "View starting at" IF TEMP$ = "View"

		SCOPE::PRG_ITEM = "FLD" + FORMAT$(MLOOP, "<0>##")

 Reenter:	SCOPE::SCOPE_EXIT = 0%

		SELECT MLOOP

		CASE 1%

	!++
	! Abstract:FLD001
	!	^*(01) Location _#\*
	!	.p
	!	The ^*Location _#\* field
	!	contanis a user defined code to identify
	!	a company location.
	!	.p
	!	The field will accommodate up to four (4) alphanumeric characters.
	!
	! Index:
	!	.x Location>Number
	!
	!--

			UTL_LOCATION::LOCATION = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"02;18", TEMP$, UTL_LOCATION::LOCATION, &
				MFLAG, "'E", MVALUE)

		CASE 2%

	!++
	! Abstract:FLD002
	!	^*(02) Name\*
	!	.p
	!	The ^*Name\* field
	!	contains the name of a company location.
	!	.p
	!	The field will accommodate forty (40) alphanumeric characters.
	!
	! Index:
	!	.x Location>Name
	!	.x Name>Location
	!
	!--

			UTL_LOCATION::LOCNAME = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"03;18", TEMP$, UTL_LOCATION::LOCNAME, &
				MFLAG, "'E", MVALUE)

		CASE 3%

	!++
	! Abstract:FLD003
	!	^*(03) Address 1\*
	!	.p
	!	The ^*Address 1\* field
	!	contains the first line of the mailing address
	!	and shipping address for a specific location.  It is
	!	recommended in the event of a one line only street address,
	!	this field be left blank and field (6) be used for the
	!	street address.
	!	.p
	!	The field will accommodate up to twenty-five (25) alphanumeric
	!	characters each to for the first line mailing address and
	!	first line shipping address.
	!
	! Index:
	!	.x Location>Address
	!	.x Address>Location
	!
	!--


			UTL_LOCATION::ADDRESS1 = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"05;18", TEMP$, UTL_LOCATION::ADDRESS1, &
				MFLAG, "'E", MVALUE)

			IF TEMP$ = "Add" AND (MFLAG AND 1%) = 0%
			THEN
				UTL_LOCATION::SHPADDRESS1 = &
					UTL_LOCATION::ADDRESS1
			END IF

			UTL_LOCATION::SHPADDRESS1 = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"05;48", TEMP$, UTL_LOCATION::SHPADDRESS1, &
				MFLAG, "'E", MVALUE)

		CASE 4%

	!++
	! Abstract:FLD004
	!	^*(04) Address 2\*
	!	.p
	!	The ^*Address 2\* field
	!	contains the second line of the mailing address
	!	and shipping address for a specific location. It is
	!	recommended that in the event of a one line only street
	!	address, field (5) be left blank and this field be used
	!	for the street address.
	!	.p
	!	The field will accommodate up to twenty-one (21) alphanumeric
	!	characters each for the second line mailing address and second
	!	line shipping address.
	!
	! Index:
	!	.x Location>Address
	!	.x Address>Location
	!
	!--

			UTL_LOCATION::ADDRESS2 = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"06;18", TEMP$, UTL_LOCATION::ADDRESS2, &
				MFLAG, "'E", MVALUE)

			IF TEMP$ = "Add" AND (MFLAG AND 1%) = 0%
			THEN
				UTL_LOCATION::SHPADDRESS2 = &
					UTL_LOCATION::ADDRESS2
			END IF

			UTL_LOCATION::SHPADDRESS2 = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"06;48", TEMP$, UTL_LOCATION::SHPADDRESS2, &
				MFLAG, "'E", MVALUE)

		CASE 5%

	!++
	! Abstract:FLD005
	!	^*(05) City\*
	!	.p
	!	The ^*City\* field
	!	contains the city for both the mailing address and
	!	shipping address in which a specific location is situated.
	!	.p
	!	The field will accommodate up to ten (10) alphanumeric
	!	characters each for the "mailing" city and the "shipping" city.
	!
	! Index:
	!	.x Location>City
	!	.x City>Location
	!
	!--


			UTL_LOCATION::CITY = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"07;18", TEMP$, UTL_LOCATION::CITY, MFLAG, &
				"'E", MVALUE)

			IF TEMP$ = "Add" AND (MFLAG AND 1%) = 0%
			THEN
				UTL_LOCATION::SHPCITY = UTL_LOCATION::CITY
			END IF

			UTL_LOCATION::SHPCITY = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"07;48", TEMP$, UTL_LOCATION::SHPCITY, MFLAG, &
				"'E", MVALUE)

		CASE 6%

	!++
	! Abstract:FLD006
	!	^*(06) State\*
	!	.p
	!	The ^*State\* field
	!	contains the State code for both the mailing address
	!	and a shipping address for a specific company location.
	!	.p
	!	The field will accommodate a two (2) character State postal
	!	code for both the "mailing" State and the "shipping" State.
	!
	! Index:
	!	.x Location>State
	!	.x State>Location
	!
	!--

			UTL_LOCATION::STATE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"08;18", TEMP$, UTL_LOCATION::STATE, MFLAG, &
				"'E", MVALUE)

			IF TEMP$ = "Add" AND (MFLAG AND 1%) = 0%
			THEN
				UTL_LOCATION::SHPSTATE = UTL_LOCATION::STATE
			END IF

			UTL_LOCATION::SHPSTATE = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"08;48", TEMP$, UTL_LOCATION::SHPSTATE, MFLAG, &
				"'E", MVALUE)

		CASE 7%

	!++
	! Abstract:FLD007
	!	^*(07) Zip\*
	!	.p
	!	The ^*Zip\* field
	!	contains the postal zip code for both the mailing
	!	address and the shipping address for a specific company
	!	location.
	!	.p
	!	The field will accommodate up to ten (10) alphanumeric
	!	characters for both the mailing zip code and the shipping
	!	zip code.
	!
	! Index:
	!	.x Location>Zip Code
	!	.x Zip Code>Location
	!
	!--


			UTL_LOCATION::ZIP = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"09;18", TEMP$, UTL_LOCATION::ZIP, MFLAG, &
				"'E", MVALUE)

			IF TEMP$ = "Add" AND (MFLAG AND 1%) = 0%
			THEN
				UTL_LOCATION::SHPZIP= UTL_LOCATION::ZIP
			END IF

			UTL_LOCATION::SHPZIP = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"09;48", TEMP$, UTL_LOCATION::SHPZIP, MFLAG, &
				"'E", MVALUE)

		CASE 8%

	!++
	! Abstract:FLD008
	!	^*(08) County\*
	!	.p
	!	The ^*County\* field
	!	contains a code representing the county for both the mailing and shipping
	!	address of a location.
	!	.p
	!	This field will accommodate a two (2) alphabetic character code as
	!	defined in the County Codes file of the Utility module.
	!	.p
	!	This field may be left blank if there is no specific need to identify a
	!	county.
	!
	! Index:
	!	.x Location>County
	!	.x County>Location
	!
	!--

			UTL_LOCATION::COUNTY = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"10;18", TEMP$, UTL_LOCATION::COUNTY, MFLAG, &
				"'E", MVALUE)

			IF TEMP$ = "Add" AND (MFLAG AND 1%) = 0%
			THEN
				UTL_LOCATION::SHPCOUNTY = UTL_LOCATION::COUNTY
			END IF

			UTL_LOCATION::SHPCOUNTY = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"10;48", TEMP$, UTL_LOCATION::SHPCOUNTY, &
				MFLAG, "'E", MVALUE)

		CASE 9%

	!++
	! Abstract:FLD009
	!	^*(09) Country\*
	!	.p
	!	The ^*Country\* field
	!	conatins the code for the country for the mailing and shipping address for a
	!	specific location.
	!	.p
	!	This field will accommodate a two (2) alphabetic character country code
	!	as defined in the Country Codes file of the Utility module.
	!	.p
	!	If it is not necessary to identify the country for a location, this field
	!	may be left blank.
	!
	! Index:
	!	.x Location>Country
	!	.x Country>Location
	!
	!--

			UTL_LOCATION::COUNTRY = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"11;18", TEMP$, UTL_LOCATION::COUNTRY, MFLAG, &
				"'E", MVALUE)

			IF TEMP$ = "Add" AND (MFLAG AND 1%) = 0%
			THEN
				UTL_LOCATION::SHPCOUNTRY = UTL_LOCATION::COUNTRY
			END IF

			UTL_LOCATION::SHPCOUNTRY = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"11;48", TEMP$, UTL_LOCATION::SHPCOUNTRY, &
				MFLAG, "'E", MVALUE)

			UTL_REGION::COUNTRY = UTL_LOCATION::COUNTRY

		CASE 10%

	!++
	! Abstract:FLD010
	!	^*(10) Region\*
	!	.p
	!	The ^*Region\* field
	!	contains the code representing the
	!	region in which the location situated or assigned.
	!	.p
	!	The field will accommodate up to two (2) alphanumeric characters
	!
	! Index:
	!	.x Location>Region
	!	.x Region>Location
	!
	!--

			UTL_LOCATION::REGION = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"12;18", TEMP$, UTL_LOCATION::REGION, &
				MFLAG, "'E", MVALUE)

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F14)
			THEN
				IF MAIN_WINDOW(UTL_MAIN_REGION.ID, "V0") = 1%
				THEN
					UTL_LOCATION::REGION = &
						UTL_REGION::REGION
				END IF
				GOTO Reenter
			END IF

			IF (SCOPE::SCOPE_EXIT = SMG$K_TRM_F17)
			THEN
				IF MAIN_WINDOW(UTL_MAIN_REGION.ID, "M0") = 1%
				THEN
					UTL_LOCATION::REGION = &
						UTL_REGION::REGION
				END IF
				GOTO Reenter
			END IF

		CASE 11%

	!++
	! Abstract:FLD011
	!	^*(11) Location Group\*
	!	.p
	!	The ^*Location Group\* field
	!	contains the Group to which the Location
	!	is assigned.
	!	.p
	!	The field will accommodate up to two (2) alphanumeric characters.
	!
	! Index:
	!	.x LocationGroup
	!	.x Group>Location
	!
	!--


			UTL_LOCATION::LOCGROUP = ENTR_3STRING(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"13;18", TEMP$, UTL_LOCATION::LOCGROUP, &
				MFLAG, "'E", MVALUE)

		CASE 12%

	!++
	! Abstract:FLD012
	!	^*(12) Phone\*
	!	.p
	!	The ^*Phone\* field
	!	contains an office and a shipping telephone number for a location.
	!	.p
	!	This field will accommodate up to ten (10) numeric characters.  The system
	!	will automatically edit the phone number with special characters.  Only the
	!	ten numbers should be entered.  If only seven (7) numbers are entered, the
	!	area code will automatically be left blank.
	!
	! Index:
	!	.x Location>Phone
	!	.x Phone>Location
	!
	!--

			UTL_LOCATION::PHONE = ENTR_3PHONE(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"14;18", TEMP$, UTL_LOCATION::PHONE, MFLAG, &
				0%, MVALUE)

			IF TEMP$ = "Add" AND (MFLAG AND 1%) = 0%
			THEN
				UTL_LOCATION::SHPPHONE = UTL_LOCATION::PHONE
			END IF

			UTL_LOCATION::SHPPHONE = ENTR_3PHONE(SCOPE, &
				SMG_WINDOW::WNUMBER, &
				"14;48", TEMP$, UTL_LOCATION::SHPPHONE, MFLAG, &
				0%, MVALUE)

		END SELECT

		SCOPE::PRG_ITEM = TEMP1$

	!
	! Test values
	!
20300	CASE OPT_TESTENTRY
		UTL_MAIN_LOCATION = 0%

		SELECT MLOOP

		CASE 1%
			IF UTL_LOCATION::LOCATION = ""
			THEN
				UTL_MAIN_LOCATION = 1%
			ELSE
				IF (MVALUE = "ADD")
				THEN
					WHEN ERROR IN
						GET #SMG_WINDOW::CHAN, &
							KEY #0% EQ UTL_LOCATION::LOCATION + "", &
							REGARDLESS
					USE
						CONTINUE 32767 IF ERR = 155%
						EXIT HANDLER
					END WHEN

					UTL_MAIN_LOCATION = 2%
					CALL ENTR_3MESSAGE(SCOPE, &
						"Record Already Exists", 0%)
				END IF
			END IF

		CASE 10%
			IF UTL_LOCATION::REGION <> ""
			THEN
				UTL_MAIN_LOCATION = FUNC_TESTENTRY(SMG_WINDOW, &
					UTL_LOCATION::COUNTRY + &
					UTL_LOCATION::REGION, &
					UTL_REGION::DESCRIPTION, &
					"PR", MLOOP, "PRG", &
					"Region", UTL_MAIN_REGION.ID)
			ELSE
				UTL_REGION::DESCRIPTION  = ""
			END IF

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(UTL_REGION::DESCRIPTION, 33%), &
				12%, 23%, , SMG$M_BOLD)

		END SELECT

	CASE OPT_DISPLAY

		IF (SMG_WINDOW::HFLAG(10%) AND 2%) = 0%
		THEN
			UTL_REGION::DESCRIPTION = &
				STRING$(LEN(UTL_REGION::DESCRIPTION), A"?"B) &
				IF MAIN_WINDOW(UTL_MAIN_REGION.ID, &
				"Q0" + UTL_LOCATION::COUNTRY + &
				UTL_LOCATION::REGION) <> 1%

			UTL_REGION::DESCRIPTION  = "" &
				IF UTL_LOCATION::REGION = ""

			SMG_STATUS% = SMG$PUT_CHARS(SMG_WINDOW::WNUMBER, &
				LEFT(UTL_REGION::DESCRIPTION, 33%), &
				12%, 23%, , SMG$M_BOLD)
		END IF

	!
	! Set UTL_LOCATION_OLD value
	!
20500	CASE OPT_SETOLD
		UTL_LOCATION_OLD = UTL_LOCATION

	!
	! Restore UTL_LOCATION_OLD value
	!
	CASE OPT_RESETOLD
		UTL_LOCATION = UTL_LOCATION_OLD

	!
	! Set default value
	!
	CASE OPT_SETDEFAULT
		UTL_LOCATION_DEF = UTL_LOCATION

	!
	! Restore default value
	!
	CASE OPT_RESETDEFAULT
		UTL_LOCATION = UTL_LOCATION_DEF

	!
	! View header
	!
	CASE OPT_VIEW
		SELECT MLOOP

		!
		! Title (One line only)
		!
		CASE 1%
			MVALUE = "  Loc  Name" + SPACE$(14%) + "Address1" + &
				SPACE$(18%) + "Address2" + SPACE$(14%) + &
				"City            " + &
				"St ZIP        Country Phone      Reg Grp"

		!
		! Positions of lines
		!
		CASE 2%
			MVALUE = "007,025,051,073,089,092,103,111,122,126"

		!
		! Convert current record into text
		!
		CASE 3%
			MVALUE = UTL_LOCATION::LOCATION + " " + &
				LEFT(UTL_LOCATION::LOCNAME, 17%) + " " + &
				UTL_LOCATION::ADDRESS1 + " " + &
				UTL_LOCATION::ADDRESS2 + " " + &
				UTL_LOCATION::CITY + " " + &
				UTL_LOCATION::STATE + " " + &
				UTL_LOCATION::ZIP + " " + &
				UTL_LOCATION::COUNTRY + "      " + &
				UTL_LOCATION::PHONE + " " + &
				UTL_LOCATION::REGION + "  " + &
				UTL_LOCATION::LOCGROUP


		END SELECT

	!
	! Find
	!
	CASE OPT_FIND
		SELECT MLOOP
		CASE 0%
			FIND #SMG_WINDOW::CHAN, &
				KEY #0% GE UTL_LOCATION::LOCATION + "", &
				REGARDLESS
		CASE 1%
			FIND #SMG_WINDOW::CHAN, &
				KEY #1% GE UTL_LOCATION::REGION + &
				UTL_LOCATION::LOCATION, &
				REGARDLESS
		CASE 2%
			FIND #SMG_WINDOW::CHAN, &
				KEY #2% GE UTL_LOCATION::LOCGROUP + &
				UTL_LOCATION::LOCATION, &
				REGARDLESS
		CASE 3%
			FIND #SMG_WINDOW::CHAN, &
				KEY #3% GE UTL_LOCATION::STATE + &
				UTL_LOCATION::LOCATION, &
				REGARDLESS
		END SELECT

	END SELECT

27000	EXIT FUNCTION

29000	!
	! Trap errors
	!
	ON ERROR GO BACK

32767	END FUNCTION
	!+-+-+
	! More Menu option hidden in mast.
	!++
	! Abstract:DEPARTMENT
	!	^*Department\*
	!	.p
	!	The ^*Department\* option
	!	maintains the Department file.
	!
	! Index:
	!
	!--
