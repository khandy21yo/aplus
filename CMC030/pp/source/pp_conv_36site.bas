1	!
	! Special program to convert PP_SITE from old to new format
	!
	! History:
	!	10/26/96 - Kevin Handy
	!
	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	%INCLUDE "SOURCE:[PP.OPEN]PP_SITE.HB"
	MAP (PP_SITE) PP_SITE_CDD PP_SITE

	!
	! File Layout for: PP.PP_SITE
	!
	! Pacific Pride Site File
	!

	RECORD PP_SITE_OLD_CDD
		! Element =
		! Description = Host #
		STRING HOST = 3
		! Element =
		! Description = Site Code
		STRING SITE = 2
		! Element =
		! Description = site Type
		STRING STYPE = 1
		! Element = NAME
		! Description = Site Name
		STRING SNAME = 30
		! Element = ADDRESS
		! Description = Address Line
		STRING ADDRESS = 25
		! Element = CITY
		! Description = City
		STRING CITY = 15
		! Element = STATE
		! Description = State
		STRING STATE = 2
		! Element = ZIP
		! Description = Zip code
		STRING ZIP = 10
		! Element =
		! Description = Local Sale Location
		STRING LOCSALE = 3
		! Element =
		! Description = Foreign Sale Location
		STRING FORSALE = 3
		! Element =
		! Description = Foreign Purchase Location
		STRING FORPUR = 3
	END RECORD

	MAP (PP_SITE_OLD) PP_SITE_OLD_CDD PP_SITE_OLD

100	NAME "PP_SITE.MAS" AS "PP_SITE.MAS_OLD"

120	%INCLUDE "SOURCE:[PP.OPEN]PP_SITE.CRE"

200	!======================================================================
	! PP_SITE file (open read only)
	!======================================================================

	CALL ASSG_CHANNEL(PP_SITE_OLD.CH%, STAT%)

	PP_SITE_OLD.NAME$ = PP_SITE.DEV$ + "PP_SITE.MAS_OLD"

	OPEN PP_SITE_OLD.NAME$ FOR INPUT AS FILE PP_SITE_OLD.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP PP_SITE_OLD, &
		PRIMARY KEY &
		( &
			PP_SITE_OLD::HOST, &
			PP_SITE_OLD::SITE, &
			PP_SITE_OLD::STYPE &
		), &
		ACCESS READ, ALLOW MODIFY

300	RESET #PP_SITE_OLD.CH%

320	GET #PP_SITE_OLD.CH%

330	PP_SITE::HOST		= PP_SITE_OLD::HOST
	PP_SITE::SITE		= PP_SITE_OLD::SITE
	PP_SITE::STYPE		= PP_SITE_OLD::STYPE
	PP_SITE::SNAME		= PP_SITE_OLD::SNAME
	PP_SITE::ADDRESS	= PP_SITE_OLD::ADDRESS
	PP_SITE::CITY		= PP_SITE_OLD::CITY
	PP_SITE::STATE		= PP_SITE_OLD::STATE
	PP_SITE::ZIP		= PP_SITE_OLD::ZIP
	PP_SITE::LOCSALE	= PP_SITE_OLD::LOCSALE
	PP_SITE::FORSALE	= PP_SITE_OLD::FORSALE
	PP_SITE::FORPUR		= PP_SITE_OLD::FORPUR

	PUT #PP_SITE.CH%

	GOTO 320

32767	END
