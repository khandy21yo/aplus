10	OPTION SIZE = (INTEGER LONG, REAL GFLOAT)

	ON ERROR GOTO 19000

	!
	! Include files
	!
	%INCLUDE "FUNC_INCLUDE:FUNCTION.HB"

	MAP (SCOPE) SCOPE_STRUCT SCOPE

	%INCLUDE "SOURCE:[AD.OPEN]AD_35ASSET.HB"
	MAP (AD_35ASSET)		AD_35ASSET_CDD	AD_35ASSET

	!
	! Open the keyboard
	!
	CALL READ_INITIALIZE

	RECORD AD_TEMP

		STRING ASSET_NUM = 10%, &
		STRING DESCRIPTION = 40%, &
		STRING ASSET_TYPE = 2%, &
		STRING LOCATION = 4%, &
		STRING DEPT_NUM = 6%, &
		STRING SERIAL_NUM = 20%, &
		STRING SERVDATE = 8%, &
		REAL COST, &
		REAL SALVAGE, &
		REAL BONUS, &
		REAL ITC
	END RECORD

	MAP (AD_TEMP) AD_TEMP AD_TEMP

100	!
	! Open UTL_DEVICE file and read information
	!
	CALL ASSG_CHANNEL(AD_TEMP.CH%, STAT%)
	CALL READ_DEVICE("AD_35ASSET", AD_35ASSET.DEV$, STAT%)

	AD_35ASSET.NAME$ = AD_35ASSET.DEV$ + "AD_35ASSET_TEMP.MAS"

	OPEN AD_35ASSET.NAME$ FOR INPUT AS FILE AD_TEMP.CH%, &
		ORGANIZATION INDEXED FIXED, &
		MAP AD_TEMP, &
		PRIMARY KEY &
			AD_TEMP::ASSET_NUM, &
		ALTERNATE KEY &
		( &
			AD_TEMP::ASSET_TYPE, &
			AD_TEMP::ASSET_NUM &
		)	CHANGES, &
		ALTERNATE KEY &
		( &
			AD_TEMP::SERVDATE, &
			AD_TEMP::ASSET_NUM &
		)	CHANGES, &
		ALTERNATE KEY &
		( &
			AD_TEMP::LOCATION, &
			AD_TEMP::DEPT_NUM, &
			AD_TEMP::ASSET_NUM &
		)	CHANGES, &
		ACCESS READ, ALLOW MODIFY

200	%INCLUDE "SOURCE:[AD.OPEN]AD_35ASSET.CRE"

3000	!
	! ASSET open file does not exist, so create it
	!
	!======================================================================
	CALL ENTR_3MESSAGE(SCOPE, "Creating new  AD_35ASSET file", 1%)

	RESET #AD_TEMP.CH%

3010	WHEN ERROR IN
		GET #AD_TEMP.CH%, REGARDLESS
	USE
		CONTINUE 15000 IF ERR = 11%
		EXIT HANDLER
	END WHEN

	AD_35ASSET::ASSET_NUM	= AD_TEMP::ASSET_NUM
	AD_35ASSET::DESCRIPTION	= AD_TEMP::DESCRIPTION
	AD_35ASSET::ASSET_TYPE	= AD_TEMP::ASSET_TYPE
	AD_35ASSET::LOCATION	= AD_TEMP::LOCATION
	AD_35ASSET::DEPT_NUM	= AD_TEMP::DEPT_NUM
	AD_35ASSET::SERIAL_NUM	= AD_TEMP::SERIAL_NUM
	AD_35ASSET::SERVDATE	= AD_TEMP::SERVDATE
	AD_35ASSET::COST	= AD_TEMP::COST
	AD_35ASSET::SALVAGE	= AD_TEMP::SALVAGE
	AD_35ASSET::BONUS	= AD_TEMP::BONUS
	AD_35ASSET::ITC		= AD_TEMP::ITC
	AD_35ASSET::ITCREDUCE	= 0.0
	AD_35ASSET::UNITS	= 0.0

	PUT #AD_35ASSET.CH%

	GOTO 3010

 ExitProgram:
15000	!*******************************************************************
	! Exit program
	!*******************************************************************

	CALL SUBR_3EXITPROGRAM(SCOPE, "", "")

	%PAGE

 HelpError:
	!******************************************************************
	! Help Message for an error
	!******************************************************************
	CALL HELP_34MESSAGE(SCOPE, NUM1$(ERL) + " " + ERT$(ERR), &
		"E", ERN$, FILENAME$, NUM1$(ERR))
	GOTO ExitProgram

	%Page

19000	!*******************************************************************
	! Error trapping
	!*******************************************************************

	!
	! Untrapped error
	!
	FILENAME$ = ""
	RESUME HelpError

32767	END
