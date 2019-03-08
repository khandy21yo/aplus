%TITLE "Example USEROPEN"
%SBTTL "Useropen Routine to obtain RAB address"
%IDENT "Version 1.1"
FUNCTION LONG Get_rab_address ( Fabdef User_fab, Rabdef User_rab, LONG Channel )
!++
! FUNCTIONAL DESCRIPTION:
!
! Save the address of the RMS Record Access Block allocated by the caller
! in a global symbol. Open the file and return the status from RMS.
!
! FORMAL PARAMETERS (Standard for all BASIC USEROPEN procedures)
!
! User_fab Address of RMS File Access Block
! User_rab Address of RMS Record Access Block
! Channel Logical Unit assigned to file by caller.
!
! RETURN VALUE: RMS Status value
!
! GLOBAL COMMON USAGE
!
! RAB_ptr Single longword PSECT used to pass RAB address to caller.
!
!--
OPTION INACTIVE = SETUP, &
CONSTANT TYPE = INTEGER, &
TYPE = EXPLICIT
%NOLIST
%INCLUDE "$FABDEF" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET"
%INCLUDE "$RABDEF" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET"
%INCLUDE "$RMSDEF" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET"
%INCLUDE "STARLET" %FROM %LIBRARY "SYS$LIBRARY:BASIC$STARLET"
%LIST
!+
! Common area used to pass RAB address to caller.
!-
COMMON (RAB_ptr) LONG rab_address
DECLARE LONG Rms_status
!+
! Save RAB address in global symbol known to caller.
! Perform standard RMS open sequence
!-
Rab_address = LOC(User_rab::rab$b_bid)
Rms_status = Sys$open( User_fab )
IF Rms_status AND Rms$_normal
THEN
Rms_status = Sys$connect( User_rab )
END IF
END FUNCTION Rms_status

