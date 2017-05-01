DEFINE RECORD CDD$TOP.UTL.UTL_REPORTX

        DESCRIPTION IS /*Report Settings Structure*/.

        UTL_REPORTX_CDD STRUCTURE.

        /* Element =
        Description = report number */
        REPNUM                  DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Program device */
        PRODEV                  DATATYPE IS TEXT SIZE IS 32.

        /* Element =
        Description = Program name */
        PRONAM                  DATATYPE IS TEXT SIZE IS 40.

        /* Element =
        Description = Spooler name */
        SPOOL                   DATATYPE IS TEXT SIZE IS 32.

        /* Element =
        Description = Option data */
        OPTDEF                  ARRAY 0:9 DATATYPE IS TEXT SIZE IS 20.

        /* Element =
        Description = Default output file/device name */
        DEFOUT                  DATATYPE IS TEXT SIZE IS 20.

        /* Element =
        Description = Printer type groups */
        ITEMGROUP               ARRAY 0:9 DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Defaults for printer groups */
        ITEM                    ARRAY 0:9 DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Starting page */
        STARTP                  DATATYPE IS SIGNED LONGWORD.

        /* Element =
        Description = Ending page */
        ENDP                    DATATYPE IS SIGNED LONGWORD.

        /* Element =
        Description = Copies */
        COPIES                  DATATYPE IS SIGNED LONGWORD.

        /* Element =
        Description = Page length */
        PAGELEN                 DATATYPE IS SIGNED LONGWORD.

        /* Element =
        Description = Report width */
        REPWIDTH                DATATYPE IS SIGNED LONGWORD.

        /* Element =
        Description = Report date */
        REPDATE                 DATATYPE IS TEXT SIZE IS 32.

        /* Element =
        Description = Print report date on report (yn) */
        REPYN                   DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Output flag 1 - Display 2 - Spool 3 - Ou */
        PRINTTO                 DATATYPE IS SIGNED LONGWORD.

        /* Element =
        Description = Autoscroll flag */
        AUTOSCROLL              DATATYPE IS SIGNED LONGWORD.

        /* Element =
        Description = Printer type */
        PRINTTYPE               DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Printer initilization string */
        PRINTINIT               DATATYPE IS TEXT SIZE IS 64.

        /* Element =
        Description = Printer finilization string */
        PRINTFINISH             DATATYPE IS TEXT SIZE IS 64.

        /* Element =
        Description = Control string to go to next page */
        NEXTPAGE                DATATYPE IS TEXT SIZE IS 64.

        /* Element =
        Description = Output to local printer */
        TOLOCAL                 DATATYPE IS TEXT SIZE IS 32.

        /* Element =
        Description = Output to screen again */
        TOSCREEN                DATATYPE IS TEXT SIZE IS 32.

        /* Element =
        Description = Next program to run */
        NEXTRUN                 DATATYPE IS TEXT SIZE IS 64.

        /* Element =
        Description = Channel for report output */
        CHAN                    DATATYPE IS SIGNED LONGWORD.

        /* Element =
        Description = Exit status */
        STAT                    DATATYPE IS SIGNED LONGWORD.

        /* Element =
        Description = Page number */
        PAGENO                  DATATYPE IS SIGNED LONGWORD.

        /* Element =
        Description = Line number */
        LINENO                  DATATYPE IS SIGNED LONGWORD.

        /* Element =
        Description = Starting date */
        SDATE                   DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Starting time */
        STIME                   DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Window for selection menu */
        WINDOW                  DATATYPE IS SIGNED LONGWORD.

        /* Element =
        Description = Cannot detach flag */
        DETACH                  DATATYPE IS SIGNED LONGWORD.

        /* Element =
        Description = Spooler From Name */
        SPOOLFORM               DATATYPE IS TEXT SIZE IS 20.

        /* Element =
        Description = Spaces to add to left margin */
        OFFSET                  DATATYPE IS SIGNED LONGWORD.

        /* Element = TIME
        Description = Run After Time (HHMMSS) */
        AFTERTIME               DATATYPE IS TEXT SIZE IS 6.

        /* Element = YESNO
        Description = Run Background? */
        BACKGROUND              DATATYPE IS TEXT SIZE IS 1.

        END UTL_REPORTX_CDD STRUCTURE.

END UTL_REPORTX.
