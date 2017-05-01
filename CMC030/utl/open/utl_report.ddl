DEFINE RECORD CDD$TOP.UTL.UTL_REPORT

        DESCRIPTION IS /*Utility Report File*/.

        UTL_REPORT_CDD STRUCTURE.

        /* Element =
        Description = System name */
        SYSTEM                  DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Subsystem name */
        SUBSYS                  DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = report number */
        REPNUM                  DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Description */
        REPDES                  DATATYPE IS TEXT SIZE IS 30.

        /* Element =
        Description = Program device */
        PRODEV                  DATATYPE IS TEXT SIZE IS 32.

        /* Element =
        Description = Program name */
        PRONAM                  DATATYPE IS TEXT SIZE IS 40.

        /* Element =
        Description = Can report be spooled */
        CANSPOOL                DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Can it be displayed */
        CANDISP                 DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Can it go to a device */
        CANDEV                  DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Can it go to a file */
        CANFILE                 DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Can it run detached */
        CANDET                  DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Report Date used (Y/N) */
        REPYN                   DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Description of options */
        DESCR                   ARRAY 0:9 DATATYPE IS TEXT SIZE IS 20.

        /* Element =
        Description = Type of option */
        OPTTYPE                 ARRAY 0:9 DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Length of option */
        OPTLEN                  ARRAY 0:9 DATATYPE IS SIGNED WORD.

        /* Element =
        Description = Valid items in options */
        VALID                   ARRAY 0:9 DATATYPE IS TEXT SIZE IS 20.

        /* Element =
        Description = Option data required? */
        REQUIRE                 ARRAY 0:9 DATATYPE IS TEXT SIZE IS 1.

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
        Description = Program to chain to */
        CHAINTO                 DATATYPE IS TEXT SIZE IS 20.

        /* Element =
        Description = Printer type */
        PRINTTYPE               DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Last run date */
        LASTRUNDATE             DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Last run time */
        LASTRUNTIME             DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Base run date */
        BASERUNDATE             DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Run frequency */
        RUNFREQ                 DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Report Width in Characters */
        REPWID                  DATATYPE IS SIGNED WORD.

        /* Element =
        Description = Spooler From Name */
        SPOOLFORM               DATATYPE IS TEXT SIZE IS 20.

        END UTL_REPORT_CDD STRUCTURE.

END UTL_REPORT.
