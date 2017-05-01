DEFINE RECORD CDD$TOP.SS.SS_CUS_SYSMENU

        DESCRIPTION IS /*System Menu*/.

        SS_CUS_SYSMENU_CDD STRUCTURE.

        /* Element =
        Description = Customer Number */
        CUSNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Tape Number */
        TAPE                    DATATYPE IS TEXT SIZE IS 2.

        /* Element = DATE
        Description = Installation Date (YYYYMMDD) */
        INSDAT                  DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = System Name */
        SYSTEM                  DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = System Menu Number */
        MENNUM                  DATATYPE IS TEXT SIZE IS 6.

        END SS_CUS_SYSMENU_CDD STRUCTURE.

END SS_CUS_SYSMENU.
