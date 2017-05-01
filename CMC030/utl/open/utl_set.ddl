DEFINE RECORD CDD$TOP.UTL.UTL_SET

        DESCRIPTION IS /*CMC Utility Set File*/.

        UTL_SET_CDD STRUCTURE.

        /* Element =
        Description = Program name */
        PROGRAMNAME             DATATYPE IS TEXT SIZE IS 39.

        /* Element =
        Description = Item number */
        ITEM                    DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = System name */
        SYSTEM                  DATATYPE IS TEXT SIZE IS 2.

        /* Element = YESNO
        Description = Yes or No Flag for Undefined Input */
        ALLOWUND                DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Unused */
        UNUSED                  DATATYPE IS TEXT SIZE IS 3.

        /* Element =
        Description = Hard/Soft/Field default */
        HARD                    DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Data */
        SDATA                   DATATYPE IS TEXT SIZE IS 30.

        /* Element =
        Description = Data Format */
        FDATA                   DATATYPE IS TEXT SIZE IS 30.

        /* Element =
        Description = Unused Fiels */
        UNUSED2                 DATATYPE IS TEXT SIZE IS 4.

        END UTL_SET_CDD STRUCTURE.

END UTL_SET.
