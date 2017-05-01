DEFINE RECORD CDD$TOP.UTL.UTL_MACROS

        DESCRIPTION IS /*User Macros Definition*/.

        UTL_MACROS_CDD STRUCTURE.

        /* Element =
        Description = User Macro Command */
        COMMAND                 DATATYPE IS TEXT SIZE IS 20.

        /* Element = DESCRIPTION
        Description = Description */
        DESCRIPTION             DATATYPE IS TEXT SIZE IS 40.

        /* Element =
        Description = Menu Path or Command */
        CMD                     DATATYPE IS TEXT SIZE IS 40.

        END UTL_MACROS_CDD STRUCTURE.

END UTL_MACROS.
