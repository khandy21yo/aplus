DEFINE RECORD CDD$TOP.UTL.UTL_SYSTEM

        DESCRIPTION IS /*System Description File*/.

        UTL_SYSTEM_CDD STRUCTURE.

        /* Element =
        Description = */
        SYSTEM                  DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = */
        DESCRIPTION             DATATYPE IS TEXT SIZE IS 40.

        /* Element =
        Description = */
        SYSTEM_NUM              DATATYPE IS TEXT SIZE IS 2.

        END UTL_SYSTEM_CDD STRUCTURE.

END UTL_SYSTEM.
