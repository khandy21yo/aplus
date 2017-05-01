DEFINE RECORD CDD$TOP.TK.TK_APPLICATION

        DESCRIPTION IS /*TK Application File*/.

        TK_APPLICATION_CDD STRUCTURE.

        /* Element =
        Description = Module number */
        MNUMB                   DATATYPE IS TEXT SIZE IS 15.

        /* Element =
        Description = Module name */
        MNAME                   DATATYPE IS TEXT SIZE IS 50.

        /* Element =
        Description = Module description */
        MDESC                   DATATYPE IS TEXT SIZE IS 50.

        /* Element =
        Description = Module catagory */
        MCATA                   DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Module location */
        MLOCA                   DATATYPE IS TEXT SIZE IS 64.

        END TK_APPLICATION_CDD STRUCTURE.

END TK_APPLICATION.
