DEFINE RECORD CDD$TOP.GL.GL_YYYY_PP

        DESCRIPTION IS /*General Ledger Transaction File*/.

        GL_YYYY_PP_CDD STRUCTURE.

        /* Element =
        Description = */
        ACCT                    DATATYPE IS TEXT SIZE IS 18.

        /* Element =
        Description = */
        SOURCE                  DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = */
        REFNO                   DATATYPE IS TEXT SIZE IS 16.

        /* Element =
        Description = */
        TRANDAT                 DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = */
        DESCR                   DATATYPE IS TEXT SIZE IS 30.

        /* Element =
        Description = */
        AMOUNT                  DATATYPE IS G_FLOATING.

        /* Element =
        Description = */
        XREFNO                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = */
        POSTIM                  DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = */
        POSDAT                  DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = */
        CKNO                    DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = */
        TRANKEY                 DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = */
        SUBACC                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = */
        OPERATION               DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = */
        UNITS                   DATATYPE IS G_FLOATING.

        /* Element =
        Description = */
        HOURS                   DATATYPE IS G_FLOATING.

        /* Element =
        Description = */
        UPDSTA                  DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = */
        BTHNUM                  DATATYPE IS TEXT SIZE IS 6.

        END GL_YYYY_PP_CDD STRUCTURE.

END GL_YYYY_PP.
