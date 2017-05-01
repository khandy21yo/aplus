DEFINE RECORD CDD$TOP.GL.GL_GJ_LINE

        DESCRIPTION IS /*General Journal Line File*/.

        GL_GJ_LINE_CDD STRUCTURE.

        /* Element =
        Description = */
        JOURNAL                 DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = */
        ITEMNUM                 DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = */
        ACCT                    DATATYPE IS TEXT SIZE IS 18.

        /* Element =
        Description = */
        SOURCE                  DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = */
        DESCR                   DATATYPE IS TEXT SIZE IS 30.

        /* Element =
        Description = */
        TRANDAT                 DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = */
        AMOUNT                  DATATYPE IS G_FLOATING.

        /* Element =
        Description = */
        CKNO                    DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = */
        XREFNO                  DATATYPE IS TEXT SIZE IS 10.

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
        POSTIM                  DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = */
        POSDAT                  DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = */
        BATCH                   DATATYPE IS TEXT SIZE IS 6.

        END GL_GJ_LINE_CDD STRUCTURE.

END GL_GJ_LINE.
