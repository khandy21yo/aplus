DEFINE RECORD CDD$TOP.GL.GL_FINSTA

        DESCRIPTION IS /*Financial Statement Command File*/.

        GL_FINSTA_CDD STRUCTURE.

        /* Element =
        Description = */
        PROMPT                  DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = */
        DESCR                   DATATYPE IS TEXT SIZE IS 30.

        /* Element =
        Description = */
        REPTITLE                DATATYPE IS TEXT SIZE IS 50.

        /* Element =
        Description = */
        CMDFIL                  DATATYPE IS TEXT SIZE IS 30.

        /* Element =
        Description = */
        FINTYPE                 DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = */
        FINCMD                  ARRAY 0:8 DATATYPE IS TEXT SIZE IS 20.

        /* Element =
        Description = */
        REPTITLEA               DATATYPE IS TEXT SIZE IS 50.

        /* Element =
        Description = */
        REPTITLEB               DATATYPE IS TEXT SIZE IS 50.

        /* Element =
        Description = */
        REPTITLEC               DATATYPE IS TEXT SIZE IS 50.

        /* Element =
        Description = */
        REPTITLED               DATATYPE IS TEXT SIZE IS 50.

        END GL_FINSTA_CDD STRUCTURE.

END GL_FINSTA.
