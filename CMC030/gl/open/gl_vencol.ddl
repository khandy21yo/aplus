DEFINE RECORD CDD$TOP.GL.GL_VENCOL

        DESCRIPTION IS /*User Defined Report Table*/.

        GL_VENCOL_CDD STRUCTURE.

        /* Element =
        Description = Record Key */
        RECKEY                  DATATYPE IS TEXT SIZE IS 8.

        /* Element = PERIOD
        Description = Fiscal year (YYYY) and Cycle (PP) */
        FROMPER                 DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Column Title "A" */
        COL_TITLEA              ARRAY 0:10 DATATYPE IS TEXT SIZE IS 12.

        /* Element =
        Description = Column Title "B" */
        COL_TITLEB              ARRAY 0:10 DATATYPE IS TEXT SIZE IS 12.

        /* Element =
        Description = Account Wildcard */
        COL_ACCOUNT             ARRAY 0:10 DATATYPE IS TEXT SIZE IS 45.

        /* Element =
        Description = Store/Detail Code Flag */
        COL_FLAG                DATATYPE IS TEXT SIZE IS 1.

        /* Element =
        Description = Title */
        COL_TITLE               DATATYPE IS TEXT SIZE IS 40.

        END GL_VENCOL_CDD STRUCTURE.

END GL_VENCOL.
