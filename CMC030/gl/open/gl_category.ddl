DEFINE RECORD CDD$TOP.GL.GL_CATEGORY

        DESCRIPTION IS /*Chart of Account Category Description*/.

        GL_CATEGORY_CDD STRUCTURE.

        /* Element = CATEGORY
        Description = Category */
        CATEGORY                DATATYPE IS TEXT SIZE IS 4.

        /* Element = DESCRIPTION6
        Description = Description */
        DESCR                   DATATYPE IS TEXT SIZE IS 40.

        /* Element =
        Description = Description */
        TITLEDESC               DATATYPE IS TEXT SIZE IS 10.

        END GL_CATEGORY_CDD STRUCTURE.

END GL_CATEGORY.
