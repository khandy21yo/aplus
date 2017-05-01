DEFINE RECORD CDD$TOP.TK.TK_CONSTANT

        DESCRIPTION IS /*CMC Constatnts*/.

        TK_CONSTANT_CDD STRUCTURE.

        /* Element = CONSTNAME
        Description = Constant name */
        CONSTNAME               DATATYPE IS TEXT SIZE IS 39.

        /* Element = CLASS
        Description = Class */
        CLASS                   DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Constatnt value */
        CONST                   DATATYPE IS SIGNED LONGWORD.

        END TK_CONSTANT_CDD STRUCTURE.

END TK_CONSTANT.
