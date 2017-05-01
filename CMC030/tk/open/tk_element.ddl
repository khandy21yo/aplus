DEFINE RECORD CDD$TOP.TK.TK_ELEMENT

        DESCRIPTION IS /*Element Definition*/.

        TK_ELEMENT_CDD STRUCTURE.

        /* Element =
        Description = Element */
        ELEMENT                 DATATYPE IS TEXT SIZE IS 39.

        /* Element =
        Description = Element description */
        DESCR                   DATATYPE IS TEXT SIZE IS 60.

        /* Element =
        Description = Type */
        ETYPE                   DATATYPE IS TEXT SIZE IS 20.

        /* Element =
        Description = Size */
        ESIZE                   DATATYPE IS SIGNED WORD.

        /* Element =
        Description = Test file structure name */
        TESTSTRUCT              DATATYPE IS TEXT SIZE IS 39.

        END TK_ELEMENT_CDD STRUCTURE.

END TK_ELEMENT.
