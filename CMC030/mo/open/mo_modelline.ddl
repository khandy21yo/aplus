DEFINE RECORD CDD$TOP.MO.MO_MODELLINE

        DESCRIPTION IS /*Model Line Master File*/.

        MO_MODELLINE_CDD STRUCTURE.

        /* Element =
        Description = Model Code */
        MODELCODE               DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Make Size */
        MSIZE                   DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Make Class */
        CLASS                   DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Option Group */
        OPTGROUP                DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Option Code */
        OPTN                    DATATYPE IS TEXT SIZE IS 4.

        END MO_MODELLINE_CDD STRUCTURE.

END MO_MODELLINE.
