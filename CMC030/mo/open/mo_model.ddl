DEFINE RECORD CDD$TOP.MO.MO_MODEL

        DESCRIPTION IS /*Model Base Definition File*/.

        MO_MODEL_CDD STRUCTURE.

        /* Element =
        Description = Model Code */
        MODELCODE               DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Make Size */
        MSIZE                   DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Make Class */
        CLASS                   DATATYPE IS TEXT SIZE IS 4.

        /* Element = PRODUCT
        Description = Product Number */
        PRODUCT                 DATATYPE IS TEXT SIZE IS 14.

        /* Element = PRODUCT
        Description = Box Product Number */
        BPRODUCT                DATATYPE IS TEXT SIZE IS 14.

        END MO_MODEL_CDD STRUCTURE.

END MO_MODEL.
