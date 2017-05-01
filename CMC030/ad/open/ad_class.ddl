DEFINE RECORD CDD$TOP.AD.AD_CLASS

        DESCRIPTION IS /*Asset Classes*/.

        AD_CLASS_CDD STRUCTURE.

        /* Element =
        Description = Asset class */
        ASSCLASS                DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Description */
        DESCRIPTION             DATATYPE IS TEXT SIZE IS 40.

        /* Element =
        Description = Class life */
        LIFE                    DATATYPE IS SIGNED WORD.

        /* Element =
        Description = General depreciation system */
        GDS                     DATATYPE IS SIGNED WORD.

        /* Element =
        Description = Alternative depreciation system */
        ADS                     DATATYPE IS SIGNED WORD.

        END AD_CLASS_CDD STRUCTURE.

END AD_CLASS.
