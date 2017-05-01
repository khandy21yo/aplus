DEFINE RECORD CDD$TOP.MO.MO_MODELCODE

        DESCRIPTION IS /*Model Description Table*/.

        MO_MODELCODE_CDD STRUCTURE.

        /* Element =
        Description = Model Code */
        MODELCODE               DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Model Description */
        DESCR                   DATATYPE IS TEXT SIZE IS 40.

        END MO_MODELCODE_CDD STRUCTURE.

END MO_MODELCODE.
