DEFINE RECORD CDD$TOP.MO.MO_MAKETYPE

        DESCRIPTION IS /*Make Type Description Table*/.

        MO_MAKETYPE_CDD STRUCTURE.

        /* Element =
        Description = Type Code */
        MTYPE                   DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Type Description */
        DESCR                   DATATYPE IS TEXT SIZE IS 40.

        END MO_MAKETYPE_CDD STRUCTURE.

END MO_MAKETYPE.
