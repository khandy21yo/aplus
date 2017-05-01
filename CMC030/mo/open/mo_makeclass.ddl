DEFINE RECORD CDD$TOP.MO.MO_MAKECLASS

        DESCRIPTION IS /*Make Class Description Table*/.

        MO_MAKECLASS_CDD STRUCTURE.

        /* Element =
        Description = Class Code */
        CLASS                   DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Class Description */
        DESCR                   DATATYPE IS TEXT SIZE IS 40.

        END MO_MAKECLASS_CDD STRUCTURE.

END MO_MAKECLASS.
