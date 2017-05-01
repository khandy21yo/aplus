DEFINE RECORD CDD$TOP.MO.MO_MAKELINE

        DESCRIPTION IS /*Make Template Line Table*/.

        MO_MAKELINE_CDD STRUCTURE.

        /* Element =
        Description = Dealer Model of Make */
        MAKE                    DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Beginning Year for Make (YYYY) */
        YEAR                    DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Type of Make */
        MTYPE                   DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Size of Make */
        MSIZE                   DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Model Code */
        MODELCODE               DATATYPE IS TEXT SIZE IS 4.

        END MO_MAKELINE_CDD STRUCTURE.

END MO_MAKELINE.
