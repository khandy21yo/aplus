DEFINE RECORD CDD$TOP.MO.MO_MAKESIZE

        DESCRIPTION IS /*Make Size Description Table*/.

        MO_MAKESIZE_CDD STRUCTURE.

        /* Element =
        Description = Make Size Code */
        MSIZE                   DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Make Size Code Description */
        DESCR                   DATATYPE IS TEXT SIZE IS 40.

        END MO_MAKESIZE_CDD STRUCTURE.

END MO_MAKESIZE.
