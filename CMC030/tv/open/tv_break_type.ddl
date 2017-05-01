DEFINE RECORD CDD$TOP.TV.TV_BREAK_TYPE

        DESCRIPTION IS /*TV Break Type File*/.

        TV_BREAK_TYPE_CDD STRUCTURE.

        /* Element =
        Description = Break type (LB, SB, NB, etc.) */
        BTYPE                   DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Description */
        DESCR                   DATATYPE IS TEXT SIZE IS 20.

        /* Element =
        Description = Flag (0-avail, 1-no avail, 2-noavl/int) */
        BFLAG                   DATATYPE IS TEXT SIZE IS 1.

        END TV_BREAK_TYPE_CDD STRUCTURE.

END TV_BREAK_TYPE.
