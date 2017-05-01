DEFINE RECORD CDD$TOP.BS.BS_RATE

        DESCRIPTION IS /*Rate File*/.

        BS_RATE_CDD STRUCTURE.

        /* Element =
        Description = */
        PRG                     DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = */
        RATEUOM                 DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = */
        EFFDATE                 DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = */
        RATE                    DATATYPE IS G_FLOATING.

        END BS_RATE_CDD STRUCTURE.

END BS_RATE.
