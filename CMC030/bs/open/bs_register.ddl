DEFINE RECORD CDD$TOP.BS.BS_REGISTER

        DESCRIPTION IS /*Register File*/.

        BS_REGISTER_CDD STRUCTURE.

        /* Element =
        Description = */
        CLIENT                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = */
        PRG                     DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = */
        PERIOD                  DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = */
        RATEUOM                 DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = */
        INITIALS                DATATYPE IS TEXT SIZE IS 3.

        /* Element =
        Description = */
        LENGTH                  DATATYPE IS G_FLOATING.

        /* Element =
        Description = */
        AMOUNT                  DATATYPE IS G_FLOATING.

        /* Element =
        Description = */
        POSTDATE                DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = */
        POSTTIME                DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = */
        BATCH                   DATATYPE IS TEXT SIZE IS 6.

        END BS_REGISTER_CDD STRUCTURE.

END BS_REGISTER.
