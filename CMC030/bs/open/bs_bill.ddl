DEFINE RECORD CDD$TOP.BS.BS_BILL

        DESCRIPTION IS /*Bill File*/.

        BS_BILL_CDD STRUCTURE.

        /* Element =
        Description = */
        CLIENT                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = */
        PRG                     DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = */
        RATEUOM                 DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = */
        RATE                    DATATYPE IS G_FLOATING.

        /* Element =
        Description = */
        LENGTH                  DATATYPE IS G_FLOATING.

        /* Element =
        Description = */
        AMOUNT                  DATATYPE IS G_FLOATING.

        /* Element =
        Description = */
        INITIALS                DATATYPE IS TEXT SIZE IS 3.

        END BS_BILL_CDD STRUCTURE.

END BS_BILL.
