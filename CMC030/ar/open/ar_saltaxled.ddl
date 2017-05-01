DEFINE RECORD CDD$TOP.AR.AR_SALTAXLED

        DESCRIPTION IS /*Sales Tax Monthly Report File*/.

        AR_SALTAXLED_CDD STRUCTURE.

        /* Element =
        Description = Tax Type */
        TAXTYP                  DATATYPE IS TEXT SIZE IS 1.

        /* Element = CUSTOMER
        Description = Customer Number */
        CUSNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element = INVOICE
        Description = Invoice number */
        INVNUM                  DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Sales Tax Amount */
        AMOUNT                  DATATYPE IS G_FLOATING.

        /* Element = BATCH
        Description = Batch number used for posting */
        BATCH                   DATATYPE IS TEXT SIZE IS 6.

        /* Element =
        Description = Transaction date */
        TRADAT                  DATATYPE IS TEXT SIZE IS 8.

        END AR_SALTAXLED_CDD STRUCTURE.

END AR_SALTAXLED.
