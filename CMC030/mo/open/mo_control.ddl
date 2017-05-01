DEFINE RECORD CDD$TOP.MO.MO_CONTROL

        DESCRIPTION IS /*Manufacture Order Controling File*/.

        MO_CONTROL_CDD STRUCTURE.

        /* Element = ORDNUM
        Description = Control Order Number */
        ORDNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element = DATE
        Description = Last Close/Purge Date (YYYYMMDD) */
        PURGDATE                DATATYPE IS TEXT SIZE IS 8.

        /* Element =
        Description = Activity status */
        STATUS_FLAG             DATATYPE IS TEXT SIZE IS 1.

        /* Element = INVOICE
        Description = Invoice number */
        LAST_INV                DATATYPE IS TEXT SIZE IS 8.

        END MO_CONTROL_CDD STRUCTURE.

END MO_CONTROL.
