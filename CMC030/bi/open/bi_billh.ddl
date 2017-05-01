DEFINE RECORD CDD$TOP.BI.BI_BILLH

        DESCRIPTION IS /*Insurance Journal Header*/.

        BI_BILLH_CDD STRUCTURE.

        /* Element = INSURED
        Description = Insured number */
        INSURED                 DATATYPE IS TEXT SIZE IS 10.

        /* Element = PATIENT
        Description = Patient Number */
        PATIENT                 DATATYPE IS TEXT SIZE IS 10.

        /* Element = STATIONMAN
        Description = Station man (operator) */
        STATIONMAN              DATATYPE IS TEXT SIZE IS 10.

        /* Element = INVOICE
        Description = Invoice number */
        INVNUM                  DATATYPE IS TEXT SIZE IS 8.

        /* Element = DATE
        Description = Invoice Date (YYYYMMDD) */
        INVDATE                 DATATYPE IS TEXT SIZE IS 8.

        END BI_BILLH_CDD STRUCTURE.

END BI_BILLH.
