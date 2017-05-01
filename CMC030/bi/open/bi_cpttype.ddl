DEFINE RECORD CDD$TOP.BI.BI_CPTTYPE

        DESCRIPTION IS /*CPT Type and Account Number Table*/.

        BI_CPTTYPE_CDD STRUCTURE.

        /* Element = CPTTYPE
        Description = CPT Type */
        CPTTYPE                 DATATYPE IS TEXT SIZE IS 2.

        /* Element =
        Description = Description */
        DESCRIPTION             DATATYPE IS TEXT SIZE IS 40.

        /* Element = ACCOUNT
        Description = General Ledger Account Number */
        ACCOUNT                 DATATYPE IS TEXT SIZE IS 18.

        END BI_CPTTYPE_CDD STRUCTURE.

END BI_CPTTYPE.
