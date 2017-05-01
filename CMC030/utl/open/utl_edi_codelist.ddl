DEFINE RECORD CDD$TOP.UTL.UTL_EDI_CODELIST

        DESCRIPTION IS /*EDI Code List*/.

        UTL_EDI_CODELIST_CDD STRUCTURE.

        /* Element = CODE3
        Description = Data Element Reference Number */
        REFERENCE               DATATYPE IS TEXT SIZE IS 6.

        /* Element = CODE3
        Description = Code */
        CODE                    DATATYPE IS TEXT SIZE IS 4.

        /* Element = DESCR
        Description = Definition */
        DESCR                   DATATYPE IS TEXT SIZE IS 60.

        END UTL_EDI_CODELIST_CDD STRUCTURE.

END UTL_EDI_CODELIST.
