DEFINE RECORD CDD$TOP.UTL.UTL_EDI_DATAELEMENT

        DESCRIPTION IS /*EDI Data Element Table*/.

        UTL_EDI_DATAELEMENT_CDD STRUCTURE.

        /* Element = CODE3
        Description = Data element reference number */
        REFERENCE               DATATYPE IS TEXT SIZE IS 6.

        /* Element = TITLE
        Description = Data element title */
        TITLE                   DATATYPE IS TEXT SIZE IS 60.

        /* Element =
        Description = Minumum Length */
        MMIN                    DATATYPE IS SIGNED WORD.

        /* Element =
        Description = Maximum Length */
        MMAX                    DATATYPE IS SIGNED WORD.

        /* Element = TYPE
        Description = Type */
        TTYP                    DATATYPE IS TEXT SIZE IS 2.

        END UTL_EDI_DATAELEMENT_CDD STRUCTURE.

END UTL_EDI_DATAELEMENT.
