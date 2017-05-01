DEFINE RECORD CDD$TOP.PO.PO_NOTES

        DESCRIPTION IS /*Notes file*/.

        PO_NOTES_CDD STRUCTURE.

        /* Element = CODE
        Description = Note Code */
        NOTECODE                DATATYPE IS TEXT SIZE IS 2.

        /* Element = DESCRIPTION
        Description = Description */
        DESCR                   DATATYPE IS TEXT SIZE IS 40.

        END PO_NOTES_CDD STRUCTURE.

END PO_NOTES.
