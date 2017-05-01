DEFINE RECORD CDD$TOP.UTL.FORM_GROUP

        DESCRIPTION IS /*Form Group Definition*/.

        FORM_GROUP_CDD STRUCTURE.

        /* Element =
        Description = */
        FGROUP                  DATATYPE IS TEXT SIZE IS 16.

        /* Element =
        Description = */
        POINTER                 DATATYPE IS SIGNED LONGWORD.

        /* Element =
        Description = */
        NUMBER                  DATATYPE IS SIGNED LONGWORD.

        END FORM_GROUP_CDD STRUCTURE.

END FORM_GROUP.
