DEFINE RECORD CDD$TOP.TV.TV_SKED_WORK

        DESCRIPTION IS /*File Used to Hold Commercials to Schedule*/.

        TV_SKED_WORK_CDD STRUCTURE.

        /* Element =
        Description = Priority */
        PRIORITY                DATATYPE IS SIGNED WORD.

        /* Element = TV_FRMNUM
        Description = Form Number */
        FRMNUM                  DATATYPE IS TEXT SIZE IS 8.

        /* Element = TV_CUSNUM
        Description = Customer Number */
        CUSNUM                  DATATYPE IS TEXT SIZE IS 10.

        /* Element =
        Description = Schedule number */
        SKED_NUM                DATATYPE IS TEXT SIZE IS 2.

        END TV_SKED_WORK_CDD STRUCTURE.

END TV_SKED_WORK.
