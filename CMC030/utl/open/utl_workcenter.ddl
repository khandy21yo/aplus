DEFINE RECORD CDD$TOP.UTL.UTL_WORKCENTER

        DESCRIPTION IS /*Work Center Profile*/.

        UTL_WORKCENTER_CDD STRUCTURE.

        /* Element = LOCATION
        Description = Location number */
        LOCATION                DATATYPE IS TEXT SIZE IS 4.

        /* Element = DEPT_NUM
        Description = Department number */
        DEPT_NUM                DATATYPE IS TEXT SIZE IS 6.

        /* Element = WORK_CENTER
        Description = Work Center */
        WORK_CENTER             DATATYPE IS TEXT SIZE IS 4.

        /* Element =
        Description = Description */
        DESCRIPTION             DATATYPE IS TEXT SIZE IS 40.

        /* Element = PHONE
        Description = Phone number */
        PHONE                   DATATYPE IS TEXT SIZE IS 10.

        END UTL_WORKCENTER_CDD STRUCTURE.

END UTL_WORKCENTER.
