DEFINE RECORD CDD$TOP.PR.PR_SKILLS

        DESCRIPTION IS /*Skill Descriptions*/.

        PR_SKILLS_CDD STRUCTURE.

        /* Element =
        Description = Employee Skill */
        SKILL                   DATATYPE IS TEXT SIZE IS 6.

        /* Element = DESCRIPTION
        Description = Skill Description */
        DESCRIPTION             DATATYPE IS TEXT SIZE IS 40.

        /* Element =
        Description = EEO Sort Order */
        EEOSORT                 DATATYPE IS TEXT SIZE IS 2.

        END PR_SKILLS_CDD STRUCTURE.

END PR_SKILLS.
