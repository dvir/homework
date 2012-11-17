#ifndef PGSTUDENT_H
#define PGSTUDENT_H
#include "student.h"
class PGStudent : public Student {
    public:
        PGStudent(std::vector<std::string> data, int elective_courses_count);
        virtual void study(Course& c);
};
#endif
