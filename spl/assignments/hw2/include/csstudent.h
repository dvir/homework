#ifndef CSSTUDENT_H
#define CSSTUDENT_h
#include "student.h"
class CSStudent : public Student {
    public:
        CSStudent(std::vector<std::string> data, int elective_courses_count);
        virtual void study(Course& c);
};
#endif
